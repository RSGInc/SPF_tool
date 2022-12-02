import os
import numpy as np
import pandas as pd
from tools.legacy_tools.joint_tours import Joint_tour
from collections import defaultdict
from core.functions import add_quote_char

HOUSEHOLD_COLUMNS = pd.read_csv(os.path.join(os.path.dirname(__file__), 'static/household_columns.csv'),
                                index_col="key", names=["key", "name"], header=None).name.to_dict()

class Household:
    """Household class"""

    def __init__(self, hh, constants):
        self.fields = {'HH_ID': hh.SAMPN}
        # self.hh_id = hh_id
        # TODO: could use OrderedDict instead for possibly improved efficiency
        self.persons = ([])

        # Populate fields
        for col in set(hh.index).intersection(HOUSEHOLD_COLUMNS.values()):
            self.fields[col] = hh[col]

        # dict that allow multiple values for each key
        self.joint_episodes = defaultdict(list)

        # trip departure time (in minutes) is mapped to a list of Joint_trip objects
        self.unique_jt_groups = (
            []
        )  # each list element corresponds to trips jointly made by a travel group
        self.unique_jtours = []  # each list element corresponds to a Joint_tour object
        self.error_flag = False
        self.tags = []
        self.recode_tags = []
        self.constants = constants

    # def print_header(fp):
    #     _header = ["HH_ID", "NUM_PERS", "AREA"]
    #     fp.write(",".join(["%s" % field for field in _header]) + "\n")
    #
    # def print_vals(self, fp):
    #     _vals = [self.hh_id, len(self.persons)]
    #     fp.write(",".join(["%s" % v for v in _vals]) + "\n")


    def print_header(fp):
        # fp.write(','.join(['%s' %field for field in self.fields.keys()])+'\n')
        _header = []
        # TODO: save a sorted copy of the dict to avoid repeated sorting
        for _col_num, _col_name in sorted(HOUSEHOLD_COLUMNS.items()):
            _header.append(_col_name)
        fp.write(",".join(["%s" % name for name in _header]) + "\n")

    def print_vals(self, fp):
        # fp.write(','.join(['%s' %value for value in self.fields.values()])+'\n')
        self.fields['NUM_PERS'] = len(self.persons)

        if "ERROR" in self.fields:
            self.fields["ERROR"] = add_quote_char(self.fields["ERROR"])
        _vals = []
        for _col_num, _col_name in sorted(HOUSEHOLD_COLUMNS.items()):
            if _col_name in self.fields:
                _vals.append(self.fields[_col_name])
            else:
                _vals.append(np.NAN)
        fp.write(",".join(["%s" % value for value in _vals]) + "\n")


    def get_id(self):
        return self.fields['HH_ID']

    def add_person(self, person):
        self.persons.append(person)

    def add_joint_episodes(self, joint_episodes):
        for _jt in joint_episodes:
            self.joint_episodes[_jt.get_depart_time()].append(_jt)

    def get_person(self, per_id):
        """return the Person object with the given person id"""
        for _person in self.persons:
            if _person.per_id == per_id:
                return _person

    def log_recode(self, msg):
        self.recode_tags.append(msg)

    def log_warning(self, msg):
        self.tags.append(msg)

    def log_error(self, err_msg=None):
        self.error_flag = True
        if err_msg:
            self.tags.append(err_msg)

    def print_tags(self, fp):
        fp.write("HH_ID={}\n".format(self.get_id()))
        for _tag in self.tags:
            fp.write("\t" + _tag + "\n")

    def print_recode_tags(self, fp):
        for _tag in self.recode_tags:
            fp.write("HH_ID={}\t".format(self.get_id()))
            fp.write(_tag + "\n")

    def _error_num_joint_episodes(self, jt_list):
        """check if there are enough joint trips to make up the reported travel group(s)"""
        """ joint trips in the list are assumed to have been sorted by travel party        """
        _start_ix = int(0)
        _max_ix = len(jt_list)
        _error = False
        # joint trip objects departing at this time do not necessarily correspond to the same travel episode
        while _start_ix < _max_ix:
            # given the reported travel party size of the current joint trip object
            # check if there are this many joint trips remaining in the list
            _size = jt_list[_start_ix].number_hh  # size of travel party
            _stop_ix = int(_start_ix + _size)
            if _stop_ix > _max_ix:
                # there are fewer joint trip entries than expected
                _error = True
                break
            _start_ix = _stop_ix

        return _error

    def process_joint_episodes(self):

        COMPOSITION = self.constants.get("COMPOSITION")

        _dep_time_list = sorted(
            self.joint_episodes.keys()
        )  # sorted list of joint-trip departure times as key values
        # process each dep_time entry at a time
        # assign each consistent group of unlinked trip entries a unique joint trip id
        _joint_ultrip_id = 0
        # _d = 0                          #index into _dep_time_list
        _d = -1  # index into _dep_time_list
        _d_max = len(_dep_time_list)

        # while _d < _d_max:
        while (_d + 1) < _d_max:
            # move list index on to the next departure time group
            _d = _d + 1

            _dep_time = _dep_time_list[_d]
            _jt_list = self.joint_episodes[
                _dep_time
            ]  # _jt_list includes all household members' joint travels departing at this time

            _jt_list.sort(
                key=lambda jt: jt.travel_party
            )  # sort joint trip objects by reported travel party
            _error_merge = False

            #
            # Attempt to resolve inconsistency in reported departure
            # times by merging trip entries departing within the error time buffer
            # Assuming that reported number of participants are accurate
            #
            if self._error_num_joint_episodes(
                _jt_list
            ):  # number of joint trips not matching up with the travel size(s) reported
                # try merge with the next lot of trips of "similar" departure time
                """
                _error_merge = True #<--remove this once counting is done and uncommented the following if-else!
                """
                if (_d + 1) < _d_max:
                    _next_dep_time = _dep_time_list[_d + 1]
                    if (_next_dep_time - _dep_time) <= self.constants.get(
                        "NEGLIGIBLE_DELTA_TIME"
                    ):
                        _tmp_jt_list = _jt_list.copy()
                        _tmp_jt_list.extend(
                            self.joint_episodes[_next_dep_time]
                        )  # try merge two lists
                        _tmp_jt_list.sort(
                            key=lambda jt: (jt.travel_party, jt.depart_time)
                        )  # sort by reported travel party THEN by departure time
                        # check if the merged list has enough trip records
                        if not self._error_num_joint_episodes(_tmp_jt_list):
                            self.log_recode(
                                "Inconsistent departure times: trips departing at {}:{} "
                                "and {}:{} are assumed to be made together".format(
                                    _dep_time // 60,
                                    _dep_time % 60,
                                    _next_dep_time // 60,
                                    _next_dep_time % 60,
                                )
                            )
                            # merged list has enough trip records, replace _jt_list with the merged list
                            _jt_list = _tmp_jt_list
                            # advance down the departure time list
                            _d = _d + 1
                            _dep_time = _next_dep_time
                        else:
                            # merging did not help resolve the issue
                            _error_merge = True
                    else:
                        # departure times not close enough for merging
                        _error_merge = True
                else:
                    # no trips to be considered for merging
                    _error_merge = True

            if _error_merge:
                # there are fewer joint trip entries than expected
                msg = (
                    f"Number of joint trips departing at {_dep_time // 60}:{_dep_time % 60} "
                    f"differs from at least one of the reported travel group size"
                )
                # self.log_error()
                for _jt in _jt_list:
                    _jt.log_error(msg)
                continue

            # Scan through the joint trips to identify any errors in reported
            # number of hh members or reported travel participants
            _start_ix = int(0)
            _max_ix = len(_jt_list)
            _error_travel_party = False
            _error_number_hh = False
            # joint trip objects departing at this time do not necessarily correspond to the same travel episode
            while _start_ix < _max_ix:
                _size = _jt_list[
                    _start_ix
                ].number_hh  # number of hh members reported by the entry of the group
                _stop_ix = int(_start_ix + _size)
                # make pair-wise comparison among joint trip entries of the current travel group to verify consistency
                for _i in range(_start_ix, _stop_ix - 1):
                    if _jt_list[_i].number_hh == _jt_list[_i + 1].number_hh:
                        # reported number of travelers is consistent
                        if _jt_list[_i].travel_party == _jt_list[_i + 1].travel_party:
                            # reported travel party is consistent
                            pass
                        else:
                            # reported travel party is inconsistent
                            _error_travel_party = True
                    else:
                        # reported number of travelers is inconsistent
                        _error_number_hh = True

                # Attempt to resolve inconsistency in reported travel party
                # Assuming that reported departure times and number of participants are accurate
                if _error_travel_party & (not _error_number_hh):

                    # merge the travel party entries across all joint trips
                    # if the merged set has the same number of participants as reported in number_hh
                    # then use the merged set as the 'true' set to continue
                    # otherwise, log as an error and break from loop
                    _set_participants = set(
                        _jt_list[_start_ix].travel_party
                    )  # union of the reported participants
                    _set_persons = {
                        _jt_list[_start_ix].get_per_id()
                    }  # ID of persons who made the trips
                    for _i in range(_start_ix + 1, _stop_ix):
                        _set_participants |= set(_jt_list[_i].travel_party)
                        _set_persons |= {_jt_list[_i].get_per_id()}
                    if len(_set_participants) == _jt_list[_start_ix].number_hh:
                        # use the merged set of participants as the 'true' set
                        for _i in range(_start_ix, _stop_ix):
                            _jt_list[_i].recode_travel_party(_set_participants)
                    elif len(_set_persons) == _jt_list[_start_ix].number_hh:
                        # use the list of persons who made the trips as the 'true' set
                        for _i in range(_start_ix, _stop_ix):
                            _jt_list[_i].recode_travel_party(_set_persons)
                    else:
                        msg = "Joint trips departing at {}:{} reported inconsistent participants".format(
                            _dep_time // 60, _dep_time % 60
                        )
                        # self.log_error(msg)
                        for _jt in _jt_list:
                            _jt.log_error(msg)
                        break

                # Attempt to resolve inconsistency in reported number of participants
                # Assuming that reported departure times and travel participants are accurate
                if _error_number_hh & (not _error_travel_party):
                    # 'fix' the incorrect number_hh and continue
                    _number_travelers = len(_jt_list[_start_ix].travel_party)
                    for _i in range(_start_ix, _stop_ix):
                        _jt_list[_i].recode_number_travelers(_number_travelers)

                # inconsistencies found in both reported number of participants and reported travel participants
                if _error_number_hh & _error_travel_party:
                    # log as an error and break from loop, for now
                    msg = (
                        f"Joint trips departing at {_dep_time // 60}:{_dep_time % 60} "
                        f"reported inconsistent info about participants & travel group sizes"
                    )
                    # self.log_error(msg)
                    for _jt in _jt_list:
                        _jt.log_error(msg)
                    break

                # if reach this point, a consistent subset of joint episode entries have been found for a travel group
                # increase the id value by 1
                _joint_ultrip_id = _joint_ultrip_id + 1

                # add the current slice of joint episode entries as a joint travel episode/group
                self.unique_jt_groups.append(_jt_list[_start_ix:_stop_ix])

                # look up the driver of the group, if any
                # also, check presence of adult and children in travel party
                _driver_ix = (
                    None  # index where the driver of the joint travel party is found
                )
                _num_adults = 0
                for _i in range(_start_ix, _stop_ix):
                    if _jt_list[_i].driver > 0:
                        _driver_ix = _i
                    _num_adults = _num_adults + int(
                        _jt_list[_i].parent_trip.per_obj.get_is_adult()
                    )

                # TODO: verify that a driver is indeed found for an auto joint trip

                # derive travel party composition
                _composition = COMPOSITION["MIXED"]
                if _num_adults == _size:
                    # number of adults equals number of people in the travel party
                    _composition = COMPOSITION["ALL_ADULTS"]
                elif _num_adults == 0:
                    # no adults
                    _composition = COMPOSITION["ALL_CHILDREN"]

                # go back and update the joint-unlinked-trip records
                for _i in range(_start_ix, _stop_ix):
                    # joint ultrip id
                    _jt_list[_i].set_joint_ultrip_id(_joint_ultrip_id)
                    # chauffuer id
                    if (_driver_ix is not None) & (
                        _i != _driver_ix
                    ):  # skip the driver him/her-self
                        _jt_list[_i].set_chauffuer(_jt_list[_driver_ix].driver)
                    # travel group composition
                    _jt_list[_i].set_composition(_composition)

                # point to the start of the next group of joint trips
                _start_ix = _stop_ix
            # end-while loop for processing each travel group
        # end-for loop for processing each departure time
        # now, go through all the person trips and determine all joint legs of a given trip are properly joint
        for p in self.persons:
            for tour in p.tours:
                for trip in tour.trips:
                    trip.set_jt_grouping_status()

    def process_joint_tours(self):
        """determine partially vs fully joint tour"""
        # a tour is fully joint if all the constituting trips are joint-grouped and made by the same group of people
        JOINT_CAT = self.constants.get("JOINT_CAT")

        _jt_dict = (
            dict()
        )  # dictionary with jt_id as the key and ([per ids],[tour ids],[trip ids], [tour lengths]) as the value
        _start_jtour = (
            -1
        )  # index into unique_jt_groups that marks the start of potentially a fully joint tour
        _end_jtour = (
            -1
        )  # index into unique_jt_groups that marks the end of a fully joint tour
        _num_seq_jtrips = (
            0  # number of sequential joint trips found so far in a joint tour
        )
        _num_jtours = 0  # number of joint tours found so far
        _prev_jt = ([], [])
        for _jt_group_idx, _cur_jt_group in enumerate(self.unique_jt_groups):
            # _cur_jt_group is a list of joint episodes that were made together by household members

            # first check if the parent trips are all joint and grouped.
            # if not, stop processing this group of joint travel
            _all_grouped = True
            for _jt in _cur_jt_group:
                if not (
                    _jt.parent_trip.get_joint_status() == JOINT_CAT["JOINT-GROUPED"]
                ):
                    _all_grouped = False
            if not _all_grouped:
                continue

            # all person trips involved are grouped. now check how they match up at the tour level
            # refresh lists for the current group of joint trips
            _pids = []
            _tour_ids = []
            _trip_ids = []
            _tour_lens = []
            for _jt in _cur_jt_group:
                # _jt is a Joint_ultrip object associated with a person in the travel party
                _pids.append(_jt.parent_trip.get_per_id())
                _tour_ids.append(_jt.parent_trip.get_tour_id())
                _trip_ids.append(_jt.parent_trip.get_id())
                _tour_lens.append(len(_jt.parent_trip.tour_obj.trips))

            # necessary condition for a joint episode group to be the start of a joint tour:
            # (1) trip_id the same and equal to 1 for all participants
            # (2) length of corresponding tour is the same for all participants
            if (
                (len(set(_trip_ids)) == 1)
                & (_trip_ids[0] == 1)
                & (len(set(_tour_lens)) == 1)
            ):
                _start_jtour = _jt_group_idx
                _num_seq_jtrips = 1
            # necessary condition for a joint episode group to be part
            # of the same joint tour as the previous joint episode:
            # (1)count of sequential number of joint episode is greater than 0
            # (2)identical set of person id and tour id as the previous joint episode
            # (3)trip_id the same for all participants and all equal to the previous trip's id plus 1
            elif (
                (_num_seq_jtrips > 0)
                & ((_pids, _tour_ids) == _prev_jt)
                & (len(set(_trip_ids)) == 1)
                & (_trip_ids[0] == _num_seq_jtrips + 1)
            ):
                _num_seq_jtrips = _num_seq_jtrips + 1
                if _num_seq_jtrips == _tour_lens[0]:
                    # this is the last episode in the joint tour
                    _end_jtour = _jt_group_idx
                    _num_jtours = _num_jtours + 1
                    self.set_joint_tour(
                        _num_jtours,
                        self.unique_jt_groups[_start_jtour : _end_jtour + 1],
                    )
                    # reset markers
                    _start_jtour = -1
                    _end_jtour = -1
                    _num_seq_jtrips = 0
            else:  # this joint trip is part of a partially joint tour
                # reset markers
                _start_jtour = -1
                _end_jtour = -1
                _num_seq_jtrips = 0

            _prev_jt = (_pids, _tour_ids)

        # now that all fully joint tours have been identified, go back and classify each tour as one of the following:
        # (1) independent tours:        all trips.JOINT==NOT-JOINT
        # (2) partially-joint tours:    all trips.JOINT<>JOINT; some are FULLY-JOINT, some are NOT-JOINT
        # (3) fully-joint tours:        tour.get_is_fully_joint()==True
        # (4) partially-joint problematic tours: some trips are NOT-JOINT, some are JOINT (not grouped)
        # (5) jointness-unclear tours :    no NOT-JOINT, some are JOINT
        for psn in self.persons:
            for tour in psn.tours:
                tour.set_joint_status()

    def set_joint_tour(self, jtour_id, jt_groups):
        """label join trips in the jt_groups list as being part of a fully joint tour with an id of jtour_id"""
        for _jt_group in jt_groups:
            for _jt in _jt_group:
                _jt.set_jtour_id(jtour_id)
        self.unique_jtours.append(Joint_tour(jtour_id, jt_groups, self.constants))

    def process_escort_trips(self):
        ESCORT_EVENT = self.constants.get("ESCORT_EVENT")

        # scan through each unique joint episode group
        for _jt_group in self.unique_jt_groups:
            # find the driver in the group, if any
            _driver = set([_jt.driver for _jt in _jt_group])
            _num_driver = len(_driver)
            _driver_id = list(_driver)[0]
            _escorted_pers = set()  # ID of hh members who are escorted
            _escorting_jt = (
                set()
            )  # Joint_ultrip objects of hh members who are not the driver but also escorting

            if (_num_driver == 1) & (_driver_id > 0):  # TODO: prevent driver id==nan
                # there is exactly one driver on this unlinked joint trip
                # locate the driver's joint trip to see if joint trip was associated with a drop-off or pick-up activity
                _drop_off = False
                _pick_up = False
                _driver_jt = None
                for _jt in _jt_group:
                    if _jt.get_per_id() == _driver_id:
                        # found driver's trip
                        _drop_off = (
                            _jt.get_dest_escort_pudo() == ESCORT_EVENT["DROP_OFF"]
                        )
                        _pick_up = _jt.get_orig_escort_pudo() == ESCORT_EVENT["PICK_UP"]
                        _driver_jt = _jt
                        break

                if _drop_off:
                    # if trip was for dropping off
                    # scan through joint trip record for the passengers
                    for _jt in _jt_group:
                        if _jt.parent_trip.get_is_escorting():
                            # hh member who is escorting (could be the driver)
                            _escorting_jt.add(_jt)
                        else:
                            # found a person being dropped off
                            _escorted_pers.add(
                                _jt.get_per_id()
                            )  # add person to the driver's set of escorted persons
                            # set person's trip as being dropped off at destination
                            _jt.parent_trip.set_escorted(ESCORT_EVENT["DROP_OFF"])

                if _pick_up:
                    # if trip was for picking up, the person(s) being picked up is given by
                    # the difference between the travel party at the origin and
                    # travel party at the destination of the current trip
                    # note that the previous trip may not be a joint travel
                    _prev_set = set(_driver_jt.get_orig_travel_party())
                    _cur_set = set(_driver_jt.get_travel_party())
                    _set_diff = (
                        _cur_set - _prev_set
                    )  # persons being picked up at trip origin
                    _escorted_pers = _escorted_pers | _set_diff
                    # set escorted person's trip as being picked up at origin
                    for _jt in _jt_group:
                        if _jt.get_per_id() in _set_diff:
                            # found a person being picked up
                            _jt.parent_trip.set_escorted(ESCORT_EVENT["PICK_UP"])
                        else:
                            # otherwise person is escorting -> add to the set of escorting trips
                            _escorting_jt.add(_jt)

                if _driver_id in _escorted_pers:
                    _escorted_pers.remove(_driver_id)

                # set escorting-related fields in driver's and any other escorting individuals' trip
                if _drop_off:
                    _pudo = ESCORT_EVENT["DROP_OFF"]
                elif _pick_up:
                    _pudo = ESCORT_EVENT["PICK_UP"]
                else:
                    _pudo = ESCORT_EVENT["NEITHER"]
                for _jt in _escorting_jt:
                    _jt.parent_trip.set_escorting(_pudo, _escorted_pers)

            elif _num_driver > 1:
                self.log_error(
                    "more than one driver found for joint trip #{}".format(
                        _jt_group[0].jt_id
                    )
                )

    def process_escort_tours(self):
        for _per in self.persons:
            for _tour in _per.tours:
                _tour.set_escorting_fields()
                _tour.set_escorted_fields()

    def print_joint_trips(self, fp):
        _dep_time_list = sorted(
            self.joint_episodes.keys()
        )  # sorted list of departure times as key values
        # print in order of dep_time
        for _dep_time in _dep_time_list:
            _jt_list = self.joint_episodes[_dep_time]
            for _jt in _jt_list:
                _jt.print_vals(fp)

    def print_unique_joint_trips(self, fp):
        for (
            _jt_gp
        ) in (
            self.unique_jt_groups
        ):  # each item in the list is itself a list of joint trip objects
            _jt_gp[0].print_vals_unique(
                fp
            )  # print from the first joint trip (the same if any arbitrary joint trip in the group)

    def print_unique_joint_tours(self, fp):
        for (
            _jtour
        ) in self.unique_jtours:  # each item in the list is a Joint_tour object
            _jtour.print_vals(fp)
