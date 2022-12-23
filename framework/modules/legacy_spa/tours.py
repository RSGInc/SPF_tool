import pandas as pd
import numpy as np
import math
import os
from core.utils import calculate_duration, convert2minutes, add_quote_char

TOUR_COLUMNS = pd.read_csv(os.path.join(os.path.dirname(__file__), 'static/tour_columns.csv'),
                                index_col="key", names=["key", "name"], header=None).name.to_dict()

class Tour:
    """Tour class"""

    def __init__(self, hh_obj, per_obj, constants, tour_id, day_id, is_subtour=0, trips=None):
        self.hh_obj = hh_obj
        self.per_obj = per_obj
        self.tour_id = int(tour_id)
        self.is_AW_subtour = is_subtour
        self.trips = []


        if trips is not None:
            # this is a AW_subtour
            # re-set trips' tour_obj to the current tour & reset ID field
            for _trip in trips:
                _trip.tour_obj = self
                _trip.fields["TOUR_ID"] = self.tour_id
            # add trips to this tour's list of trips
            self.trips.extend(trips)
            # renumber trips is needed when tour is created for a subtour
            self.renumber_trips()

        self.fields = {
            "HH_ID": hh_obj.get_id(),
            "PER_ID": per_obj.get_id(),
            "TOUR_ID": self.tour_id,
            "DAYNO": day_id,
            "IS_SUBTOUR": self.is_AW_subtour,
            "FULLY_JOINT": 0,
        }

        self.error_flag = False
        self.constants = constants

        self.set_partial(
            constants.get("PARTIAL_TOUR")["NOT_PARTIAL"]
        )  # set as default value

    def add_trip(self, trip):
        self.trips.append(trip)

    def add_trips(self, trip_list):
        # add the new trips from trip_list to the end of the existing list of trips
        self.trips.extend(trip_list)

    def renumber_trips(self):
        """renumber the trips in the trip list based on their physical order in the list"""
        for _i, _trip in enumerate(self.trips):  # trip index starts with 0
            _trip.set_id(_i + 1)  # trip id starts with 1

    def get_id(self):
        return self.tour_id

    def get_trip(self, trip_id):
        # trips in the list are sequentially numbered from 1
        return self.trips[trip_id - 1]

    def get_mode(self):
        if "TOURMODE" in self.fields:  # make sure the key exists
            return self.fields["TOURMODE"]

    def get_purp(self):
        if "TOURPURP" in self.fields:  # make sure the key exists
            return self.fields["TOURPURP"]

    def contains_joint_trip(self, jtrip_id):
        val = False
        for _trip in self.trips:
            if _trip.get_jtripID() == jtrip_id:
                val = True
                break
            else:
                val = False
        return val

    def get_is_escorting(self):
        escorting = 0
        for _trip in self.trips:
            if _trip.get_is_escorting():
                escorting = 1
                break
        return escorting

    def get_partial_status(self):
        PARTIAL_TOUR = self.constants.get("PARTIAL_TOUR")

        if "PARTIAL_TOUR" in self.fields:  # make sure the key exists
            return self.fields["PARTIAL_TOUR"]
        else:
            return PARTIAL_TOUR["NOT_PARTIAL"]

    def get_is_subtour(self):
        _is_subtour = 0
        if "IS_SUBTOUR" in self.fields:  # make sure the key exists
            if self.fields["IS_SUBTOUR"] == 1:
                _is_subtour = 1
        return _is_subtour

    def get_outbound_stops(self):
        if "OUTBOUND_STOPS" in self.fields:  # make sure the key exists
            return self.fields["OUTBOUND_STOPS"]
        else:
            return None  # TODO: this would be error

    def get_inbound_stops(self):
        if "INBOUND_STOPS" in self.fields:  # make sure the key exists
            return self.fields["INBOUND_STOPS"]
        else:
            return None  # TODO: this would be error

    def get_num_trips(self):
        return len(self.trips)

    def get_is_fully_joint(self):
        fully_joint = 0
        if "FULLY_JOINT" in self.fields:  # make sure the key exists
            if self.fields["FULLY_JOINT"] == 1:
                fully_joint = 1
        return fully_joint

    def log_error(self, err_msg=None):
        self.error_flag = True
        self.fields["ERROR"] = "E: "
        if err_msg:
            self.hh_obj.log_error(
                "\t Person#{} Tour#{}: ".format(self.per_obj.get_id(), self.tour_id)
                + err_msg
            )
            self.fields["ERROR"] = self.fields["ERROR"] + err_msg

    def _calc_tour_mode(self):
        TRANSIT_MODES = self.constants.get("TRANSIT_MODES")
        TRIP_MODE = self.constants.get("TRIPMODE")

        # List is used for potential grouping implementation
        TOUR_MODE = self.constants.get("TOURMODE")
        # TOUR_MODE = {k: v if isinstance(v, list) else [v] for k, v in self.constants.get("TOUR_MODE").items()}

        TRIP_MODE_INVERSE = {v: k for k, v in TRIP_MODE.items()}
        TRANSIT_MODE_NAMES = [TRIP_MODE_INVERSE[x] for x in TRANSIT_MODES]

        _tour_mode = -1

        # put the modes used on all trips in one list
        trip_df = pd.DataFrame(
            [{k: x.fields.get(k) for k in ["TRIPMODE", "DIST"]} for x in self.trips]
        )
        _modes_used = set(trip_df.TRIPMODE.to_list())

        # If any of the modes are transit, it's a transit tour
        if len(_modes_used.intersection(TRANSIT_MODE_NAMES)) > 0:
            _modes_used = _modes_used.intersection(TRANSIT_MODE_NAMES)

        # If still multiple modes, choose longest. If tie, choose first
        if len(_modes_used) > 1:
            _tour_mode = [trip_df[trip_df.DIST == trip_df.DIST.max()].iloc[0].TRIPMODE.astype(int)]
            # _tour_mode = [k for k, v in TRIP_MODE.items() if v == _new_mode]
        else:
            _tour_mode = list(_modes_used)

        assert len(_tour_mode) == 1, "Multiple matching modes!"

        _tour_mode = _tour_mode[0]

        return _tour_mode

    def _get_is_driver(self):
        _is_driver = int(0)
        for _trip in self.trips:
            if (
                "ISDRIVER" in _trip.fields
            ):  # make sure that the key exists before looking up its value
                if _trip.fields["ISDRIVER"] > 0:
                    _is_driver = 1
                    break
        return _is_driver

    def _set_outbound_stops(self, first_ob_trip, last_ob_trip):
        _num_ob_stops = last_ob_trip - first_ob_trip
        self.fields["OUTBOUND_STOPS"] = _num_ob_stops
        for _j in range(0, _num_ob_stops):
            # set field values for each outbound stop
            _label = "OSTOP_" + str(_j + 1) + "_"
            self.fields[_label + "PLACENO"] = self.trips[_j].fields["DEST_PLACENO"]
            self.fields[_label + "X"] = self.trips[_j].fields["DEST_X"]
            self.fields[_label + "Y"] = self.trips[_j].fields["DEST_Y"]
            self.fields[_label + "ARR_HR"] = self.trips[_j].fields["DEST_ARR_HR"]
            self.fields[_label + "ARR_MIN"] = self.trips[_j].fields["DEST_ARR_MIN"]
            self.fields[_label + "ARR_BIN"] = self.trips[_j].fields["DEST_ARR_BIN"]
            self.fields[_label + "DEP_HR"] = self.trips[_j].fields["DEST_DEP_HR"]
            self.fields[_label + "DEP_MIN"] = self.trips[_j].fields["DEST_DEP_MIN"]
            self.fields[_label + "DEP_BIN"] = self.trips[_j].fields["DEST_DEP_BIN"]

            # calculate stop duration based on arrival and departure at stop
            (
                self.fields[_label + "DUR_HR"],
                self.fields[_label + "DUR_MIN"],
            ) = calculate_duration(
                self.fields[_label + "ARR_HR"],
                self.fields[_label + "ARR_MIN"],
                self.fields[_label + "DEP_HR"],
                self.fields[_label + "DEP_MIN"],
            )
            self.fields[_label + "DUR_BIN"] = (
                1 + self.fields[_label + "DEP_BIN"] - self.fields[_label + "ARR_BIN"]
            )

            self.fields[_label + "PURP"] = self.trips[_j].fields["DEST_PURP"]
            self.fields[_label + "MODE"] = self.trips[_j].fields["TRIPMODE"]
            self.fields[_label + "PUDO"] = self.trips[_j].fields["DEST_ESCORTING"]

            # set the stop number in the trip record too
            self.trips[_j].set_stop_number_on_half_tour(_j + 1)

        # if _num_ob_stops>4:
        #    self.log_error("found {} outbound stops".format(_num_ob_stops))

    def _set_inbound_stops(self, first_ib_trip, last_ib_trip):
        _num_ib_stops = int(last_ib_trip - first_ib_trip)
        self.fields["INBOUND_STOPS"] = _num_ib_stops
        for _j in range(first_ib_trip, last_ib_trip):
            # set field values for each inbound stop
            _label = "ISTOP_" + str(_j - first_ib_trip + 1) + "_"
            self.fields[_label + "PLACENO"] = self.trips[_j].fields["DEST_PLACENO"]
            self.fields[_label + "X"] = self.trips[_j].fields["DEST_X"]
            self.fields[_label + "Y"] = self.trips[_j].fields["DEST_Y"]
            self.fields[_label + "ARR_HR"] = self.trips[_j].fields["DEST_ARR_HR"]
            self.fields[_label + "ARR_MIN"] = self.trips[_j].fields["DEST_ARR_MIN"]
            self.fields[_label + "ARR_BIN"] = self.trips[_j].fields["DEST_ARR_BIN"]
            self.fields[_label + "DEP_HR"] = self.trips[_j].fields["DEST_DEP_HR"]
            self.fields[_label + "DEP_MIN"] = self.trips[_j].fields["DEST_DEP_MIN"]
            self.fields[_label + "DEP_BIN"] = self.trips[_j].fields["DEST_DEP_BIN"]
            # calculate stop duration based on arrival and departure at stop
            (
                self.fields[_label + "DUR_HR"],
                self.fields[_label + "DUR_MIN"],
            ) = calculate_duration(
                self.fields[_label + "ARR_HR"],
                self.fields[_label + "ARR_MIN"],
                self.fields[_label + "DEP_HR"],
                self.fields[_label + "DEP_MIN"],
            )
            self.fields[_label + "DUR_BIN"] = (
                1 + self.fields[_label + "DEP_BIN"] - self.fields[_label + "ARR_BIN"]
            )

            self.fields[_label + "PURP"] = self.trips[_j].fields["DEST_PURP"]
            self.fields[_label + "MODE"] = self.trips[_j].fields["TRIPMODE"]
            self.fields[_label + "PUDO"] = self.trips[_j].fields["DEST_ESCORTING"]

            # set the stop number in the trip record too
            self.trips[_j].set_stop_number_on_half_tour(_j - first_ib_trip + 1)

        # if _num_ib_stops>4:
        #    self.log_error("found {} inbound stops".format(_num_ib_stops))

    def populate_attributes(self):
        """determine tour attributes based on its constituting trips"""
        PARTIAL_TOUR = self.constants.get("PARTIAL_TOUR")
        PURPOSE = self.constants.get("PURPOSE")

        if self.get_partial_status() == PARTIAL_TOUR["PARTIAL_START"]:
            # assume primary destination is the origin of first trip of the day
            _prim_purp = self.trips[0].get_orig_purpose()
            self.fields["TOURPURP"] = _prim_purp

            # tour mode
            self.fields["TOURMODE"] = self._calc_tour_mode()

            # is person a driver on any trip
            self.fields["DRIVER"] = self._get_is_driver()

            # tour destination: given by trips[_prim_i]
            self.fields["DEST_PLACENO"] = self.trips[0].fields["ORIG_PLACENO"]
            self.fields["DEST_X"] = self.trips[0].fields["ORIG_X"]
            self.fields["DEST_Y"] = self.trips[0].fields["ORIG_Y"]
            self.fields["DEST_TAZ"] = self.trips[0].fields['DEST_TAZ']
            # self.fields["ORIG_TAZ"] = self.trips[0].fields['ORIG_TAZ']

            # self.fields['DEST_MODE']            = self.trips[0].fields['TRIPMODE']

            # self.fields['PRIMDEST_ARRIVE_HOUR'] = self.trips[_prim_i].fields['DEST_ARR_HR']
            # self.fields['PRIMDEST_ARRIVE_MIN']  = self.trips[_prim_i].fields['DEST_ARR_MIN']
            self.fields["PRIMDEST_DEPART_HOUR"] = self.trips[0].fields["ORIG_DEP_HR"]
            self.fields["PRIMDEST_DEPART_MIN"] = self.trips[0].fields["ORIG_DEP_MIN"]
            self.fields["PRIMDEST_DEPART_BIN"] = self.trips[0].fields["ORIG_DEP_BIN"]

            # last trip back at tour anchor
            _last_i = len(self.trips) - 1  # trips[_last_i] is the last trip on tour
            self.fields["ANCHOR_ARRIVE_HOUR"] = self.trips[_last_i].fields[
                "DEST_ARR_HR"
            ]
            self.fields["ANCHOR_ARRIVE_MIN"] = self.trips[_last_i].fields[
                "DEST_ARR_MIN"
            ]
            self.fields["ANCHOR_ARRIVE_BIN"] = self.trips[_last_i].fields[
                "DEST_ARR_BIN"
            ]

            self.fields["ORIG_MODE"] = self.trips[_last_i].fields["TRIPMODE"]

            # cannot calculate tour duration based on departure from anchor and arrival at anchor
            # no outbound stops
            self._set_outbound_stops(0, 0)

            # inbound stops
            self._set_inbound_stops(
                0, _last_i
            )  # inbound stops are dest of trips[0] to trips[_last_i-1]

        elif self.get_partial_status() == PARTIAL_TOUR["PARTIAL_END"]:
            _prim_purp = self.trips[
                -1
            ].get_dest_purpose()  # set to purpose at destination of last trip of the day
            self.fields["TOURPURP"] = _prim_purp

            # tour mode
            self.fields["TOURMODE"] = self._calc_tour_mode()

            # is person a driver on any trip
            self.fields["DRIVER"] = self._get_is_driver()

            # tour origin: origin of 1st trip on tour
            self.fields["ORIG_PLACENO"] = self.trips[0].fields["ORIG_PLACENO"]
            self.fields["ORIG_TAZ"] = self.trips[0].fields['ORIG_TAZ']
            self.fields["ORIG_X"] = self.trips[0].fields["ORIG_X"]
            self.fields["ORIG_Y"] = self.trips[0].fields["ORIG_Y"]

            self.fields["ANCHOR_DEPART_HOUR"] = self.trips[0].fields["ORIG_DEP_HR"]
            self.fields["ANCHOR_DEPART_MIN"] = self.trips[0].fields["ORIG_DEP_MIN"]
            self.fields["ANCHOR_DEPART_BIN"] = self.trips[0].fields["ORIG_DEP_BIN"]

            # tour destination: given by trips[-1] aka last trip
            self.fields["DEST_PLACENO"] = self.trips[-1].fields["DEST_PLACENO"]
            self.fields["DEST_X"] = self.trips[-1].fields["DEST_X"]
            self.fields["DEST_Y"] = self.trips[-1].fields["DEST_Y"]
            self.fields["DEST_MODE"] = self.trips[-1].fields["TRIPMODE"]
            self.fields["DEST_TAZ"] = self.trips[-1].fields['DEST_TAZ']

            self.fields["PRIMDEST_ARRIVE_HOUR"] = self.trips[-1].fields["DEST_ARR_HR"]
            self.fields["PRIMDEST_ARRIVE_MIN"] = self.trips[-1].fields["DEST_ARR_MIN"]
            self.fields["PRIMDEST_ARRIVE_BIN"] = self.trips[-1].fields["DEST_ARR_BIN"]
            # self.fields['PRIMDEST_DEPART_HOUR'] = self.trips[_prim_i].fields['DEST_DEP_HR']
            # self.fields['PRIMDEST_DEPART_MIN']  = self.trips[_prim_i].fields['DEST_DEP_MIN']

            # last trip back at tour anchor unknown
            # cannot calculate tour duration based on departure from anchor and arrival at anchor

            # outbound stops
            self._set_outbound_stops(
                0, len(self.trips) - 1
            )  # outbound stops are dest of trips[0] to trips[_prim_i-1]

            # no inbound stops
            self._set_inbound_stops(len(self.trips) - 1, len(self.trips) - 1)

        else:
            _prim_i = (
                self._find_prim_stop()
            )  # destination of trips[_prim_i] is the primary destination

            # determine tour purpose (at primary destination)
            _prim_purp = self.trips[_prim_i].fields["DEST_PURP"]
            self.fields["TOURPURP"] = _prim_purp

            # extract at-work subtours from the current tour if the current tour is a WORK tour
            # this needs to be done before processing any outbound and inbound stops
            if _prim_purp == PURPOSE["WORK"]:
                self._set_AW_subtours()
                # TODO: trip id may have been changed when subtours are created
                # quick-fix: re-calculate primary destination
                _prim_i = (
                    self._find_prim_stop()
                )  # destination of trips[_prim_i] is the primary destination
                _prim_purp = self.trips[_prim_i].fields["DEST_PURP"]
                self.fields["TOURPURP"] = _prim_purp

            # tour mode
            self.fields["TOURMODE"] = self._calc_tour_mode()

            # is person a driver on any trip
            self.fields["DRIVER"] = self._get_is_driver()

            # tour origin: origin of 1st trip on tour
            self.fields["ORIG_PLACENO"] = self.trips[0].fields["ORIG_PLACENO"]
            self.fields["ORIG_X"] = self.trips[0].fields["ORIG_X"]
            self.fields["ORIG_Y"] = self.trips[0].fields["ORIG_Y"]
            self.fields["ANCHOR_DEPART_HOUR"] = self.trips[0].fields["ORIG_DEP_HR"]
            self.fields["ANCHOR_DEPART_MIN"] = self.trips[0].fields["ORIG_DEP_MIN"]
            self.fields["ANCHOR_DEPART_BIN"] = self.trips[0].fields["ORIG_DEP_BIN"]
            self.fields["ORIG_TAZ"] = self.trips[0].fields['ORIG_TAZ']

            # tour destination: given by trips[_prim_i]
            self.fields["DEST_PLACENO"] = self.trips[_prim_i].fields["DEST_PLACENO"]
            self.fields["DEST_X"] = self.trips[_prim_i].fields["DEST_X"]
            self.fields["DEST_Y"] = self.trips[_prim_i].fields["DEST_Y"]
            self.fields["DEST_MODE"] = self.trips[_prim_i].fields["TRIPMODE"]
            self.fields["DEST_TAZ"] = self.trips[_prim_i].fields['DEST_TAZ']

            self.fields["PRIMDEST_ARRIVE_HOUR"] = self.trips[_prim_i].fields[
                "DEST_ARR_HR"
            ]
            self.fields["PRIMDEST_ARRIVE_MIN"] = self.trips[_prim_i].fields[
                "DEST_ARR_MIN"
            ]
            self.fields["PRIMDEST_ARRIVE_BIN"] = self.trips[_prim_i].fields[
                "DEST_ARR_BIN"
            ]

            self.fields["PRIMDEST_DEPART_HOUR"] = self.trips[_prim_i].fields[
                "DEST_DEP_HR"
            ]
            self.fields["PRIMDEST_DEPART_MIN"] = self.trips[_prim_i].fields[
                "DEST_DEP_MIN"
            ]
            self.fields["PRIMDEST_DEPART_BIN"] = self.trips[_prim_i].fields[
                "DEST_DEP_BIN"
            ]

            # last trip back at tour anchor
            _last_i = len(self.trips) - 1  # trips[_last_i] is the last trip on tour
            self.fields["ANCHOR_ARRIVE_HOUR"] = self.trips[_last_i].fields[
                "DEST_ARR_HR"
            ]
            self.fields["ANCHOR_ARRIVE_MIN"] = self.trips[_last_i].fields[
                "DEST_ARR_MIN"
            ]
            self.fields["ANCHOR_ARRIVE_BIN"] = self.trips[_last_i].fields[
                "DEST_ARR_BIN"
            ]
            self.fields["ORIG_MODE"] = self.trips[_last_i].fields["TRIPMODE"]

            # calculate tour duration based on departure from anchor and arrival at anchor
            (
                self.fields["TOUR_DUR_HR"],
                self.fields["TOUR_DUR_MIN"],
            ) = calculate_duration(
                self.fields["ANCHOR_DEPART_HOUR"],
                self.fields["ANCHOR_DEPART_MIN"],
                self.fields["ANCHOR_ARRIVE_HOUR"],
                self.fields["ANCHOR_ARRIVE_MIN"],
            )
            self.fields["TOUR_DUR_BIN"] = (
                1 + self.fields["ANCHOR_ARRIVE_BIN"] - self.fields["ANCHOR_DEPART_BIN"]
            )

            if len(self.trips) == 1:  # loop tour
                # check if purpose is 'loop
                if not self.fields["TOURPURP"] == PURPOSE["LOOP"]:
                    self.log_error(
                        "Only 1 trip in the tour, but purpose is {}".format(
                            self.fields["TOURPURP"]
                        )
                    )
                # outbound stops
                self._set_outbound_stops(0, 0)
                # inbound stops
                self._set_inbound_stops(0, 0)
            else:
                # outbound stops
                self._set_outbound_stops(
                    0, _prim_i
                )  # outbound stops are dest of trips[0] to trips[_prim_i-1]
                # inbound stops
                self._set_inbound_stops(
                    _prim_i + 1, _last_i
                )  # inbound stops are dest of trips[_prim_i+1] to trips[_last_i-1]

        # calculate tour distance
        if self.constants.get("COMPUTE_TRIP_DIST"):
            _dist = 0
            _missing_dist = False
            for _trip in self.trips:
                if _trip.get_dist() >= 0:
                    _dist = _dist + _trip.get_dist()
                else:
                    _missing_dist = True
                    self.log_error("cannot compute tour distance")
                    break
            if _missing_dist:
                self.fields["DIST"] = np.NAN
            else:
                self.fields["DIST"] = _dist

    def set_partial(self, is_partial):
        self.fields["PARTIAL_TOUR"] = is_partial

    def set_fully_joint(self, jtour_id):
        self.fields[
            "FULLY_JOINT"
        ] = 1  # initialize field to 0 (otherwise will be written out as 'nan')
        self.fields["JTOUR_ID"] = jtour_id

    def set_parent(self, parent_tour):
        """set field values relating to the parent tour"""
        self.fields["PARENT_TOUR_ID"] = parent_tour.get_id()
        self.fields["PARENT_TOUR_MODE"] = parent_tour.get_mode()

    def set_per_type(self, per_type):
        self.fields["PERSONTYPE"] = per_type

    def set_joint_tour_purp(self, purp):
        self.fields["JOINT_TOUR_PURP"] = purp

    def set_joint_status(self):
        JOINT_TOUR_CAT = self.constants.get("JOINT_TOUR_CAT")
        JOINT_CAT = self.constants.get("JOINT_CAT")

        # classify a tour as one of the following:
        # (1) independent tours:        no trips are joint
        # (2) fully-joint tours:        tour.get_is_fully_joint()==True
        #                               (all trips are joint and made by the same group of people)
        # (3) partially-joint tours:    at least 1 joint trip; all joint trips are properly grouped; but not fully joint
        # (4) joint, but problematic tours :    at least 1 problematic joint trip
        num_trips = self.get_num_trips()
        num_grouped_joint_trips = self.get_num_grouped_joint_trips()
        num_problem_joint_trips = self.get_num_prob_joint_trips()
        num_indep_trips = num_trips - num_grouped_joint_trips - num_problem_joint_trips

        if num_trips == num_indep_trips:
            status = JOINT_TOUR_CAT["INDEPENDENT"]
        elif self.get_is_fully_joint():
            status = JOINT_TOUR_CAT["FULL_JOINT"]
        elif num_grouped_joint_trips > 0 & num_problem_joint_trips == 0:
            status = JOINT_TOUR_CAT["PART_JOINT"]
        else:
            status = JOINT_TOUR_CAT["PROB_JOINT"]

        self.fields["JOINT_STATUS"] = status

    def get_num_grouped_joint_trips(self):
        JOINT_CAT = self.constants.get("JOINT_CAT")

        count = 0
        for trip in self.trips:
            if trip.get_joint_status() == JOINT_CAT["JOINT-GROUPED"]:
                count = count + 1
        return count

    def get_num_prob_joint_trips(self):
        JOINT_CAT = self.constants.get("JOINT_CAT")

        count = 0
        for trip in self.trips:
            if trip.get_joint_status() == JOINT_CAT["JOINT"]:
                count = count + 1
        return count

    def set_escortee_purp(self, jtrip_id, escortee_purp):
        for _trip in self.trips:
            if _trip.get_jtripID() == jtrip_id:
                if _trip.fields["IS_INBOUND"] == 0:
                    self.fields["OUT_ESCORTEE_TOUR_PURP"] = escortee_purp
                else:
                    self.fields["INB_ESCORTEE_TOUR_PURP"] = escortee_purp

    def set_escorted_fields(self):
        ESCORT_EVENT = self.constants.get("ESCORT_EVENT")
        ESCORT_TYPE = self.constants.get("ESCORT_TYPE")

        _escorted = False
        _chauffuer_set = set()
        _out_escort_type = np.NAN
        _inb_escort_type = np.NAN
        _out_chauffuer = np.NAN
        _inb_chauffuer = np.NAN
        _out_chauffer_purp = np.NAN
        _inb_chauffer_purp = np.NAN
        _out_chauffer_ptype = np.NAN
        _inb_chauffer_ptype = np.NAN
        _household = self.hh_obj
        _purpose = self.get_purp()
        for _trip in self.trips:
            # set union with the trip's chauffeur, if applicable
            if _trip.get_escorted() != ESCORT_EVENT["NEITHER"]:
                _escorted = True
                _chauffuer_set.add(_trip.get_chauffuer())
            # find chauffeur for outbound leg of the tour
            if (_trip.fields["IS_INBOUND"] == 0) & (
                _trip.get_escorted() != ESCORT_EVENT["NEITHER"]
            ):
                _out_chauffuer = _trip.get_chauffuer()
                _out_jtripID = _trip.get_jtripID()
                # find tour purpose for chauffeur for outbound jtripID
                _out_chauffuer_per_Obj = _household.get_person(_out_chauffuer)
                if _out_chauffuer_per_Obj is None:
                    _out_chauffer_ptype = 0
                else:
                    _out_chauffer_ptype = _out_chauffuer_per_Obj.fields["PERSONTYPE"]
                    for _ctour in _out_chauffuer_per_Obj.tours:
                        if _ctour.contains_joint_trip(_out_jtripID) is True:
                            _out_chauffer_purp = _ctour.get_purp()
                            _ctour.set_escortee_purp(_out_jtripID, _purpose)
            # find chauffeur for inbound leg of the tour
            if (_trip.fields["IS_INBOUND"] == 1) & (
                _trip.get_escorted() != ESCORT_EVENT["NEITHER"]
            ):
                _inb_chauffuer = _trip.get_chauffuer()
                _inb_jtripID = _trip.get_jtripID()
                # find tour purpose for chauffeur for inbound jtripID
                _inb_chauffuer_per_Obj = _household.get_person(_inb_chauffuer)
                if _inb_chauffuer_per_Obj is None:
                    _inb_chauffer_ptype = 0
                else:
                    _inb_chauffer_ptype = _inb_chauffuer_per_Obj.fields["PERSONTYPE"]
                    for _ctour in _inb_chauffuer_per_Obj.tours:
                        if _ctour.contains_joint_trip(_inb_jtripID) is True:
                            _inb_chauffer_purp = _ctour.get_purp()
                            _ctour.set_escortee_purp(_inb_jtripID, _purpose)
        # code escort type on outbound and inbound leg of the tour
        if not math.isnan(_out_chauffer_purp):
            if ((_out_chauffer_purp > 0) & (_out_chauffer_purp <= 3)) | (
                _out_chauffer_purp == 10
            ):
                _out_escort_type = ESCORT_TYPE["RIDE_SHARING"]
            elif (_out_chauffer_purp > 3) & (_out_chauffer_purp < 10):
                _out_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif (_out_chauffer_ptype == 4) | (_out_chauffer_ptype == 5):
            _out_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif ((_out_chauffer_ptype > 0) & (_out_chauffer_ptype <= 3)) | (
            _out_chauffer_ptype == 6
        ):
            _out_escort_type = ESCORT_TYPE["RIDE_SHARING"]
        else:
            _out_escort_type = ESCORT_TYPE["NO_ESCORT"]

        if not math.isnan(_inb_chauffer_purp):
            if ((_inb_chauffer_purp > 0) & (_inb_chauffer_purp <= 3)) | (
                _inb_chauffer_purp == 10
            ):
                _inb_escort_type = ESCORT_TYPE["RIDE_SHARING"]
            elif (_inb_chauffer_purp > 3) & (_inb_chauffer_purp < 10):
                _inb_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif (_inb_chauffer_ptype == 4) | (_inb_chauffer_ptype == 5):
            _inb_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif ((_inb_chauffer_ptype > 0) & (_inb_chauffer_ptype <= 3)) | (
            _inb_chauffer_ptype == 6
        ):
            _inb_escort_type = ESCORT_TYPE["RIDE_SHARING"]
        else:
            _inb_escort_type = ESCORT_TYPE["NO_ESCORT"]

        self.fields["ESCORTED_TOUR"] = int(_escorted)
        self.fields["CHAUFFUER_ID"] = (
            '"' + ",".join(["%s" % id for id in list(_chauffuer_set)]) + '"'
        )  # in case there are more than 1 chauffuer
        self.fields["OUT_ESCORT_TYPE"] = _out_escort_type
        self.fields["OUT_CHAUFFUER_ID"] = _out_chauffuer
        self.fields["OUT_CHAUFFUER_PURP"] = _out_chauffer_purp
        self.fields["OUT_CHAUFFUER_PTYPE"] = _out_chauffer_ptype
        self.fields["INB_ESCORT_TYPE"] = _inb_escort_type
        self.fields["INB_CHAUFFUER_ID"] = _inb_chauffuer
        self.fields["INB_CHAUFFUER_PURP"] = _inb_chauffer_purp
        self.fields["INB_CHAUFFUER_PTYPE"] = _inb_chauffer_ptype

    def set_escorting_fields(self):
        ESCORT_EVENT = self.constants.get("ESCORT_EVENT")
        ESCORT_TYPE = self.constants.get("ESCORT_TYPE")
        MAND_TYPES = self.constants.get('MAND_PERSONTYPES')

        # initialize set of escorted hh members to empty set
        _escort_pers_set = set()
        _out_escorting = 0
        _inb_escorting = 0
        _out_escort_type = np.NaN
        _inb_escort_type = np.NaN
        _tour_purp = self.get_purp()
        _perObj = self.per_obj
        _pertype = _perObj.fields["PERSONTYPE"]
        for _trip in self.trips:
            # set union with the trip's list of escorted individuals
            _escort_pers_set = _escort_pers_set | set(_trip.escort_pers)

            # number of escorting episodes in outbound/inbound direction
            # check if ESCORTING key exists, otherwise no escortin on this trip
            if "ESCORTING" in _trip.fields:
                if (_trip.fields["IS_INBOUND"] == 0) & (
                    _trip.fields["ESCORTING"] != ESCORT_EVENT["NEITHER"]
                ):
                    _out_escorting = _out_escorting + 1
                if (_trip.fields["IS_INBOUND"] == 1) & (
                    _trip.fields["ESCORTING"] != ESCORT_EVENT["NEITHER"]
                ):
                    _inb_escorting = _inb_escorting + 1

        # code escorting type for outbound and inbound leg
        if (not math.isnan(_tour_purp)) & (_out_escorting > 0):
            if ((_tour_purp > 0) & (_tour_purp <= 3)) | (_tour_purp == 10):
                _out_escort_type = ESCORT_TYPE["RIDE_SHARING"]
            elif (_tour_purp > 3) & (_tour_purp < 10):
                _out_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif (_pertype in MAND_TYPES) & (_out_escorting > 0):
        # elif ((_pertype == 4) | (_pertype == 5)) & (_out_escorting > 0):
            _out_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif (_pertype not in MAND_TYPES) & (_out_escorting > 0):
        # elif (((_pertype > 0) & (_pertype <= 3)) | (_pertype == 6)) & (_out_escorting > 0):
            _out_escort_type = ESCORT_TYPE["RIDE_SHARING"]
        else:
            _out_escort_type = ESCORT_TYPE["NO_ESCORT"]

        if (not math.isnan(_tour_purp)) & (_inb_escorting > 0):
            if ((_tour_purp > 0) & (_tour_purp <= 3)) | (_tour_purp == 10):
                _inb_escort_type = ESCORT_TYPE["RIDE_SHARING"]
            elif (_tour_purp > 3) & (_tour_purp < 10):
                _inb_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif ((_pertype == 4) | (_pertype == 5)) & (_inb_escorting > 0):
            _inb_escort_type = ESCORT_TYPE["PURE_ESCORT"]
        elif (((_pertype > 0) & (_pertype <= 3)) | (_pertype == 6)) & (
            _inb_escorting > 0
        ):
            _inb_escort_type = ESCORT_TYPE["RIDE_SHARING"]
        else:
            _inb_escort_type = ESCORT_TYPE["NO_ESCORT"]

        _num_escorted = len(_escort_pers_set)
        _escorted_list = sorted(list(_escort_pers_set))

        # set fields
        self.fields["ESCORTING_TOUR"] = int(_num_escorted > 0)
        self.fields["NUM_PERSONS_ESCORTED"] = _num_escorted
        self.fields["OUT_ESCORTING_TYPE"] = _out_escort_type
        self.fields["INB_ESCORTING_TYPE"] = _inb_escort_type
        self.fields["OUT_ESCORTING_EPISODES"] = _out_escorting
        self.fields["INB_ESCORTING_EPISODES"] = _inb_escorting
        for _i in range(0, _num_escorted):
            # TODO: as many fields are set as needed, but only 5(?) persons will be written out
            self.fields["ESCORT_PERS_" + str(_i + 1)] = _escorted_list[_i]

    def _set_AW_subtours(self):
        PURPOSE = self.constants.get("PURPOSE")
        PARTIAL_TOUR = self.constants.get("PARTIAL_TOUR")

        """ scan through trips in the tour to identify and extract any at-work subtours """
        """ prim_i: trips list index of the primary destination                         """
        _to_work = []  # list indices of the trips arriving at work
        _from_work = []  # list indices of the trips departing from work
        _num_subtours = 0  # counter initialized to 0

        for _i, _trip in enumerate(self.trips):
            # _trip = self.trips[_i]
            if _trip.fields["ORIG_PURP"] == PURPOSE["WORK"]:
                # found a trip leaving work
                _from_work.append(_i)
            if _trip.fields["DEST_PURP"] == PURPOSE["WORK"]:
                # found a trip going to work
                _to_work.append(_i)

        _num_trips_to_work = len(_to_work)
        _num_trips_from_work = len(_from_work)
        if _num_trips_to_work == _num_trips_from_work:
            if _num_trips_from_work > 1:
                # there is at least 1 at-work subtour
                # drop the first trip to work from subsequent processing
                del _to_work[0]
                # drop the last trip to work from subsequent processing
                del _from_work[-1]
                # remaining items in the _to_work and _from_work lists mark the start and end of a subtour

                _trips_to_remove = []
                # list indices of trips that constitute at-work subtour(s) and need to be removed from the parent tour
                for _start_i, _end_i in zip(
                    _from_work, _to_work
                ):  # process each subtour
                    # _start_i: list index of the trip leaving from work
                    # _end_i:   list index of the trip returning to work
                    _num_subtours = _num_subtours + 1  # update counter
                    # create the subtour
                    _subtour_trips = self.trips[
                        _start_i : _end_i + 1
                    ]  # take a slice of the list that contains trips for the subtour
                    _subtour = self.per_obj.add_subtour(
                        _subtour_trips
                    )  # create & add new tour to the person's list
                    _subtour.set_parent(
                        self
                    )  # set the parent tour attributes for the subtour
                    _trips_to_remove.extend(
                        _subtour_trips
                    )  # add trips to the to-be-removed list
                    # keep record of the subtour's id
                    self.fields[
                        "CHILD_TOUR_ID_" + str(_num_subtours)
                    ] = _subtour.get_id()

                # all subtours have been created, now clean up the parent tour's trip list
                for _trip in _trips_to_remove:
                    self.trips.remove(_trip)
                # renumber the trips
                self.renumber_trips()
        elif self.fields["PARTIAL_TOUR"] == PARTIAL_TOUR["NOT_PARTIAL"]:
            self.log_error("to- and from-work trips not matching up")

        # TODO: provide warning msg when there are not enough
        #  place holders for child tour id (currently table allows for 3)
        # set field value
        self.fields["NUM_SUBTOURS"] = _num_subtours

        return _num_subtours

    def _find_prim_stop(self):
        PARTIAL_TOUR = self.constants.get("PARTIAL_TOUR")

        if len(self.trips) == 1:  # single trip in the tour ->loop?
            _prim_trip = 0
        elif self.get_partial_status() == PARTIAL_TOUR["NOT_PARTIAL"]:
            _prim_trip = self._find_prim_by_score()
        else:
            _prim_trip = None

        return _prim_trip

    def _find_prim_by_score(self):
        PURPOSE = self.constants.get("PURPOSE")

        # TODO. WHAT THE HECK IS THIS?
        d = [0, 60, 120, 180, 240, 300, 360, 420, 480]
        score = {
            PURPOSE["WORK"]: [8, 4, 2, 1.5, 1.4, 1.3, 1.2, 1.1, 1],
            PURPOSE["UNIVERSITY"]: [8, 4, 2, 1.5, 1.4, 1.3, 1.2, 1.1, 1],
            PURPOSE["SCHOOL"]: [8, 4, 2, 1.5, 1.4, 1.3, 1.2, 1.1, 1],
            PURPOSE["ESCORTING"]: [8, 6, 4, 3.5, 3.4, 3.3, 3.2, 3.1, 3],
            PURPOSE["SHOPPING"]: [10, 6, 4, 3.5, 3.4, 3.3, 3.2, 3.1, 3],
            PURPOSE["MAINTENANCE"]: [10, 6, 4, 3.5, 3.4, 3.3, 3.2, 3.1, 3],
            PURPOSE["EAT OUT"]: [12, 7, 5, 4.5, 4.4, 4.3, 4.2, 4.1, 4],
            PURPOSE["SOCIAL/VISIT"]: [14, 8, 6, 5.5, 5.4, 5.3, 5.2, 5.1, 5],
            PURPOSE["DISCRETIONARY"]: [10, 6, 4, 3.5, 3.4, 3.3, 3.2, 3.1, 3],
            PURPOSE["WORK-RELATED"]: [8, 4, 2, 1.5, 1.4, 1.3, 1.2, 1.1, 1],
            PURPOSE["LOOP"]: [20, 10, 7, 6.5, 6.4, 6.3, 6.2, 6.1, 6],
            PURPOSE["OTHER"]: [20, 10, 7, 6.5, 6.4, 6.3, 6.2, 6.1, 6],
        }

        # scan through all but the last trip in tour to identify primary activity/destination
        # _prim_i = -1            #list item no. corresponding to the primary destination
        _prim_i = None  # list item no. corresponding to the primary destination
        _min_score = 99999999  # lowest score found so far
        _num_trips = len(self.trips)
        for _i in range(0, _num_trips - 1):
            _trip = self.trips[_i]
            _stop_purp = _trip.fields["DEST_PURP"]
            (_hours, _minutes) = calculate_duration(
                _trip.fields["DEST_ARR_HR"],
                _trip.fields["DEST_ARR_MIN"],
                _trip.fields["DEST_DEP_HR"],
                _trip.fields["DEST_DEP_MIN"],
            )
            _stop_dur = convert2minutes(_hours, _minutes)
            _trip_dur = convert2minutes(
                _trip.fields["TRIP_DUR_HR"], _trip.fields["TRIP_DUR_MIN"]
            )
            _i_score = np.interp(_stop_dur + 2 * _trip_dur, d, score[_stop_purp])
            if _i_score < _min_score:
                _min_score = _i_score
                _prim_i = _i
        # destination of the _i'th trip in the list is the primary destination
        return _prim_i

    """
    def _find_prim_stop(self):
        ### scan through all but the last trip in tour to identify primary activity/destination
        _prim_i = -1        #list item no. corresponding to the primary destination 
        _max_dur = -1       #maximum activity duration (at a stop) found so far 
        _num_trips = len(self.trips)
        for _i in range(0, _num_trips-1):
            _trip = self.trips[_i]
            _stop_purp = _trip.fields["DEST_PURP"]
            if _stop_purp in [ NewPurp["WORK"], NewPurp["UNIVERSITY"], NewPurp["SCHOOL"] ]:
                #it is one of the mandatory activities  
                _prim_i = _i
                #no need to continue scanning if a primary destination is found
                break
            else:
                #it is a non-mandatory activity
                (hours, minutes) = calculate_duration(_trip.fields["DEST_ARR_HR"],_trip.fields["DEST_ARR_MIN"],
                                                              _trip.fields["DEST_DEP_HR"],_trip.fields["DEST_DEP_MIN"])
                _stop_dur = convert2minutes(hours,minutes)
                if _stop_dur > _max_dur:
                    _max_dur = _stop_dur
                    _prim_i = _i
        #destination of the _i'th trip in the list is the primary destination
        return _prim_i               
    """

    def print_header(fp):
        # fp.write(','.join(['%s' %field for field in self.fields.keys()])+'\n')
        _header = []
        # TODO: save a sorted copy of the dict to avoid repeated sorting
        for _col_num, _col_name in sorted(TOUR_COLUMNS.items()):
            _header.append(_col_name)
        fp.write(",".join(["%s" % name for name in _header]) + "\n")

    def print_vals(self, fp):
        # fp.write(','.join(['%s' %value for value in self.fields.values()])+'\n')
        if "ERROR" in self.fields:
            self.fields["ERROR"] = add_quote_char(self.fields["ERROR"])
        _vals = []
        for _col_num, _col_name in sorted(TOUR_COLUMNS.items()):
            if _col_name in self.fields:
                _vals.append(self.fields[_col_name])
            else:
                _vals.append(np.NAN)
        fp.write(",".join(["%s" % value for value in _vals]) + "\n")
