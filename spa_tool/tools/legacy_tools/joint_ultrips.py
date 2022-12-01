import numpy as np
import math
from core import functions


class Joint_ultrip:
    """Joint episode class"""

    def __init__(
        self,
        trip,
        num_tot_travelers,
        num_hh_mem,
        dep_time,
        arr_time,
        hh_travelers,
        constants,
    ):
        self.parent_trip = trip
        self.depart_time = dep_time
        self.arrival_time = arr_time
        self.number_tot = int(num_tot_travelers)
        self.number_hh = int(
            num_hh_mem
        )  # number of household members (as reported) who traveled together
        self.travel_party = [
            int(pid) for pid in hh_travelers if pid > 0
        ]  # list of valid IDs of household members in the travel party
        self.travel_party.sort()
        self.jt_id = (
            np.NAN
        )  # ID assigned to each group of trips made together by multiple household members
        self.jtour_id = (
            np.NAN
        )  # ID assigned to each group of trips made together by multiple household members
        self.composition = np.NAN
        self.driver = np.NAN
        self.driver_tour = np.NAN
        self.driver_trip = np.NAN
        self.error_flag = False  # initialize error flag to false
        self.error_msg = ""
        self.constants = constants

    def add_driver(self, per_id, tour_id, trip_id):
        self.driver = per_id
        self.driver_tour = tour_id
        self.driver_trip = trip_id

    def get_depart_time(self):
        return self.depart_time

    def get_arrival_time(self):
        return self.arrival_time

    def get_num_tot_travelers(self):
        return self.number_tot

    def get_num_hh_travelers(self):
        return self.number_hh

    def get_hh_id(self):
        return self.parent_trip.get_hh_id()

    def get_per_id(self):
        return self.parent_trip.get_per_id()

    def get_tour(self):
        return self.parent_trip.tour_obj

    def get_travel_party(self):
        return self.travel_party

    def get_dest_escort_pudo(self):
        return self.parent_trip.get_dest_escort_pudo()

    def get_orig_escort_pudo(self):
        ESCORT_EVENT = self.constants.get("ESCORT_EVENT")

        _cur_trip_id = self.parent_trip.get_id()
        _pudo = ESCORT_EVENT["NEITHER"]
        if _cur_trip_id > 1:
            _prev_trip = self.parent_trip.tour_obj.get_trip(_cur_trip_id - 1)
            _pudo = _prev_trip.get_dest_escort_pudo()
        return _pudo

    def get_orig_travel_party(self):
        _cur_trip_id = self.parent_trip.get_id()
        _party = []
        if _cur_trip_id > 1:
            _prev_trip = self.parent_trip.tour_obj.get_trip(_cur_trip_id - 1)
            if _prev_trip.get_is_joint():
                _party = _prev_trip.get_joint_descriptions()[-1].get_travel_party()
            else:
                _party = [self.get_per_id()]
        return _party

    def get_joint_ultrip_id(self):
        return self.jt_id

    def set_joint_ultrip_id(self, jt_id):
        self.jt_id = jt_id
        # self.parent_trip.add_joint_ultrip_ids(jt_id)    #update in the trip record

    def set_chauffuer(self, chauffuer_id):
        self.driver = chauffuer_id
        self.parent_trip.set_chauffuer(chauffuer_id)  # update in the trip record

    def set_composition(self, composition):
        self.composition = composition

    def set_jtour_id(self, jtour_id):
        # set its data member
        self.jtour_id = jtour_id
        # also set the parent tour as a fully joint tour
        self.parent_trip.tour_obj.set_fully_joint(jtour_id)

    def recode_travel_party(self, set_participants):
        list_participants = list(set_participants)
        list_participants.sort()
        if list_participants != self.travel_party:
            self.parent_trip.log_recode(
                "reset TRAVEL PARTY from {} to {}".format(
                    self.travel_party, list_participants
                )
            )
            self.travel_party = list_participants

    def recode_number_travelers(self, number_travelers):
        if number_travelers != self.number_hh:
            self.parent_trip.log_recode(
                "reset NUMBER OF PARTICIPANTS on trip from {} to {}".format(
                    self.number_hh, number_travelers
                )
            )
            self.number_hh = number_travelers

    def log_error(self, err_msg=None):
        self.error_flag = True
        if err_msg:  # if there is an error message
            self.error_msg = self.error_msg + "E: " + err_msg
            self.parent_trip.log_error(err_msg)

    def print_header(fp):
        _header = [
            "HH_ID",
            "PER_ID",
            "TOUR_ID",
            "TRIP_ID",
            "LEG_DEST_PLACENO",
            "JTRIP_ID",
            "NUMBER_HH",
            "CHAUFFUER_ID",
            "ORIG_DEP_HR",
            "ORIG_DEP_MIN",
            "DEST_ARR_HR",
            "DEST_ARR_MIN",
            "DEST_PURP",
            "PARTY",
            "ERROR",
        ]
        fp.write(",".join(["%s" % field for field in _header]) + "\n")

    """            
    def print_vals(self, fp):
        pt = self.parent_trip
        _vals=[pt.get_hh_id(), pt.get_per_id(), pt.get_tour_id(), pt.trip_id, pt.fields["DEST_PLACENO"], self.jt_id,  
               self.number_hh, self.driver, 
               pt.fields["ORIG_DEP_HR"], pt.fields["ORIG_DEP_MIN"], pt.fields["DEST_ARR_HR"], pt.fields["DEST_ARR_MIN"], 
               pt.fields["DEST_PURP"]]
        fp.write(','.join(['%s' %value for value in _vals]))
        fp.write(','+'_'.join(['%s' %int(pid) for pid in sorted(self.travel_party)]))   #print travel party
        fp.write(','+self.error_msg+'\n')                                               #print error message
    """

    def print_vals(self, fp):
        pt = self.parent_trip
        _vals = [
            pt.get_hh_id(),
            pt.get_per_id(),
            pt.get_tour_id(),
            pt.trip_id,
            pt.fields["DEST_PLACENO"],
            self.jt_id,
            self.number_hh,
            self.driver,
            math.floor(self.depart_time / 60),
            self.depart_time % 60,
            math.floor(self.arrival_time / 60),
            self.arrival_time % 60,
            pt.fields["DEST_PURP"],
        ]
        fp.write(",".join(["%s" % value for value in _vals]))
        fp.write(
            "," + "_".join(["%s" % int(pid) for pid in sorted(self.travel_party)])
        )  # print travel party
        fp.write(
            "," + functions.add_quote_char(self.error_msg) + "\n"
        )  # print error message

    def print_header_unique(fp):
        _header = ["HH_ID", "JTRIP_ID", "NUMBER_HH"]
        # PERSON_1 to PERSON_9
        for _i in range(1, 10):
            _header.append("PERSON_" + str(_i))
        _header.append("COMPOSITION")
        fp.write(",".join(["%s" % field for field in _header]) + "\n")

    def print_vals_unique(self, fp):
        # write out hh id, joint trip id, and size of travel group
        fp.write(
            "{},{},{}".format(self.parent_trip.get_hh_id(), self.jt_id, self.number_hh)
        )
        # write out IDs of people in the travel party
        _party = sorted(self.travel_party)
        for _i in range(0, self.number_hh):
            fp.write(",{}".format(_party[_i]))
        # fill nan up to person 9
        for _i in range(self.number_hh, 9):
            fp.write(",nan")
        # write out group composition
        fp.write(",{}\n".format(self.composition))
