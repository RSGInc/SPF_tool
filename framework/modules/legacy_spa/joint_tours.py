from core import utils
import numpy as np

JT_TOUR_COLUMNS = ['HH_ID',
                   'JTOUR_ID',
                   'DAYNO',
                   'NUMBER_HH',
                   'COMPOSITION',
                   'JOINT_PURP',
                   'PERSON_1',
                   'PERSON_2',
                   'PERSON_3',
                   'PERSON_4',
                   'PERSON_5',
                   'PERSON_6',
                   'PERSON_7',
                   'PERSON_8',
                   'PERSON_9'
                   ]

class Joint_tour:
    """Joint tour class"""

    # upon creation of a joint tour, determine the joint tour purpose and pass it on to the constituting person tours
    def __init__(self, jtour_id, jtrips, constants):
        self.jtour_id = jtour_id
        self.jtrips = jtrips  # list of Joint_trip objects
        self.day_id = jtrips[0][0].day_id
        self.error_msg = ""
        self.error_flag = False
        self.constants = constants

        # find all the person tours associated with this fully joint tour
        self.person_tours = []
        for jt in jtrips[0]:
            ptrip = jt.parent_trip
            ptour = ptrip.tour_obj
            self.person_tours.append(ptour)

        self.joint_purp = self.find_joint_purp()

        for ptour in self.person_tours:
            ptour.set_joint_tour_purp(self.joint_purp)

        self.update_field_vals()

    def update_field_vals(self):
        # write out hh id, joint tour id, and size of travel group
        # use the 0'th joint trip in the 0'th joint trip group
        # since all joint trips in the jtrips list contain the same information
        _jt = self.jtrips[0][0]
        self.fields = {
            "HH_ID": _jt.get_hh_id(),
            "JTOUR_ID": _jt.jtour_id,
            "DAYNO": _jt.day_id,
            "NUMBER_HH": _jt.number_hh,
            'COMPOSITION': _jt.composition,
            'JOINT_PURP': self.joint_purp
        }

        _parties = sorted(_jt.travel_party)
        person_fields = {f'PERSON_{i}': _parties[i-1] if i <= _jt.number_hh else np.NAN for i in range(1, 10)}
        self.fields = {**self.fields, **person_fields}



    def find_joint_purp(self):
        """given the person tour associated with this fully joint tour, return the joint tour purpose"""
        # this is intended to prevent joint tour purpose being coded as escorting.
        purp_list = []
        PURPOSE = self.constants.get("PURPOSE")

        for tour in self.person_tours:
            purp = tour.get_purp()
            if (purp is not PURPOSE["ESCORTING"]) & (purp not in purp_list):
                purp_list.append(purp)
        # in theory, there should be only 1 purpose
        if len(purp_list) == 0:
            joint_purp = PURPOSE["ESCORTING"]
            # self.log_error("No valid joint purpose found")
        elif len(purp_list) > 1:
            # self.log_error("More than 1 joint purposes found: "+','.join(['%s' %purp for purp in purp_list]))
            # determine based on purpose hierarchy
            joint_purp = min(purp_list)
        else:
            # there is exactly one (non-escorting) purpose found across all participants
            joint_purp = purp_list[0]
        return joint_purp

    def log_error(self, err_msg=None):
        self.error_flag = True
        if err_msg:  # if there is an error message
            self.error_msg = self.error_msg + "E: " + err_msg

    def coalesce_data(self):
        if "ERROR" in self.fields.keys():
            self.fields["ERROR"] = utils.add_quote_char(self.fields["ERROR"])
        _vals = [self.fields[x] if x in self.fields.keys() else np.NAN for x in JT_TOUR_COLUMNS]

    def print_header(fp):
        fp.write(",".join(["%s" % name for name in JT_TOUR_COLUMNS]) + "\n")

    def print_vals(self, fp):
        # fp.write(','.join(['%s' %value for value in self.fields.values()])+'\n')
        if "ERROR" in self.fields.keys():
            self.fields["ERROR"] = utils.add_quote_char(self.fields["ERROR"])
        _vals = [self.fields[x] if x in self.fields.keys() else np.NAN for x in JT_TOUR_COLUMNS]
        fp.write(",".join(["%s" % value for value in _vals]) + "\n")