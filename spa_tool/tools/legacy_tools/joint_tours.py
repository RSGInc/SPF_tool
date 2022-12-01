class Joint_tour:
    """Joint tour class"""

    # upon creation of a joint tour, determine the joint tour purpose and pass it on to the constituting person tours
    def __init__(self, jtour_id, jtrips, constants):
        self.jtour_id = jtour_id
        self.jtrips = jtrips  # list of Joint_trip objects
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

    def print_header(fp):
        _header = ["HH_ID", "JTOUR_ID", "NUMBER_HH"]
        # PERSON_1 to PERSON_9
        for _i in range(1, 10):
            _header.append("PERSON_" + str(_i))
        _header.extend(["COMPOSITION", "JOINT_PURP"])
        fp.write(",".join(["%s" % field for field in _header]) + "\n")
        # Trip.print_header(fp)

    def print_vals(self, fp):
        # write out hh id, joint tour id, and size of travel group
        # use the 0'th joint trip in the 0'th joint trip group
        # since all joint trips in the jtrips list contain the same information
        _jt = self.jtrips[0][0]
        fp.write("{},{},{}".format(_jt.get_hh_id(), _jt.jtour_id, _jt.number_hh))
        # write out IDs of people in the travel party
        _party = sorted(_jt.travel_party)
        for _i in range(0, _jt.number_hh):
            fp.write(",{}".format(_party[_i]))
        # fill nan up to person 9
        for _i in range(_jt.number_hh, 9):
            fp.write(",nan")
        # write out group composition, joint purpose, and error
        fp.write(",{},{}\n".format(_jt.composition, self.joint_purp))
