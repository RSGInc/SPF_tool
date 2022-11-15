import os

import numpy as np
import pandas as pd

from core import functions
from tools.legacy_tools.households import Household
from tools.legacy_tools.persons import Person
from tools.legacy_tools.tours import Tour
from tools.legacy_tools.trips import Trip
from tools.legacy_tools.joint_tours import Joint_tour
from tools.legacy_tools.joint_ultrips import Joint_ultrip

class LegacySPA():
    def __init__(self, namespace, **kwargs):
        self.namespace = namespace
        self.kwargs = self.update_kwargs(**kwargs)
        self.data = functions.load_pipeline_tables(namespace, self.kwargs)
        self.constants = self.default_constants()

    def default_constants(self):
        # FIXME Can put the value labels in here from the dictionary as default rather than have users hard code them
        constants = functions.read_mappings(**self.kwargs.get('specs'))
        def default_map(vals):
            vals = sorted(vals)
            return dict(zip(vals, vals))

        if not constants.get('SurveyTpurp2Purp'):
            constants['SurveyTpurp2Purp'] = default_map( self.data['trip'].TPURP.unique())

        return constants


    def update_kwargs(self, **kwargs):
        if not kwargs.get('module'):
            settings_file = os.path.join(self.namespace.config, 'settings.yaml')
            kwargs = {**kwargs, **functions.read_config(settings_file).get('PROCESSING_STEPS').get('LegacySPA')}

        assert kwargs.get('specs'), 'Missing spec files for legacy SPA tool'
        kwargs['specs'] = {k: v if os.path.isabs(v) else os.path.join(self.namespace.config, v)
                          for k, v in kwargs.get('specs').items()}

        return kwargs

    def run(self):
        # Legacy Constants
        OUT_DIR = self.namespace.output
        SurveyWorkPurp = self.constants.get('SurveyWorkPurp')
        SurveyWorkRelatedPurp = self.constants.get('SurveyWorkRelatedPurp')
        SurveyHomeCode = self.constants.get('SurveyHomeCode')
        SurveyChangeModeCode = self.constants.get('SurveyChangeModeCode')
        WORK_LOCATION_BUFFER = self.constants.get('WORK_LOCATION_BUFFER')
        PARTIAL_TOUR = self.constants.get('PARTIAL_TOUR')

        # add_place_distance('route.csv', 'place.csv', 'placeWithDist.csv' )
        # BMP[09/08/17] - survey data processed separately for each survey day

        for k in ['person', 'household', 'trip']:
            assert self.data.get(k) is not None, f'Missing {k} in data'

        # Format place file from trips
        df_place = self.data.get('trip')
        df_per = self.data.get('person')
        df_hh = self.data.get('household')

        if not os.path.exists(OUT_DIR):
            os.makedirs(OUT_DIR)

        hh_list = []
        # loop through and process the PLACE data frame group for each household for each day
        # create the household, person, tour, and trip objects
        of_count = len(df_place.groupby(['SAMPN']))
        iter = 0
        for (hhid), df_persons in df_place.groupby(['SAMPN']):
            # create a new household object
            hh = Household(hhid, constants=self.constants)
            hh_list.append(hh)
            iter += 1
            # ({100*round(iter/of_count}}")

            if iter % round(0.1 * of_count) == 0:
                print(f"{round(100 * iter / of_count)}% complete")

            # loop through each person
            for (hhid, pid), df_psn_places in df_persons.groupby(['SAMPN', 'PERNO']):

                # locate the corresponding person in PERSON table
                df_cur_per = df_per[(df_per['SAMPN'] == hhid) & (df_per['PERNO'] == pid)]

                # create a new person object
                psn = Person(hh, pid, df_cur_per, self.constants)

                num_places_for_person = len(df_psn_places)
                print(f"No. of place entries for person {pid} of household {hhid}: {num_places_for_person}")

                # first make sure that the place entries are ordered by place number since they will be processed sequentially later
                df_psn_places = df_psn_places.sort_values('PLANO')

                # recode TPURP from "work" or "other activities at work" to "work-related" if the PLACE is not the primary place of work
                (wxcord, wycord) = (df_psn_places['WXCORD'].dropna().drop_duplicates().to_list(),
                                    df_psn_places['WXCORD'].dropna().drop_duplicates().to_list())

                if (wxcord and wycord) and (wxcord != 0 and wycord != 0):
                    # both X and Y coordinates for work location are found
                    for row_index, row in df_psn_places.iterrows():
                        # check place coordinates against work coordinates if TPURP is work or other activities at work
                        if row['TPURP'] in SurveyWorkPurp:
                            xcord = row['XCORD']
                            ycord = row['YCORD']
                            buffer_dist = 0
                            if (xcord and ycord) and (xcord != 0 and ycord !=0):
                                buffer_dist = row['BUFFER_DIST']
                            # if (not xcord==wxcord) | (not ycord==wycord):
                            if (buffer_dist > WORK_LOCATION_BUFFER):
                                # not the place of work -> recode TPURP to work-related
                                df_psn_places['TPURP'][row_index] = SurveyWorkRelatedPurp
                                psn.log_recode(
                                    "work activity reported for PLANO={}, which is not the primary work location; recode activity as work-related".format(
                                        row['PLANO']))

                # FIXME: CLEANUP, these seems weirdly hardcoded and not all of them used.
                # scan through place records and find consecutive records that correspond to the same linked trip
                count_trips_on_tour = 0
                count_places_on_trip = 0
                cur_row = 0
                max_row = num_places_for_person - 1  # row index starts with 0 (not 1) and only need to scan up to the 2nd last row
                cur_tour_start_row = 0  # points to the origin (HOME) of the current tour
                cur_trip_start_row = 0  # points to the origin of the current trip
                new_trip = True
                cur_tourid = 0  # tour no. gets incremented whenever a new tour is encountered
                cur_tripid = 0  # trip no. is incremented whenever a new trip is encountered

                while (cur_row < max_row):
                    # current PLACE marks the start of a new tour
                    # create a new tour object for the person
                    cur_tourid = cur_tourid + 1
                    tour = Tour(hh, psn, self.constants, cur_tourid)
                    psn.add_tour(tour)
                    cur_tripid = 0

                    # true if next PLACE marks the start of a different tour
                    new_tour = (df_psn_places['TPURP'].iloc[cur_row + 1] in SurveyHomeCode)
                    # TPURP codes 1,2 mean 'home'
                    new_trip = (df_psn_places['TPURP'].iloc[cur_row + 1] != SurveyChangeModeCode) | \
                               (
                                (df_psn_places['TPURP'].iloc[cur_row + 1] == SurveyChangeModeCode) &
                                (cur_row + 1 == max_row)
                               )
                    # true if next PLACE marks the start of a different trip, or if next SPLACE is change mode and last stop of the day
                    # TPURP codes 7 means 'change mode'

                    if (new_tour | new_trip):
                        # found last place before the destination of the current trip
                        # create a new trip object to represent the current linked trip
                        cur_tripid = cur_tripid + 1
                        trip = Trip(hh, psn, tour, self.constants, cur_tripid)

                        # process current linked trip, which is described by rows starting at cur_trip_start_row and ending at cur_row+1
                        is_joint = trip.populate_attributes(df_psn_places[cur_trip_start_row:(cur_row + 2)])

                        # add trip to the current tour object
                        tour.add_trip(trip)

                        # add joint trip to the current household object to be processed later
                        if is_joint:
                            hh.add_joint_episodes(trip.get_joint_descriptions())

                        # reset counter/pointer for the next trip
                        count_places_on_trip = 0
                        cur_trip_start_row = cur_row + 1

                    # update row pointer
                    cur_row = cur_row + 1

                if num_places_for_person <= 1:  # each traveling person would have more than 1 PLACE entry
                    psn.log_warning("Did not travel")

                # end - processing PLACE records for the person

                # check if the first and last tours are partial tours
                num_HB_tours = len(psn.tours)
                for i, tour in enumerate(psn.tours):
                    _partial_code = PARTIAL_TOUR['NOT_PARTIAL']
                    if i == 0:  # 1st tour of the day - check orig of first trip
                        if not tour.trips[0].is_orig_home():
                            _partial_code = PARTIAL_TOUR['PARTIAL_START']
                            psn.log_warning("Was not at home at the beginning of day")

                    # note: need to use 'if' as opposed to 'elif' below because 'elif' would miss cases where a person can have only 1 tour that is PARTIAL_END
                    if i == (num_HB_tours - 1):  # last tour of the day - check dest of last trip
                        if not tour.trips[-1].is_dest_home():
                            _partial_code = PARTIAL_TOUR['PARTIAL_END']
                            psn.log_warning("Did not return home at the end of day")
                    tour.set_partial(_partial_code)

                # at this point, all items in the person's tours list are home-based tours
                # most tour-level fields have not yet been derived
                for tour in psn.tours:
                    # derive tour-level attributes based on trip objects
                    # work-based subtours are generated and added to the end of the tours list during the process
                    tour.populate_attributes()
                    # set direction of each trip
                    # this is required for generating escorting fields
                    for trip in tour.trips:
                        trip.set_trip_direction()

            # joint trips are processed after all tour/person level fields have been processed and inconsistencies identified

            hh.process_joint_episodes()
            hh.process_escort_trips()

            # identify joint tour after joint and escort trips have been identified
            hh.process_joint_tours()
            hh.process_escort_tours()  # derive tour-level escort-related fields from trip-level escort-related fields

            # propagate person-level attributes to tour-level, tour-level attributes to trip-level
            for psn in hh.persons:
                for tour in psn.tours:
                    tour.set_per_type(psn.get_per_type())
                    for trip in tour.trips:
                        trip.set_tour_attributes()
                        trip.set_per_type(psn.get_per_type())

        # print_in_separate_files(hh_list, OUT_DIR)
        functions.print_in_same_files(hh_list, OUT_DIR)

    def add_place_distance(self, route_file, place_file, out_file):
        # read in ROUTE records into a data frame object
        df_route = pd.read_csv(self.constants.get('IN_DIR') + route_file, quotechar='"', encoding='ISO-8859-1')
        # read in PLACE records into a data frame object, df_place
        df_place = pd.read_csv(self.constants.get('IN_DIR') + place_file, quotechar='"', encoding='ISO-8859-1')

        df_route = df_route.rename(columns={'DPLANO': 'PLANO'})
        route_grouped = df_route.groupby(['SAMPN', 'PERNO', 'OPLANO', 'PLANO']).sum().reset_index()
        df_place = pd.merge(df_place, route_grouped[['SAMPN', 'PERNO', 'PLANO', 'Distance']], how='left',
                            on=['SAMPN', 'PERNO', 'PLANO'])

        # write out to a new place file
        df_place.to_csv(self.constants.get('IN_DIR') + out_file)

    def print_in_separate_files(self, hh_list, out_dir):
        # specify output files
        hh_file = open(out_dir + 'clean_hh/households.csv', 'w')
        per_file = open(out_dir + 'clean_hh/persons.csv', 'w')
        trip_file = open(out_dir + 'clean_hh/trips.csv', 'w')
        tour_file = open(out_dir + 'clean_hh/tours.csv', 'w')
        joint_trip_file = open(out_dir + 'clean_hh/joint_ultrips.csv', 'w')
        unique_jtrip_file = open(out_dir + 'clean_hh/unique_joint_ultrips.csv', 'w')
        unique_jtour_file = open(out_dir + 'clean_hh/unique_joint_tours.csv', 'w')

        err_log_file = open(out_dir + 'error_log.txt', 'w')
        problem_hh_file = open(out_dir + 'err_hh/households_with_err.csv', 'w')
        problem_per_file = open(out_dir + 'err_hh/persons_from_err_hh.csv', 'w')
        problem_trip_file = open(out_dir + 'err_hh/trips_from_err_hh.csv', 'w')
        problem_tour_file = open(out_dir + 'err_hh/tours_from_err_hh.csv', 'w')
        problem_joint_trip_file = open(out_dir + 'err_hh/joint_ultrips_from_err_hh.csv', 'w')
        problem_unique_jtrip_file = open(out_dir + 'err_hh/unique_joint_ultrips_from_err_hh.csv', 'w')
        problem_unique_jtour_file = open(out_dir + 'err_hh/unique_joint_tours_from_err_hh.csv', 'w')
        recode_log_file = open(out_dir + 'recode_log.txt', 'w')

        # print column headers in tables
        Household.print_header(hh_file)
        Person.print_header(per_file)
        Trip.print_header(trip_file)
        Tour.print_header(tour_file)
        Joint_ultrip.print_header(joint_trip_file)
        Joint_ultrip.print_header_unique(unique_jtrip_file)
        Joint_tour.print_header(unique_jtour_file)

        Household.print_header(problem_hh_file)
        Person.print_header(problem_per_file)
        Trip.print_header(problem_trip_file)
        Tour.print_header(problem_tour_file)
        Joint_ultrip.print_header(problem_joint_trip_file)
        Joint_ultrip.print_header_unique(problem_unique_jtrip_file)
        Joint_tour.print_header(problem_unique_jtour_file)

        num_err_hh = 0
        for hh in hh_list:
            # print log of all recodes
            hh.print_recode_tags(recode_log_file)

            # households with error tag go to one set of files
            if hh.error_flag == True:
                # household contains error: print error messages to one file; print trips/j-trips to a separate trip file
                num_err_hh = num_err_hh + 1
                hh.print_tags(err_log_file)
                hh.print_vals(problem_hh_file)
                hh.print_joint_trips(problem_joint_trip_file)
                hh.print_unique_joint_trips(problem_unique_jtrip_file)
                hh.print_unique_joint_tours(problem_unique_jtour_file)
                for psn in hh.persons:
                    psn.print_vals(problem_per_file)
                    for tour in psn.tours:
                        for trip in tour.trips:
                            trip.print_vals(problem_trip_file)
                        tour.print_vals(problem_tour_file)
            else:
                # 'clean' households go to another set of files
                hh.print_vals(hh_file)
                hh.print_joint_trips(joint_trip_file)
                hh.print_unique_joint_trips(unique_jtrip_file)
                hh.print_unique_joint_tours(unique_jtour_file)
                for psn in hh.persons:
                    psn.print_vals(per_file)
                    for tour in psn.tours:
                        for trip in tour.trips:
                            trip.print_vals(trip_file)
                        tour.print_vals(tour_file)

        hh_file.close()
        per_file.close()
        trip_file.close()
        tour_file.close()
        joint_trip_file.close()
        unique_jtrip_file.close()
        unique_jtour_file.close()

        err_log_file.close()
        problem_hh_file.close()
        problem_per_file.close()
        problem_trip_file.close()
        problem_tour_file.close()
        problem_joint_trip_file.close()
        problem_unique_jtrip_file.close()
        recode_log_file.close()

        print("{} households processed.".format(len(hh_list)))
        print("{} households contain error.".format(num_err_hh))

    def print_in_same_files(self, hh_list, out_dir):
        """ print problematic records (relating to joint travel) in the same files as the 'clean' records """
        # specify output files
        hh_file = open(out_dir + 'households.csv', 'w')
        per_file = open(out_dir + 'persons.csv', 'w')
        trip_file = open(out_dir + 'trips.csv', 'w')
        joint_trip_file = open(out_dir + 'joint_ultrips.csv', 'w')
        unique_jtrip_file = open(out_dir + 'unique_joint_ultrips.csv', 'w')
        tour_file = open(out_dir + 'tours.csv', 'w')
        unique_jtour_file = open(out_dir + 'unique_joint_tours.csv', 'w')

        err_log_file = open(out_dir + 'error_log.txt', 'w')
        recode_log_file = open(out_dir + 'recode_log.txt', 'w')

        # print column headers in tables
        Trip.print_header(trip_file)
        Joint_ultrip.print_header(joint_trip_file)
        Joint_ultrip.print_header_unique(unique_jtrip_file)
        Tour.print_header(tour_file)
        Joint_tour.print_header(unique_jtour_file)
        Person.print_header(per_file)
        Household.print_header(hh_file)

        num_err_perons = 0
        num_err_hh = 0
        count_persons = 0
        count_tours = 0
        count_trips = 0

        for hh in hh_list:
            # print log of all recodes
            hh.print_recode_tags(recode_log_file)

            if hh.error_flag == True:
                num_err_hh = num_err_hh + 1
                hh.print_tags(err_log_file)

            hh.print_vals(hh_file)
            hh.print_joint_trips(joint_trip_file)
            hh.print_unique_joint_trips(unique_jtrip_file)
            hh.print_unique_joint_tours(unique_jtour_file)
            for psn in hh.persons:
                count_persons = count_persons + 1
                psn.print_vals(per_file)
                if psn.error_flag == True:
                    num_err_perons = num_err_perons + 1
                for tour in psn.tours:
                    count_tours = count_tours + 1
                    tour.print_vals(tour_file)
                    for trip in tour.trips:
                        count_trips = count_trips + 1
                        trip.print_vals(trip_file)

        hh_file.close()
        per_file.close()
        trip_file.close()
        joint_trip_file.close()
        tour_file.close()
        unique_jtrip_file.close()
        unique_jtour_file.close()
        err_log_file.close()
        recode_log_file.close()

        print("Processed {} households, {} individuals, {} person-trips, {} person-tours.".format(len(hh_list),
                                                                                                  count_persons,
                                                                                                  count_trips,
                                                                                                  count_tours))
        print("{} households contain error.".format(num_err_hh))
        print("{} persons contain error.".format(num_err_perons))
