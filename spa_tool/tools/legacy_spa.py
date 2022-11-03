import os
from core.functions import *
from core.configs import read_config
from tools.households import *
from tools.persons import *

class LegacySPA():
    def __init__(self, namespace, **kwargs):
        self.namespace = namespace
        self.kwargs = self.update_kwargs(**kwargs)
        self.input_tables = self.load_tables()

    def update_kwargs(self, **kwargs):
        if not kwargs.get('module'):
            settings_file = os.path.join(self.namespace.config, 'settings.yaml')
            kwargs = {**kwargs, **read_config(settings_file).get('PROCESSING_STEPS').get('LegacySPA')}

        return kwargs

    def load_tables(self):
        if self.kwargs.get('from_pipeline'):
            assert self.kwargs.get('pipeline').get(self.kwargs.get('data_directory'))
            input_tables = self.kwargs.get('pipeline').get(self.kwargs.get('data_directory'))
        else:
            dir = self.kwargs.get('data_directory')
            tables = self.kwargs.get('tables')

            file_list = self.get_appended_data_list(dir, tables)

            csv_dict = {os.path.splitext(os.path.split(k)[-1])[0]: k for k in file_list}
            input_tables = {k: pd.read_csv(path, index_col=f'{k}_id') for k, path in csv_dict.items()}

        return input_tables

    def get_appended_data_list(self, folder, tables):
        if not os.path.isabs(folder):
            sources = [self.namespace.data] if not isinstance(self.namespace.data, list) else self.namespace.data
            full_path = [os.path.join(dir, folder) for dir in sources if
                         os.path.isdir(os.path.join(dir, folder))]
        else:
            full_path = folder

        if len(full_path) != 1:
            print(f'{len(full_path)} files found for "{folder}" input! Expecting only 1.')
        assert len(full_path) == 1
        full_path = full_path[0]

        if tables:
            files = [os.path.join(full_path, x + '.csv') for x in tables]
        else:
            files = [os.path.join(full_path, x) for x in os.listdir(full_path) if not os.path.isdir(x) and '.csv' in x]

        if len(files) == 0:
            print('No input files found in this directory!')

        assert len(files) > 0

        return files


    def run(self):
        # Legacy Constants
        IN_DIR = self.kwargs.get('IN_DIR')
        OUT_DIR = self.kwargs.get('OUT_DIR_PATH')
        SurveyWorkPurp = self.kwargs.get('SurveyWorkPurp')
        SurveyWorkRelatedPurp = self.kwargs.get('SurveyWorkRelatedPurp')
        SurveyHomeCode = self.kwargs.get('SurveyHomeCode')
        SurveyChangeModeCode = self.kwargs.get('SurveyChangeModeCode')
        NewPartialTour = self.kwargs.get('NewPartialTour')
        WORK_LOCATION_BUFFER = self.kwargs.get('WORK_LOCATION_BUFFER')

        # add_place_distance('route.csv', 'place.csv', 'placeWithDist.csv' )
        # BMP[09/08/17] - survey data processed separately for each survey day
        assert self.kwargs.get('pipeline').get('preprocessed')
        for k in ['place', 'person', 'household']:
            assert self.kwargs.get('results').get(k)

        df_place = self.kwargs.get('results').get('place')
        df_per = self.kwargs.get('results').get('person')
        df_hh = self.kwargs.get('results').get('household')

        if not os.path.exists(OUT_DIR):
            os.makedirs(OUT_DIR)

        hh_list = []
        # loop through and process the PLACE data frame group for each household for each day
        # create the household, person, tour, and trip objects
        for (hhid), df_persons in df_place.groupby(['SAMPN']):

            # locate the corresponding hh in HOUSEHOLD table
            df_cur_hh = df_hh[(df_hh['SAMPN'] == hhid)]

            # create a new household object
            hh = Household(hhid, df_cur_hh['AREA'].iloc[0])
            hh_list.append(hh)

            # loop through each person
            for (hhid, pid), df_psn_places in df_persons.groupby(['SAMPN', 'PERNO']):

                # locate the corresponding person in PERSON table
                df_cur_per = df_per[(df_per['SAMPN'] == hhid) & (df_per['PERNO'] == pid)]

                # create a new person object
                psn = Person(hh, pid, df_cur_per)

                num_places_for_person = len(df_psn_places)
                print("No. of place entries for person " + str(pid) + " of household " + str(hhid) + ": " + str(
                    num_places_for_person))

                # first make sure that the place entries are ordered by place number since they will be processed sequentially later
                df_psn_places = df_psn_places.sort_index(by='PLANO')

                # recode TPURP from "work" or "other activities at work" to "work-related" if the PLACE is not the primary place of work
                (wxcord, wycord) = (df_cur_per['WXCORD'].iloc[0], df_cur_per['WYCORD'].iloc[0])
                if not ((wxcord == 0) | (wycord == 0)):
                    # both X and Y coordinates for work location are found
                    for row_index, row in df_psn_places.iterrows():
                        # check place coordinates against work coordinates if TPURP is work or other activities at work
                        if row['TPURP'] in SurveyWorkPurp:
                            xcord = row['XCORD']
                            ycord = row['YCORD']
                            buffer_dist = 0
                            if not ((xcord == 0) | (ycord == 0)):
                                buffer_dist = row['bufferDist']
                            # if (not xcord==wxcord) | (not ycord==wycord):
                            if (buffer_dist > WORK_LOCATION_BUFFER):
                                # not the place of work -> recode TPURP to work-related
                                df_psn_places['TPURP'][row_index] = SurveyWorkRelatedPurp
                                psn.log_recode(
                                    "work activity reported for PLANO={}, which is not the primary work location; recode activity as work-related".format(
                                        row['PLANO']))

                # scan through place records and find consecutive records that correspond to the same linked trip
                count_trips_on_tour = 0
                count_places_on_trip = 0
                cur_row = 0
                max_row = num_places_for_person - 1  # row index starts with 0 (not 1) and only need to scan up to the 2nd last row
                cur_tour_start_row = 0  # points to the origin (HOME) of the current tour
                cur_trip_start_row = 0  # points to the origin of the current trip
                new_tour = True  # initialize to true so that a new tour object will be created upon entering the while loop
                new_trip = True
                cur_tourid = 0  # tour no. gets incremented whenever a new tour is encountered
                cur_tripid = 0  # trip no. is incremented whenever a new trip is encountered

                while (cur_row < max_row):

                    if new_tour:
                        # current PLACE marks the start of a new tour
                        # create a new tour object for the person
                        cur_tourid = cur_tourid + 1
                        tour = Tour(hh, psn, cur_tourid)
                        psn.add_tour(tour)
                        cur_tripid = 0

                    # true if next PLACE marks the start of a different tour
                    new_tour = (df_psn_places['TPURP'].iloc[cur_row + 1] in SurveyHomeCode)
                    # TPURP codes 1,2 mean 'home'
                    new_trip = (df_psn_places['TPURP'].iloc[cur_row + 1] != SurveyChangeModeCode) | (
                                (df_psn_places['TPURP'].iloc[cur_row + 1] == SurveyChangeModeCode) & (
                                    cur_row + 1 == max_row))
                    # true if next PLACE marks the start of a different trip, or if next PLACE is change mode and last stop of the day
                    # TPURP codes 7 means 'change mode'

                    if (new_tour | new_trip):
                        # found last place before the destination of the current trip
                        # create a new trip object to represent the current linked trip
                        cur_tripid = cur_tripid + 1
                        trip = Trip(hh, psn, tour, cur_tripid)

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
                    _partial_code = NewPartialTour['NOT_PARTIAL']
                    if i == 0:  # 1st tour of the day - check orig of first trip
                        if not tour.trips[0].is_orig_home():
                            _partial_code = NewPartialTour['PARTIAL_START']
                            psn.log_warning("Was not at home at the beginning of day")

                    # note: need to use 'if' as opposed to 'elif' below because 'elif' would miss cases where a person can have only 1 tour that is PARTIAL_END
                    if i == (num_HB_tours - 1):  # last tour of the day - check dest of last trip
                        if not tour.trips[-1].is_dest_home():
                            _partial_code = NewPartialTour['PARTIAL_END']
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
        print_in_same_files(hh_list, OUT_DIR)
