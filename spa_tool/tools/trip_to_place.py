##################################################################
# Script for converting a trips table into a place table
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
from copy import deepcopy
from core import functions
from core.modules import SPAModelBase

# TODO Set this up so that it can be run as a workflow step
class TripsToPlace(SPAModelBase):
    def __init__(self, namespace, **kwargs):
        super().__init__(namespace, **kwargs)

    def run(self):
        self.trip_to_place()
        return self.place

    def trip_to_place(self):

        per = self.input_tables.get('person')
        hh = self.input_tables.get('household')
        trip = self.input_tables.get('trip')

        assert all([True for x in ['trip', 'household', 'person'] if x in self.input_tables.keys()]),\
            'trip, person, and household table required for trip to place conversion'

        # Make deep copy of trip for initial place file
        trip_place = deepcopy(trip)

        # Setup Place Number
        trip_place.rename(columns={'TRPNO': 'PLANO'}, inplace=True)
        trip_place.PLANO += 1

        # Trim columns
        # TODO Could create a keymap to generalize this
        trip_place = trip_place[[
            'SAMPN', 'PERNO', 'PLANO', 'DAYNO', 'TPURP', 'PNAME',
            'TAZ', 'YCORD', 'XCORD', 'WXCORD', 'WYCORD',
            'DEP_HR', 'DEP_MIN', 'ARR_HR', 'ARR_MIN',
            'PER1', 'PER2', 'PER3', 'PER4', 'PER5', 'PER6',
            'TOTTR', 'TOTTR_NEXT', 'HHMEM', 'MODE', 'TOLL_NO', 'DRIVER', 'DISTANCE'
        ]]

        # Setup home as first place for every person for every day
        first_place = pd.merge(
            per[['SAMPN', 'PERNO']],
            pd.DataFrame({'DAYNO': trip_place.DAYNO.unique()}),
            how='cross').set_index('SAMPN')

        # Add the Home XY coordinates
        first_place = first_place.join(hh.set_index('SAMPN')[['HH_ZONE_ID', 'HXCORD', 'HYCORD']])
        first_place['PLANO'] = 1
        first_place = first_place.rename(columns = {'HXCORD': 'XCORD',
                                                    'HYCORD': 'YCORD',
                                                    'HH_ZONE_ID': 'TAZ'})

        # Add the Work XY coordinates
        first_place = first_place.reset_index().merge(
            trip_place[['SAMPN', 'PERNO', 'WXCORD', 'WYCORD']].drop_duplicates(),
            on=['SAMPN', 'PERNO'],
            how='left'
        )

        # Add the home zones to 'places'
        place = pd.concat([first_place, trip_place], axis=0).reset_index(drop=True)

        # Slow groupby.apply method
        # def place_attributes(df):
        #     print(df.SAMPN.iloc[0])
        #     # Shift trip departure up one to be departure from place
        #     df[['DEP_HR', 'DEP_MIN', 'TOTTR_NEXT']] = df[['DEP_HR', 'DEP_MIN', 'TOTTR_NEXT']].shift(-1)
        #
        #     # Set new day at 3am
        #     df.loc[df.PLANO == 1, ['ARR_HR', 'ARR_MIN']] = [3, 0]
        #     df.loc[df.PLANO.idxmax(), ['DEP_HR', 'DEP_MIN']] = [2, 59]
        #
        #     # Set default home parameters
        #     df.loc[df.PLANO == 1, ['TOTTR', 'HHMEM']] = [-1, -1]
        #     df.loc[df.PLANO == 1, ['PER1', 'PER2', 'PER3', 'PER4', 'PER5', 'PER6']] = 0
        #
        #     return df
        # place = place.groupby(['SAMPN', 'PERNO', 'DAYNO']).apply(place_attributes)

        # Fast vectorized method
        # Assign correct attributes for place from trip data
        place = place.set_index(['SAMPN', 'PERNO', 'DAYNO']).sort_index(level=[0, 1, 2]).sort_values('PLANO')

        # Shift trip departure time up one to be departure from place
        shift_cols = ['DEP_HR', 'DEP_MIN', 'TOTTR_NEXT']
        place[shift_cols] = place.groupby(level=[0, 1, 2])[shift_cols].shift(-1)

        # Set missing traveler vals
        place.loc[place.PLANO == 1, ['TOTTR', 'HHMEM']] = -1
        place.loc[place.PLANO == 1, ['TPURP', 'MODE', 'DRIVER']] = 0
        #
        per_cols = ['PER1', 'PER2', 'PER3', 'PER4', 'PER5', 'PER6', 'TOTTR_NEXT']
        place[per_cols] = place[per_cols].fillna(0)

        # Set new day at 3am
        place.loc[place.PLANO == 1, ['ARR_HR', 'ARR_MIN']] = [3, 0]
        place = place.reset_index()
        place.loc[place.groupby(['SAMPN', 'PERNO', 'DAYNO']).PLANO.idxmax(), ['DEP_HR', 'DEP_MIN']] = [2, 59]

        # If buffer distance is missing
        if 'BUFFER_DIST' not in place.columns:
            place['BUFFER_DIST'] = functions.distance_on_unit_sphere(lat1=place.YCORD, long1=place.XCORD,
                                                                     lat2=place.WYCORD, long2=place.WXCORD)

        place = place.reset_index().rename(columns={'index': 'place_id'})

        self.place = place


    def save_tables(self, out_dir):
        if self.place is None:
            print('Data not loaded yet, run .run() on TripToPlace class')
            return
        else:
            self.place.to_csv(f'{out_dir}/place.csv', index=False)
        return

if __name__ == '__main__':
    import argparse
    from core.main import add_run_args

    # Test scripts
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.configs = 'C:\gitclones\Dubai_survey_processing\configs'
    args.data = 'C:\gitclones\Dubai_survey_processing\data'

    # Fetch data from the database
    T2P = TripsToPlace(args)
    T2P.run()
    T2P.save_tables(out_dir='../../data/preprocessed')


