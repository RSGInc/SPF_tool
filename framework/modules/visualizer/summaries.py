"""
Visualizer functions mixin class

The purpose of this is to define our summary functions cleanly in a separate file,
but still be able to use them in the Visualizer class.

"""

import itertools
import pandas as pd
import numpy as np
from core import utils
from copy import deepcopy

# TODO Modify SPA output to include the features we pulled from raw and preprocessed data
#  HH_VEH and HH_SIZE

class VisualizerHelperFunctions:

    def setup_intermediate_data(self):
        """
         Setup any intermediate data / calculations in here.

         This is useful to avoid repeating calculations, but want to keep this separate
         from the main model and input tables to avoid cross-contaminating our data.

        """
        self.setup_households()
        self.setup_person_day()
        self.setup_household_day()
        self.setup_joint_household()
        self.setup_faux_skims()
        self.setup_stops()

    # Internal functions
    def tour_type_counts(self):
        tours = deepcopy(self.input_tables.get('spa_tours'))
        tours[['HH_ID', 'PER_ID']] = tours[['HH_ID', 'PER_ID']].astype(int)

        # Ensure all persons/days are included
        person_idx = self.input_tables.get('spa_person')[['HH_ID', 'PER_ID']]
        person_idx = person_idx.astype(int).drop_duplicates().reset_index(drop=True)

        # Find all possible person-days
        _index = pd.MultiIndex.from_product([person_idx.index, tours.DAYNO.unique()],
                                              names=['HH_ID-PER_ID', 'DAYNO'])
        perdays_idx = pd.DataFrame(index=_index).reset_index()
        perdays_idx = perdays_idx.join(person_idx).drop(columns='HH_ID-PER_ID')

        # Get tour counts
        grp_cols = ["HH_ID", "PER_ID", "DAYNO"]
        tour_type_counts = {}

        # First define the filters
        # [excluding at work subtours]. joint work tours should be considered individual mandatory tours
        tour_type_counts['work'] = tours[(tours.TOURPURP == 1) & (tours.IS_SUBTOUR==0)]
        tour_type_counts['atwork'] = tours[(tours.TOURPURP == 20) & (tours.IS_SUBTOUR == 1)]
        tour_type_counts['schl'] = tours[tours.TOURPURP.isin([2, 3])]
        tour_type_counts['inm'] = tours[
            (tours.TOURPURP.isin([4, 5, 6, 7, 8, 9]) &
             (tours.FULLY_JOINT == 0) & (tours.IS_SUBTOUR == 0)) |
            ((tours.TOURPURP == 4) & (tours.FULLY_JOINT == 1))
            ]
        tour_type_counts['itour'] = tours[
            ((tours.TOURPURP <= 9) & (tours.IS_SUBTOUR == 0) & (tours.FULLY_JOINT == 0)) |
            ((tours.TOURPURP <= 4) & (tours.FULLY_JOINT == 1))
            ]
        tour_type_counts['jtour'] = tours[
            (tours.TOURPURP.isin([5, 6, 7, 8, 9]) & (tours.IS_SUBTOUR == 0) & (tours.FULLY_JOINT == 1))
            ]

        for tour_type, tour_type_df in tour_type_counts.items():
            tour_type_df = tour_type_df.groupby(grp_cols).size().to_frame(tour_type + '_count')
            tour_type_df = perdays_idx.set_index(grp_cols).join(tour_type_df).fillna(0)
            tour_type_counts[tour_type] = tour_type_df.astype(int)

        return tour_type_counts

    def setup_households(self):
        # Truncate HH_SIZE to 5 plus
        self.input_tables['spa_household']['HH_SIZE_trunc'] = self.input_tables['spa_household'].NUM_PERS.clip(1, 5)

    def setup_person_day(self):
        person_day = deepcopy(self.input_tables.get('spa_person'))
        tour_counts = self.tour_type_counts()

        # Merge counts onto person_day table
        for count_name, counts in tour_counts.items():
            if 'DAYNO' in person_day.columns:
                person_day = person_day.merge(counts, on=['HH_ID', 'PER_ID', 'DAYNO'], how='left')
            else:
                person_day = person_day.merge(counts.reset_index('DAYNO'), on=['HH_ID', 'PER_ID'], how='left')

        # Assign activity pattern
        person_day['num_tours'] = person_day[['itour_count', 'jtour_count']].sum(axis=1)
        person_day['DAP'] = 'H'
        person_day.loc[(person_day.work_count > 0) | (person_day.schl_count > 0), 'DAP'] = 'M'
        person_day.loc[(person_day.num_tours > 0) & (person_day.DAP == 'H'), 'DAP'] = 'N'
        person_day.loc[person_day.PERSONTYPE.isin([4, 5]) | (person_day.DAP == "M"), 'DAP'] = 'N'

        self.intermediate_data['person_day'] = person_day

    def setup_faux_skims(self):
        grp_cols = ['SAMPN', 'PERNO', 'DAYNO']
        skim_cols = ['TAZ', 'XCORD', 'YCORD', 'DISTANCE']
        place = deepcopy(self.input_tables['pre_place'][grp_cols + skim_cols])
        place = place[place.TAZ != 0] # 0 TAZ was fillna(0)

        # Setup zone data
        # FIXME The zone file is not aligned with TAZs?
        #  Going to use mean XY from place file instead
        # zones = self.input_tables['zones']
        # zones = zones.loc[~zones.OLDZONE.isnull(), ['OLDZONE', 'X', 'Y']]
        # zones = zones.rename(columns={'OLDZONE': 'TAZ', 'X': 'XCORD', 'Y': 'YCORD'})
        # zones.TAZ = zones.TAZ.astype(int)
        # set(zones.TAZ.unique()).difference(place.TAZ.unique())
        # set(place.TAZ.unique()).difference(zones.TAZ.unique())
        zones = place.groupby('TAZ')[['XCORD', 'YCORD']].mean()
        zones = zones[~zones.index.isin([0])].reset_index()

        # The XY cord for destination is the current place and origin is the previous place,
        # shifting place down 1 for each person-day, and drop the first place in each.
        _dests = place[skim_cols].rename(columns={'TAZ': 'dTAZ', 'XCORD': 'dX', 'YCORD': 'dY'})
        _origins = place.groupby(['SAMPN', 'PERNO', 'DAYNO']).shift(1)
        _origins = _origins.rename(columns={'TAZ': 'oTAZ', 'XCORD': 'oX', 'YCORD': 'oY'}).drop(columns='DISTANCE')

        # Left join, dropping NA home places
        place_skims = _origins[~_origins.oTAZ.isnull()].join(_dests)
        place_skims.oTAZ = place_skims.oTAZ.astype(int)

        # Drop duplicates, take median for repeat OD pairs to avoid outliers
        place_skims = place_skims.drop_duplicates()
        place_skims = place_skims.groupby(['oTAZ', 'dTAZ']).median()

        # Create cartesian product to find missing ODs
        TAZ_list = place.TAZ.unique()
        _index = pd.MultiIndex.from_product([TAZ_list, TAZ_list], names=['oTAZ', 'dTAZ'])
        _skims = pd.DataFrame(index=_index).reset_index()
        _skims = _skims.merge(place_skims, on=['oTAZ', 'dTAZ'], how='left').drop_duplicates()

        # Separate NA skim ODs
        na_skims = _skims.loc[_skims.DISTANCE.isnull(), ['oTAZ', 'dTAZ']]
        ok_skims = _skims[~_skims.DISTANCE.isnull()]

        # Fill in missing XY from zone data
        na_skims = na_skims.merge(zones.rename(columns={'TAZ': 'oTAZ', 'XCORD': 'oX', 'YCORD': 'oY'}), on='oTAZ')
        na_skims = na_skims.merge(zones.rename(columns={'TAZ': 'dTAZ', 'XCORD': 'dX', 'YCORD': 'dY'}), on='dTAZ')

        # Calculate distance
        na_skims['DISTANCE'] = utils.distance_on_unit_sphere(
            lat1=na_skims.oY, long1=na_skims.oX, lat2=na_skims.dY, long2=na_skims.dX
        )

        # Recombine
        faux_skims = pd.concat([na_skims, ok_skims], axis=0).set_index(['oTAZ', 'dTAZ'])
        assert not any(faux_skims.reset_index().duplicated()), 'Duplicate TAZ pairs in faux skims'

        if not faux_skims[faux_skims.duplicated()].empty:
            print('Some OD data appears duplicated?')
            # print(faux_skims[faux_skims.duplicated()])

        self.intermediate_data['faux_skims'] = faux_skims

    def setup_stops(self):
        tours = deepcopy(self.input_tables['spa_tours'])
        trips = deepcopy(self.input_tables['spa_trips'])

        # TODO maybe use real skim distances?
        skims = deepcopy(self.intermediate_data['faux_skims'])

        # Merge TOUR OD TAZ to trips
        tours = tours.rename(columns={'ORIG_TAZ': 'TOUROTAZ', 'DEST_TAZ': 'TOURDTAZ'}).reset_index()
        trips = trips.merge(
            tours[['HH_ID', 'PER_ID', 'TOUR_ID', 'DAYNO', 'TOUROTAZ', 'TOURDTAZ']],
            how='left',
            on=['HH_ID', 'PER_ID', 'TOUR_ID', 'DAYNO']
        )

        # Pull out stops & drop NA taz
        stops = trips[(trips.DEST_PURP > 0) & (trips.DEST_IS_TOUR_DEST == 0)]
        # stops = stops[~trips.DEST_PURP.isin([0, 11, 12])]
        stops = stops[(stops.ORIG_TAZ != 0) & (stops.DEST_TAZ != 0) &
                      (stops.TOURDTAZ != 0) & (stops.TOUROTAZ != 0)]

        # Assign final taz
        stops.loc[stops.IS_INBOUND == 0, 'FINAL_TAZ'] = stops.loc[stops.IS_INBOUND == 0, 'TOURDTAZ']
        stops.loc[stops.IS_INBOUND == 1, 'FINAL_TAZ'] = stops.loc[stops.IS_INBOUND == 1, 'TOUROTAZ']

        # Get Origin final Destination dist (OD), origin to stop dist (OS), Stop to final dest dist (SD)
        od = list(zip(stops.ORIG_TAZ, stops.FINAL_TAZ.astype(int)))
        os = list(zip(stops.ORIG_TAZ, stops.DEST_TAZ.astype(int)))
        sd = list(zip(stops.DEST_TAZ, stops.FINAL_TAZ.astype(int)))

        # Assign distances from skim matrix
        stops['OD_DIST'] = skims.loc[od].DISTANCE.to_list()
        stops['OS_DIST'] = skims.loc[os].DISTANCE.to_list()
        stops['SD_DIST'] = skims.loc[sd].DISTANCE.to_list()
        stops['OUT_DIR_DIST'] = stops.OS_DIST + stops.SD_DIST - stops.OD_DIST

        self.intermediate_data['stops'] = stops

    def get_joint_purp_list(self):
        stop_purposes = self.constants.get('STOP_PURPOSES')['JOINT']
        return  [x for _, sublist in stop_purposes.items() for x in sublist]

    def setup_household_day(self):
        df_perday = deepcopy(self.intermediate_data['person_day'])
        df_hh = deepcopy(self.input_tables['spa_household'])

        # Create hhday table
        df_hhday = df_hh.merge(df_perday[['HH_ID', 'DAYNO']].drop_duplicates(), on='HH_ID', how='right')
        # df_hhday.HH_ID = df_hhday.HH_ID.astype(int)
        df_hhday = df_hhday.set_index('HH_ID')

        # Calc NDAYS per household to normalize the hhday weight
        df_hhday = df_hhday.join(df_hhday.groupby(['HH_ID', 'DAYNO']).size().to_frame('NDAYS'))
        df_hhday['HHDAY_WEIGHT'] = df_hhday.HH_WEIGHT / df_hhday.NDAYS

        self.intermediate_data['household_day'] = df_hhday

    def setup_joint_household(self):
        df_hhday = deepcopy(self.intermediate_data['household_day'])
        df_jtours = deepcopy(self.input_tables['spa_jtours'])
        JT_PURPS = self.get_joint_purp_list()

        # Get relevant joint purposes
        jt_purp_freq = deepcopy(df_jtours[df_jtours.JOINT_PURP.isin(JT_PURPS)])
        jt_purp_freq.JOINT_PURP = jt_purp_freq.JOINT_PURP.astype(int)

        # Get counts by joint purpose
        jt_purp_freq = jt_purp_freq.groupby(['HH_ID', 'DAYNO', 'JOINT_PURP']).size().to_frame('FREQ')
        jt_purp_freq = jt_purp_freq.reset_index().pivot(index=['HH_ID', 'DAYNO'], columns='JOINT_PURP', values='FREQ')

        # Add in non-joint hh_ids
        jt_purp_freq = jt_purp_freq.reindex(index=df_hhday.index).fillna(0).astype(int)

        # Col names and Total column as binary
        prefix = jt_purp_freq.columns.name
        jt_purp_freq.columns = [jt_purp_freq.columns.name + str(x) for x in jt_purp_freq.columns]
        jt_purp_freq['JOINT'] = jt_purp_freq.apply(lambda x: int(x.sum() > 0), axis=1)

        # Assign JTF alt codes
        jt_purp_freq.loc[jt_purp_freq.JOINT == 0, 'JTF'] = 1

        # Setup multi-purpose sequence
        multi_purps = [(a, b) for i, a in enumerate(JT_PURPS) for b in JT_PURPS[(i+1):]]

        for i, purp in enumerate(JT_PURPS):
            purp_x, purp_y = multi_purps[i]
            id = i + 1
            jt_purp_freq.loc[jt_purp_freq[prefix + str(purp)] == 1, 'JTF'] = id
            jt_purp_freq.loc[jt_purp_freq[prefix + str(purp)] > 1, 'JTF'] = id + len(JT_PURPS) + 1
            jt_purp_freq.loc[(jt_purp_freq[prefix + str(purp_x)] > 0) &
                             (jt_purp_freq[prefix + str(purp_y)] > 0), 'JTF'] = id + 2*len(JT_PURPS) + 1

        df_hhday = df_hhday.join(jt_purp_freq[['JOINT', 'JTF']])

        self.intermediate_data['household_day'] = df_hhday

    def get_weighted_sum(self, df, grp_cols, wt_col):
        # Get the groupby count sum
        df_sum = df.groupby(grp_cols)[wt_col].sum()
        df_sum = df_sum.reset_index().rename(columns={wt_col: 'freq'})

        return df_sum


class VisualizerSummaries:
    # FIXME a lot of these groupby summaries are repetive and can be parameterized?
    def activePertypeDistbn(self):
        return self.get_weighted_sum(
            df=self.intermediate_data['person_day'],
            grp_cols='PERSONTYPE',
            wt_col='PER_WEIGHT'
        )

    def autoOwnership(self):
        hhveh_count = self.get_weighted_sum(
            df=self.input_tables.get('pre_household'),
            grp_cols='HH_VEH',
            wt_col='HH_WEIGHT'
        )
        hhveh_count.HH_VEH = hhveh_count.HH_VEH.astype(int)
        return hhveh_count

    def avgStopOutofDirectionDist_vis(self):
        stops = deepcopy(self.intermediate_data['stops'])

        average_stop_out_dist = {}
        for tour_type, purposes in self.constants.get('STOP_PURPOSES').items():
            is_joint = 1 if tour_type == 'JOINT' else 0
            is_sub = 1 if tour_type == 'SUBTOUR' else 0
            for k, tpurps in purposes.items():
                tpurps = tpurps if isinstance(tpurps, list) else [tpurps]

                df_filtered = stops[stops.TOURPURP.isin(tpurps) &
                                    (stops.FULLY_JOINT == is_joint) &
                                    (stops.SUBTOUR == is_sub)]
                if df_filtered.empty:
                    avg = None
                else:
                    avg = np.average(df_filtered.OUT_DIR_DIST, weights=df_filtered.TRIP_WEIGHT)

                average_stop_out_dist[k] = avg

        return average_stop_out_dist

    def dapSummary_vis(self):
        dap_summary = self.get_weighted_sum(
            df=self.intermediate_data.get('person_day'),
            grp_cols=['PERSONTYPE', 'DAP'],
            wt_col='PER_WEIGHT'
        )

        # Get totals
        margins = dap_summary.groupby('DAP').sum().reset_index()
        margins['PERSONTYPE'] = 'Total'
        dap_summary = pd.concat([dap_summary, margins])

        return dap_summary

    def hhSizeDist(self):
        return self.get_weighted_sum(df=self.input_tables['spa_household'],
                                     grp_cols='HH_SIZE_trunc',
                                     wt_col='HH_WEIGHT')

    def hhsizeJoint(self):
        return self.get_weighted_sum(df=self.intermediate_data['household_day'],
                                     grp_cols=['HH_SIZE_trunc', 'JOINT'],
                                     wt_col='HH_WEIGHT')

    def inmSummary_vis(self):
        tours = deepcopy(self.input_tables['spa_tours'])
        perday = deepcopy(self.intermediate_data['person_day'])
        JOINT_PURPS = self.get_joint_purp_list()

        # Get individual non-mandatory tour count
        inm_filter = tours.TOURPURP.isin(JOINT_PURPS) & (tours.FULLY_JOINT == 0) & (tours.IS_SUBTOUR == 0)
        inm_tour_count = tours[inm_filter].groupby(['HH_ID', 'PER_ID', 'DAYNO']).size().to_frame('nmtours')

        # Merge onto person days
        perday = perday.merge(inm_tour_count, on=['HH_ID', 'PER_ID', 'DAYNO'], how='left')
        perday.nmtours = perday.nmtours.fillna(0).astype(int)

        # Aggregate tour counts by person type and tour frequency
        perday.nmtours = perday.nmtours.clip(0, 3)
        inm_summary = perday.groupby(['PERSONTYPE', 'nmtours']).PER_WEIGHT.sum().reset_index()
        return inm_summary.rename(columns={'PER_WEIGHT': 'freq'})

    def joint_summary(self):
        # jtfSummary <- plyr::count(hhday[!is.na(hhday$jtf),], c("jtf"), "finalweight")
        # jointComp <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION"), "finalweight")
        # jointPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("NUMBER_HH"), "finalweight")
        # jointCompPartySize <- plyr::count(jtours[jtours$JOINT_PURP>=5 & jtours$JOINT_PURP<=9,], c("COMPOSITION","NUMBER_HH"), "finalweight")

        def jointComp(self):
                pass
        def jointCompPartySize(self):
                pass
        def jointPartySize(self):
                pass
        def jointToursHHSize(self):
                pass
    def jtf(self):
        jtf = self.intermediate_data['household_day'].groupby('JTF').HH_WEIGHT.sum()
        jtf.index = jtf.index.astype(int)

        jtf_alts = self.constants.get('JTF_ALTS')
        jtf_alts = pd.Series(jtf_alts.keys(), index=jtf_alts.values(), name='alt_name')
        jtf_summary = pd.concat([jtf_alts, jtf], axis=1).fillna(0).reset_index()
        return jtf_summary.rename(columns={'index': 'jtf_code', 'HH_WEIGHT': 'freq'})

    def mtfSummary(self):
            pass
    def mtfSummary_vis(self):
            pass
    def nm_tour_rates(self):
            pass
    def pertypeDistbn(self):
            pass

    def stops_summary(self):
        def stopDC_vis(self):
                pass
        def stopDeparture(self):
                pass
        def stopfreqDir_vis(self):
                pass
        def stopFreqInbProfile(self):
                pass
        def stopFreqModel_summary(self):
                pass
        def stopFreqOutProfile(self):
                pass
        def stopFreqTotProfile(self):
                pass
        def stopOutOfDirectionDC(self):
                pass
        def stopPurposeByTourPurpose(self):
                pass
        def stoppurpose_tourpurpose_vis(self):
                pass
        def stopTripDep_vis(self):
                pass

    def tmodeAS0Profile_HTS(self):
            pass
    def tmodeAS1Profile_HTS(self):
            pass
    def tmodeAS2Profile_HTS(self):
            pass
    def tmodeProfile_vis_HTS(self):
            pass

    def tod_summary(self):
        def todArrProfile(self):
                pass
        def todDepProfile(self):
                pass
        def todDurProfile(self):
                pass
        def todProfile_vis(self):
                pass
    def totals(self):
            pass
    def total_tours_by_pertype_vis(self):
            pass

    def tours_summary(self):
        def tourModeChoice(self):
                pass
        def toursPertypeDistbn(self):
                pass
        def tours_pertype_purpose(self):
                pass
        def tours_purpose_type(self):
                pass
        def tours_test(self):
                pass

    def trips_summary(self):
        def tripDeparture(self):
                pass
        def tripModeChoice(self):
                pass
        def tripModeProfile_ATW_HTS(self):
                pass
        def tripModeProfile_iDisc_HTS(self):
                pass
        def tripModeProfile_iMain_HTS(self):
                pass
        def tripModeProfile_jDisc_HTS(self):
                pass
        def tripModeProfile_jMain_HTS(self):
                pass
        def tripModeProfile_Schl_HTS(self):
                pass
        def tripModeProfile_Total_HTS(self):
                pass
        def tripModeProfile_Univ_HTS(self):
                pass
        def tripModeProfile_vis_HTS(self):
                pass
        def tripModeProfile_Work_HTS(self):
                pass
        def trips_flow(self):
                pass
