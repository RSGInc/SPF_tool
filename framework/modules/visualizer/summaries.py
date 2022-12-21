"""
Visualizer functions mixin class

The purpose of this is to define our summary functions cleanly in a separate file,
but still be able to use them in the Visualizer class.

"""

import pandas as pd
import numpy as np
from core import utils
from copy import deepcopy


class VisualizerHelperFunctions:

    def setup_intermediate_data(self):
        """
         Setup any intermediate data / calculations in here.

         This is useful to avoid repeating calculations, but want to keep this separate
         from the main model and input tables to avoid cross-contaminating our data.

        """
        self.setup_weights()
        self.setup_households()
        self.setup_person_day()
        self.setup_household_day()
        self.setup_joint_household()
        self.setup_faux_skims()
        self.setup_stops()


    # Internal functions
    def setup_tour_type_counts(self):
        tours = deepcopy(self.scenario_data.get('spa_tours'))
        tours[['HH_ID', 'PER_ID']] = tours[['HH_ID', 'PER_ID']].astype(int)

        # Ensure all persons/days are included
        person_idx = self.scenario_data.get('spa_person')[['HH_ID', 'PER_ID']]
        person_idx = person_idx.astype(int).drop_duplicates().reset_index(drop=True)

        # Find all possible person-days
        _index = pd.MultiIndex.from_product([person_idx.index, tours.DAYNO.unique()],
                                              names=['HH_ID-PER_ID', 'DAYNO'])
        perdays_idx = pd.DataFrame(index=_index).reset_index()
        perdays_idx = perdays_idx.join(person_idx).drop(columns='HH_ID-PER_ID')

        # Get tour counts
        grp_cols = ["HH_ID", "PER_ID", "DAYNO"]
        tour_type_counts = {}

        joint_purp_list = self.get_joint_purp_list()

        # First define the filters
        # [excluding at work subtours]. joint work tours should be considered individual mandatory tours
        tour_type_counts['work'] = tours[(tours.TOURPURP == 1) & (tours.IS_SUBTOUR==0)]
        tour_type_counts['atwork'] = tours[(tours.TOURPURP == 20) & (tours.IS_SUBTOUR == 1)]
        tour_type_counts['schl'] = tours[tours.TOURPURP.isin([2, 3])]
        tour_type_counts['inm'] = tours[
            (tours.TOURPURP.isin([4] + joint_purp_list) &
             (tours.FULLY_JOINT == 0) & (tours.IS_SUBTOUR == 0)) |
            ((tours.TOURPURP == 4) & (tours.FULLY_JOINT == 1))
            ]
        tour_type_counts['itour'] = tours[
            ((tours.TOURPURP <= 9) & (tours.IS_SUBTOUR == 0) & (tours.FULLY_JOINT == 0)) |
            ((tours.TOURPURP <= 4) & (tours.FULLY_JOINT == 1))
            ]
        tour_type_counts['jtour'] = tours[
            (tours.TOURPURP.isin(joint_purp_list) &
             (tours.IS_SUBTOUR == 0) &
             (tours.FULLY_JOINT == 1))
            ]

        for tour_type, tour_type_df in tour_type_counts.items():
            tour_type_df = tour_type_df.groupby(grp_cols).size().to_frame(tour_type + '_count')
            tour_type_df = perdays_idx.set_index(grp_cols).join(tour_type_df).fillna(0)
            tour_type_counts[tour_type] = tour_type_df.astype(int)

        self.intermediate_data['tour_type_counts'] = pd.concat(tour_type_counts, axis=1).droplevel(0, axis=1)
        # return tour_type_counts

    def setup_households(self):
        # Truncate HH_SIZE to 5 plus
        self.scenario_data['spa_household']['HH_SIZE_trunc'] = self.scenario_data['spa_household'].HH_SIZE.clip(1, 5)

    def setup_person_day(self):

        self.setup_tour_type_counts()

        person_day = deepcopy(self.scenario_data.get('spa_person'))
        tour_counts = deepcopy(self.intermediate_data['tour_type_counts'])

        # Merge counts onto person_day table
        person_day = person_day.merge(tour_counts.reset_index(), on=['HH_ID', 'PER_ID'])

        # Assign activity pattern
        person_day['num_tours'] = person_day[['itour_count', 'jtour_count']].sum(axis=1)
        person_day['DAP'] = 'H'
        person_day.loc[(person_day.work_count > 0) | (person_day.schl_count > 0), 'DAP'] = 'M'
        person_day.loc[(person_day.num_tours > 0) & (person_day.DAP == 'H'), 'DAP'] = 'N'
        person_day.loc[person_day.PERSONTYPE.isin([4, 5]) | (person_day.DAP == "M"), 'DAP'] = 'N'

        # Get location attributes
        df_hh = self.scenario_data['spa_household'][['HH_ID', 'HH_ZONE_ID', 'HXCORD', 'HYCORD']]
        person_day = person_day.merge(df_hh, on='HH_ID')

        # Output
        self.intermediate_data['person_day'] = person_day

    def setup_faux_skims(self):
        grp_cols = ['SAMPN', 'PERNO', 'DAYNO']
        skim_cols = ['TAZ', 'XCORD', 'YCORD', 'DISTANCE']
        place = deepcopy(self.scenario_data['pre_place'][grp_cols + skim_cols])
        place = place[place.TAZ != 0] # 0 TAZ was fillna(0)

        # Setup zone data
        # FIXME The zone file is not aligned with TAZs?
        #  Going to use mean XY from place file instead
        # zones = self.scenario_data['zones']
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
        tours = deepcopy(self.scenario_data['spa_tours'])
        trips = deepcopy(self.scenario_data['spa_trips'])

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
        stops['OUT_DIR_DIST'] = (stops.OS_DIST + stops.SD_DIST) - stops.OD_DIST

        self.intermediate_data['stops'] = stops

    def get_joint_purp_list(self):
        stop_purposes = self.constants.get('STOP_PURPOSES')['JOINT']
        return  [x for _, sublist in stop_purposes.items() for x in sublist]

    def setup_household_day(self):
        df_perday = deepcopy(self.intermediate_data['person_day'])
        df_hh = deepcopy(self.scenario_data['spa_household'])

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
        df_jtours = deepcopy(self.scenario_data['spa_jtours'])
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
        jt_purp_freq['JTOURS'] = jt_purp_freq.apply(lambda x: x.sum(), axis=1)
        jt_purp_freq['JOINT'] = (jt_purp_freq['JTOURS'] > 0).astype(int)

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

        df_hhday = df_hhday.join(jt_purp_freq[['JOINT', 'JTF', 'JTOURS']])
        df_hhday.JTF = df_hhday.JTF.astype(int)
        df_hhday['JOINT_CAT'] = df_hhday.JTOURS.clip(0, 3)

        self.intermediate_data['household_day'] = df_hhday

    def setup_weights(self):
        """
        Adds necessary weights to associated files
        """

        # Tour weights
        if 'TOUR_WEIGHT' not in self.scenario_data['spa_tours'].columns:
            tour_id_cols = ['HH_ID', 'PER_ID', 'TOUR_ID', 'DAYNO']
            df_tours = self.scenario_data['spa_tours'].merge(
                self.scenario_data['spa_trips'].groupby(tour_id_cols).TRIP_WEIGHT.mean(),
                on=tour_id_cols
            ).rename(columns={'TRIP_WEIGHT': 'TOUR_WEIGHT'})
            self.scenario_data['spa_tours'] = df_tours

        # Joint tour weights
        if 'TOUR_WEIGHT' not in self.scenario_data['spa_jtours'].columns:
            jtours_id_cols = ['HH_ID', 'JTOUR_ID', 'DAYNO']
            df_jtours = self.scenario_data['spa_jtours'].merge(
                df_tours.groupby(jtours_id_cols).TOUR_WEIGHT.mean(),
                on=jtours_id_cols
            )
            self.scenario_data['spa_jtours'] = df_jtours

    def flatten_joint_purposes(self):
        # Create joint purpose key map
        purpose_groups = deepcopy(self.constants.get('STOP_PURPOSES'))

        # Make the joint purposes identifiable
        purpose_groups['JOINT'] = {k: [x+100 for x in v] for k, v in purpose_groups['JOINT'].items()}
        purpose_groups = {k: v if isinstance(v, list) else [v] for
                          _, sublist in purpose_groups.items()
                          for k, v in sublist.items()}
        purpose_map = {val: key for key, lst in purpose_groups.items() for val in lst}

        return purpose_map

    def map_joint_purposes(self, df):
        # Create joint purpose key map
        joint_purp_list = self.get_joint_purp_list()
        purpose_map = self.flatten_joint_purposes()
        work_related = [code for code, label in purpose_map.items() if label == 'WORK-RELATED'][0]

        if 'SUBTOUR' in df.columns:
            df['IS_SUBTOUR'] = df.SUBTOUR

        # Make joint/subtour purposes identiable
        df['JOINT_TOURPURP'] = df.TOURPURP
        df.loc[df.JOINT_TOURPURP.isin(joint_purp_list) &
                     (df.FULLY_JOINT == 0), 'JOINT_TOURPURP'] += 100
        df.loc[df.IS_SUBTOUR == 1, 'JOINT_TOURPURP'] = work_related

        # Remap to tourpurp groups
        df['JOINT_TOURPURP'] = df.JOINT_TOURPURP.map(purpose_map)

        return df

    def map_to_df(self, d):
        return pd.DataFrame({'name': d.keys(), 'code': d.values()})

    def get_weighted_sum(self, df, grp_cols, wt_col):
        # Get the groupby count sum
        df_sum = df.groupby(grp_cols)[wt_col].sum()
        df_sum = df_sum.reset_index().rename(columns={wt_col: 'freq'})

        return df_sum


class VisualizerSummaries:
    # FIXME a lot of these groupby summaries are repetive and can be parameterized
    def activePertypeDistbn(self):
        return self.get_weighted_sum(
            df=self.intermediate_data['person_day'],
            grp_cols='PERSONTYPE',
            wt_col='PER_WEIGHT'
        )

    def autoOwnership(self):
        hhveh_count = self.get_weighted_sum(
            df=self.scenario_data.get('spa_household'),
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

        average_stop_out_dist = pd.DataFrame({'purpose': average_stop_out_dist.keys(),
                                              'avgDist': average_stop_out_dist.values()})

        return average_stop_out_dist

    def dapSummary_vis(self):
        dap_summary = self.get_weighted_sum(
            df=self.intermediate_data.get('person_day'),
            grp_cols=['PERSONTYPE', 'DAP'],
            wt_col='PER_WEIGHT'
        )

        # Get totals
        # margins = dap_summary.groupby('DAP').sum().reset_index()
        # margins['PERSONTYPE'] = 'Total'
        # dap_summary = pd.concat([dap_summary, margins])

        return dap_summary

    def hhSizeDist(self):
        return self.get_weighted_sum(df=self.scenario_data['spa_household'],
                                     grp_cols='HH_SIZE_trunc',
                                     wt_col='HH_WEIGHT').rename(
            columns={'HH_SIZE_trunc': 'HHSIZE'}
        )

    def hhsizeJoint(self):
        return self.get_weighted_sum(df=self.intermediate_data['household_day'],
                                     grp_cols=['HH_SIZE_trunc', 'JOINT'],
                                     wt_col='HH_WEIGHT').rename(
            columns={'HH_SIZE_trunc': 'HHSIZE'}
        )

    def inmSummary_vis(self):
        tours = deepcopy(self.scenario_data['spa_tours'])
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

    def jointSummary(self):
        return self.get_weighted_sum(df=self.intermediate_data['household_day'],
                                     grp_cols='JTF',
                                     wt_col='HHDAY_WEIGHT')

    def jointComp(self):
        joint_purp_list = self.get_joint_purp_list()
        df_jtours = self.scenario_data['spa_jtours']

        # Pre-filter by joint purps
        df_jtours = df_jtours[df_jtours.JOINT_PURP.isin(joint_purp_list)]
        return self.get_weighted_sum(df=df_jtours, grp_cols='COMPOSITION', wt_col='TOUR_WEIGHT')

    def jointCompPartySize(self):
        joint_purp_list = self.get_joint_purp_list()
        df_jtours = self.scenario_data['spa_jtours']

        # Pre-filter by joint purps
        df_jtours = df_jtours[df_jtours.JOINT_PURP.isin(joint_purp_list)]

        return self.get_weighted_sum(df=df_jtours,
                                     grp_cols=['COMPOSITION', 'NUMBER_HH'],
                                     wt_col='TOUR_WEIGHT')

    def jointPartySize(self):
        joint_purp_list = self.get_joint_purp_list()
        df_jtours = self.scenario_data['spa_jtours']

        # Pre-filter by joint purps
        df_jtours = df_jtours[df_jtours.JOINT_PURP.isin(joint_purp_list)]

        return self.get_weighted_sum(df=df_jtours, grp_cols='NUMBER_HH', wt_col='TOUR_WEIGHT')

    def jointToursHHSize(self):
        return self.get_weighted_sum(
            df=self.intermediate_data['household_day'],
            grp_cols=['HH_SIZE_trunc', 'JOINT_CAT'],
            wt_col='HHDAY_WEIGHT').rename(
            columns={'HH_SIZE_trunc': 'HHSIZE'}
        )

    def jtf(self):
        jtf = self.intermediate_data['household_day'].groupby('JTF').HH_WEIGHT.sum()
        jtf.index = jtf.index.astype(int)

        jtf_alts = self.constants.get('JTF_ALTS')
        jtf_alts = pd.Series(jtf_alts.keys(), index=jtf_alts.values(), name='alt_name')
        jtf_summary = pd.concat([jtf_alts, jtf], axis=1).fillna(0).reset_index()
        return jtf_summary.rename(columns={'index': 'jtf_code', 'HH_WEIGHT': 'freq'})

    def mtfSummary_vis(self):
        df_perday = deepcopy(self.intermediate_data['person_day'])

        # Code MTF
        df_perday['MTF'] = 0
        df_perday['MTF'] = df_perday.work_count.clip(0, 2)
        df_perday['MTF'] = df_perday.schl_count.clip(0, 2) + 3
        df_perday.loc[(df_perday.schl_count > 0) & (df_perday.work_count > 0), 'MTF'] = 5

    #     plyr::count(perday[perday$mtf > 0,], c("PERTYPE", "mtf"), "finalweight_1")
        # Get summary counts
        mtf_summary = self.get_weighted_sum(df=df_perday,
                                            grp_cols=['PERSONTYPE', 'MTF'],
                                            wt_col='PER_WEIGHT')
        return mtf_summary

    def pertypeDistbn(self):
        return self.get_weighted_sum(df=self.intermediate_data['person_day'],
                                     grp_cols=['PERSONTYPE'],
                                     wt_col='PER_WEIGHT')

    def stopDC_vis(self):
        df_stops = deepcopy(self.intermediate_data['stops'])

        # Map joint purposes
        df_stops = self.map_joint_purposes(df_stops)

        # Dir dist bin
        bins = [-np.inf] + list(range(0, 40)) + [np.inf]
        df_stops['distbin'] = pd.cut(df_stops.OUT_DIR_DIST, bins=bins, labels=False)

        # Get counts
        stop_dist_freq = self.get_weighted_sum(
            df=df_stops,
            grp_cols=['JOINT_TOURPURP', 'distbin'],
            wt_col='TRIP_WEIGHT'
        ).rename(columns={'JOINT_TOURPURP': 'PURPOSE'})

        return stop_dist_freq

    def stopfreqDir_vis(self):
        df_tours = deepcopy(self.scenario_data['spa_tours'])

        # Map joint purposes
        df_tours = self.map_joint_purposes(df_tours)

        # Get counts
        stop_in_freq = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['JOINT_TOURPURP', 'INBOUND_STOPS'],
            wt_col='TOUR_WEIGHT'
        ).rename(columns={'INBOUND_STOPS': 'NSTOPS'})

        stop_out_freq = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['JOINT_TOURPURP', 'OUTBOUND_STOPS'],
            wt_col='TOUR_WEIGHT'
        ).rename(columns={'OUTBOUND_STOPS': 'NSTOPS'})

        # Merge
        stop_dir_freq = pd.merge(
            stop_in_freq, stop_out_freq,
            on=['JOINT_TOURPURP', 'NSTOPS'],
            suffixes=['_inb', '_out']
        ).rename(columns={'JOINT_TOURPURP': 'PURPOSE'})

        return stop_dir_freq

    def stopTripDep_vis(self):
        df_trips = deepcopy(self.scenario_data['spa_trips'])
        df_stops = deepcopy(self.intermediate_data['stops'])


        # Map joint purposes
        df_trips = self.map_joint_purposes(df_trips)
        df_stops = self.map_joint_purposes(df_stops)

        # Get trip dep counts
        trip_dep_freq = self.get_weighted_sum(
            df=df_trips,
            grp_cols=['JOINT_TOURPURP', 'ORIG_DEP_BIN'],
            wt_col='TRIP_WEIGHT'
        ).rename(columns={'JOINT_TOURPURP': 'purpose', 'ORIG_DEP_BIN': 'timebin'})

        # Get stop dep counts
        stop_dep_freq = self.get_weighted_sum(
            df=df_stops,
            grp_cols=['JOINT_TOURPURP', 'ORIG_DEP_BIN'],
            wt_col='TRIP_WEIGHT'
        ).rename(columns={'JOINT_TOURPURP': 'purpose', 'ORIG_DEP_BIN': 'timebin'})

        # merge
        stop_trip_dep = pd.merge(stop_dep_freq,
                                 trip_dep_freq,
                                 on=['timebin', 'purpose'],
                                 suffixes = ['_stop', '_trip'], how='outer').fillna(0)
        return stop_trip_dep

    def stoppurpose_tourpurpose_vis(self):
        df_stops = deepcopy(self.intermediate_data['stops'])

        # Map joint purposes
        df_stops = self.map_joint_purposes(df_stops)

        # Get counts
        stop_purp_freq = self.get_weighted_sum(
            df=df_stops,
            grp_cols=['JOINT_TOURPURP', 'DEST_PURP'],
            wt_col='TRIP_WEIGHT'
        ).rename(columns={'JOINT_TOURPURP': 'purpose', 'DEST_PURP': 'stop_purpose'})

        return stop_purp_freq

    def todProfile_vis(self):
            df_tours = deepcopy(self.scenario_data['spa_tours'])

            # Map joint purposes
            df_tours = self.map_joint_purposes(df_tours)

            def get_tod_profile(bin_col, freq_name):
                return self.get_weighted_sum(
                    df=df_tours[(df_tours[bin_col] > 0) & (df_tours.FULLY_JOINT == 0)],
                    grp_cols=[bin_col, 'JOINT_TOURPURP'],
                    wt_col='TOUR_WEIGHT'
                ).rename(
                    columns={bin_col: 'TIME_BIN',
                             'JOINT_TOURPURP': 'PURPOSE',
                             'freq': freq_name}
                ).set_index(['TIME_BIN', 'PURPOSE'])

            tod_params = [('ANCHOR_DEPART_BIN', 'FREQ_DEP'),
                          ('ANCHOR_ARRIVE_BIN', 'FREQ_ARR'),
                          ('TOUR_DUR_BIN', 'FREQ_DUR')]

            # Get TOD profiles
            tod_profiles = [get_tod_profile(bin_col, freq_name) for bin_col, freq_name in tod_params]

            # Combine profiles
            tod_profiles = pd.concat(tod_profiles, axis=1).fillna(0)

            return tod_profiles

    def totals(self):
        # only vmt modes
        df_vmt = deepcopy(self.scenario_data['spa_trips'])
        df_vmt = df_vmt[df_vmt.TRIPMODE.isin([1, 2, 3])]

        # num_travel ?
        df_vmt['NUM_TRAVEL'] = df_vmt.TRIPMODE
        df_vmt.loc[df_vmt.TRIPMODE==3, 'NUM_TRAVEL'] = 3.5
        # distance * trip weight / num_travel

        totals = {
            'total_population': self.pertypeDistbn().freq.sum(),
            'total_household': self.scenario_data['spa_household'].HH_WEIGHT.sum(),
            'total_tours': self.scenario_data['spa_tours'].TOUR_WEIGHT.sum(),
            'total_trips': self.scenario_data['spa_trips'].TRIP_WEIGHT.sum(),
            'total_stops': self.intermediate_data['stops'].TRIP_WEIGHT.sum(),
            'total_vmt': sum(df_vmt[['DIST', 'TRIP_WEIGHT']].product(axis=1) / df_vmt.NUM_TRAVEL),
            'total_population_for_rates': self.scenario_data['spa_person'].PER_WEIGHT.sum()
         }

        return pd.DataFrame({'name':totals.keys(), 'value':totals.values()})

    def total_tours_by_pertype_vis(self):
        df_tours = self.scenario_data['spa_tours']
        return self.get_weighted_sum(df=df_tours[df_tours.TOURPURP.isin(range(0, 11))],
                                     grp_cols=['PERSONTYPE'],
                                     wt_col='TOUR_WEIGHT')

    # def wfh_summary(self):
    #     df_person = deepcopy(self.scenario_data['pre_person'])
    #     # #per$worker[is.na(per$worker)] <- 0
    #     # per[EMPLY_LOC_TYPE==3, wfh := 1]
    #     # per$wfh[is.na(per$wfh)] <- 0
    #     # perday$worker[perday$PERTYPE<=2 | (perday$PERTYPE==3 & !is.na(perday$WTAZ))] <- 1
    #     # perday$worker[is.na(perday$worker)] <- 0
    #     # perday$wfh = per$wfh[match(paste(perday$SAMPN, perday$PERNO, sep = "-"),
    #     #                            paste(per$SAMPN, per$PERNO, sep = "-"))]
    #     # #perday$wfh[perday$PER_EMPLY_LOC_TYPE==3] <- 1
    #     # perday$wfh[is.na(perday$wfh)] <- 0
    #     #
    #     # districtWorkers <- ddply(perday[perday$worker==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, workers = sum(finalweight))
    #     # districtWorkers_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWorkers, by = "HDISTRICT", all.x = TRUE)
    #     # districtWorkers_df[is.na(districtWorkers_df)] <- 0
    #     #
    #     # districtWfh     <- ddply(perday[perday$worker==1 & perday$wfh==1,c("HDISTRICT", "finalweight")], c("HDISTRICT"), summarise, wfh = sum(finalweight))
    #     # districtWfh_df <- merge(x = data.frame(HDISTRICT = districtList), y = districtWfh, by = "HDISTRICT", all.x = TRUE)
    #     # districtWfh_df[is.na(districtWfh_df)] <- 0
    #     #
    #     # wfh_summary     <- cbind(districtWorkers_df, districtWfh_df$wfh)
    #     # colnames(wfh_summary) <- c("District", "Workers", "WFH")
    #     # totalwfh        <- data.frame("Total", sum((perday$worker==1)*perday$finalweight), sum((perday$worker==1 & perday$wfh==1)*perday$finalweight))
    #     # colnames(totalwfh) <- colnames(wfh_summary)
    #     # wfh_summary <- rbind(wfh_summary, totalwfh)
    #     # write.csv(wfh_summary, file.path(outdir, "wfh_summary.csv"), row.names = F)

    def workTLFD(self):
        self.scenario_data['pre_person'][['EMPLY_LOC_TYPE', 'HDISTRICT', 'HDIST']]
        df_perday = self.intermediate_data['person_day']

        df_workers = df_perday[(df_perday.WTAZ > 0) & (df_perday.EMPLY <= 2)]

        #         # compute TLFDs by district and total
        # tlfd_work <- ddply(workers[,c("HDISTRICT", "distbin", "finalweight")], c("HDISTRICT", "distbin"), summarise, work = sum((HDISTRICT>0)*finalweight))
        # tlfd_work <- cast(tlfd_work, distbin~HDISTRICT, value = "work", sum)
        # work_ditbins <- tlfd_work$distbin
        # tlfd_work <- transpose(tlfd_work[,!colnames(tlfd_work) %in% c("distbin")], keep.names = 'id')
        # # tlfd_work$id <- row.names(tlfd_work)
        # tlfd_work <- merge(x = districtList_df, y = tlfd_work, by = "id", all.x = TRUE)
        # tlfd_work[is.na(tlfd_work)] <- 0
        # tlfd_work <- transpose(tlfd_work[,!colnames(tlfd_work) %in% c("id")])
        # tlfd_work <- cbind(data.frame(distbin = work_ditbins), tlfd_work)
        # tlfd_work$Total <- rowSums(tlfd_work[,!colnames(tlfd_work) %in% c("distbin")])
        # names(tlfd_work) <- sub("V", "District_", names(tlfd_work))
        # tlfd_work_df <- merge(x = distBinCat, y = tlfd_work, by = "distbin", all.x = TRUE)
        # tlfd_work_df[is.na(tlfd_work_df)] <- 0

