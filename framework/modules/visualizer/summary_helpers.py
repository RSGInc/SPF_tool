import pandas as pd
from core import utils
from copy import deepcopy


class VisualizerInputData:
    def modify_input_data(self, drop_labourers=False):
        """
                These functions are to modify the original data frames.
                Should be done sparingly
        """

        self.setup_zone_xwalk()
        self.setup_households()
        self.setup_person()
        self.setup_tours()

    def setup_households(self):
        df_hh = self.scenario_data['spa_household']
        xwalk = self.intermediate_data['xwalk']

        xwalk = xwalk.rename(columns={'DISTRICT': 'HDISTRICT', 'TAZ': 'HTAZ'})
        df_hh['HTAZ'] = df_hh.HH_ZONE_ID

        # Truncate HH_SIZE to 5 plus
        df_hh['HH_SIZE_trunc'] = df_hh.HH_SIZE.clip(1, 8)
        df_hh['HH_VEH_trunc'] = df_hh.HH_VEH.clip(0, 4)


        # District
        df_hh = df_hh.merge(xwalk, on='HTAZ', how='left')

        # Number of adults
        df_per = deepcopy(self.scenario_data['spa_person'])
        num_adults = df_per[df_per.AGE_CAT >= 4].groupby('HH_ID').size().to_frame('ADULTS')
        num_adults = num_adults.reindex(index=df_hh.HH_ID).fillna(1) # Must have 1 adult?
        df_hh = df_hh.merge(num_adults, on='HH_ID', how='left')

        self.scenario_data['spa_household'] = df_hh

    def setup_person(self):
        df_per = self.scenario_data['spa_person']
        # df_per.loc[df_per.WTAZ <= 0, 'WTAZ'] = -9

        xwalk = self.intermediate_data['xwalk'].rename(columns={'DISTRICT': 'WDISTRICT',
                                                                'TAZ': 'WTAZ'})
        df_hh = self.scenario_data['spa_household'][['HH_ID', 'HH_ZONE_ID', 'HXCORD',
                                                     'HYCORD', 'HTAZ', 'HDISTRICT']]

        # Get location attributes
        df_per = df_per.merge(df_hh, on='HH_ID')
        df_per = df_per.merge(xwalk, on='WTAZ', how='left')

        self.scenario_data['spa_person'] = df_per

    def setup_tours(self):
        df_hh = deepcopy(self.scenario_data['spa_household'])
        df_tours = self.scenario_data['spa_tours']

        # Total stops
        df_tours['TOTAL_STOPS'] = df_tours.INBOUND_STOPS + df_tours.OUTBOUND_STOPS

        # Auto sufficiency
        df_tours = df_tours.merge(df_hh[['HH_ID', 'HH_VEH', 'ADULTS']], on='HH_ID', how='left')
        df_tours.loc[df_tours.HH_VEH == 0, 'AUTOSUFF'] = 0
        df_tours.loc[(df_tours.HH_VEH < df_tours.ADULTS) & (df_tours.HH_VEH > 0), 'AUTOSUFF'] = 1
        df_tours.loc[(df_tours.HH_VEH >= df_tours.ADULTS) & (df_tours.HH_VEH > 0), 'AUTOSUFF'] = 2

        # Return
        self.scenario_data['spa_tours'] = df_tours

class VisualizerIntermediateData:

    def setup_intermediate_data(self):
        """
         Setup any intermediate data / calculations in here.

         This is useful to avoid repeating calculations, but want to keep this separate
         from the main model and input tables to avoid cross-contaminating our data.

        """

        self.setup_faux_skims()
        self.setup_weights()
        self.setup_person_day()
        self.setup_household_day()
        self.setup_joint_household()
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
        work_purp = self.constants.get('PURPOSE')['WORK']
        atwork = [v for k, v in self.constants.get('PURPOSE').items() if k in ['WORK', 'WORK-RELATED']]
        schl = [v for k, v in self.constants.get('PURPOSE').items() if k in ['UNIVERSITY', 'SCHOOL']]

        # First define the filters
        # [excluding at work subtours]. joint work tours should be considered individual mandatory tours
        tour_type_counts['work'] = tours[(tours.TOURPURP == work_purp) & (tours.IS_SUBTOUR == 0)]
        tour_type_counts['atwork'] = tours[tours.TOURPURP.isin(atwork) & (tours.IS_SUBTOUR == 1)]
        tour_type_counts['schl'] = tours[tours.TOURPURP.isin(schl)]
        tour_type_counts['inm'] = tours[
            (tours.TOURPURP.isin( [4] + joint_purp_list ) &
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

    def setup_zone_xwalk(self):
        xwalk = deepcopy(self.scenario_data['xwalk'])
        zones = deepcopy(self.input_tables['shared_data']['zones_gis'])

        # Fetch only the zones we need (no external ones)
        xwalk = xwalk[xwalk.Sector.isin((zones.SEC_NUM//100).unique())]

        # Labels
        xwalk['DISTRICT'] = 'Sector_' + xwalk.Sector.astype(str)

        # FIXME This can be deleted?
        # df_zones = self.scenario_data['zones']
        # df_zones['DISTRICT'] = df_zones.NAME.str.split('_\\(', expand=True)[0]
        # df_zones['DISTRICT'] = 'Sector_' + df_zones.SECTOR.astype(str)
        # df_zones.loc[df_zones.SECTOR == 0, 'DISTRICT'] = None
        # xwalk = df_zones[['OLDZONE', 'DISTRICT']].rename(columns={'OLDZONE': 'TAZ'})
        # xwalk = xwalk[xwalk.TAZ > 0].drop_duplicates()

        self.intermediate_data['xwalk'] = xwalk

    def setup_person_day(self):

        self.setup_tour_type_counts()

        person_day = deepcopy(self.scenario_data.get('spa_person'))
        tour_counts = deepcopy(self.intermediate_data['tour_type_counts'])
        non_worker_types = self.constants['NONWORKER_TYPES']

        # Merge counts onto person_day table
        person_day = person_day.merge(tour_counts.reset_index(), on=['HH_ID', 'PER_ID'])

        # Assign activity pattern
        person_day['num_tours'] = person_day[['itour_count', 'jtour_count']].sum(axis=1)
        person_day['DAP'] = 'H'
        person_day.loc[(person_day.work_count > 0) | (person_day.schl_count > 0), 'DAP'] = 'M'
        person_day.loc[(person_day.num_tours > 0) & (person_day.DAP == 'H'), 'DAP'] = 'N'
        person_day.loc[person_day.PERSONTYPE.isin(non_worker_types) & (person_day.DAP == "M"), 'DAP'] = 'N'

        # Distance to work, school, university
        skims = self.intermediate_data['faux_skims']

        workers = person_day[person_day.EMPLY.isin([1, 2]) & (person_day.WTAZ > 0) & (person_day.EMPLY_LOC_TYPE != 3)]
        students = person_day[(person_day.STAZ > 0) & (person_day.STUDE == 1)]
        univ = person_day[(person_day.STAZ > 0) & (person_day.STUDE == 2)]

        # Get distance to work, school, univ
        person_day.loc[workers.index, 'WDIST'] = skims.loc[zip(workers.HTAZ, workers.WTAZ)].DISTANCE.values
        person_day.loc[students.index, 'SDIST'] = skims.loc[zip(students.HTAZ, students.STAZ)].DISTANCE.values
        person_day.loc[univ.index, 'SDIST'] = skims.loc[zip(univ.HTAZ, univ.STAZ)].DISTANCE.values

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

        # The XY cord for destination is the current place and origi                                            n is the previous place,
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



class VisualizerFunctions:

    def get_joint_purp_list(self):
        stop_purposes = self.constants.get('STOP_PURPOSES')['JOINT']
        return  [x for _, sublist in stop_purposes.items() for x in sublist]

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

    def get_weighted_sum(self,
                         df,
                         grp_cols,
                         wt_col,
                         fillna=True,
                         total_along=None,
                         label_cols=None):

        grp_cols = [grp_cols] if isinstance(grp_cols, str) else grp_cols
        total_along = [total_along] if isinstance(total_along, str) else total_along

        # Get the groupby count sum
        df_sum = df.groupby(grp_cols)[wt_col].sum()
        df_sum = df_sum.reset_index().rename(columns={wt_col: 'freq'})

        if label_cols:
            df_sum = self.add_labels(df_sum, label_cols)

        if fillna and len(grp_cols) > 1:
            # Preserve combinations & fill NA
            indices = [df_sum[x].drop_duplicates().values for x in grp_cols]
            _index = pd.MultiIndex.from_product(indices, names=grp_cols)
            df_sum = df_sum.set_index(grp_cols).reindex(index=_index).fillna(0)
            # Remove index back out
            df_sum = df_sum.reset_index()

        # Add total
        if total_along:
            index_cols = None if total_along == grp_cols else grp_cols
            df_sum = self.get_total_along(df_sum, total_along, index_cols)

        return df_sum

    def get_total_along(self, df, total_along, index_cols=None):
        index_cols = [index_cols] if isinstance(index_cols, str) else index_cols
        total_along = [total_along] if isinstance(total_along, str) else total_along

        if index_cols:
            concat_names = list(set(index_cols).difference(total_along))
            # df_total = pd.concat({'Total': df.groupby(total_along).freq.sum()}, names=concat_names).to_frame('freq')
            df_total = pd.concat({'Total': df.groupby(concat_names).freq.sum()}, names=total_along).to_frame('freq')
            df_sum = pd.concat(
                [
                    df.set_index(index_cols),
                    df_total.reset_index().set_index(index_cols)
                ], axis=0)
            df_sum = df_sum.reset_index()

        else:
            df_sum = df
            df_sum.loc[-1] = ('Total', df_sum.freq.sum())

        return df_sum

    def add_labels(self, df, label_names):

        # Make a dict if string or list passed
        label_names = [label_names] if isinstance(label_names, str) else label_names
        if isinstance(label_names, list):
            label_names = {k: None for k in label_names}

        for label_name, var_name in label_names.items():
            label_map = {v: k for k, v in self.constants.get(label_name).items()}

            assert label_map, f'Missing label for {label_name}'

            df[label_name] = df[label_name].map(label_map)

            if var_name:
                df = df.rename(columns={label_name: var_name})

        return df