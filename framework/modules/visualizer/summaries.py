"""
Visualizer functions mixin class

The purpose of this is to define our summary functions cleanly in a separate file,
but still be able to use them in the Visualizer class.

"""

import os
import pandas as pd
import numpy as np
from copy import deepcopy

class VisualizerSummaries:
    # FIXME a lot of these groupby summaries are repetive and can be parameterized
    def activePertypeDistbn(self):
        active_pertype = self.get_weighted_sum(
            df=self.intermediate_data['person_day'],
            grp_cols='PERSONTYPE',
            wt_col='PER_WEIGHT',
            label_cols='PERSONTYPE'
        )
        return active_pertype


    def autoOwnership(self):
        hhveh_count = self.get_weighted_sum(
            df=self.scenario_data.get('spa_household'),
            grp_cols='HH_VEH_trunc',
            wt_col='HH_WEIGHT'
        )
        hhveh_count.HH_VEH_trunc = hhveh_count.HH_VEH_trunc.astype(int)
        return hhveh_count.rename(columns={'HH_VEH_trunc': 'HH_VEH'})

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
            wt_col='PER_WEIGHT',
            label_cols='PERSONTYPE',
            total_along='PERSONTYPE'
        )

        return dap_summary

    def hhSizeDist(self):
        return self.get_weighted_sum(df=self.scenario_data['spa_household'],
                                     grp_cols='HH_SIZE_trunc',
                                     wt_col='HH_WEIGHT').rename(
            columns={'HH_SIZE_trunc': 'HHSIZE'}
        )

    def hhsizeJoint(self):
        return self.get_weighted_sum(
            df=self.intermediate_data['household_day'],
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
        inm_summary = inm_summary.rename(columns={'PER_WEIGHT': 'freq'})

        # add labels
        inm_summary = self.add_labels(inm_summary, ['PERSONTYPE'])
        _index = pd.MultiIndex.from_product([inm_summary.PERSONTYPE.unique(), inm_summary.nmtours.unique()],
                                            names=['PERSONTYPE', 'nmtours'])
        inm_summary = inm_summary.set_index(['PERSONTYPE', 'nmtours']).reindex(index=_index).fillna(0).reset_index()

        # Get totals
        inm_summary = self.get_total_along(inm_summary, total_along='PERSONTYPE', index_cols=['PERSONTYPE', 'nmtours'])

        return inm_summary.reset_index()

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

        joint_hhsize = self.intermediate_data['household_day']
        joint_hhsize.HH_SIZE_trunc = joint_hhsize.HH_SIZE_trunc.astype(int)

        joint_hhsize = self.get_weighted_sum(
            df=joint_hhsize,
            grp_cols=['HH_SIZE_trunc', 'JOINT_CAT'],
            wt_col='HHDAY_WEIGHT',
            label_cols='JOINT_CAT',
            total_along='HH_SIZE_trunc')

        return joint_hhsize.rename(columns={'HH_SIZE_trunc': 'HHSIZE'})

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
                                            wt_col='PER_WEIGHT',
                                            label_cols=['PERSONTYPE', 'MTF'],
                                            total_along='PERSONTYPE')

        return mtf_summary.reset_index()

    def pertypeDistbn(self):
        pertype_dist = self.get_weighted_sum(df=self.intermediate_data['person_day'],
                                             grp_cols=['PERSONTYPE'],
                                             wt_col='PER_WEIGHT',
                                             label_cols='PERSONTYPE',
                                             total_along='PERSONTYPE')
        # pertype_dist.loc[-1] = ('Total', pertype_dist.freq.sum())
        return pertype_dist


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
            wt_col='TRIP_WEIGHT',
            total_along='JOINT_TOURPURP'
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
            wt_col='TOUR_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'INBOUND_STOPS': 'NSTOPS'})

        stop_out_freq = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['JOINT_TOURPURP', 'OUTBOUND_STOPS'],
            wt_col='TOUR_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'OUTBOUND_STOPS': 'NSTOPS'})

        # Merge
        stop_dir_freq = pd.merge(
            stop_in_freq, stop_out_freq,
            on=['JOINT_TOURPURP', 'NSTOPS'],
            suffixes=['_inb', '_out']
        ).rename(columns={'JOINT_TOURPURP': 'PURPOSE'})


        values = np.sort(stop_dir_freq.NSTOPS.unique())
        value_map = {k: f'{k}p' if k==values.max() else str(k) for k in values}
        stop_dir_freq.NSTOPS = stop_dir_freq.NSTOPS.map(value_map)

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
            wt_col='TRIP_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'JOINT_TOURPURP': 'purpose', 'ORIG_DEP_BIN': 'timebin'})

        # Get stop dep counts
        stop_dep_freq = self.get_weighted_sum(
            df=df_stops,
            grp_cols=['JOINT_TOURPURP', 'ORIG_DEP_BIN'],
            wt_col='TRIP_WEIGHT',
            total_along='JOINT_TOURPURP'
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
            wt_col='TRIP_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'JOINT_TOURPURP': 'TOUR_PURPOSE', 'DEST_PURP': 'PURPOSE'})

        # Add labels
        stop_purp_freq = self.add_labels(stop_purp_freq, 'PURPOSE')

        return stop_purp_freq

    def stopfreq_total_vis(self):
        df_tours = deepcopy(self.scenario_data['spa_tours'])

        # Map joint purposes
        df_tours = self.map_joint_purposes(df_tours)

        # Get counts
        stop_freq_total = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['JOINT_TOURPURP', 'TOTAL_STOPS'],
            wt_col='TOUR_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'INBOUND_STOPS': 'NSTOPS',
                          'JOINT_TOURPURP': 'PURPOSE'})

        return stop_freq_total

    def todProfile_vis(self):
            df_tours = deepcopy(self.scenario_data['spa_tours'])

            # Map joint purposes
            df_tours = self.map_joint_purposes(df_tours)

            def get_tod_profile(bin_col, freq_name):
                df_sum = self.get_weighted_sum(
                    df=df_tours[(df_tours[bin_col] > 0) & (df_tours.FULLY_JOINT == 0)],
                    grp_cols=[bin_col, 'JOINT_TOURPURP'],
                    wt_col='TOUR_WEIGHT',
                    total_along='JOINT_TOURPURP')

                df_sum = df_sum.rename(
                    columns={bin_col: 'TIME_BIN',
                             'JOINT_TOURPURP': 'PURPOSE',
                             'freq': freq_name}
                )
                df_sum = df_sum.set_index(['TIME_BIN', 'PURPOSE'])
                return df_sum

            tod_params = [('ANCHOR_DEPART_BIN', 'FREQ_DEP'),
                          ('ANCHOR_ARRIVE_BIN', 'FREQ_ARR'),
                          ('TOUR_DUR_BIN', 'FREQ_DUR')]

            # Get TOD profiles
            tod_profiles = [get_tod_profile(bin_col, freq_name) for bin_col, freq_name in tod_params]

            # Combine profiles
            tod_profiles = pd.concat(tod_profiles, axis=1).fillna(0)

            return tod_profiles.reset_index()

    def totals(self):
        # only vmt modes
        df_vmt = deepcopy(self.scenario_data['spa_trips'])
        df_vmt = df_vmt[df_vmt.TRIPMODE.isin([1, 2, 3])]

        # num_travel ?
        df_vmt['NUM_TRAVEL'] = df_vmt.TRIPMODE
        df_vmt.loc[df_vmt.TRIPMODE == 3, 'NUM_TRAVEL'] = 3.5
        # distance * trip weight / num_travel

        totals = {
            'total_population': self.pertypeDistbn().freq.sum(),
            'total_households': self.scenario_data['spa_household'].HH_WEIGHT.sum(),
            'total_tours': self.scenario_data['spa_tours'].TOUR_WEIGHT.sum(),
            'total_trips': self.scenario_data['spa_trips'].TRIP_WEIGHT.sum(),
            'total_stops': self.intermediate_data['stops'].TRIP_WEIGHT.sum(),
            'total_vmt': sum(df_vmt[['DIST', 'TRIP_WEIGHT']].product(axis=1) / df_vmt.NUM_TRAVEL),
            'total_population_for_rates': self.scenario_data['spa_person'].PER_WEIGHT.sum()
         }

        return pd.DataFrame({'name': totals.keys(), 'value': totals.values()})

    def total_tours_by_pertype_vis(self):
        df_tours = self.scenario_data['spa_tours']
        pertype_tours = self.get_weighted_sum(
            df=df_tours[df_tours.TOURPURP.isin(range(0, 11))],
            grp_cols=['PERSONTYPE'],
            wt_col='TOUR_WEIGHT',
            label_cols='PERSONTYPE')

        return pertype_tours

    def workTLFD(self):
        df_perday = self.intermediate_data['person_day']
        workers = deepcopy(df_perday[(df_perday.WTAZ > 0) & ~df_perday.HDISTRICT.isnull()])

        dist_bin = self.constants['parameters'].get('distBin')
        bin_breaks = [int(x.split('-')[0]) for x in dist_bin]

        # Distance bins
        workers['distbin'] = pd.cut(workers.WDIST, bins=bin_breaks + [np.inf], labels=False)

        # Get weighted sum and cast to wide
        df_tlfd = self.get_weighted_sum(df=workers, grp_cols=['HDISTRICT', 'distbin'], wt_col='PER_WEIGHT')
        df_tlfd = df_tlfd.pivot(index='distbin', columns='HDISTRICT')
        df_tlfd = df_tlfd.droplevel(level=0, axis=1).reset_index().set_index('distbin')
        df_tlfd.columns.name = None

        # Reindex to ensure all districts & bin breaks included
        df_zones = self.scenario_data['zones']
        district_list = df_zones.DISTRICT.dropna().unique()
        df_tlfd = df_tlfd.reindex(index=bin_breaks)
        df_tlfd = df_tlfd.T.reindex(index=district_list).T.fillna(0).reset_index()

        df_tlfd['Total'] = df_tlfd.sum(axis=1)

        return df_tlfd

    def schlTLFD(self):
        df_perday = self.intermediate_data['person_day']
        students = deepcopy(df_perday[(df_perday.STAZ > 0) & (df_perday.STUDE == 1) & ~df_perday.HDISTRICT.isnull()])

        dist_bin = self.constants['parameters'].get('distBin')
        bin_breaks = [int(x.split('-')[0]) for x in dist_bin]

        # Distance bins
        students['distbin'] = pd.cut(students.SDIST, bins=bin_breaks + [np.inf], labels=False)

        # Get weighted sum and cast to wide
        df_tlfd = self.get_weighted_sum(df=students, grp_cols=['HDISTRICT', 'distbin'], wt_col='PER_WEIGHT')
        df_tlfd = df_tlfd.pivot(index='distbin', columns='HDISTRICT')
        df_tlfd = df_tlfd.fillna(0).droplevel(level=0, axis=1).reset_index().set_index('distbin')
        df_tlfd.columns.name = None

        # Reindex to ensure all districts included
        df_zones = self.scenario_data['zones']
        district_list = df_zones.DISTRICT.dropna().unique()
        df_tlfd = df_tlfd.reindex(index=bin_breaks)
        df_tlfd = df_tlfd.T.reindex(index=district_list).T.fillna(0).reset_index()

        df_tlfd['Total'] = df_tlfd.sum(axis=1)

        return df_tlfd

    def univTLFD(self):
        df_perday = self.intermediate_data['person_day']
        univ = deepcopy(df_perday[(df_perday.STAZ > 0) & (df_perday.STUDE == 2) & ~df_perday.HDISTRICT.isnull()])

        dist_bin = self.constants['parameters'].get('distBin')
        bin_breaks = [int(x.split('-')[0]) for x in dist_bin]

        # Distance bins
        univ['distbin'] = pd.cut(univ.SDIST, bins=bin_breaks + [np.inf], labels=False)

        # Get weighted sum and cast to wide
        df_tlfd = self.get_weighted_sum(df=univ, grp_cols=['HDISTRICT', 'distbin'], wt_col='PER_WEIGHT')
        df_tlfd = df_tlfd.pivot(index='distbin', columns='HDISTRICT')
        df_tlfd = df_tlfd.fillna(0).droplevel(level=0, axis=1).reset_index().set_index('distbin')
        df_tlfd.columns.name = None

        # Reindex to ensure all districts included
        df_zones = self.scenario_data['zones']
        district_list = df_zones.DISTRICT.dropna().unique()
        df_tlfd = df_tlfd.reindex(index=bin_breaks)
        df_tlfd = df_tlfd.T.reindex(index=district_list).T.fillna(0).reset_index()

        df_tlfd['Total'] = df_tlfd.sum(axis=1)

        return df_tlfd

    def countyFlows(self):
        df_perday = self.intermediate_data['person_day']
        workers = deepcopy(df_perday[(df_perday.WTAZ > 0)])

        flows = pd.crosstab(index=workers.HDISTRICT, columns=workers.WDISTRICT,
                            values=workers.PER_WEIGHT, aggfunc=sum,
                            dropna=False, margins=True, margins_name='Total').fillna(0)

        return flows.reset_index()

    def tmodeProfile_vis(self):
        df_tours = deepcopy(self.scenario_data['spa_tours'])
        df_tours = self.map_joint_purposes(df_tours)

        # Get number of tours by auto sufficiency, mode, and jointpurp/purp
        tmode_profile = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['AUTOSUFF', 'TOURMODE', 'JOINT_TOURPURP'],
            wt_col='TOUR_WEIGHT',
            label_cols='TOURMODE',
            total_along='JOINT_TOURPURP'
        )
        # Recode as labels
        keymap = {0: 'freq_as0', 1: 'freq_as1', 2: 'freq_as2'}
        tmode_profile.AUTOSUFF = tmode_profile.AUTOSUFF.map(keymap)

        # Pivot to wide, get sum column
        tmode_profile = tmode_profile.pivot(index=['TOURMODE', 'JOINT_TOURPURP'],
                                            columns='AUTOSUFF',
                                            values='freq').fillna(0).reset_index()
        tmode_profile = tmode_profile.rename_axis(None, axis=1).rename(columns={'JOINT_TOURPURP': 'PURPOSE'})
        tmode_profile['freq_all'] = tmode_profile[keymap.values()].sum(axis=1)

        return tmode_profile


    # FIXME Just copying from R output for now
    def esctype_by_childtype(self):
        path = os.path.join(self.namespace.data,
                            'visualizer/summaries',
                            'esctype_by_childtype.csv')
        return pd.read_csv(path)

    def esctype_by_chauffeurtype(self):
        path = os.path.join(self.namespace.data,
                            'visualizer/summaries',
                            'esctype_by_chauffeurtype.csv')
        return pd.read_csv(path)

    def worker_school_escorting(self):
        path = os.path.join(self.namespace.data,
                            'visualizer/summaries',
                            'worker_school_escorting.csv')
        return pd.read_csv(path)

    def wfh_summary(self):
        pass

    def tourDistProfile_vis(self):
        # Labels / Breaks
        tour_purps = self.get_joint_purp_list()
        dist_bin = self.constants['parameters'].get('distBin')
        bin_breaks = [int(x.split('-')[0]) for x in dist_bin]

        # The tours
        df_tours = deepcopy(self.scenario_data['spa_tours'])

        # Map joint purposes
        df_tours = self.map_joint_purposes(df_tours)

        # Distance bins
        df_tours['distbin'] = pd.cut(df_tours.DIST, bins=bin_breaks + [np.inf], labels=False)

        # Get counts
        tour_dist = self.get_weighted_sum(
            df=df_tours,
            grp_cols=['JOINT_TOURPURP', 'distbin'],
            wt_col='TOUR_WEIGHT',
            total_along='JOINT_TOURPURP'
        ).rename(columns={'JOINT_TOURPURP': 'PURPOSE'})

        return tour_dist

    def mandTripLengths(self):
        df_perday = self.intermediate_data['person_day']

        workers = deepcopy(df_perday[(df_perday.WTAZ > 0)])
        students = deepcopy(df_perday[(df_perday.STAZ > 0) & (df_perday.STUDE == 1)])
        univ = deepcopy(df_perday[(df_perday.STAZ > 0) & (df_perday.STUDE == 2)])

        # Weighted mean
        mand_trip_lengths = {
            'Work': workers.groupby('HDISTRICT').apply(lambda x: (x.WDIST * x.PER_WEIGHT).mean()),
            'Schl': students.groupby('HDISTRICT').apply(lambda x: (x.SDIST * x.PER_WEIGHT).mean()),
            'Univ': univ.groupby('HDISTRICT').apply(lambda x: (x.SDIST * x.PER_WEIGHT).mean())
        }
        mand_trip_lengths = pd.concat(mand_trip_lengths, axis=1).fillna(0)

        mand_trip_lengths.loc['Total'] = mand_trip_lengths.sum()

        return mand_trip_lengths.reset_index()

    def nonMandTripLengths(self):
        # Labels / Breaks
        tour_purps = self.get_joint_purp_list()

        # The tours
        df_tours = deepcopy(self.scenario_data['spa_tours'])
        df_tours = df_tours[df_tours.TOURPURP.isin(tour_purps) & (df_tours.IS_SUBTOUR == 0)]

        # Map joint purposes
        df_tours = self.map_joint_purposes(df_tours)

        # Get sum
        tour_dist = df_tours.groupby('JOINT_TOURPURP').apply(lambda x: (x.DIST * x.TOUR_WEIGHT).mean())
        tour_dist = tour_dist.to_frame('AvgTripLength')
        tour_dist.loc['Total'] = tour_dist.sum()
        tour_dist = tour_dist.reset_index().rename(columns = {'JOINT_TOURPURP': 'PURPOSE'})

        return tour_dist

    def tripModeProfile_vis(self):
        df_trips = deepcopy(self.scenario_data['spa_trips'])

        # Map joint tour purpose
        tripmode_profile = self.map_joint_purposes(df_trips)

        # Get number of trip modes by tour modes
        tripmode_profile = self.get_weighted_sum(
            df=tripmode_profile,
            grp_cols=['TRIPMODE', 'TOURMODE', 'JOINT_TOURPURP'],
            wt_col='TRIP_WEIGHT',
            label_cols=['TRIPMODE', 'TOURMODE'],
            total_along=['JOINT_TOURPURP'],
        )

        # Also sum along tour mode
        tripmode_profile = self.get_total_along(tripmode_profile, total_along='TOURMODE',
                                                index_cols=['TRIPMODE', 'TOURMODE', 'JOINT_TOURPURP'])

        # Add grp_var
        tripmode_profile['grp_var'] = tripmode_profile.groupby(['TOURMODE', 'JOINT_TOURPURP']).grouper.group_info[0]

        return tripmode_profile.rename(columns={'JOINT_TOURPURP': 'PURPOSE'})

