import pandas as pd
from copy import deepcopy
from core.utils import distance_on_unit_sphere

class LongExpressions:
    def get_XYCORD(self, coord_type, coord):
        coord_type = (
            [coord_type.upper()]
            if isinstance(coord_type, str)
            else [x.upper() for x in coord_type]
        )

        assert all(
            [True if x in ["LAT", "LON"] else False for x in coord_type]
        ), "invalid coordinate type, must be LAT/LON or [LAT, LON]"

        # Split string to coord pair
        xy = coord.str.split(",", expand=True).rename(columns={0: "LAT", 1: "LON"})
        xy = xy[["LAT", "LON"]]

        # Truncate string to 6 decimal places
        def trunc_cord(cord):
            # Fix weird "25.25." typo and remote certain unwanted characters
            cord = cord.str.replace("25\.25\.", "25.", regex=False).str.strip(". \t \xad")

            # Remove non numeric characters, except decimal
            cord = cord.str.replace(r"[^\d.]", "", regex=True)

            cord_split = cord.str.split(".", expand=True)
            return (cord_split[0] + "." + cord_split[1].str.slice(0, 6)).astype(float)

        xy = xy.apply(lambda x: trunc_cord(x))

        return xy[coord_type].squeeze()

    def get_WXYCORD(self, coord_type):
        trip = self.input_tables['trip']

        # Get fixed work locations
        worktrips = trip[trip.trip_purp.isin([1])]

        # Set fixed locations when purpose always goes to same place
        fixed_work_trips_idx = worktrips[
            worktrips.groupby("person_id").dest_latlon.transform("count") == 1
        ].index
        fixed_coords = self.get_XYCORD(coord_type, trip.dest_latlon).loc[fixed_work_trips_idx]

        # return pd.concat([trip, fixed_coords], axis=1)[coord_id].fillna(0)
        # Pull in person_id and assign work location for all 'trip' rows for each person
        person_trips = trip[["person_id"]].merge(fixed_coords, on="trip_id")
        person_trips_xy = (
            trip.reset_index()
            .merge(person_trips, on="person_id", how="left")
            .set_index("trip_id")[coord_type]
        )
        return person_trips_xy.squeeze()

    def get_fixed_XYCORD(self, coord_type, purposes):
        trip = self.input_tables['trip']

        purposes = purposes if isinstance(purposes, list) else [purposes]
        # Get fixed work locations

        fixedtrips = trip[trip.trip_purp.isin(purposes)]
        fixed_trips_idx = fixedtrips[
            fixedtrips.groupby("person_id").dest_latlon.transform("count") == 1
        ].index
        fixed_coords = self.get_XYCORD(coord_type, trip.dest_latlon).loc[fixed_trips_idx]

        # return pd.concat([trip, fixed_coords], axis=1)[coord_id].fillna(0)
        # Pull in person_id and assign work location for all 'trip' rows for each person
        person_trips = trip[["person_id"]].merge(fixed_coords, on="trip_id")
        person_trips_xy = (
            trip.reset_index()
            .merge(person_trips, on="person_id", how="left")
            .set_index("trip_id")[coord_type]
        )
        return person_trips_xy.squeeze()

    def get_TOTTR(self):
        trip = self.input_tables['trip']
        trip_hhcompanions_pnum = self.input_tables['trip_hhcompanions_pnum']

        tottr = pd.concat([
            trip.trip_companions,
            trip_hhcompanions_pnum.groupby('trip_id').size()
        ], axis=1).fillna(0).sum(axis=1).astype(int)

        return tottr

    def get_TOTTR_NEXT(self):
        trip = self.input_tables['trip']
        trip_hhcompanions_pnum = self.input_tables['trip_hhcompanions_pnum']

        n_companions = pd.concat(
            [
                trip[["trip_companions", "person_id"]],
                trip_hhcompanions_pnum.groupby("trip_id").size(),
            ],
            axis=1,
        )
        n_companions = (
            n_companions.reset_index().set_index(["trip_id", "person_id"]).sum(axis=1)
        )
        n_companions = (
            n_companions.groupby("person_id").shift(1).fillna(0).astype(int).droplevel(1)
        )

        return n_companions

    def get_buffer_dist(self):
        trip = self.input_tables['trip']

        work_latlon = self.get_WXYCORD(["LAT", "LON"], trip).rename(
            columns={"LAT": "WLAT", "LON": "WLON"}
        )
        trip_latlon = self.get_XYCORD(["LON", "LAT"], trip.dest_latlon)
        latlon = pd.concat([trip_latlon, work_latlon], axis=1)

        dist = distance_on_unit_sphere(
            lat1=latlon.LAT, long1=latlon.LON, lat2=latlon.WLAT, long2=latlon.WLON
        )

        return dist

    def get_hov(self, hov_type):
        trip = self.input_tables['trip']
        trip_hhcompanions_pnum = self.input_tables['trip_hhcompanions_pnum']

        # Person is driver in auto
        is_driver = trip.mode_mainline.isin([2, 11, 12, 13])

        # Person is a passenger in auto
        is_pax = trip.mode_mainline.isin([3])

        # Trip has no other passengers
        no_pax = (
            pd.concat(
                [trip.trip_companions, trip_hhcompanions_pnum.groupby("trip_id").size()],
                axis=1,
            )
            .fillna(0)
            .sum(axis=1)
            .astype(int)
            == 0
        )

        # Trip has one passenger
        one_pax = (
            pd.concat(
                [trip.trip_companions, trip_hhcompanions_pnum.groupby("trip_id").size()],
                axis=1,
            )
            .fillna(0)
            .sum(axis=1)
            .astype(int)
            == 1
        )

        # Trip has 2 or more passengers
        twomore_pax = (
            pd.concat(
                [trip.trip_companions, trip_hhcompanions_pnum.groupby("trip_id").size()],
                axis=1,
            )
            .fillna(0)
            .sum(axis=1)
            .astype(int)
            > 1
        )

        if hov_type == "SOV":
            return is_driver & no_pax
        if hov_type == "HOV2":
            return (is_driver & one_pax) | (is_pax & ~twomore_pax)
        if hov_type == "HOV3+":
            return (is_driver & twomore_pax) | (is_pax & twomore_pax)

    def get_transit_type(self, mode):

        trip = self.input_tables['trip']
        access_mode, transit_mode = mode.split("-")

        # Other transit modes preceding mainline mode are 'walk'
        # FIXME Should put this as a setting in the config file
        amode = {
            "WALK": [1, 4, 5, 6, 7],
            "PNR": [2, 10, 11, 12, 13, 14, 15],
            "KNR": [3, 8, 9],
        }[access_mode]
        tmode = {"METRO": 4, "BUS": 5}[transit_mode]

        # Filter the transit trips only
        transit_trips = trip[trip.mode_mainline.isin([tmode])]

        # mode columns
        mode_cols = ["mode_" + str(x) for x in range(1, 6)]

        # Re-shape to long to allow convenient groupby function
        df_mode_seq = pd.melt(
            transit_trips,
            id_vars="mode_mainline",
            value_vars=mode_cols,
            var_name="sequence",
            value_name="part_mode",
            ignore_index=False,
        )

        # sequence string to integer
        df_mode_seq.sequence = df_mode_seq.sequence.str.strip("mode_").astype(int)

        # Which mode part is mainline
        df_mode_seq["is_mainline"] = False
        df_mode_seq.loc[
            df_mode_seq.mode_mainline == df_mode_seq.part_mode, "is_mainline"
        ] = True

        # Check if mainline mode is only mode, otherwise check if walk, drive, or dropoff
        res = pd.Series(False, index=trip.index)
        # tripid, df = list(df_mode_seq.groupby(level=0))[17]
        for tripid, df in df_mode_seq.groupby(level=0):

            # If access mode is walk and if transit is only mode, then walk-transit
            if access_mode == "WALK" and df.loc[df.sequence == 1, "is_mainline"].all():
                res.loc[tripid] = True

            # If access mode is before mainline transit mode
            elif not df[~df.is_mainline & df.part_mode.isin(amode)].empty:
                res.loc[tripid] = (
                    df[df.sequence.isin(df[df.is_mainline].sequence - 1)]
                    .part_mode.isin(amode)
                    .any()
                )

        return res

    def get_escort_type(self, event):
        trip = deepcopy(self.input_tables['trip'])

        # Total travelers on trip & next trip
        trip['total_trip_companions'] = self.get_TOTTR()
        trip['total_trip_companions_next'] = self.get_TOTTR_NEXT()

        # Filter by only joint household trips
        is_hhtrip = ~trip.trip_hhcompanions_pnum.isnull()

        # Get the joint hh trips where someone gets dropped off
        is_dropoff = (trip.total_trip_companions - trip.total_trip_companions_next) > 0
        is_pickup = (trip.total_trip_companions - trip.total_trip_companions_next) < 0

        # The person is the driver
        is_driver = trip.mode_mainline.isin([2, 11, 12, 13])

        # Purpose is school
        is_school = trip.trip_purp.isin([3])

        #
        is_pu = is_hhtrip & is_driver & is_school & is_pickup & ~is_dropoff
        is_do = is_hhtrip & is_driver & is_school & ~is_pickup & is_dropoff
        is_pu_nonhh = ~is_hhtrip & is_driver & is_school & is_pickup & ~is_dropoff
        is_do_nonhh = ~is_hhtrip & is_driver & is_school & ~is_pickup & is_dropoff
        # is_neither = ~(is_hhtrip & is_driver & is_school & ~is_pickup & ~is_dropoff)
        is_neither = ~(is_pu | is_do | is_pu_nonhh | is_do_nonhh)

        if event == 'NEITHER':
            return is_neither
        if event == 'PICK_UP':
            return is_pu
        if event == 'DROP_OFF':
            return is_do
        if event == 'PICK_UP_NON_HH':
            return is_pu_nonhh
        if event == 'DROP_OFF_NON_HH':
            return is_do_nonhh

        # FIXME This isn't right, since it's at the trip level and should be a tour level
        if event == 'BOTH_PUDO':
            return is_hhtrip & is_driver & is_school & is_pickup & is_dropoff

    def get_escort_trips(self):
        trip = deepcopy(self.input_tables['trip'])

        # Total travelers on trip & next trip
        trip['total_trip_companions'] = self.get_TOTTR()
        trip['total_trip_companions_next'] = self.get_TOTTR_NEXT()

        # Filter by only joint household trips
        is_hhtrip = ~trip.trip_hhcompanions_pnum.isnull()

        # Get the joint hh trips where someone gets dropped off
        is_dropoff = (trip.total_trip_companions - trip.total_trip_companions_next) > 0
        is_pickup = (trip.total_trip_companions - trip.total_trip_companions_next) < 0

        # The person is the driver
        is_driver = trip.mode_mainline.isin([2, 11, 12, 13])

        # Purpose is school
        is_school = trip.trip_purp.isin([3])

        # escort_trips = trip[is_hhtrip & is_driver & (is_pickup | is_dropoff)]
        escort_trips = is_hhtrip & is_driver & is_school & (is_pickup | is_dropoff)

        return escort_trips

    def get_loop_trips(self):
        trip = deepcopy(self.input_tables['trip'])
        return (trip.trip_num == 1) & (trip.trip_purp == 8)

    def convert_taz(self, old_taz):
        zones = deepcopy(self.input_tables['zone']).reset_index()[['OLDZONE', 'zone_id']]
        zones = zones[~zones.OLDZONE.isnull()]
        zones.OLDZONE = zones.OLDZONE.astype(int)
        taz_xwalk = zones[['OLDZONE', 'zone_id']]

        # Add missing values
        missing_taz = list(set(old_taz.unique()).difference(taz_xwalk.OLDZONE))
        taz_xwalk = pd.concat([taz_xwalk, pd.DataFrame({'OLDZONE': missing_taz, 'zone_id': missing_taz})])
        taz_xwalk = taz_xwalk.set_index('OLDZONE')

        # Fetch the new taz ID by index value
        new_taz = taz_xwalk.loc[old_taz]

        # set the old index on the new
        new_taz.index = old_taz.index
        new_taz.index.name = old_taz.index.name

        return new_taz.zone_id.astype(int)

    def get_fixed_TAZ(self, purpose):
        trip = deepcopy(self.input_tables['trip'])
        person = deepcopy(self.input_tables['person'])
        purpose = [purpose] if not isinstance(purpose, list) else purpose

        taz = pd.merge(
            person.reset_index(),
            trip[trip.trip_purp.isin(purpose)].groupby('person_id')['dest_zone_id'].first().reset_index(),
            how='left'
        ).set_index('person_id').dest_zone_id.fillna('-9').astype(int)

        taz = self.convert_taz(taz)

        return taz
