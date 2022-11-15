import pandas as pd
from spa_tool.core.functions import distance_on_unit_sphere

def get_XYCORD(coord, trip):
    coord = [coord.upper()] if isinstance(coord, str) else [x.upper() for x in coord]

    assert all([True if x in ['LAT', 'LON'] else False for x in coord]), \
        'invalid coordinate type, must be LAT/LON or [LAT, LON]'

    # Split string to coord pair
    xy = trip.dest_latlon.str.split(',', expand=True).rename(columns={0: 'LAT', 1: 'LON'})

    # Strip invalid characters from ends of string and fix weird case where latitude had 25.25.194185
    # xy[xy.apply(lambda x: x.str.count('\.')>1).any(axis=1)]
    xy = xy.apply(lambda x: x.str.replace('25\.25\.', '25.').str.strip('. \t \xad').astype(float))

    return xy[coord].squeeze()


def get_WXYCORD(coord, trip):
    # Get fixed work locations
    worktrips = trip[trip.trip_purp.isin([1])]
    fixed_work_trips_idx = worktrips[worktrips.groupby('person_id').dest_latlon.transform('count') == 1].index
    fixed_coords = get_XYCORD(coord, trip).loc[fixed_work_trips_idx]

    # return pd.concat([trip, fixed_coords], axis=1)[coord_id].fillna(0)
    # Pull in person_id and assign work location for all 'trip' rows for each person
    person_trips = trip[['person_id']].merge(fixed_coords, on='trip_id')
    person_trips_xy = trip.reset_index().merge(person_trips, on='person_id', how='left').set_index('trip_id')[coord]
    return person_trips_xy.squeeze()


def get_TOTTR_NEXT(trip, trip_hhcompanions_pnum):
    n_companions = pd.concat([trip[['trip_companions', 'person_id']],
                              trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1)
    n_companions = n_companions.reset_index().set_index(['trip_id', 'person_id']).sum(axis=1)
    n_companions = n_companions.groupby('person_id').shift(1).fillna(0).astype(int).droplevel(1)
    return n_companions

def get_buffer_dist(trip):
    work_latlon = get_WXYCORD(['LAT', 'LON'], trip).rename(columns={'LAT': 'WLAT', 'LON': 'WLON'})
    trip_latlon = get_XYCORD(['LON', 'LAT'], trip)
    latlon = pd.concat([trip_latlon, work_latlon], axis=1)

    dist = distance_on_unit_sphere(lat1=latlon.LAT, long1=latlon.LON, lat2=latlon.WLAT, long2=latlon.WLON)

    return dist

def get_mode_bool(mode, trip, trip_hhcompanions_pnum):
    if mode == 'SOV':
        (trip.mode_mainline.isin([2,11,12,13]) & (pd.concat([trip.trip_companions, trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1).fillna(0).sum(axis=1).astype(int)==0))
    if mode == 'HOV2':
        (trip.mode_mainline.isin([2,3,11,12,13]) & (pd.concat([trip.trip_companions, trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1).fillna(0).sum(axis=1).astype(int)==1)) | \
        (trip.mode_mainline.isin([3]) & (pd.concat([trip.trip_companions, trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1).fillna(0).sum(axis=1).astype(int)==0))
    if mode == 'HOV3+':
       (trip.mode_mainline.isin([2,3,11,12,13]) & (pd.concat([trip.trip_companions, trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1).fillna(0).sum(axis=1).astype(int)>1))
    if mode == 'WALK':
        trip.mode_mainline.isin([1])
    if mode == 'BIKE':
        trip.mode_mainline.isin([14])
    if mode == 'TNC / TAXI':
        trip.mode_mainline.isin([8,9])
    if mode == 'TRANSIT-BUS':
        trip.mode_mainline.isin([5])
    if mode == 'WALK-BUS':
        trip.mode_mainline.isin([5])

        mode_cols = ['mode_' + str(x) for x in range(1, 6)]

        [x for x in mode_cols]

        trip[mode_cols].apply(lambda x: )


        trip.mode_2
        trip.mode_3
        trip.mode_4
        trip.mode_5
        trip.mode_6

    if mode == 'WALK-METRO':
        pass
    if mode == 'KNR-BUS':
        pass
    if mode == 'PNR-BUS':
        pass
    if mode == 'PNR-METRO':
        pass
    if mode == 'KNR-METRO':
        pass
    if mode == 'TRANSIT-METRO':
        trip.mode_mainline.isin([4])
    if mode == 'COMPANY-BUS':
        trip.mode_mainline.isin([6])
    if mode == 'SCHOOLBUS':
        trip.mode_mainline.isin([7])
    if mode == 'OTHER':
        trip.mode_mainline.isin([10,15])