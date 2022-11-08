import pandas as pd

def get_WXYCORD(coord, trip):

    assert coord in ['Y', 'X'], f'Invalid coord type {coord}, must be "X" or "Y"'
    coord_id = 0 if coord == 'Y' else 1

    # Get fixed work locations
    worktrips = trip[trip.trip_purp.isin([1])]
    fixed_work_trips_idx = worktrips[worktrips.groupby('person_id').dest_latlon.transform('count') == 1].index
    fixed_coords = trip.loc[fixed_work_trips_idx].dest_latlon.str.split(',', expand=True)[coord_id]

    # return pd.concat([trip, fixed_coords], axis=1)[coord_id].fillna(0)
    # Pull in person_id and assign work location for all 'trip' rows for each person
    person_trips = trip[['person_id']].merge(fixed_coords, on='trip_id')
    person_trips_xy = trip.reset_index().merge(person_trips, on='person_id', how='left').set_index('trip_id')[coord_id]
    return person_trips_xy


def get_TOTTR_NEXT(trip, trip_hhcompanions_pnum):
    n_companions = pd.concat([trip[['trip_companions', 'person_id']],
                              trip_hhcompanions_pnum.groupby('trip_id').size()], axis=1)
    n_companions = n_companions.reset_index().set_index(['trip_id', 'person_id']).sum(axis=1)
    n_companions = n_companions.groupby('person_id').shift(1).fillna(0).astype(int).droplevel(1)
    return n_companions

