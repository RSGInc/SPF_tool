import pandas as pd

hh = pd.read_csv('../processed_inputs/households.csv', index_col='household_id')
per = pd.read_csv('../processed_inputs/persons.csv', index_col='per_id')
day = pd.read_csv('../processed_inputs/day.csv', index_col='day_id')
trip = pd.read_csv('../processed_inputs/trips.csv', index_col='trip_id')
veh = pd.read_csv('../processed_inputs/vehicles.csv', index_col='veh_id')


per.is_labourer.fillna(0, inplace=True)
hh.has_labourer.fillna(0, inplace=True)


# Household labourer count
hh.groupby('has_labourer').count()


labour_counts = per.reset_index().merge(
    day[['household_id','person_num','person_weight']].drop_duplicates(),
    on=['household_id','person_num']
)
labour_counts['persons'] = 1
person_counts = labour_counts.groupby('is_labourer')[['persons','person_weight']].sum()


per.join(trip.person_weight)

day.person_weight.drop_duplicates()
trip[['is_labourer', 'person_weight']].drop_duplicates()

hh.groupby('has_labourer')['household_id'].count()




hh = hh.merge(
        per.groupby('household_id')['is_labourer'].sum()\
            .reset_index().rename(columns={'is_labourer':'hh_labourers'}),
        on='household_id'
)