import pandas as pd
import os

# data
hh = pd.read_csv('../processed_inputs/household.csv', index_col='household_id')
per = pd.read_csv('../processed_inputs/person.csv', index_col='person_id')
day = pd.read_csv('../processed_inputs/day.csv', index_col='day_id')
trip = pd.read_csv('../processed_inputs/trip.csv', index_col='trip_id')
veh = pd.read_csv('../processed_inputs/vehicle.csv', index_col='vehicle_id')

# expressions
expressions = pd.read_csv('../configs/field_mapping.csv')

