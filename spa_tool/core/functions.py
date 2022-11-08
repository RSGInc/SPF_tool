import os
import pandas as pd
import math
import yaml
from collections import defaultdict


def read_mappings(**file_paths):
    def get_defaultdict(map):
        if isinstance(map, dict) and 'default' in map.keys():
            default = map.pop('default')
            return defaultdict(lambda: default, map)
        else:
            return map

    var_map = {}
    for file, path in file_paths.items():
        if '.csv' in path:
            var_map[file] = pd.read_csv(path, index_col='key', names=['key', 'name'], header=None).name.to_dict()
        else:
            with open(path, 'r') as f:
                mapping = yaml.safe_load(f)
            var_map = {**var_map,
                       **{map_name: get_defaultdict(m) for map_name, m in mapping.items()}}

    return var_map



def read_config(file_path):
    with open(file_path, 'r') as file:
        config = yaml.safe_load(file)

    # Setup default values for any missing
    defaults = {
                'PROCESSING_STEPS': None
                }
    config = {k: config.setdefault(k, v) for k, v in defaults.items()}

    return config


def load_pipeline_tables(namespace, kwargs):
    if kwargs.get('from_pipeline'):
        assert kwargs.get('pipeline').get(kwargs.get('data_directory')),\
            f"{kwargs.get('data_directory')} not found in pipeline."
        input_tables = kwargs.get('pipeline').get(kwargs.get('data_directory'))
    else:
        folder = kwargs.get('data_directory')
        tables = kwargs.get('tables')

        # Get file list
        if not os.path.isabs(folder):
            sources = [namespace.data] if not isinstance(namespace.data, list) else namespace.data
            full_path = [os.path.join(src, folder) for src in sources if os.path.isdir(os.path.join(src, folder))]
        else:
            full_path = folder

        assert len(full_path) == 1, f'{len(full_path)} files found for "{folder}" input!'\
                                    'If >1, check for multiple files in data folders.'\
                                    'If 0, check if correct folder specified, or is empty!'

        full_path = full_path[0]

        if tables:
            file_list = [os.path.join(full_path, x + '.csv') for x in tables]
        else:
            file_list = [os.path.join(full_path, x) for x in os.listdir(full_path)
                         if not os.path.isdir(x) and '.csv' in str(x)]

        assert len(file_list) > 0, 'No input files found in this directory!'

        csv_dict = {os.path.splitext(os.path.split(k)[-1])[0]: k for k in file_list}
        input_tables = {k: pd.read_csv(path, index_col=f'{k}_id') for k, path in csv_dict.items()}

    return input_tables


def distance_on_unit_sphere(lat1, long1, lat2, long2):
    """source: http://www.johndcook.com/python_longitude_latitude.html """
    # returns distance in mile
    
    # Convert latitude and longitude to 
    # spherical coordinates in radians.
    degrees_to_radians = math.pi/180.0
        
    # phi = 90 - latitude
    phi1 = (90.0 - lat1)*degrees_to_radians
    phi2 = (90.0 - lat2)*degrees_to_radians
        
    # theta = longitude
    theta1 = long1*degrees_to_radians
    theta2 = long2*degrees_to_radians
        
    # Compute spherical distance from spherical coordinates.
        
    # For two locations in spherical coordinates 
    # (1, theta, phi) and (1, theta, phi)
    # cosine( arc length ) = 
    #    sin phi sin phi` cos(theta-theta`) + cos phi cos phi`
    # distance = rho * arc length
    
    cos = (math.sin(phi1)*math.sin(phi2)*math.cos(theta1 - theta2) + 
           math.cos(phi1)*math.cos(phi2))
    arc = math.acos(cos)

    # Multiply arc by the radius of the earth in feet
    return arc*3960


def calculate_duration(start_hr, start_min, end_hr, end_min):
    # TODO: check how time is coded when clock goes over 12am into the next time
    start_time = start_hr*60+start_min  # convert to minutes
    end_time = end_hr*60+end_min        # convert to minutes
    total_minutes = end_time - start_time
    dur_hr = total_minutes//60
    dur_min = total_minutes % 60
    return dur_hr, dur_min
    

def convert2minutes(hours, minutes):
    """ given a duration of (hours, minutes), return the equivalent in minutes """
    return hours*60+minutes


def add_quote_char(string):
    return '"'+string+'"'
