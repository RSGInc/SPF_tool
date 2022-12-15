import os
import math
import yaml
import json
import pandas_weighting
import pandas as pd
import numpy as np
from collections import defaultdict


class JSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if hasattr(obj, 'to_json'):
            return obj.to_json(orient='records')
        return json.JSONEncoder.default(self, obj)


def read_mappings(**file_paths):
    def get_defaultdict(map):
        if isinstance(map, dict) and "default" in map.keys():
            default = map.pop("default")
            return defaultdict(lambda: default, map)
        else:
            return map

    var_map = {}
    for file, path in file_paths.items():
        if ".csv" in path:
            var_map[file] = pd.read_csv(
                path, index_col="key", names=["key", "name"], header=None
            ).name.to_dict()
        else:
            with open(path, "r") as f:
                mapping = yaml.safe_load(f)
            var_map = {
                **var_map,
                **{map_name: get_defaultdict(m) for map_name, m in mapping.items()},
            }

    return var_map


def read_config(file_path):
    with open(file_path, "r") as file:
        config = yaml.safe_load(file)

    # # Setup default values for any missing
    # defaults = {"PROCESSING_STEPS": None, "START_FROM": None}

    for k, v in config.items():
        config[k] = config.setdefault(k, v)

    return config


def load_pipeline_tables(kwargs):
    input_tables = {}

    if not kwargs.get("data"):
        return input_tables

    if kwargs.get("from_pipeline", True) and kwargs.get("pipeline").get(kwargs.get("data_directory")) is not None:
        assert kwargs.get("pipeline").get(
            kwargs.get("data_directory")
        ), f"{kwargs.get('data_directory')} not found in pipeline."

        input_tables = kwargs.get("pipeline").get(kwargs.get("data_directory"))

    else:
        for table_name, table_params in kwargs.get("data").items():
            if isinstance(table_params, dict) and not table_params.get("read_csv", True):
                continue

            if isinstance(table_params, str) and '.csv' in table_params:
                table_dir = table_params
                index_col = None
            else:
                table_dir = table_params.get("file")
                index_col = table_params.get("index")

            # Check if multiple found
            if isinstance(table_dir, list):
                assert len(table_dir) == 1, (
                    f'{len(table_dir)} files found for "{table_dir}" input!'
                    "If >1, check for multiple files in data folders."
                    "If 0, check if correct folder specified, or is empty!"
                )
                table_dir = table_dir[0]

            input_tables[table_name] = pd.read_csv(table_dir, index_col=index_col)

    return input_tables


def distance_on_unit_sphere(lat1, long1, lat2, long2):
    """source: http://www.johndcook.com/python_longitude_latitude.html"""
    # returns distance in mile

    # Convert latitude and longitude to
    # spherical coordinates in radians.
    degrees_to_radians = math.pi / 180.0

    # phi = 90 - latitude
    phi1 = (90.0 - lat1) * degrees_to_radians
    phi2 = (90.0 - lat2) * degrees_to_radians

    # theta = longitude
    theta1 = long1 * degrees_to_radians
    theta2 = long2 * degrees_to_radians

    # Compute spherical distance from spherical coordinates.
    # For two locations in spherical coordinates
    # (1, theta, phi) and (1, theta, phi)
    # cosine( arc length ) =
    #    sin phi sin phi` cos(theta-theta`) + cos phi cos phi`
    # distance = rho * arc length
    cos = np.sin(phi1) * np.sin(phi2) * np.cos(theta1 - theta2) + np.cos(phi1) * np.cos(
        phi2
    )
    # Multiply arc by the radius of the earth in feet
    return np.arccos(cos) * 3960


def calculate_duration(start_hr, start_min, end_hr, end_min):
    # TODO: check how time is coded when clock goes over 12am into the next time
    start_time = start_hr * 60 + start_min  # convert to minutes
    end_time = end_hr * 60 + end_min  # convert to minutes
    total_minutes = end_time - start_time
    dur_hr = total_minutes // 60
    dur_min = total_minutes % 60
    return dur_hr, dur_min


def convert2minutes(hours, minutes):
    """given a duration of (hours, minutes), return the equivalent in minutes"""
    return hours * 60 + minutes


def add_quote_char(string):
    return '"' + string + '"'


def summarize(table, stat_codebook, weight_col=None):
    pd.DataFrame.weight = pandas_weighting.weight

    # codebook must have field_name and stat_type fields
    cat_filt = stat_codebook.field_name.isin(
        table.columns
    ) & stat_codebook.stat_type.isin(["Categorical"])
    cat_cols = stat_codebook[cat_filt].field_name.drop_duplicates()

    num_filt = stat_codebook.field_name.isin(
        table.columns
    ) & stat_codebook.stat_type.isin(["Numeric"])
    num_cols = stat_codebook[num_filt].field_name.drop_duplicates()

    if weight_col:
        num_cols = [x for x in num_cols if x != weight_col]
    else:
        table["wt"] = 1
        weight_col = "wt"

    # Summary stats of numeric and categorical variables
    stats = {}
    if len(num_cols) > 0:
        stats["Numeric"] = table[num_cols].weight(table[weight_col]).describe()

    if len(cat_cols) > 0:
        cat_stats = {c: table.groupby(c)[weight_col].sum() for c in cat_cols}
        # Update index to match
        for c, v in cat_stats.items():
            cat_stats[c].index = v.index.astype(int)
        # Concatenate to dataframe
        stats["Categorical"] = (
            pd.concat(cat_stats, axis=1)
            .sort_index()
            .reset_index()
            .rename(columns={"index": "Category"})
        )

    return stats


def find_source_root(file, sources):
    if not os.path.isabs(file):
        if not isinstance(sources, list):
            sources = [sources]
        path = [
            os.path.join(dir, file)
            for dir in sources
            if os.path.isfile(os.path.join(dir, file))
        ]

        assert (
            len(path) <= 1
        ), f'{len(path)} files found for "{file}" input! Expecting only 1.'
        if len(path) == 1:
            file = path[0]
    return file

# def recursive_file_dict(file_dict, root_dir, flat_dict={}):
#     for k, v in file_dict.items():
#         if isinstance(v, str):
#             flat_dict[k] = find_source_root(v, root_dir)
#         elif v.get('file'):
#             v['file'] = find_source_root(v.get('file'), root_dir)
#             flat_dict[k] = v
#         else:
#             recursive_file_dict(v, os.path.join(root_dir, k), flat_dict)
#
#     return flat_dict

def recursive_file_dict(file_dict, root_dir):
    flat_dict = {}
    def flatten(d, root_dir, fd={}):
        for k, v in d.items():
            if isinstance(v, str):
                fd[k] = find_source_root(v, root_dir)
            elif v.get('file'):
                v['file'] = find_source_root(v.get('file'), root_dir)
                fd[k] = v
            else:
                flatten(v, os.path.join(root_dir, k), fd)
        return fd

    flat_dict = flatten(file_dict, root_dir)

    return flat_dict

# def recursive_file_dict(file_dict, root_dir):
#     def flatten(key, value, root_dir):
#         if isinstance(value, str):
#             return (key, find_source_root(value, root_dir))
#         elif value.get('file'):
#             value['file'] = find_source_root(value.get('file'), root_dir)
#             return (key, value)
#         else:
#             return [(k, v) for k, v in recursive_file_dict(value, os.path.join(root_dir, key)).items()]
#
#     # flat = {k: v for k, v in file_dict.items()}
#     items = [item for k, v in file_dict.items() for item in flatten(k, v, root_dir)]
#
#     return dict(items)