##################################################################
# Script for converting a trips table into a place table
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
from copy import deepcopy
import os
from core import functions

# TODO Set this up so that it can be run as a workflow step
class TripsToPlace():
    def __init__(self, namespace, **kwargs):
        self.namespace = namespace
        self.kwargs = self.update_kwargs(**kwargs)
        self.input_tables = functions.load_pipeline_tables(self.namespace, self.kwargs)
        self.place = None

    def run(self):
        self.run_conversion()
        return self.place

    def set_global_tables(self):
        # This function can be used to create local variables as table names for debugging and script testing
        # e.g., ExpressionPreProcess(input_tables="..//folder...").set_locals()
        for k, df in self.input_tables.items():
            globals()[k] = df

    def update_kwargs(self, **kwargs):
        if not kwargs.get('module'):
            settings_file = os.path.join(self.namespace.config, 'settings.yaml')
            kwargs = {**kwargs, **functions.read_config(settings_file).get('PROCESSING_STEPS').get('ExpressionPreProcess')}

        if not os.path.isabs(kwargs.get('mapping_file')):
            kwargs['mapping_file'] = os.path.join(self.namespace.config, kwargs.get('mapping_file'))

        assert kwargs.get('mapping_file') and kwargs.get('data_directory')

        return kwargs

    def save_tables(self, out_dir):
        if self.place is None:
            print('Data not loaded yet, run .run_expressions() on ExpressionPreProcess class')
            return
        else:
            self.place.to_csv(f'{out_dir}.csv', index=False)
        return

if __name__ == '__main__':
    import argparse
    from core.main import add_run_args

    # Test scripts
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.config='C:\gitclones\Dubai_survey_processing\configs'
    args.data = 'C:\gitclones\Dubai_survey_processing\data'

    # Fetch data from the database
    from accessdb import GetDBData
    # DBData = GetDBData(db_path, configs_dir)
    # DBData.get_tables()
    # DBData.data

    PP = TripsToPlace(args, input_tables='../../data/raw')
    PP.run_conversion()

    for table_name, table in PP.output_tables.items():
        # Need to filter out for categorical
        table.describe().to_csv(os.path.join('../../data/preprocessed/stats', table_name + '.csv'))

    PP.save_tables(out_dir='../../data/preprocessed')


