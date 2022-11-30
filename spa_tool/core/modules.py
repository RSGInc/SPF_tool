##################################################################
# This establishes the base model to inherit base functions.
# Creates basic building block for users. Less is more! Simpler is better!
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
from copy import deepcopy
import os
from core import functions

class SPAModelBase:
    def __init__(self, namespace, **kwargs):
        self.namespace = self.check_namespace(namespace)
        self.kwargs = self.update_kwargs(**kwargs)
        self.input_tables = functions.load_pipeline_tables(self.namespace, self.kwargs)

    def check_namespace(self, namespace):
        assert 'configs' in namespace and 'data' in namespace
        return namespace

    def set_global_tables(self):
        # This function can be used to create local variables as table names for debugging and script testing
        # e.g., ExpressionPreProcess(input_tables="..//folder...").set_locals()
        for k, df in self.input_tables.items():
            globals()[k] = df

    def update_kwargs(self, **kwargs):
        # This adds the settings file data to the kwargs passed
        if not kwargs.get('module'):
            settings_file = os.path.join(self.namespace.configs, 'settings.yaml')
            kwargs = {
                **kwargs,
                **functions.read_config(settings_file).get('PROCESSING_STEPS').get(self.__class__.__name__)
            }

        # Append file paths
        namespace_map = {
            'configs': self.namespace.configs,
            'data': self.namespace.data
        }
        for spec in ['configs', 'data']:
            if not kwargs.get(spec):
                continue
            for file_name, file_params in kwargs.get(spec).items():
                if isinstance(file_params, dict):
                    file_params['file'] = self.append_source_root(file_params['file'], namespace_map[spec])
                else:
                    file_params = self.append_source_root(file_params, namespace_map[spec])
                kwargs[spec][file_name] = file_params

        return kwargs

    def append_source_root(self, file, sources):
        if not os.path.isabs(file):
            if not isinstance(sources, list):
                sources = [sources]
            path = [os.path.join(dir, file) for dir in sources if
                    os.path.isfile(os.path.join(dir, file))]

            assert len(path) <= 1, f'{len(path)} files found for "{file}" input! Expecting only 1.'
            if len(path) == 1:
                file = path[0]
        return file

    # TODO Make generalizable
    # def save_tables(self, out_dir):
    #     if self.place is None:
    #         print('Data not loaded yet, run .run_expressions() on ExpressionPreProcess class')
    #         return
    #     else:
    #         self.place.to_csv(f'{out_dir}.csv', index=False)
    #     return






