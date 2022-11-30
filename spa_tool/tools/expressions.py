##################################################################
# Script for processing extracted input files for SPA tool
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
import os
import sys
import importlib.util
from core import functions
from core import modules

class ExpressionPreProcess(modules.SPAModelBase):
    def __init__(self, namespace, **kwargs):
        super().__init__(namespace, **kwargs)
        self.init_long_expressions()

    def run(self):
        self.run_expressions()
        return self.output_tables

    def init_long_expressions(self):
        self.expressions = pd.read_csv(self.kwargs.get('configs').get('mapping_file'), dtype=str)
        module_file = self.kwargs.get('configs').get('long_expressions')
        if module_file:
            module_name = os.path.splitext(module_file)[0]

            if not os.path.isabs(module_file):
                module_file = os.path.join(self.namespace.config, module_file)

            assert os.path.exists(module_file), "Can't find long expression module"

            spec = importlib.util.spec_from_file_location(module_name, module_file)
            LongExpressions = importlib.util.module_from_spec(spec)
            sys.modules[module_name] = LongExpressions
            spec.loader.exec_module(LongExpressions)

            method_list = [attr for attr in dir(LongExpressions)
                           if callable(getattr(LongExpressions, attr)) and attr.startswith('__') is False]

            for func in method_list:
                self.__setattr__(func, LongExpressions.__getattribute__(func))


    def run_expressions(self):
        self.output_tables = {}

        for k, df in self.input_tables.items():
            locals()[k] = df

        tables = {}
        # For each table
        for table_name, table_expressions in self.expressions.groupby('Table'):
            # For each categorical field, if not categorical, it skips to field value
            table_cols = {}
            if table_name.strip()[0] == '#':
                continue
            for field, expr_df in table_expressions.groupby('Field'):
                # For each field value expression
                field_vals = {}
                for val, expr_ in expr_df.set_index('Values').iterrows():
                    if not expr_.isnull().Expression and expr_.Expression != '':
                        try:
                            field_vals[val] = eval(expr_.Expression)
                        except:
                            print(f'Expression failed: {expr_.Expression} for field {field} in table {table_name}')
                            field_vals[val] = eval(expr_.Expression)


                if len(field_vals) > 0:
                    if expr_.Type.lower() == 'categorical':
                        concat = pd.concat(field_vals, axis=1)

                        if any(concat.sum(axis=1) > 1):
                            print(f'Some records have multiple values assigned in field {field} for {table_name}, check the field_mapping!')
                            print(f'Multiple value records are for rows:\n {locals()[table_name].loc[concat.sum(axis=1) > 1].head()}')
                            print(concat[concat.sum(axis=1) != 1].head())

                        if any(concat.sum(axis=1) < 1):
                            print(f'Some records have no values assigned in field {field} for {table_name}, check the field_mapping!')
                            print(f'Missing records are for rows:\n {locals()[table_name].loc[concat.sum(axis=1) < 1].head()}')
                            print(concat[concat.sum(axis=1) != 1].head())

                        assert not any(concat.sum(axis=1) != 1)

                        melted = concat.melt(ignore_index=False)
                        table_cols[field] = melted[melted.value].variable
                    else:
                        table_cols[field] = next(iter(list(field_vals.values())))

            # Check indices are consistent
            if 'SAMPN' in table_cols.keys():
                first_ind = table_cols['SAMPN'].sort_index().index
            else:
                first_ind = list(table_cols.values())[0].sort_index().index
            [print(f'{k} does not match table index') for k, v in table_cols.items() if not first_ind.equals(v.sort_index().index)]

            assert all(first_ind.equals(x.sort_index().index) for x in table_cols.values())

            if len(table_cols) > 0:
                tables[table_name] = pd.concat(table_cols, axis=1).reindex(table_expressions.Field.unique(), axis=1)

        self.output_tables = tables

    def save_tables(self, out_dir):
        if self.output_tables is None:
            print('Data not loaded yet, run .run_expressions() on ExpressionPreProcess class')
            return
        else:
            for k, df in self.output_tables.items():
                df.to_csv(f'{out_dir}/{k}.csv', index=False)

        return

if __name__ == '__main__':
    import argparse
    from core.main import add_run_args

    # Test scripts
    parser = argparse.ArgumentParser()
    add_run_args(parser)
    args = parser.parse_args()

    # manually inject args
    args.configs = 'C:\gitclones\Dubai_survey_processing\configs'
    args.data = 'C:\gitclones\Dubai_survey_processing\data'

    # Fetch data from the database
    from accessdb import GetDBData
    # DBData = GetDBData(db_path, configs_dir)
    # DBData.get_tables()
    # DBData.data

    PP = ExpressionPreProcess(args, input_tables='../../data/raw')
    PP.run_expressions()

    for table_name, table in PP.output_tables.items():
        # Need to filter out for categorical
        table.describe().to_csv(os.path.join('../../data/preprocessed/stats', table_name + '.csv'))

    PP.save_tables(out_dir='../../data/preprocessed')


