##################################################################
# Script for processing extracted input files for SPA tool
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
import os

from core.configs import read_config

class ExpressionPreProcess:
    def __init__(self, namespace, **kwargs):

        if not kwargs.get('mapping_file'):
            settings_file = os.path.join(args.config, 'settings.yaml')
            kwargs = {**kwargs, **read_config(settings_file).get('PROCESSING_STEPS').get('ExpressionPreProcess')}

        assert kwargs.get('mapping_file')
        if not os.path.isabs(kwargs.get('mapping_file')):
            kwargs['mapping_file'] = os.path.join(namespace.config, kwargs.get('mapping_file'))

        assert kwargs.get('input_tables') or kwargs.get('results')

        if 'input_tables' not in kwargs.keys() and 'results' in kwargs.keys():
            assert kwargs.get('results').get('raw_data')
            kwargs['input_tables'] = kwargs.get('results').get('raw_data')

        self.expressions = pd.read_csv(kwargs['mapping_file'], dtype=str)
        self.input_tables = self.load_tables(kwargs.get('input_tables'))

    def run(self):
        self.run_expressions()
        return self.output_tables

    def set_global_tables(self):
        # This function can be used to create local variables as table names for debugging and script testing
        # e.g., ExpressionPreProcess(input_tables="..//folder...").set_locals()
        for k, df in self.input_tables.items():
            globals()[k] = df

    def load_tables(self, input_tables):

        assert isinstance(input_tables, dict) or (os.path.exists(input_tables) and len(files) > 0)
        if not isinstance(input_tables, dict):
            files = [f for f in os.listdir(input_tables) if os.path.isfile(os.path.join(input_tables, f))]
            csv_dict = {os.path.splitext(k)[0]: k for k in files}
            df_dict = {k: pd.read_csv(os.path.join(input_tables, csv), index_col=f'{k}_id')
                       for k, csv in csv_dict.items()}
            return df_dict
        else:
            return input_tables

    def run_expressions(self):
        self.output_tables = {}

        # self.set_global_tables()
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
                tables[table_name] = pd.concat(table_cols, axis=1).reindex(table_expressions.Field, axis=1)

        self.output_tables = tables


    def save_tables(self, out_dir):
        if self.output_tables is None:
            print('Data not loaded yet, run .run_expressions() on ExpressionPreProcess class')
            return
        else:
            for k, df in self.output_tables.items():
                df.to_csv(f'{out_dir}/{k}.csv', index=True)

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

    # Fetch data from the database
    from accessdb import GetDBData
    # DBData = GetDBData(db_path, configs_dir)
    # DBData.get_tables()
    # DBData.data

    PP = ExpressionPreProcess(args, input_tables='../../intermediate/raw_tables')
    PP.run_expressions()

    for table_name, table in PP.output_tables.items():
        # Need to filter out for categorical
        table.describe().to_csv(os.path.join('../../intermediate/processed/stats', table_name+'.csv'))

    PP.save_tables(out_dir='../../intermediate/processed')


