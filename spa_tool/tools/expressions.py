##################################################################
# Script for processing extracted input files for SPA tool
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################

import pandas as pd
import os

class ExpressionPreProcess:
    def __init__(self, config_dir, input_tables):
        self.data = []
        self.config_dir = config_dir
        self.expressions = pd.read_csv('../../configs/field_mapping.csv', dtype=str)
        self.load_tables(input_tables)
        self.run_expressions()

    def set_global_tables(self):
        # This function can be used to create local variables as table names for debugging and script testing
        # e.g., ExpressionPreProcess(input_tables="..//folder...").set_locals()
        for k, df in self.input_tables.items():
            globals()[k] = df

    def load_tables(self, input_tables):
        assert isinstance(input_tables, dict) or (os.path.exists(input_tables) and len(os.listdir(input_tables)) > 0)
        if not isinstance(input_tables, dict) :
            csv_dict = {os.path.splitext(k)[0]: k for k in os.listdir(input_tables)}
            df_dict = {k: pd.read_csv(os.path.join(input_tables, csv), index_col=f'{k}_id')
                       for k, csv in csv_dict.items()}
            self.input_tables = df_dict
        else:
            self.input_tables = input_tables

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
                            print(f'Multiple value records are for rows:\n {globals()[table_name].loc[concat.sum(axis=1) > 1].head()}')
                            print(concat[concat.sum(axis=1) != 1].head())

                        if any(concat.sum(axis=1) < 1):
                            print(f'Some records have no values assigned in field {field} for {table_name}, check the field_mapping!')
                            print(f'Missing records are for rows:\n {globals()[table_name].loc[concat.sum(axis=1) < 1].head()}')
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
    # Test scripts
    # Raw Survey data location on RSG cloud
    data_dir = os.path.join('C:\\Users\\{}'.format(os.getlogin()),
                            'Resource Systems Group, Inc',
                            'Model Development - Dubai RTA ABM Development Project',
                            'data\\fromIBI/2014 Survey Data'
                            )
    db_path = os.path.join(data_dir, '2014-12-20_SP_RP_Data_Aur//RP_2014_141218.accdb')
    configs_dir = '../../configs'

    # Fetch data from the database
    from proc_accessdb import GetDBData
    # DBData = GetDBData(db_path, configs_dir)
    # DBData.get_tables()
    # DBData.data

    PP = ExpressionPreProcess(config_dir=configs_dir, input_tables='../../intermediate/raw_tables')
    PP.save_tables(out_dir='../../intermediate/processed')


