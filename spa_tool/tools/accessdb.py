##################################################################
# Script for extracting raw data from ms access db
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################
import pandas as pd
from pandas_weighting import weight
import pyodbc
import os

from core import functions
from core import modules

class GetAccessDBData(modules.SPAModelBase):
    names_map = {
        'H': 'household',
        'H6': 'vehicle',
        'P': 'person',
        'T1': 'day',
        'T2': 'trip',
        '_Activities': 'coded_activities',
        '_Person groups': 'coded_person_group'
    }

    def __init__(self, namespace, **kwargs):
        # This passes arguments to the base model
        super().__init__(namespace, **kwargs)
        self.codebook = pd.read_csv(self.kwargs.get('configs').get('dictionary_file'))
        self.codevalues = pd.read_csv(self.kwargs.get('configs').get('categories_file'))

    def run(self):
        self.read_accessdb()
        return self.raw_data_formatted

    def read_accessdb(self):
        database_path = self.kwargs.get('data').get('database').get('file')

        driver = '{Microsoft Access Driver (*.mdb, *.accdb)}'
        conn_string = f'DRIVER={driver};DBQ={database_path}'

        with pyodbc.connect(conn_string) as conn:
            cursor = conn.cursor()
            table_names = [x.table_name for x in cursor.tables() if 'MSys' not in x.table_name]
            self.raw_data = {tb: pd.read_sql(f'SELECT * FROM "{tb}"', conn) for tb in table_names}

        names = self.names_map
        codegroup = self.codebook.groupby('Form')
        
        self.coded_activities = self.raw_data['_Activities']
        self.coded_person_groups = self.raw_data['_Person groups']

        formatted = {
            names[f]: self.raw_data[f].rename(columns=dict(zip(codes.Question, codes.Shortname)))
            for f, codes in codegroup
            }

        # Concatenate person_id for vehicles
        formatted['vehicle']['person_id'] = (
                formatted['vehicle'].household_id.astype(str) +
                formatted['vehicle'].veh_ownership.astype(int).astype(str).str.zfill(2)
        )
        formatted['vehicle'].loc[formatted['vehicle'].veh_ownership.isin([-1]), 'person_id'] = -1
        formatted['vehicle'].person_id = formatted['vehicle'].person_id.astype(int)

        formatted = {k: self.clean_dtypes(k, df) for k, df in formatted.items()}

        formatted = self.calc_weights(formatted)
        self.raw_data_formatted = {**formatted, **self.many2many_df(formatted)}

        return

    def many2many_df(self, df_dict):
        exploded_dict = {}
        for _, varitem in self.codebook[self.codebook['Unit type'] == 'list'].iterrows():
            df = df_dict[varitem.Table]
            df = df[df[varitem.Shortname] != '']

            exploded = df[varitem.Shortname].str.split(',').explode().astype(int).to_frame()
            exploded['household_id'] = df.loc[exploded.index]['household_id']
            exploded_dict[varitem.Shortname] = exploded.reset_index().rename_axis(varitem.Shortname + '_id')

        return exploded_dict

    def clean_dtypes(self, df_name, df):
        if df_name in ['person', 'household']:
            df.set_index(f'{df_name}_id', inplace=True)
        else:
            df.index.name = f'{df_name}_id'
            df.index += 1
        codebook = self.codebook.set_index('Table')
        unit_group = codebook.loc[df_name].set_index('Unit type')['Shortname'].groupby(level=0)
        for unit, cols in unit_group:
            cols = list(set(cols).intersection(df.columns))
            unit_dtype = {'date': str, 'time': str, 'cat': str,
                          'int': int, 'bool': bool, 'str': str,
                          'float': float, 'list': str}[unit]
            if unit == 'time':
                df[cols] = df[cols].apply(lambda x: x.astype(str).str[11:])
            elif unit_dtype == bool or unit_dtype == int:
                df[cols] = df[cols].fillna(0)
            elif unit == 'cat':
                df[cols] = df[cols].fillna(-999).astype(int)
                # df[cols] = df[cols].astype('Int64').astype(str)
            elif unit == 'str' or unit == 'list':
                df[cols] = df[cols].fillna('')
            else:
                df[cols] = df[cols].astype(unit_dtype)

        for c in ['hoh_name', 'ph', 'email', 'person_name']:
            if c in df.columns:
                df.drop(columns=c, inplace=True)

        return df

    def calc_weights(self, tables):
        # Add in weights. There are some persons with missing weights?
        wt_cols = ['household_id', 'person_id', 'person_weight']
        weights = tables['day'][wt_cols].drop_duplicates().reset_index(drop=True)

        weighted_tables = {}
        for table_name, table in tables.items():
            # If household table, calculate the mean weight
            if 'person_weight' in table.columns:
                table_weighted = table
            elif table_name == 'household':
                hh_weights = weights.groupby('household_id').person_weight.mean()
                table_weighted = table.merge(hh_weights, on='household_id', how='left')
            else:
                index_col = table.index.name
                table_weighted = table.reset_index().merge(weights, on=['household_id', 'person_id'], how='left')
                table_weighted = table_weighted.set_index(index_col)

            # Rename weight column
            table_weighted = table_weighted.rename(columns={'person_weight': table_name + '_weight'})

            # Find missing
            missing = table_weighted[table_weighted[table_name + '_weight'].isnull()]
            table_weighted = table_weighted[~table_weighted[table_name + '_weight'].isnull()]

            weighted_tables[table_name] = table_weighted
            if not missing.empty:
                weighted_tables['missing_weights_' + table_name] = missing

        return weighted_tables

    def summary_stats(self, output_dir):
        pd.DataFrame.weight = weight

        if not os.path.isdir(output_dir):
            output_dir = os.path.join(self.namespace.output, output_dir)

        # Add hhsize
        tables = self.raw_data_formatted
        tables['household']['hh_size'] = tables['household'][['hh_under6yr','hh_over6yr']].sum(axis=1)

        # Setup codebook
        summarize_codebook = self.codebook[['Table', 'Shortname', 'Summarize']].rename(
            columns={'Table': 'table',
                     'Shortname': 'field_name',
                     'Summarize': 'stat_type'})

        # Add in this one off
        summarize_codebook = pd.concat(
            [summarize_codebook,
             pd.DataFrame({'table': ['household'], 'field_name': ['hh_size'], 'stat_type': ['Categorical']})],
        axis=0)

        # Produce summaries
        for table_name, table in tables.items():

            if table_name not in summarize_codebook.table.unique():
                continue

            # Cleanup the data before summarizing
            table = table.replace(-1, None)

            results = {
                # Get unweighted values, initialize dummy item
                'unweighted': functions.summarize(table, summarize_codebook),
                # Add weighted results where applicable
                'weighted': functions.summarize(table, summarize_codebook, weight_col=table_name + '_weight')
            }

            # Save to excel
            with pd.ExcelWriter(os.path.join(output_dir, 'stats_' + table_name + '.xlsx'), mode='w') as writer:
                for weight_type, table_results in results.items():
                    for stat_type, df in table_results.items():
                        df.to_excel(writer, sheet_name='{} ({})'.format(stat_type, weight_type))

    def save_tables(self, output_dir):
        if not os.path.isdir(output_dir):
            output_dir = os.path.join(self.namespace.output, output_dir)

        if self.raw_data_formatted is None:
            print('Data not loaded yet, run .get_tables() on GetDBData class')
            return
        else:
            for k, df in self.raw_data_formatted.items():
                df.to_csv(f'{output_dir}/{k}.csv', index=True)

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
    args.data = [
        'C:\gitclones\Dubai_survey_processing\data',
        'C:\\Users\\nick.fournier\\Resource Systems Group, Inc\\Model Development - Dubai RTA ABM Development Project\\data\\fromIBI\\2014 Survey Data\\2014-12-20_SP_RP_Data_Aur'
    ]
    args.output = 'C:\gitclones\Dubai_survey_processing\data'

    # Fetch data from the database
    DBData = GetDBData(args)
    DBData.read_accessdb()

    DBData.summary_stats('raw/stats')
    DBData.save_tables('raw')
