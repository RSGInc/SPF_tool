##################################################################
# Script for extracting raw data from ms access db
# Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################
import pandas as pd
from pandas_weighting import weight
import pyodbc
import os

from core import functions

class GetDBData:
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
        # if settings not passed, read from settings
        inputs = ['dictionary_file', 'categories_file', 'database_file', 'output_name']
        if any([True for x in inputs if x not in kwargs.keys()]):
            settings_file = os.path.join(namespace.config, 'settings.yaml')
            settings = functions.read_config(settings_file)
            assert settings.get('PROCESSING_STEPS')
            assert settings.get('PROCESSING_STEPS').get('GetDBData')
            kwargs = {**kwargs, **settings.get('PROCESSING_STEPS').get('GetDBData')}

        inputs_map = {
            'dictionary_file': namespace.config,
            'categories_file': namespace.config,
            'database_file': namespace.data
        }

        for f, sources in inputs_map.items():
            assert kwargs.get(f)
            kwargs[f] = self.append_kwargs_sources(kwargs.get(f), sources)

        self.namespace = namespace
        self.kwargs = kwargs
        self.database_path = self.kwargs.get('database_file')
        self.codebook = pd.read_csv(self.kwargs.get('dictionary_file'))
        self.codevalues = pd.read_csv(self.kwargs.get('categories_file'))

    def append_kwargs_sources(self, file, sources):
        if not os.path.isabs(file):
            assert sources, f'Missing source for {file}'
            if not isinstance(sources, list):
                sources = [sources]
            path = [os.path.join(dir, file) for dir in sources if
                    os.path.isfile(os.path.join(dir, file))]
            if len(path) != 1:
                print(f'{len(path)} files found for "{file}" input! Expecting only 1.')
            assert len(path) == 1

            file = path[0]
        return file

    def run(self):
        self.read_accessdb()
        return self.raw_data_formatted

    def read_accessdb(self):
        driver = '{Microsoft Access Driver (*.mdb, *.accdb)}'
        conn_string = f'DRIVER={driver};DBQ={self.database_path}'

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

    def summary_stats(self, output_dir):
        pd.DataFrame.weight = weight

        if not os.path.isdir(output_dir):
            output_dir = os.path.join(self.namespace.output, output_dir)

        def summarize(table, weight_col=None):
            codes = self.codebook

            # cat_filt = codes.Shortname.isin(table.columns) & codes['Unit type'].isin(['cat', 'bool'])
            cat_filt = codes.Shortname.isin(table.columns) & codes.Summarize.isin(['Categorical'])
            cat_cols = codes[cat_filt].Shortname.drop_duplicates()

            # num_filt = codes.Shortname.isin(table.columns) & codes['Unit type'].isin(['int', 'float'])
            num_filt = codes.Shortname.isin(table.columns) & codes.Summarize.isin(['Numeric'])
            num_cols = self.codebook[num_filt].Shortname.drop_duplicates()

            if weight_col:
                num_cols = [x for x in num_cols if x != weight_col]
            else:
                table['wt'] = 1
                weight_col = 'wt'

            # Summary stats of numeric and categorical variables
            stats = {}
            if len(num_cols) > 0:
                stats['Numeric'] = table[num_cols].weight(table[weight_col]).describe()

            if len(cat_cols) > 0:
                cat_stats = {c: table.groupby(c)[weight_col].sum() for c in cat_cols}
                # Update index to match
                for c, v in cat_stats.items():
                    cat_stats[c].index = v.index.astype(int)
                # Concatenate to dataframe
                stats['Categorical'] = pd.concat(cat_stats, axis=1).sort_index().reset_index().rename(columns={'index': 'Category'})

            return stats


        for table_name, table in self.raw_data_formatted.items():

            if table_name not in self.codebook.Table.unique():
                continue

            # Add in weights. There are some persons with missing weights?
            wt_cols = ['household_id', 'person_id', 'person_weight']
            weights = self.raw_data_formatted['day'][wt_cols].drop_duplicates().reset_index(drop=True)

            # Get unweighted values, initialize dummy item
            results = {
                'unweighted': summarize(table),
            }

            if table_name not in ['household', 'day', 'trip']:
                table_weighted = table.merge(weights, on=['household_id', 'person_id'], how='outer')

                # Find missing
                missing = table_weighted[table_weighted.person_weight.isnull()]
                table_weighted = table_weighted[~table_weighted.person_weight.isnull()]
                missing.to_csv(os.path.join(output_dir, table_name + '_missing_weights.csv'))
                # Add weighted results where applicable
                results['weighted'] = summarize(table_weighted, weight_col='person_weight')

            # Save to excel
            with pd.ExcelWriter(os.path.join(output_dir, table_name + '.xlsx'), mode='w') as writer:
                for weight_type, table_results in results.items():
                    for stat_type, df in table_results.items():
                        df.to_excel(writer, sheet_name='{} ({})'.format(stat_type, weight_type))
                    # res['numeric'].to_excel(writer, sheet_name='Numeric ({})'.format(weight_type))
                    # res['categorical'].to_excel(writer, sheet_name='Categorical ({})'.format(weight_type))

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
    args.config = 'C:\gitclones\Dubai_survey_processing\configs'
    args.data = [
        'C:\gitclones\Dubai_survey_processing\data',
        'C:\\Users\\nick.fournier\\Resource Systems Group, Inc\\Model Development - Dubai RTA ABM Development Project\\data\\fromIBI\\2014 Survey Data\\2014-12-20_SP_RP_Data_Aur'
    ]
    args.output = 'C:\gitclones\Dubai_survey_processing\data'

    # Fetch data from the database
    DBData = GetDBData(args)
    DBData.get_tables()

    DBData.summary_stats('raw/stats')
    DBData.save_tables('raw')
