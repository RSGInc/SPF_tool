##################################################################
### Script for processing extracting raw data from ms access db
### Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2022
##################################################################
import pandas as pd
from pandas.api.types import is_numeric_dtype
import pyodbc
import os


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

    def __init__(self, db_path, configs_dir, output_dir):
        self.codebook = pd.read_csv(os.path.join(configs_dir, 'codebook.csv'))
        self.codevalues = pd.read_csv(os.path.join(configs_dir, 'codevalues.csv'))
        self.data = self.get_tables(db_path)
        self.save_tables(output_dir)

    def get_tables(self, db_path):
        driver = '{Microsoft Access Driver (*.mdb, *.accdb)}'
        conn_string = f'DRIVER={driver};DBQ={db_path}'

        with pyodbc.connect(conn_string) as conn:
            cursor = conn.cursor()
            table_names = [x.table_name for x in cursor.tables() if 'MSys' not in x.table_name]
            data = {tb: pd.read_sql(f'select * from "{tb}"', conn) for tb in table_names}

        names = self.names_map
        codegroup = self.codebook.groupby('Form')
        
        self.coded_activities = data['_Activities']
        self.coded_person_groups = data['_Person groups']

        formatted = {
            names[f]: data[f].rename(columns=dict(zip(codes.Question, codes.Shortname))) for f, codes in codegroup
            }
        
        formatted['household'].set_index('household_id')
        formatted = {k: self.clean_dtypes(k, df) for k, df in formatted.items()}
        formatted = {**formatted, **self.many2many_df(formatted)}

        return formatted

    def many2many_df(self, df_dict):
        exploded_dict = {}
        for _, varitem in self.codebook[self.codebook['Unit type'] == 'list'].iterrows():
            df = df_dict[varitem.Table]
            df = df[df[varitem.Shortname] != '']

            exploded = df[varitem.Shortname].str.split(',').explode().astype(int).to_frame()
            exploded['household_id'] = df.loc[exploded.index]['household_id']
            exploded_dict[varitem.Shortname] = exploded.reset_index()

        return exploded_dict

    def clean_dtypes(self, df_name, df):
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
            if unit_dtype == bool or unit_dtype == int:
                df[cols] = df[cols].fillna(0)
            if unit == 'cat':
                df[cols] = df[cols].astype('Int64').astype(str)
            if unit == 'str' or unit == 'list':
                df[cols] = df[cols].fillna('')
            df[cols] = df[cols].astype(unit_dtype)

        if df_name == 'person':
            df = df.drop(columns=['person_name'])

        if df_name == 'household':
            df = df.drop(columns=['hoh_name', 'ph', 'email'])

        return df


    def save_tables(self, output_dir):
        for k, df in self.data.items():
            df.to_csv(f'{output_dir}/{k}.csv', index=True)


if __name__ == '__main__':
    # Test scripts

    # Raw Survey data location on RSG cloud
    data_dir = os.path.join('C:\\Users\\{}'.format(os.getlogin()),
                'Resource Systems Group, Inc',
                'Model Development - Dubai RTA ABM Development Project',
                'data\\fromIBI/2014 Survey Data'
    )
    db_path = os.path.join(data_dir, '2014-12-20_SP_RP_Data_Aur//RP_2014_141218.accdb')
    codebook_dir = '../../configs'
    output_dir = '../../output/processed_inputs'

    # Fetch data from the database
    DBData = GetDBData(db_path, codebook_dir, output_dir)





