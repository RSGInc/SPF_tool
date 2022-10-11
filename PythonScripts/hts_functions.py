# Setup functions

import pandas as pd
import pyodbc


class GetDBData:
    names_map = {
        'H': 'households',
        'H6': 'vehicles',
        'P': 'persons',
        'T1': 'trips_cover',
        'T2': 'trips',
        '_Activities': 'codes_activities',
        '_Person groups': 'codes_person_group'
    }

    def __init__(self, db_path, codebook):
        self.raw_data = self.get_dbtables(db_path)
        self.data = self.format_tables(codebook)


    def get_dbtables(self, db_path):
        driver = '{Microsoft Access Driver (*.mdb, *.accdb)}'
        conn_string = f'DRIVER={driver};DBQ={db_path}'

        with pyodbc.connect(conn_string) as conn:
            cursor = conn.cursor()
            table_names = [x.table_name for x in cursor.tables() if 'MSys' not in x.table_name]
            data = {tb: pd.read_sql(f'select * from "{tb}"', conn) for tb in table_names}

        return data

    def format_tables(self, codebook):
        formatted = {
            self.names_map[f]: self.raw_data[f].rename(columns=dict(zip(n.Question, n.Shortname)))
            for f, n in codebook.groupby('Form')
        }

        return formatted






