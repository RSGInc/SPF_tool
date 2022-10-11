##################################################################
### Script for processing raw Dubai survey data to ABM tables
### Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2020
##################################################################
import os
import pandas as pd
from PythonScripts import hts_functions

# Raw Survey data location on RSG cloud
data_dir = os.path.join('C:\\Users\\{}'.format(os.getlogin()),
            'Resource Systems Group, Inc',
            'Model Development - Dubai RTA ABM Development Project',
            'data\\fromIBI/2014 Survey Data'
)
db_path = os.path.join(data_dir, '2014-12-20_SP_RP_Data_Aur//RP_2014_141218.accdb')
codebook_path = os.path.join(data_dir, 'RP_Questionnaire_Codebook.xlsx')

# Read in the codebook values
codebook = pd.read_excel(codebook_path, sheet_name='Codebook')
codevalues = pd.read_excel(codebook_path, sheet_name='Code Values')

# Fetch data from the database
DBData = hts_functions.GetDBData(db_path, codebook)
