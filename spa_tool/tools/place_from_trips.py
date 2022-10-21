##################################################################
### Script for processing raw Dubai survey data to ABM tables
### Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2020
##################################################################
import os
from proc_accessdb import GetDBData

# Raw Survey data location on RSG cloud
data_dir = os.path.join('C:\\Users\\{}'.format(os.getlogin()),
            'Resource Systems Group, Inc',
            'Model Development - Dubai RTA ABM Development Project',
            'data\\fromIBI/2014 Survey Data'
)
db_path = os.path.join(data_dir, '2014-12-20_SP_RP_Data_Aur//RP_2014_141218.accdb')
codebook_dir = 'configs'
output_dir = 'output/processed_inputs'

# Fetch data from the database
DBData = GetDBData(db_path, codebook_dir, output_dir)


