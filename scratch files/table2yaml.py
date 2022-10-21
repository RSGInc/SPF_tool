import pandas as pd
import yaml

#codevalues = pd.read_csv('../configs/codevalues.csv')

excel_dir = os.path.join('C:\\Users\\{}'.format(os.getlogin()),
            'Resource Systems Group, Inc',
            'Model Development - Dubai RTA ABM Development Project',
            'data\\fromIBI/2014 Survey Data',
            'RP_Questionnaire_Codebook.xlsx')

codevalues = pd.read_excel(excel_dir, sheet_name='Code Values').set_index('Form')
codebook = pd.read_excel(excel_dir, sheet_name='Codebook').set_index('Form')


cols = ['Question','Description','Unit type']
out_dict = {}
for form, formdf in codevalues.set_index('Form').groupby('Form'):
    codebook.loc[form].to_dict('r')

    grouped = formdf.set_index(cols).groupby(cols)
    for k, df in grouped:        
        out_dict[form] = {**dict(zip(['Question','Description','Unit type'], k)),
                    **{'Value label': dict(zip(df.Values, df.Labels))}}


with open('../configs/codedvalues.yaml'):




