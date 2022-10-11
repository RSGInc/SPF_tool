#######################################################
### Script for converting codebook wide to longform
### Author: Nicholas Fournier nick.fournier@rsginc.com, Oct, 2020
#######################################################


import pandas as pd

df = pd.read_csv(
    'C:/Users/nick.fournier/'
    'OneDrive - Resource Systems Group, '
    'Inc/Desktop/RP_Questionnaire.csv'
    )

def varvals_to_df(vals):
    vals = vals[1].dropna()
    vals, codevals = vals[:4], vals[4:]
    if not codevals.empty:
        vals = pd.concat([vals]*len(codevals), axis=1).T
        vals['Values'] = codevals.str.slice(2).values
        vals['Labels'] = codevals.str.slice(0, 1).values
    else:
        vals = pd.DataFrame(vals).T
        vals['Values'] = None
        vals['Labels'] = None        

    return(vals.reset_index(drop=True))


varvals = [varvals_to_df(varvals) for varvals in df.iterrows()]
varvals_df = pd.concat(varvals, axis=0).reset_index(drop=True)

varvals_df.to_csv(    'C:/Users/nick.fournier/'
    'OneDrive - Resource Systems Group, '
    'Inc/Desktop/RP_Questionnaire_codevalues.csv', index=False)
