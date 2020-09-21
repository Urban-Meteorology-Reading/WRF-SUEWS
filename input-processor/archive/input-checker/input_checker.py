# %%
#import yaml
#yaml.warnings({'YAMLLoadWarning': False})
import xarray as xr
import numpy as np
import pandas as pd
import json
import glob
import os

# %%
# the check list file with ranges and logics
check_file = 'check_file_suews.json'

# opening the check list file


def open_check_file(check_file):
    
    with open(check_file) as cf:
        
        cr = json.load(cf)
        
    # making the keys upper case to be consistent with wrfinputs
    cr_temp = {}
    for key in cr.keys():
        # for some reason pop() did not work here!!
        cr_temp[key.upper()] = cr[key]

    return cr_temp

# checking the range of each parameter


def check_range(var, values, cr):

    min_v = cr[var]['param']['min']
    max_v = cr[var]['param']['max']
    description = ' should be between '+str(min_v)+' and '+str(max_v)
    is_accepted_flag = False

    for value in np.nditer(values):
        if min_v <= value <= max_v:
            is_accepted_flag = True

    if(not is_accepted_flag):
        is_accepted = 'No'
        suggestion = 'change the parameter to fall into the acceptable range'
    else:
        is_accepted = 'Yes'
        suggestion = ''

    return [var, is_accepted, description, suggestion]


def check_zd_zh(var, values, cr):

    return 0

# checks for suews parameters


def check_var_suews(var, values, cr, df_sum):

    logic = cr[var]['logic']

    if logic == 'range':
        out_list = check_range(var, values, cr)
    elif logic == 'zd-zh':
        out_list = check_zd_zh(var, values, cr)

    df_sum.loc[len(df_sum)] = out_list

    return df_sum


cr = open_check_file(check_file)
# getting all the wrfinput files
fl_wrfinput = glob.glob('wrfinputs/wrfinput_d0?')

for wrfinput in fl_wrfinput:
    df_sum = pd.DataFrame(
        columns=['Parameter', 'Is acceptable?', 'Description', 'Suggestion'])

    print('Working on '+wrfinput)
    print('==========================================================================')
    print('==================== Check Table for ' +
        wrfinput+' ========================')

    ds = xr.open_dataset(wrfinput)

    for var in ds.data_vars: 
        if 'SUEWS' in var:
            df_sum = check_var_suews(var, ds[var].values, cr, df_sum)
    print(df_sum)
