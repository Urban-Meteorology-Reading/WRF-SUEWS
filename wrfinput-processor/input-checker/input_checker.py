#%%
import xarray as xr
import numpy as np
import json
import glob
import os

#%%
check_file='check_file_suews.json' # the check list file with ranges and logics

# opening the check list file
def open_check_file(check_file):

    with open(check_file) as cf:
        cr=json.load(cf)
    #making the keys upper case to be consistent with wrfinputs
    cr_temp={}
    for key in cr.keys():
        cr_temp[key.upper()]=cr[key] #for some reason pop() did not work here!!   
    
    return cr_temp

# checking the range of each parameter
def check_range(var,values,cr):

    min_v=cr[var]['min']
    max_v=cr[var]['max']
    is_accepted_range=False
    for value in np.nditer(values):
        if min_v <= value <= max_v:
            is_accepted_range=True

    return is_accepted_range,min_v,max_v

# main checks for suews parameters. It consists of:
# 1- checking the range
# 2- checking the logic !!TO BE DONE
def check_var_suews(var,values,cr):

    is_accepted_range,min_v,max_v=check_range(var,values,cr)
    if(not is_accepted_range):
        print('The range for '+var+' is NOT acceptable.')
        print(var+ ' should between '+str(min_v)+' and '+str(max_v))
        print('\n')
    else:
        print('The range for '+var+' is acceptable.\n')

    # the following should be developed to check the logic other than min-max
    #check_logic(var,values)

cr=open_check_file(check_file)
# getting all the wrfinput files
fl_wrfinput = glob.glob('wrfinputs/wrfinput_d0?')

for wrfinput in fl_wrfinput:
    print('Working on '+wrfinput)
    print('============================================')
    print('============================================')
    ds = xr.open_dataset(wrfinput)
    for var in ds.data_vars:
        if 'SUEWS' in var:
            check_var_suews(var,ds[var].values,cr) 