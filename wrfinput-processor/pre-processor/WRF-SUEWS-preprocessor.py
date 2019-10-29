#%%
from utility.SUEWS_param import getting_SUEWS_params
from utility.change_to_suews import change_input_to_SUEWS
from utility.g1_g6 import g1_g6
from utility.timezone_collector import set_timezone
import os
import shutil
#%%
steps = {'extract_params': 0,
        'change_to_SUEWS': 0,
        'g1_g6': 1,
        'timezone': 0
        }
################################################
citynames = ['London', 'Swindon']
first_day_str = '2012-04-10'
################################################
if steps['extract_params'] == 1:
    print('Extracting SUEWS parameters . . . ')
    for cityname in citynames:
        print('preparing for '+cityname+' . . .')
        getting_SUEWS_params(cityname, first_day_str)
    
################################################
if steps['change_to_SUEWS'] == 1:
    print('Adding SUEWS inputs to WRF inputs . . .')
    change_input_to_SUEWS()


#### all specific modification should be done here!!!
    
################################################
finalize=0
if steps['g1_g6'] == 1:
    print('Modifying g1-g6 . . .')
    g1_g6()
    out='output/2-g1_g6_changed/'
    finalize=1
################################################
if steps['timezone'] == 1:
    print('Changing timezone values . . . ')
    set_timezone()
    out='output/3-timzone_changed'
    finalize=1

#%%

if finalize==1:
    src_files = os.listdir(out)
    for file_name in src_files:
        full_file_name = os.path.join(out, file_name)
        if os.path.isfile(full_file_name):
            print('copying '+full_file_name+' to '+'output/final')
            shutil.copy(full_file_name, 'output/final')

# %%
