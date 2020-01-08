# %%
from utility.SUEWS_param import getting_SUEWS_params
from utility.change_to_suews import change_input_to_SUEWS
from utility.g1_g6 import g1_g6
from utility.timezone_collector import set_timezone
from utility.modify_London import modify_all_London
import os
import shutil
import json
################################################
steps = {'clean_dirs': 1,
         'extract_params': 0,
         'modify_trans': 1,
         'change_to_SUEWS': 1,
         'modify_London': 1,
         'g1_g6': 1,
         'timezone': 0
         }
################################################
citynames = ['London', 'Swindon']
values_trans = [0.2039, 0.2105]
first_day_str = '2012-04-10'
finalize = 0
################################################
if steps['clean_dirs'] == 1:
    dir_paths = ['1-changed_to_SUEWS/',
                 '2-g1_g6_changed/',
                 '3-timezone_changed/',
                 'final/'
                 ]
    for dir_path in dir_paths:
        print('cleaning output/'+dir_path+' . . .')
        src_files = os.listdir('output/'+dir_path)
        for file_name in src_files:
            os.remove('output/'+dir_path+file_name)
################################################
if steps['extract_params'] == 1:
    print('Extracting SUEWS parameters . . . ')
    for cityname in citynames:
        print('preparing for '+cityname+' . . .')
        getting_SUEWS_params(cityname, first_day_str)
################################################
if steps['modify_trans'] == 1:
    print('Modifying taransmisivity . . .')
    for cityname, value in zip(citynames, values_trans):
        with open('output/SUEWS_param_'+'Swindon'+'.json') as var_json:
            vars_to_add = json.load(var_json)
            vars_to_add['transdiff_SUEWS']['value'] = [value]

        new_json = 'output/SUEWS_param_'+cityname+'.json'
        print('Modifying '+new_json+' . . .')
        with open(new_json, 'w') as fp:
            json.dump(vars_to_add, fp, indent=4)
################################################
if steps['change_to_SUEWS'] == 1:
    print('Adding SUEWS inputs to WRF inputs . . .')
    change_input_to_SUEWS()
################################################
# all specific modification should be done here!!!
if steps['modify_London'] == 1:
    print('Modifying London domain . . .')
    modify_all_London()
    os.remove('output/1-changed_to_SUEWS/wrfinput_d03.suews')
    name1 = 'output/1-changed_to_SUEWS/wrfinput_d03.suews.new'
    name2 = 'output/1-changed_to_SUEWS/wrfinput_d03.suews'
    os.rename(name1, name2)
################################################
if steps['g1_g6'] == 1:
    print('Modifying g1-g6 . . .')
    g1_g6(first_day_str)
    out = 'output/2-g1_g6_changed/'
    finalize = 1
################################################
if steps['timezone'] == 1:
    print('Changing timezone values . . . ')
    set_timezone()
    out = 'output/3-timzone_changed'
    finalize = 1
################################################
if finalize == 1:
    src_files = os.listdir(out)
    for file_name in src_files:
        full_file_name = os.path.join(out, file_name)
        if os.path.isfile(full_file_name):
            print('copying '+full_file_name+' to '+'output/final')
            shutil.copy(full_file_name, 'output/final')
