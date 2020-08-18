# %%
from utility.SUEWS_param import getting_SUEWS_params
from utility.change_to_suews import change_input_to_SUEWS
from utility.parameters import parameters
from utility.timezone_collector import set_timezone
from utility.modify_London import modify_all_London
import os
import shutil
import json
################################################
# Dashboard for controling the steps: 0=no 1=yes
steps = {'clean_dirs': 1,
         'extract_params_cities': 1,
         'extract_params_vegs': 1,
         'extract_params_extra_lands': 1,
         'modify_trans': 1,
         'change_to_SUEWS': 1,
         'modify_London': 1,
         'parameters': 1,
         'timezone': 0
         }
################################################
citynames = ['London', 'Swindon']

# #values_trans = [0.2039, 0.2105] # April
# values_trans = [0.01641, 0.01756] # April
# first_day_str = '2012-04-10' # April

# #values_trans = [0.186, 0.0712] # January
# values_trans = [0.1257, 0.1216] # January
# first_day_str = '2012-01-10' # January

# #values_trans = [0.0781, 0.0294] # July
# values_trans = [0.0528, 0.0097] # July
# first_day_str = '2012-07-15' # July

# #values_trans = [0.1485, 0.0876] # October
values_trans = [0.2153, 0.01400] # October
first_day_str = '2012-10-1' # October
################################################
finalize = 0
if steps['clean_dirs'] == 1:
    dir_paths = ['1-changed_to_SUEWS/',
                 '2-parameters_changed/',
                 '3-timezone_changed/',
                 'final/'
                 ]
    for dir_path in dir_paths:
        print('cleaning output/'+dir_path+' . . .')
        src_files = os.listdir('output/'+dir_path)
        for file_name in src_files:
            os.remove('output/'+dir_path+file_name)
################################################
if steps['extract_params_cities'] == 1:
    print('Extracting SUEWS parameters for cities . . . ')
    for cityname in citynames:
        print('preparing for '+cityname+' . . .')
        getting_SUEWS_params(cityname, first_day_str)
################################################
if steps['extract_params_vegs'] == 1:
    print('Extracting SUEWS parameters for vegetations . . . ')
    for veg_type in ['EveTr','DecTr','Grass']:
        print('preparing for '+veg_type+' . . .')
        getting_SUEWS_params(citynames[0],first_day_str,
                            veg_spin=1,veg_type=veg_type)
################################################
if steps['extract_params_extra_lands'] == 1:
    print('Extracting SUEWS parameters for 4 extera lands . . . ')
    for veg_type in ['G1','G2','G3','G4']:
        print('preparing for '+veg_type+' . . .')
        getting_SUEWS_params(citynames[1],first_day_str,
                            veg_spin=1,veg_type=veg_type)
################################################
if steps['modify_trans'] == 1:
    print('Modifying taransmisivity . . .')
    for cityname, value in zip(citynames, values_trans):
        with open('output/SUEWS_param_'+cityname+'.json') as var_json:
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
if steps['parameters'] == 1:
    print('Modifying parameters . . .')
    parameters(first_day_str)
    out = 'output/2-parameters_changed/'
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
