# %%
#import yaml
#yaml.warnings({'YAMLLoadWarning': False})
import json
from pathlib import Path
import supy as sp
import numpy as np
import f90nml

path_runcontrol = Path('sample_run') / 'RunControl.nml'
df_state_init = sp.init_supy(path_runcontrol)

##################### JSON ######################################

with open('../SUEWS_param.json') as suews_file:
    suews_params = json.load(suews_file)


def find_insert_value_json():

    for key in suews_params.keys():

        param = key.split('_SUEWS')[0].lower()

        df_columns = df_state_init.columns

        if (param in df_columns):

            suews_params[key]['value'] = list(df_state_init[param].iloc[0])

        elif (param+'_id' in df_columns):

            suews_params[key]['value'] = list(
                df_state_init[param+'_id'].iloc[0])

        else:
            print('Parameter '+'"'+param+'"'+' was not found in df_state')


find_insert_value_json()

new_json = 'SUEWS_param_new.json'
print('creating '+new_json)
with open(new_json, 'w') as fp:
    json.dump(suews_params, fp, indent=4)


##################### NAMELIST ######################################

nml = f90nml.read('../namelist.suews')
df_columns = df_state_init.columns


profiles = {'ahprof_24hr', 'humactivity_24hr', 'popprof_24hr',
            'traffprof_24hr', 'wuprofa_24hr', 'wuprofm_24hr', 'laipower'}

coefs = {'ohm_coef', 'waterdist'}

patch_change = {'coeff', 'method'}

for key in patch_change:

    for item in nml[key].items():

        value = df_state_init[item[0]].iloc[0]

        if item[0] in coefs:

            dim_str = df_state_init[item[0]].columns[-1][1:-1]
            dims = tuple(int(s)+1 for s in dim_str.split(', ') if s.isdigit())
            order='C'


            new_value = [l.tolist() for l in
                        list(np.reshape(list(value), dims, order=order))]

        elif item[0] in profiles:

            dim_str = df_state_init[item[0]].columns[-1][1:-1]
            dims = tuple(int(s)+1 for s in dim_str.split(', ')
                        if s.isdigit())[::-1]
            new_value = [l.tolist() for l in
                        list(np.reshape(list(value), dims, order='F'))]

        else:

            new_value = list(value)

        nml[key][item[0]] = new_value

new_nml = 'namelist.suews.new'
print('creaitng '+new_nml)
nml.write(new_nml, force=True)
