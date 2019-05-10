# %%
import yaml
yaml.warnings({'YAMLLoadWarning': False})
import json
from pathlib import Path
import supy as sp
import numpy as np
import f90nml
from datetime import datetime
import pandas as pd

def spinup_SUEWS():

    print('Initializing SUEWS variables.....')
    df_state_init, df_forcing = sp.load_SampleData()

    path_runcontrol = Path('./sample_run_mod') / 'RunControl.nml'
    df_state_init = sp.init_supy(path_runcontrol)

    first_day_str='2012-4-10'
    first_day=datetime.strptime(first_day_str, '%Y-%m-%d')

    print('Rotating the time based on the first day of '+first_day_str)
    first_part=df_forcing.loc[df_forcing[df_forcing.index>=first_day].index]
    second_part=df_forcing.loc[df_forcing[
        df_forcing.index<first_day-pd.Timedelta('1 days')
        ].index]
    second_part.index=second_part.index+pd.Timedelta('366 days')

    df_forcing_2=first_part.append(second_part)
    df_forcing_2.iy=df_forcing_2.index.year
    df_forcing_2.index.freq=first_part.index.freq

    round_number=0
    error=1000
    while (error >= 0.5):
        round_number=round_number+1
        print('Running SUEWS for round number '+str(round_number)+'.....')
        df_output, df_state_final = sp.run_supy(
                                    df_forcing_2, df_state_init,
                                    save_state=False)
        final_state=df_state_final[ df_state_init.columns.levels[0]
                                ].iloc[1]
        df_state_init.iloc[0]=final_state
        soilstore_before=df_state_final.soilstore_id.iloc[0]
        soilstore_after =df_state_final.soilstore_id.iloc[1]
        diff_soil=sum(abs(soilstore_after-soilstore_before))
        error=100*diff_soil/soilstore_before.mean()
        print('Change in soil store in % = '+str(error))

    print('SUEWS spinup is finished. Total number of runs = '+ str(round_number))
    
    return df_state_init


df_state_init = spinup_SUEWS()

print('Putting NetRadiationMethod = 1')
df_state_init.netradiationmethod=1
df_state_init.rename(columns={'soilstore_id':'soilmoist'},inplace=True)
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
