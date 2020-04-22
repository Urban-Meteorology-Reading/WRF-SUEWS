#%%
import pandas as pd
from datetime import datetime
import numpy as np
import supy as sp
from pathlib import Path
#%%
def spin_lai_albedo(maxalb,minalb,maxlai,minlai,baset,basete,first_day_str):
    path_runcontrol = Path('runs/run_'+'Swindon') / 'RunControl.nml'
    df_state_init = sp.init_supy(path_runcontrol)
    grid = df_state_init.index[0]
    df_forcing = sp.load_forcing_grid(path_runcontrol, grid)
    df_state_init.loc[:,'albmax_evetr']=maxalb[0]
    df_state_init.loc[:,'albmax_dectr']=maxalb[1]
    df_state_init.loc[:,'albmax_grass']=maxalb[2]


    df_state_init.loc[:,'albmin_evetr']=minalb[0]
    df_state_init.loc[:,'albmin_dectr']=minalb[1]
    df_state_init.loc[:,'albmin_grass']=minalb[2]
    df_state_init.loc[:,'laimax']=maxlai
    df_state_init.loc[:,'laimin']=minlai
    df_state_init.loc[:,'baset']=baset
    df_state_init.loc[:,'basete']=basete

    first_day = datetime.strptime(first_day_str, '%Y-%m-%d')

    print('Rotating the time based on the first day of '+first_day_str)
    first_part = df_forcing.loc[df_forcing[df_forcing.index >= first_day].index]
    second_part = df_forcing.loc[df_forcing[
        df_forcing.index < first_day-pd.Timedelta('1 days')
        ].index]
    second_part.index = second_part.index+pd.Timedelta('366 days')

    df_forcing_2 = first_part.append(second_part)
    df_forcing_2.iy = df_forcing_2.index.year
    df_forcing_2.index.freq = first_part.index.freq
    round_number=0
    print('getting initial lai and albedo for changed grids . . .')
    while (round_number <=4):
        round_number = round_number+1
        print('Running SUEWS for round number '+str(round_number)+'.....')
        df_output, df_state_final = sp.run_supy(
                df_forcing_2, df_state_init,
                save_state=False)
        final_state = df_state_final[df_state_init.columns.levels[0]
                                    ].iloc[1]
        df_state_init.iloc[0] = final_state

    # %%
    #df_output.DailyState.dropna()['LAI_Grass'].plot()
    #df_output.DailyState.dropna().filter(like='AlbGrass').plot()
    alb_init=df_state_final.loc[:,'alb'].iloc[1][2:5].values
    lai_init=df_state_final.loc[:,'lai_id'].iloc[1].values
    return alb_init,lai_init


# %%
