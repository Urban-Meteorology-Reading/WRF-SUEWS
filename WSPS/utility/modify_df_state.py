import pandas as pd
import numpy as np

def generate_array_dif(attrs_site, attr,level):
    dar = [0.75,0.5,.25]
    dar[level] = attrs_site[attr].values[0]
    return dar


def generate_array_same(attrs_site, attr):
    a = attrs_site[attr].values[0]
    return [a, a, a]


def modify_attr(df_state_init,df_phenol,veg_type):
    attrs_site = df_phenol[df_phenol.lc == veg_type]
    df_state_init.loc[:, 'popdensdaytime']=0
    df_state_init.loc[:, 'popdensnighttime']=0
    df_state_init.loc[:, 'numcapita']=0


    if attrs_site.lc.values[0] == 'DecTr':
        ar = [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]
        level = 1
        df_state_init.albmin_dectr=attrs_site['minalb'].values[0]
        df_state_init.albmax_dectr=attrs_site['maxalb'].values[0]
        df_state_init.albdectr_id=df_state_init.albmin_dectr

    elif attrs_site.lc.values[0] == 'EveTr':
        ar = [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
        level = 0
        df_state_init.albmin_evetr=attrs_site['minalb'].values[0]
        df_state_init.albmax_evetr=attrs_site['maxalb'].values[0]
        df_state_init.albevetr_id=df_state_init.albmin_evetr

    elif attrs_site.lc.values[0] == 'Grass':
        ar = [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]
        level = 2
        df_state_init.albmin_grass=attrs_site['minalb'].values[0]
        df_state_init.albmax_grass=attrs_site['maxalb'].values[0]
        df_state_init.albgrass_id=df_state_init.albmin_grass

    else:
        print('The land cover type is not found! using the default one')

    df_state_init.loc[:, 'sfr'] = ar
    df_state_init.loc[:, 'laimin'] = generate_array_dif(attrs_site, 'minlai',level)
    df_state_init.loc[:, 'laimax'] = generate_array_dif(attrs_site, 'maxlai',level)
    df_state_init.loc[:, 'gddfull'] = generate_array_same(attrs_site, 'gddfull')
    df_state_init.loc[:, 'sddfull'] = generate_array_same(attrs_site, 'sddfull')
    df_state_init.loc[:, 'basete'] = generate_array_same(attrs_site, 'basete')
    df_state_init.loc[:, 'baset'] = generate_array_same(attrs_site, 'baset')
    df_state_init.lai_id = df_state_init.loc[:, 'laimin']

    df_state_init.maxconductance=attrs_site['maxg'].values[0]
    df_state_init.g1=attrs_site['g1'].values[0]
    df_state_init.g2=attrs_site['g2'].values[0]
    df_state_init.g3=attrs_site['g3'].values[0]
    df_state_init.g4=attrs_site['g4'].values[0]
    df_state_init.g5=attrs_site['g5'].values[0]
    df_state_init.g6=attrs_site['g6'].values[0]

    return df_state_init