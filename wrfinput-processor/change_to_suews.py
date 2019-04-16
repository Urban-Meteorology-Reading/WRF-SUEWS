#!/usr/bin/env python
import xarray as xr
import numpy as np
import glob
import os
os.chdir('mod-input-python')

# load base nc file for modification
ds_base = xr.open_dataset('wrfinput_d01')


# funcitons to add new variables with expanded dimensions
def gen_var_expand(name, rules, var_base=ds_base['T2'].copy(deep=True)):
    var_new = var_base.rename(name.upper())
    var_attrs = var_base.attrs
    (name_dim, axis, rep) = rules[0]
    val_init = rules[1]
    var_new = np.repeat(
        var_new.expand_dims(
            name_dim, axis=axis), rep, axis=axis)

    var_new.attrs = var_attrs
    var_new.attrs['MemoryOrder'] = 'XYZ'
    var_new.attrs[u'description'] = name
    var_new.attrs['stagger'] = 'Z'
    var_new.attrs['units'] = rules[2]
    # set default values
    var_new.values = np.ones_like(var_new.values) * val_init

    return var_new


# funcitons to add new variables with the same dimensionality
def gen_var_keep(name, rules, var_base=ds_base['T2'].copy(deep=True)):
    var_new = var_base.rename(name.upper())
    var_new.attrs[u'description'] = name
    var_new.attrs['units'] = rules[2]
    val_init = rules[1]
    # set default values
    var_new.values = np.ones_like(var_new.values) * val_init

    return var_new


# A generic wrapper to generate a variable for SUEWS
def gen_var(name, rules, var_base=ds_base['T2'].copy()):
    if type(rules[0]) is tuple:
        var_new = gen_var_expand(name, rules, var_base)
    else:
        var_new = gen_var_keep(name, rules, var_base)

    return var_new


#  rules to create input variable needed by SUEWS
# TODO: add default values
# rule pattern:
# var_name: [('new_dim', new_pos_axis, number_in_axis),value,units] if the first one is tuple
# then it gives information for furthur dimensions
dict_rules_suews = {
    'LAI_SUEWS': [('lai', 1, 3), 2,''],
    'albDecTr_SUEWS': [0, 0.1,''],
    'albEveTr_SUEWS': [0, 0.1,''],
    'albGrass_SUEWS': [0, 0.1,''],
    'NumCapita': [0, 54,'ha-1'],

    'SoilStoreCap': [('nsurf', 1, 7), [150., 150., 150., 150., 150., 150., 0.], 'mm'],
    'SoilDepth': [0, 350, 'mm'],
    'SatHydraulicConduct': [0, 5E-4, 'mm s-1'],
    'AlbMin_DecTr': [0, 0.12,''],
    'AlbMax_DecTr': [0, 0.18,''],
    'AlbMin_EveTr': [0, 0.11,''],
    'AlbMax_EveTr': [0, 0.12,''],
    'AlbMin_Grass': [0, 0.18,''],
    'AlbMax_Grass': [0, 0.21,''],
    'CapMin_dec': [0, 0.3,'mm'],
    'CapMax_dec': [0, 0.8,'mm'],
    'PorMin_dec': [0, 0.2,''],
    'PorMax_dec': [0, 0.6,''],
    'DRAINRT': [0, 0.25,'mm h-1'],
    'RAINCOVER': [0, 1,''],
    'RAINMAXRES': [0, 10,'mm'],
    'FlowChange': [0, 0, ''],
    'PipeCapacity': [0, 100,''],
    'RunoffToWater': [0, 0.1, ''],
    'StateLimit': [0, 54,''],
    'WetThresh': [('nsurf', 1, 7), 54, 'mm'],
    'BaseTHDD': [('nsurf', 1, 7), 54, 'mm'],



    'PopDensDaytime': [0, 54, 'ha-1'],
    'PopDensNighttime': [0, 54, 'ha-1'],
    'DecidCap_SUEWS': [0, 10,''],
    'porosity_SUEWS': [0, 0.1,''],
    'GDD_SUEWS': [('gdd', 1, 5), 0,''],
    'HDD_SUEWS': [('hdd', 1, 12), 0,''],
    'state_SUEWS': [('nsurf', 1, 7), 1,''],
    'soilmoist_SUEWS': [('nsurf', 1, 7), 150,''],
    'surf_var_SUEWS': [('nsurf', 1, 7), 10,''],
    'qn1_av_SUEWS': [0, 0,''],
    'qn1_s_SUEWS': [0, 0,''],
    'dqndt_SUEWS': [0, 0,''],
    'dqnsdt_SUEWS': [0, 0,''],
    'MeltWaterStore': [('nsurf', 1, 7), 10,''],
    'SnowAlb': [0, 0.1,''],
    'WUDay': [('wu', 1, 9), 10,''],
    'z0m_in': [0, .1,''],
    'zdm_in': [0, .1,'']}


# added SUEWS required input to a single wrfinput file
def add_SUEWS_wrfinput_single(x_file):
    print 'working on:', x_file
    # get base file
    ds_base = xr.open_dataset(x_file)
    # NB: variables in wrfinput have to be named in CAPITALISED strings
    ds_new = xr.Dataset({
        name.upper(): gen_var(name, rules, ds_base['T2'].copy(deep=True))
        for name, rules in dict_rules_suews.items()})

    # merge with ds_base for export
    ds_merged = ds_base.update(ds_new)

    # chnage state_SUEWS of water surface to a larger value
    ds_merged['state_SUEWS'.upper()][:, -1] = 150

    # amke sure SUEWS scheme option is set
    ds_merged.attrs['SF_SURFACE_PHYSICS'] = 9

    # delete 'coordinates' attribute as xarray is unhappy with it
    # print('Removing attr coordinates from:')
    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            # print(var)
            del ds_merged[var].attrs['coordinates']
    # ds_merged['LAI_SUEWS']
    # export merged dataset to a new file
    file_out = x_file+'.suews'
    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
    print 'SUEWS input has beened added to:', file_out

    return file_out


# modify all files in a loop:
def add_SUEWS_wrfinput(wrfinput_list):
    fl_out_list = [
        add_SUEWS_wrfinput_single(
            x_file) for x_file in wrfinput_list]
    return fl_out_list


# get file list to modify
fl_wrfinput_base = glob.glob('wrfinput_d0?')

# modify them and save with .suews as suffix
add_SUEWS_wrfinput(fl_wrfinput_base)
