#!/usr/bin/env python
import xarray as xr
import numpy as np
import glob


# load base nc file for modification
ds_base = xr.open_dataset('wrfinput_d01.base.30d.nc')


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
    # set default values
    var_new.values = np.ones_like(var_new.values) * val_init

    return var_new


# funcitons to add new variables with the same dimensionality
def gen_var_keep(name, rules, var_base=ds_base['T2'].copy(deep=True)):
    var_new = var_base.rename(name.upper())
    var_new.attrs[u'description'] = name
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


# rules to create input variable needed by SUEWS
# TODO: add default values
# rule pattern:
# var_name: ('new_dim', new_pos_axis, number_in_axis)
dict_rules_suews = {
    'LAI_SUEWS': [('lai', 1, 3), 2],
    'albDecTr_SUEWS': [0, 0.1],
    'albEveTr_SUEWS': [0, 0.1],
    'albGrass_SUEWS': [0, 0.1],
    'DecidCap_SUEWS': [0, 10],
    'porosity_SUEWS': [0, 0.1],
    'GDD_SUEWS': [('gdd', 1, 5), 0],
    'HDD_SUEWS': [('hdd', 1, 12), 0],
    'state_SUEWS': [('nsurf', 1, 7), 1],
    'soilmoist_SUEWS': [('nsurf', 1, 7), 150],
    'surf_var_SUEWS': [('nsurf', 1, 7), 10],
    'qn1_av_SUEWS': [0, 0],
    'qn1_s_SUEWS': [0, 0],
    'dqndt_SUEWS': [0, 0],
    'dqnsdt_SUEWS': [0, 0],
    'MeltWaterStore': [('nsurf', 1, 7), 10],
    'SnowAlb': [0, 0.1],
    'WUDay': [('wu', 1, 9), 10],
    'z0m_in': [0, .1],
    'zdm_in': [0, .1]}


fl_wrfinput_base = glob.glob('wrfinput*base*')


# modify all files in a loop:
for x_file in fl_wrfinput_base:
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
    print('Removing attr coordinates from:')
    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            print(var)
            del ds_merged[var].attrs['coordinates']
    # ds_merged['LAI_SUEWS']
    # export merged dataset to a new file
    file_out = x_file.replace('.base.', '.new.')
    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
