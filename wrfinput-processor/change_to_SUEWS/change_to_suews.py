#!/usr/bin/env python
#import yaml
#yaml.warnings({'YAMLLoadWarning': False})
import xarray as xr
import numpy as np
import json
import glob
import os
os.chdir('mod-input-python')

# load base nc file for modification
ds_base = xr.open_dataset('wrfinput_d01')


# funcitons to add new variables with expanded dimensions


def gen_var_expand(var_key, vars_to_add, var_base=ds_base['T2'].copy(deep=True)):
    var_new = var_base.rename(var_key.upper())
    var_attrs = var_base.attrs
    (name_dim, axis, dim) = (vars_to_add[var_key]['dim_name'],
                            vars_to_add[var_key]['axis'],
                            vars_to_add[var_key]['dim'])
    val_init = vars_to_add[var_key]['value']
    var_new = np.repeat(
        var_new.expand_dims(
            name_dim, axis=axis), dim, axis=axis)

    var_new.attrs = var_attrs
    var_new.attrs['MemoryOrder'] = 'XYZ'
    var_new.attrs[u'description'] = var_key
    var_new.attrs['stagger'] = 'Z'
    var_new.attrs['unit'] = vars_to_add[var_key]['unit']
    # set default values
    for i in range(0, dim):
        var_new.values[:, i, :, :] = val_init[i]
    return var_new


# funcitons to add new variables with the same dimensionality
def gen_var_keep(var_key, vars_to_add, var_base=ds_base['T2'].copy(deep=True)):
    var_new = var_base.rename(var_key.upper())
    var_new.attrs[u'description'] = var_key
    var_new.attrs['unit'] = vars_to_add[var_key]['unit']
    val_init = vars_to_add[var_key]['value']
    # set default values
    var_new.values = np.ones_like(var_new.values) * val_init

    return var_new


# A generic wrapper to generate a variable for SUEWS
def gen_var(var_key, vars_to_add, landuse_mask, var_base=ds_base['T2'].copy()):
    if (vars_to_add[var_key]['dim'] != 0):
        var_new = gen_var_expand(var_key, vars_to_add, var_base)
        landuse_mask_n = np.repeat(
            landuse_mask[None, ...], var_new.values.shape[1], axis=0)
        landuse_mask_n = np.repeat(
            landuse_mask_n[None, ...], var_new.values.shape[0], axis=0)
    else:
        var_new = gen_var_keep(var_key, vars_to_add, var_base)
        landuse_mask_n = np.repeat(
            landuse_mask[None, ...], var_new.values.shape[0], axis=0)

    if (vars_to_add[var_key]['mask'] == 'urban'):
        var_new.values = np.multiply(var_new.values, landuse_mask_n)
    return var_new


# variables to be added to wrfinputs
with open('../../SUEWS_param.json') as var_json:
    vars_to_add = json.load(var_json)


# finding urban masks

def urban_mask(ds_base):
    urban_category = 12
    landusef = ds_base['LANDUSEF'].values[0, urban_category, :, :]
    landuse_mask = landusef
    landuse_mask[landusef > 0] = 1
    return landuse_mask


# added SUEWS required input to a single wrfinput file

def add_SUEWS_wrfinput_single(x_file):
    print('working on:' + x_file)
    # get base file
    ds_base = xr.open_dataset(x_file)
    landuse_mask = urban_mask(ds_base)
    # NB: variables in wrfinput have to be named in CAPITALISED strings
    ds_new = xr.Dataset({
        var_key.upper(): gen_var(var_key, vars_to_add, landuse_mask, ds_base['T2'].copy(deep=True))
        for var_key in vars_to_add.keys()})

    # merge with ds_base for export
    ds_merged = ds_base.update(ds_new)

    # amke sure SUEWS scheme option is set
    ds_merged.attrs['SF_SURFACE_PHYSICS'] = 9

    # delete 'coordinates' attribute as xarray is unhappy with it

    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            del ds_merged[var].attrs['coordinates']

    # export merged dataset to a new file
    file_out = x_file+'.suews'
    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
    print('SUEWS input has beened added to:' + file_out)

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