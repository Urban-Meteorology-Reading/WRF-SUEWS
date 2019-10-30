import glob
import geopandas as gpd
from pyproj import Proj, transform
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import xarray as xr
from scipy.interpolate import griddata
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.colors as colors
from .spin_up import spin_lai_albedo

def mod_gs(ds_base, g1, g2, g3, g4, g5, g6, i, j):

    ds_base['G1'].values[0, i, j] = g1
    ds_base['G2'].values[0, i, j] = g2
    ds_base['G3'].values[0, i, j] = g3
    ds_base['G4'].values[0, i, j] = g4
    ds_base['G5'].values[0, i, j] = g5
    ds_base['G6'].values[0, i, j] = g6

    return ds_base


def mod_lai_albedo(ds_base, maxlai, minlai, maxg, maxalb, minalb, alb_init, lai_init, i, j):

    ds_base['MaxConductance_SUEWS'.upper()].values[0, :, i, j] = maxg
    ds_base['LaiMax_SUEWS'.upper()].values[0, :, i, j] = maxlai
    ds_base['LaiMin_SUEWS'.upper()].values[0, :, i, j] = minlai
    ds_base['Lai_SUEWS'.upper()].values[0, :, i, j] = lai_init

    ds_base['GDDFULL_SUEWS'.upper()].values[0, :, i, j] = [1000, 1000, 1000]
    ds_base['SDDFULL_SUEWS'.upper()].values[0, :, i, j] = [-1000, -1000, -1000]

    ds_base['albMax_EveTr_SUEWS'.upper()].values[0, i, j] = maxalb[0]
    ds_base['albMax_DecTr_SUEWS'.upper()].values[0, i, j] = maxalb[1]
    ds_base['albMax_Grass_SUEWS'.upper()].values[0, i, j] = maxalb[2]

    ds_base['albMin_EveTr_SUEWS'.upper()].values[0, i, j] = minalb[0]
    ds_base['albMin_DecTr_SUEWS'.upper()].values[0, i, j] = minalb[1]
    ds_base['albMin_Grass_SUEWS'.upper()].values[0, i, j] = minalb[2]

    ds_base['albEveTr_SUEWS'.upper()].values[0, i, j] = alb_init[0]
    ds_base['albDecTr_SUEWS'.upper()].values[0, i, j] = alb_init[1]
    ds_base['albGrass_SUEWS'.upper()].values[0, i, j] = alb_init[2]

    return ds_base


def g1_g6(first_day_str):
    x_files = glob.glob('output/1-changed_to_SUEWS/wrfinput_d0*')

    maxlai = [2.3, 4, 1.3]
    minlai = [0.8, 0.5, 0.15]
    maxg = [21.83, 33.52, 13.86]
    maxalb = [0.1, 0.14, 0.2]
    minalb = [0.07, 0.1, 0.16]
    alb_init, lai_init = spin_lai_albedo(maxalb, minalb,
                                         maxlai, minlai,
                                         first_day_str)
    g1 = [1, 1, 1]
    g2 = [104.383, 104.823, 104.47]
    g3 = [0.509, 0.529, 0.789]
    g4 = [0.772, 0.592, 0.612]
    g5 = [36.277, 36.3, 37.243]
    g6 = [0.023, 0.03, 0.025]


    for x_file in x_files:
        print('working on '+x_file)
        ds_base = xr.open_dataset(x_file)
        ds_base_orig = ds_base.copy(deep=True)
        a = {}
        a['urban'] = 0
        a['dectr'] = 0
        a['evetr'] = 0
        a['grass'] = 0
        a['bsoil'] = 0
        a['water'] = 0

        b = ds_base['LANDUSEF'].copy(deep=True)
        a['urban'] = b.values[0, 12, :, :]
        for i in [2, 3]:
            a['dectr'] = a['dectr']+b.values[0, i, :, :]
        for i in [0, 1, 4]:
            a['evetr'] = a['evetr']+b.values[0, i, :, :]
        for i in [5, 6, 7, 8, 9, 11, 13]:
            a['grass'] = a['grass']+b.values[0, i, :, :]
        for i in [15, 17, 18, 19]:
            a['bsoil'] = a['bsoil']+b.values[0, i, :, :]
        for i in [10, 16]:
            a['water'] = a['water']+b.values[0, i, :, :]

        for i in range(a['urban'].shape[0]):
            for j in range(a['urban'].shape[1]):

                mx_fr = np.max([a['urban'][i, j], a['evetr'][i, j], a['dectr']
                                [i, j], a['grass'][i, j], a['bsoil'][i, j], a['water'][i, j]])

                if (mx_fr == a['urban'][i, j]) and (a['urban'][i, j] > 0.50):
                    pass

                elif (mx_fr == a['urban'][i, j]) and (a['urban'][i, j] <= 0.50):
                    pass

                elif mx_fr == a['evetr'][i, j]:
                    ds_base = mod_gs(
                        ds_base, g1[0], g2[0], g3[0], g4[0], g5[0], g6[0], i, j)
                    ds_base = mod_lai_albedo(
                        ds_base, maxlai, minlai, maxg, maxalb, minalb, alb_init, lai_init, i, j)

                elif mx_fr == a['dectr'][i, j]:
                    ds_base = mod_gs(
                        ds_base, g1[1], g2[1], g3[1], g4[1], g5[1], g6[1], i, j)
                    ds_base = mod_lai_albedo(
                        ds_base, maxlai, minlai, maxg, maxalb, minalb, alb_init, lai_init, i, j)

                elif mx_fr == a['grass'][i, j]:
                    ds_base = mod_gs(
                        ds_base, g1[2], g2[2], g3[2], g4[2], g5[2], g6[2], i, j)
                    ds_base = mod_lai_albedo(
                        ds_base, maxlai, minlai, maxg, maxalb, minalb, alb_init, lai_init, i, j)
                else:
                    pass

        ds_merged = ds_base_orig.update(ds_base)
        for var in ds_merged.data_vars.keys():
            if 'coordinates' in ds_merged[var].attrs:
                del ds_merged[var].attrs['coordinates']

        file_out = 'output/2-g1_g6_changed/' + \
            x_file.split('output/1-changed_to_SUEWS/')[1]

        ds_merged.to_netcdf(file_out,
                            mode='w', format='NETCDF3_64BIT')
        print('SUEWS input has been added to:' + file_out)
