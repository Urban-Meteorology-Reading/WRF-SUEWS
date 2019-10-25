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


def mod_gs(ds_base,g1,g2,g3,g4,g5,g6,maxg,maxlai,i,j):
    
    ds_base['G1'].values[0,i,j]=g1
    ds_base['G2'].values[0,i,j]=g2
    ds_base['G3'].values[0,i,j]=g3
    ds_base['G4'].values[0,i,j]=g4
    ds_base['G5'].values[0,i,j]=g5
    ds_base['G6'].values[0,i,j]=g6
    
    ds_base['MaxConductance_SUEWS'.upper()].values[0,:,i,j]=maxg
    ds_base['LaiMax_SUEWS'.upper()].values[0,:,i,j]=maxlai
    
    return ds_base

def g1_g6():
    x_files=glob.glob('output/1-changed_to_SUEWS/wrfinput_d0*')

    for x_file in x_files:
        print('working on '+x_file)
        ds_base = xr.open_dataset(x_file)
        ds_base_orig=ds_base.copy(deep=True)
        a={}
        a['urban']=0
        a['dectr']=0
        a['evetr']=0
        a['grass']=0
        a['bsoil']=0
        a['water']=0
        
        b=ds_base['LANDUSEF'].copy(deep=True)
        a['urban']=b.values[0,12,:,:]
        for i in [2,3]:
            a['dectr']=a['dectr']+b.values[0,i,:,:]
        for i in [0,1,4]:
            a['evetr']=a['evetr']+b.values[0,i,:,:]    
        for i in [5,6,7,8,9,11,13]:
            a['grass']=a['grass']+b.values[0,i,:,:] 
        for i in [15,17,18,19]:
            a['bsoil']=a['bsoil']+b.values[0,i,:,:] 
        for i in [10,16]:
            a['water']=a['water']+b.values[0,i,:,:] 
        
        


        for i in range(a['urban'].shape[0]):
            for j in range(a['urban'].shape[1]):

                mx_fr=np.max([a['urban'][i,j],a['evetr'][i,j],a['dectr'][i,j],a['grass'][i,j],a['bsoil'][i,j],a['water'][i,j]])

                if (mx_fr==a['urban'][i,j]) and (a['urban'][i,j] >0.50):
                    ds_base=mod_gs(ds_base,1,251.15,0.52,0.73,1.58,0.031,10.64,0.24,i,j)
                    
                elif (mx_fr==a['urban'][i,j]) and (a['urban'][i,j] <=0.50):
                    ds_base=mod_gs(ds_base,1,266.44,0.4,0.008,16.06,0.09,85.2,2.6,i,j)

                elif mx_fr==a['dectr'][i,j]:
                    ds_base=mod_gs(ds_base,1,292.59,0.167,0.705,54.9,0.023,166.65,4.93,i,j)

                elif mx_fr==a['evetr'][i,j]:
                    ds_base=mod_gs(ds_base,1,498.17,0.8,0.23,54.97,0.009,28.19,5.1,i,j)

                elif mx_fr==a['grass'][i,j]:
                    ds_base=mod_gs(ds_base,1,266.15,0.78,0.74,18.03,0.038,9.57,2.7,i,j)
                else:
                    ds_base=mod_gs(ds_base,1,251.15,0.52,0.73,1.58,0.0031,10.64,0.24,i,j)
        
        

        ds_merged = ds_base_orig.update(ds_base)    
        for var in ds_merged.data_vars.keys():
            if 'coordinates' in ds_merged[var].attrs:
                del ds_merged[var].attrs['coordinates']

        file_out = 'output/2-g1_g6_changed/'+x_file.split('output/1-changed_to_SUEWS/')[1]

        ds_merged.to_netcdf(file_out,
                            mode='w', format='NETCDF3_64BIT')
        print('SUEWS input has been added to:' + file_out)