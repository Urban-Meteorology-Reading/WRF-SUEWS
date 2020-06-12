import geopandas as gpd
from pyproj import Proj, transform
from glob import glob
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import xarray as xr
from scipy.interpolate import griddata
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.colors as colors
import datetime
import pytz
from tzwhere import tzwhere
from timezonefinder import TimezoneFinder

def get_timezone(lat,lon):
    tf = TimezoneFinder(in_memory=True)
    timezone_str = tf.closest_timezone_at(lng=lon, lat=lat)
    timezone = pytz.timezone(timezone_str)
    dt = datetime.datetime(2019,1,1)
    offset=timezone.utcoffset(dt)
    return offset.total_seconds()/3600

def set_timezone():
    x_files=sorted(glob('output/2-parameters_changed/wrfinput_d0*'))
    for x_file in x_files:
        print('working on '+x_file)
        ds_base = xr.open_dataset(x_file)
        
        wrf_LAT=ds_base.XLAT.values[0,:,:]
        wrf_LON=ds_base.XLONG.values[0,:,:]
        
        for i in range(wrf_LAT.shape[0]):
            for j in range(wrf_LON.shape[0]):
                lat=wrf_LAT[i,j]
                lon=wrf_LON[i,j]
                ds_base['timezone_SUEWS'.upper()].values[0,i,j]=get_timezone(lat,lon)


        ds_merged = ds_base.update(ds_base)    

        for var in ds_merged.data_vars.keys():
            if 'coordinates' in ds_merged[var].attrs:
                del ds_merged[var].attrs['coordinates']

        file_out = 'output/3-timezone_changed/'+x_file.split('output/2-parameters_changed/')[1]

        ds_merged.to_netcdf(file_out,
                            mode='w', format='NETCDF3_64BIT')
        print('SUEWS input has been added to:' + file_out)