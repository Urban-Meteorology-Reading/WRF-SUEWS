import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import xarray as xr
from scipy.interpolate import griddata
import geopandas as gpd
from pyproj import Proj, transform
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.colors as colors
import glob
import rasterio
from rasterio.features import rasterize
from rasterio.transform import from_bounds



def change_bldgh():
    print('modifying building height . . .')

    df_bldg=gpd.read_file('./input/bldg_data/CMC_bldg_500mbuffer/CMC_bldg_500mbuffer.shp')
    sub_df_bldg=df_bldg.filter(['FR_N_Floor','geometry'])

    ############## ############## ############## ############## ##############
    def change_to_floor(row):
        splits=row['FR_N_Floor'].split('G')
        #return splits
        if splits[1]=='':
            return 1*(7.5/2)
        else:
            return (7.5/2)*(1+int(splits[1].split('+')[1]))
        
    sub_df_bldg['BldgH']=sub_df_bldg.apply(change_to_floor,axis=1)

    ############## ############## ############## ############## ##############
    df=sub_df_bldg #It is a shapefile dataframe with two columns of geometry and levels
    dimns = 1000, 1000
    dtype=rasterio.float64
    transform_=rasterio.transform.from_bounds(*df.total_bounds, *dimns)

    rasterize_rivernet = rasterize(
        [(shape, BldgH) for shape,BldgH in zip(df['geometry'],df['BldgH'])],
        out_shape=dimns,
        transform=transform_,
        fill=0,
        all_touched=True,
        dtype=dtype)

    with rasterio.open(
        './output/rasterized.tif', 'w',
        driver='GTiff',
        dtype=dtype,
        count=1,
        width=dimns[0],
        height=dimns[1],
        transform=transform_
    ) as dst:
        dst.write(rasterize_rivernet, indexes=1)

    ############## ############## ############## ############## ##############
    rst=rasterio.open('./output/rasterized.tif',mode='r+')
    data=rst.read(1)
    data[np.where(data==0)]=np.nan
    Xs=[]
    Ys=[]
    Ds=[]
    for i in range(rst.shape[0]):
        for j in range(rst.shape[1]):
            if ~np.isnan(data[j,i]):
                x,y=rst.transform*(i,j)
                Xs.append(x)
                Ys.append(y)
                Ds.append(data[j,i])
    ############## ############## ############## ############## ##############
    p3 = Proj(init='epsg:32644')

    p2_text='''
        +units=m +proj=lcc +lat_1=30 +lat_2=60 +lat_0=6.92 +lon_0=79.86 
        +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
        '''
    p2 = Proj(p2_text)
    ############## ############## ############## ############## ##############
    x_file='output/1-changed_to_SUEWS/wrfinput_d03.suews'
    ds_base = xr.open_dataset(x_file)
    wrf_LAT=ds_base.XLAT.values[0,:,:]
    wrf_LON=ds_base.XLONG.values[0,:,:]


    wrf_X,wrf_Y=p2(wrf_LON,wrf_LAT)

    x3, y3 = transform(p3,p2,Xs,Ys)
    ############## ############## ############## ############## ##############
    Xss=[]
    Yss=[]
    Dss=[]
    for i,j,k in zip(x3,y3,Ds):
            if (np.min(wrf_X)<=i<=np.max(wrf_X)) and (np.min(wrf_Y)<=j<=np.max(wrf_Y)):

                Xss.append(i)
                Yss.append(j)
                Dss.append(k)
    ############## ############## ############## ############## ##############
    def regrid_lower_2(Ds,wrf_X,wrf_Y,x2,y2):
        new_a=np.full_like(wrf_X,fill_value=0)
        count_a=np.full_like(wrf_X,fill_value=0)
        for x,y,val in zip(x2,y2,Ds):

            for id_x,i in enumerate(wrf_X[0,:-1]):
                if x>=i and x<=wrf_X[0,id_x+1]:
                    id_x_wrf=id_x
                    i_wrf=i

            for id_y,j in enumerate(wrf_Y[:-1,0]):
                if y>=j and y<=wrf_Y[id_y+1,0]:
                    id_y_wrf=id_y
                    j_wrf=j

            new_a[id_y_wrf,id_x_wrf]+=val
            count_a[id_y_wrf,id_x_wrf]+=1
        z=np.divide(new_a,count_a,where=~np.isnan(new_a))
        return z
    ############## ############## ############## ############## ##############

    names=['bldgH_SUEWS']
    new_all={}
    for name in names:
        print(name)
        new_0=regrid_lower_2(Ds,wrf_X,wrf_Y,Xss,Yss)
        new_all[name]=new_0
        
        ds_var=ds_base[name.upper()].values[0,:,:]
        ds_var[~np.isnan(new_0)]=new_0[~np.isnan(new_0)]
        zz=ds_base[name.upper()].values[0,:,:]
        zz[zz<0]=0
        #zz[np.isnan(new_0) & (zz!=0)]=np.mean(zz[~np.isnan(new_0)])
        ds_base[name.upper()].values[0,:,:]=zz

    ############## ############## ############## ############## ##############
    shapefile2= gpd.read_file("./input/pop_data/Day_time_Population_density_Distribution.shp")

    def make_raster(df,name_col,name_file):
        #It is a shapefile dataframe with two columns of geometry and levels
        dimns = 1000, 1000
        dtype=rasterio.float64
        transform_=rasterio.transform.from_bounds(*df.total_bounds, *dimns)

        rasterize_rivernet = rasterize(
            [(shape, BldgH) for shape,BldgH in zip(df['geometry'],df[name_col])],
            out_shape=dimns,
            transform=transform_,
            fill=0,
            all_touched=True,
            dtype=dtype)

        with rasterio.open(
            name_file, 'w',
            driver='GTiff',
            dtype=dtype,
            count=1,
            width=dimns[0],
            height=dimns[1],
            transform=transform_
        ) as dst:
            dst.write(rasterize_rivernet, indexes=1)
    ############## ############## ############## ############## ##############
    sub_daytime=shapefile2.filter(['daytime_de','geometry'])
    name_file='./output/rasterized_pop_day.tif'
    name_col='daytime_de'
    make_raster(sub_daytime,name_col,name_file)

    sub_nighttime=shapefile2.filter(['resede_den','geometry'])
    name_file='./output/rasterized_pop_night.tif'
    name_col='resede_den'
    make_raster(sub_nighttime,name_col,name_file)
    ############## ############## ############## ############## ##############
    def get_coor(name_file,wrf_X,wrf_Y,p2):
        rst=rasterio.open(name_file,mode='r+')
        data=rst.read(1)
        #data[np.where(data==0)]=np.nan
        Xs=[]
        Ys=[]
        Ds=[]
        for i in range(rst.shape[0]):
            for j in range(rst.shape[1]):
                if ~np.isnan(data[j,i]):
                    x,y=rst.transform*(i,j)

                    Xs.append(x)
                    Ys.append(y)
                    Ds.append(data[j,i])

        p3 = Proj(init='epsg:5234')
        x3, y3 = transform(p3,p2,Xs,Ys)

        Xss=[]
        Yss=[]
        Dss=[]
        for i,j,k in zip(x3,y3,Ds):
                if (np.min(wrf_X)<=i<=np.max(wrf_X)) and (np.min(wrf_Y)<=j<=np.max(wrf_Y)):

                    Xss.append(i)
                    Yss.append(j)
                    Dss.append(k)
                    
        return Xss,Yss,Dss
    ############## ############## ############## ############## ##############
    def regrid_lower_mod(Ds,wrf_X,wrf_Y,x2,y2):
        new_a=np.full_like(wrf_X,fill_value=0)
        count_a=np.full_like(wrf_X,fill_value=0)
        
        for x,y,val in zip(x2,y2,Ds):

            for id_x,i in enumerate(wrf_X[0,:-1]):
                if x>=i and x<=wrf_X[0,id_x+1]:
                    id_x_wrf=id_x
                    i_wrf=i

            for id_y,j in enumerate(wrf_Y[:-1,0]):
                if y>=j and y<=wrf_Y[id_y+1,0]:
                    id_y_wrf=id_y
                    j_wrf=j
            try:
                
                new_a[id_y_wrf,id_x_wrf]+=val
                count_a[id_y_wrf,id_x_wrf]+=1
            except:
                #pass
                print('some exception happened . . . (probably data is out of domain)')
        z=np.divide(new_a,count_a,where=~np.isnan(new_a))
        return z,count_a
    ############## ############## ############## ############## ##############
    names=['POPDENSDAYTIME_SUEWS','POPDENSNIGHTTIME_SUEWS']
    name_files=['./output/rasterized_pop_day.tif','./output/rasterized_pop_night.tif']
    new_all={}
    for name,name_file in zip(names,name_files):
        print(name)
        Xss,Yss,Dss=get_coor(name_file,wrf_X,wrf_Y,p2)
        new_0,count_a=regrid_lower_mod(Dss,wrf_X,wrf_Y,Xss,Yss)
        new_all[name]=new_0
        
        ds_var=ds_base[name.upper()].values[0,:,:]
        ds_var[~np.isnan(new_0)]=new_0[~np.isnan(new_0)]
        zz=ds_base[name.upper()].values[0,:,:]
        zz[zz<0]=0
        #zz[np.isnan(new_0) & (zz!=0)]=np.mean(zz[~np.isnan(new_0)])
        ds_base[name.upper()].values[0,:,:]=zz
    ############## ############## ############## ############## ##############
    ds_merged = ds_base.update(ds_base)    

    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            del ds_merged[var].attrs['coordinates']
            
    file_out = x_file

    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
    print('SUEWS input has been added to:' + file_out)