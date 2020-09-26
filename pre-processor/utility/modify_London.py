import geopandas as gpd
from pyproj import Proj, transform
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import xarray as xr
from scipy.interpolate import griddata

def regrid_lower(name,df,wrf_X,wrf_Y,x2,y2):
    new_a=np.full_like(wrf_X,fill_value=0)
    count_a=np.full_like(wrf_X,fill_value=0)
    for x,y,val in zip(x2,y2,df[name]):

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

def modify_all_London():

    df = pd.read_csv('data/SUEWS_SiteSelect.txt',sep='\t')
    df.columns=df.iloc[0]
    df=df.drop(df.index[0])
    df.drop(df.tail(2).index,inplace=True)
    df= df.astype(float)
    lat=df['lat']
    lon=df['lng']

    p2_text='''
    +units=m +proj=lcc +lat_1=30 +lat_2=60 +lat_0=51.51 +lon_0=-0.96 
    +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
    '''

    # p2_text='''
    # +units=m +init=ESRI:102009 +proj=lcc +lat_1=30 +lat_2=60 +lat_0=51.51 +lon_0=-0.96 
    # +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
    # '''

    p1 = Proj(init='epsg:32631')
    p2 = Proj(p2_text)
    x1, y1 = p1(list(lon),list(lat))
    x2, y2 = transform(p1,p2,x1,y1)

    x_file='./output/1-changed_to_SUEWS/wrfinput_d03.suews'
    ds_base = xr.open_dataset(x_file)
    wrf_LAT=ds_base.XLAT.values[0,:,:]
    wrf_LON=ds_base.XLONG.values[0,:,:]
    wrf_X,wrf_Y=p2(wrf_LON,wrf_LAT)

    names=['H_Bldgs','H_EveTr','H_DecTr']
    names_wrf=['bldgH_SUEWS','EveTreeH_SUEWS','DecTreeH_SUEWS']

    for name,name_wrf in zip(names,names_wrf):
        print(name)
        new_0=regrid_lower(name,df,wrf_X,wrf_Y,x2,y2)
        #new_all.update( {name : new_0} )
        ds_var=ds_base[name_wrf.upper()].values[0,:,:]
        ds_var[~np.isnan(new_0)]=new_0[~np.isnan(new_0)]
        zz=ds_base[name_wrf.upper()].values[0,:,:]
        zz[zz<0]=0
        zz[np.isnan(new_0) & (zz!=0)]=np.mean(zz[~np.isnan(new_0)])
        ds_base[name_wrf.upper()].values[0,:,:]=zz

    names=['Fr_Paved' ,'Fr_Bldgs' ,'Fr_EveTr' ,'Fr_DecTr' ,'Fr_Grass' ,'Fr_Bsoil','Fr_Water']
    new_all={}
    for name in names:
        print(name)
        new_0=regrid_lower(name,df,wrf_X,wrf_Y,x2,y2)
        new_all[name]=new_0

    new_0=new_all['Fr_Paved']
    cd=~np.isnan(new_0)
    ds_base['LANDUSEF'].values[0,12,:,:][cd]=new_all['Fr_Paved'][cd]+new_all['Fr_Bldgs'][cd]

    temp=new_all['Fr_Paved'][cd]+new_all['Fr_Bldgs'][cd]
    temp[temp==0]=0.001

    ds_base['PAVED_RATIO'].values[0,:,:][cd]=new_all['Fr_Paved'][cd]/(temp)

    ds_base['PAVED_RATIO'].values[0,:,:][cd][ds_base['PAVED_RATIO'].values[0,:,:][cd]>1]=0

    ds_base['LANDUSEF'].values[0,0,:,:][cd]=new_all['Fr_EveTr'][cd]/3
    ds_base['LANDUSEF'].values[0,1,:,:][cd]=new_all['Fr_EveTr'][cd]/3
    ds_base['LANDUSEF'].values[0,4,:,:][cd]=new_all['Fr_EveTr'][cd]/3

    ds_base['LANDUSEF'].values[0,2,:,:][cd]=new_all['Fr_DecTr'][cd]/2
    ds_base['LANDUSEF'].values[0,3,:,:][cd]=new_all['Fr_DecTr'][cd]/2

    ds_base['LANDUSEF'].values[0,5,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,6,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,7,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,8,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,9,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,11,:,:][cd]=new_all['Fr_Grass'][cd]/7
    ds_base['LANDUSEF'].values[0,13,:,:][cd]=new_all['Fr_Grass'][cd]/7

    ds_base['LANDUSEF'].values[0,15,:,:][cd]=new_all['Fr_Bsoil'][cd]/4
    ds_base['LANDUSEF'].values[0,17,:,:][cd]=new_all['Fr_Bsoil'][cd]/4
    ds_base['LANDUSEF'].values[0,18,:,:][cd]=new_all['Fr_Bsoil'][cd]/4
    ds_base['LANDUSEF'].values[0,19,:,:][cd]=new_all['Fr_Bsoil'][cd]/4


    ds_base['LANDUSEF'].values[0,10,:,:][cd]=new_all['Fr_Water'][cd]/2
    ds_base['LANDUSEF'].values[0,16,:,:][cd]=new_all['Fr_Water'][cd]/2


    shapefile2 = gpd.read_file("data/ESRI1/OAres_work_pop.shp")
    p2_text='''+units=m +proj=lcc +lat_1=30 +lat_2=60 +lat_0=51.51 +lon_0=-0.96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
    '''
    shapefile_converted=shapefile2.to_crs(p2_text)
    shapefile_converted_2=shapefile_converted.copy()


    x1=shapefile_converted_2.iloc[:]['geometry'].centroid.x
    y1=shapefile_converted_2.iloc[:]['geometry'].centroid.y
    names=['WorkPop','ResPop']
    names_wrf=['PopDensDayTime_SUEWS','PopDensNightTime_SUEWS']
    for name,name_wrf in zip(names,names_wrf):
        
        print(name)
        new_a=np.full_like(wrf_X,fill_value=0)
        
        for x,y,val in zip(x1,y1,shapefile_converted_2[name]):

            for id_x,i in enumerate(wrf_X[0,:-1]):
                if x>=i and x<=wrf_X[0,id_x+1]:
                    id_x_wrf=id_x
                    i_wrf=i

            for id_y,j in enumerate(wrf_Y[:-1,0]):
                if y>=j and y<=wrf_Y[id_y+1,0]:
                    id_y_wrf=id_y
                    j_wrf=j

            new_a[id_y_wrf,id_x_wrf]+=val/100 # to ha-1
            
        new_a[np.where(new_a==0)]=np.nan
        ds_var=ds_base[name_wrf.upper()].values[0,:,:]
        ds_var[~np.isnan(new_a)]=new_a[~np.isnan(new_a)]
        zz=ds_base[name_wrf.upper()].values[0,:,:]
        zz[zz<0]=0
        zz[np.isnan(new_a) & (zz!=0)]=np.percentile(zz[~np.isnan(new_a)],50)
        ds_base[name_wrf.upper()].values[0,:,:]=zz
        
    ds_base['PopDensDayTime_SUEWS'.upper()].values[0,37,19]=310
    ds_base['PopDensNightTime_SUEWS'.upper()].values[0,37,19]=99

# Modifying the QF coefficients to be more representive
    w1s=[0.1446,0,0.0037]
    w2s=[0.133,0,0.0038]
    qs=['qf_a_suews','qf_b_suews','qf_c_suews']
    dtemp=ds_base.copy()
    zz=dtemp['landusef'.upper()].values[0,12,:,:]

    for q,w1,w2 in zip(qs,w1s,w2s):
        
        print(q)
        qa=dtemp[q.upper()].values[0,0,:,:]
        qa[(zz<=0.50) & (qa!=0)]=w1
        dtemp[q.upper()].values[0,0,:,:]=qa
        
        qa=dtemp[q.upper()].values[0,1,:,:]
        qa[(zz<=0.50) & (qa!=0)]=w2
        dtemp[q.upper()].values[0,1,:,:]=qa
        
    ds_base=dtemp    


    ds_merged = ds_base.update(ds_base)    

    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            del ds_merged[var].attrs['coordinates']
            
    file_out = x_file+'.new'

    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
    print('SUEWS input has been added to:' + file_out)
