# import salem
import numpy as np
import pandas as pd
import xarray as xr
import netCDF4
# from salem.utils import get_demo_file
# from salem import geogrid_simulator, DataLevels
import cartopy.crs as ccrs
from cartopy.feature import NaturalEarthFeature
import matplotlib.pyplot as plt
from matplotlib.cm import get_cmap
from wrf import to_np, getvar, smooth2d, get_cartopy, cartopy_xlim, cartopy_ylim, latlon_coords
# import cartopy
# get map settings
fig_res, ax_res = plt.subplots(2, 2, sharex=True, sharey=True)
# fig_res.subplots_adjust(wspace=0, hspace=0)
# print(cartopy.__version__)

# load results
fn = '/Users/sunt05/Documents/20180813WRF-SUEWS-London/wrfout_d03_2014-07-01_00:00:00'
ds = xr.open_dataset(fn)
res_plot = ds.T2
pd.Series(ds.attrs).iloc[-28:]
dir(ds)


# facet plotting
g_simple = res_plot[60:70].plot(
    x='west_east', y='south_north', col='Time', col_wrap=5, robust=True)
ds.HFX[11]
res_plot[1].max()
np.unravel_index(res_plot[1].argmax(), res_plot[1].shape)
res_plot[1].shape
res_plot[1, 15, 59]
# print g_simple.__doc__
# dir(g_simple)


# map plotting
# Open the NetCDF file
ncfile = netCDF4.Dataset(fn)

# Get the sea level pressure
var_plot = getvar(ncfile, "T2", timeidx=90)
# var_plot
# Smooth the sea level pressure since it tends to be noisy near the mountains
# smooth_var_plot = smooth2d(var_plot, 3)
smooth_var_plot = var_plot
# Get the latitude and longitude points
lats, lons = latlon_coords(var_plot)

# Get the cartopy mapping object
cart_proj = get_cartopy(var_plot)
# cart_proj.proj4_params
# dir(cart_proj)

# Create a figure
# fig = plt.figure(figsize=(12, 9))
# Set the GeoAxes to the projection used by WRF
ax = plt.axes(projection=cart_proj)
fig = ax.figure
# res_plot[10].plot()
# res_plot[10].plot.imshow(ax=ax, transform=cart_proj)
# res_plot[10].plot(ax=ax, transform=cart_proj)

# fig
# Download and add the states and coastlines
states = NaturalEarthFeature(category='cultural', scale='50m', facecolor='none',
                             name='admin_1_states_provinces_shp')
ax.add_feature(states, linewidth=.5)
ax.coastlines('50m', linewidth=0.8)

# Make the contour outlines and filled contours for the smoothed sea level pressure.
ax.contour(to_np(lons), to_np(lats), to_np(smooth_var_plot), 10, colors="black",
           transform=ccrs.PlateCarree())
ax.contourf(to_np(lons), to_np(lats), to_np(smooth_var_plot), 10,
            transform=ccrs.PlateCarree(),
            cmap=get_cmap("jet"))

# Add a color bar

# Set the map limits.  Not really necessary, but used for demonstration.
ax.set_xlim(cartopy_xlim(smooth_var_plot))
ax.set_ylim(cartopy_ylim(smooth_var_plot))
# cartopy_xlim(smooth_var_plot)
# Add the gridlines
ax.gridlines(color="black", linestyle="dotted")

ax.set_title("WRF results")
ax.figure
# plt.show()


# try some more plots
ax = plt.axes(projection=cart_proj)
fig_res = ax.figure
fig_res = res_plot[1:17].plot.imshow(
    col='Time', col_wrap=4,
    robust=True)

dir(fig_res)

ax.pcolormesh(res_plot.XLONG[0], res_plot.XLAT[0],
              res_plot[10], transform=ccrs.PlateCarree())
# res_plot[32].plot.contourf(robust=True,ax=ax,transform=ccrs.PlateCarree(),cmap=get_cmap("jet"))
ax.add_feature(states, linewidth=.5)
ax.coastlines('50m', linewidth=0.8)

ax.figure

# line plots
ds.STATE_SUEWS.shape
ds.LH[:,[10,12],10].plot.line(x='Time')
