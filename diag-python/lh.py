#!/usr/bin/env python
from netCDF4 import Dataset
import matplotlib.pyplot as plt
from matplotlib.cm import get_cmap
import cartopy.crs as crs
from cartopy.feature import NaturalEarthFeature

from wrf import ALL_TIMES, to_np, getvar, smooth2d, get_cartopy, cartopy_xlim, cartopy_ylim, latlon_coords

# Open the NetCDF file
ncfile = Dataset("/Users/zhenkunli/Dropbox/share/results/wrfout_d01_2014-07-01_00:00:00")

# Get the sea level pressure
LH = getvar(ncfile, "LH", timeidx=ALL_TIMES)
print LH

# Smooth the sea level pressure since it tends to be noisy near the mountains
LH = smooth2d(LH, 3)

# Get the latitude and longitude points
lats, lons = latlon_coords(LH)

# Get the cartopy mapping object
cart_proj = get_cartopy(LH)
print cart_proj

# Create a figure
fig = plt.figure(figsize=(8,6))
# Set the GeoAxes to the projection used by WRF
ax = plt.axes(projection=cart_proj)

# Download and add the states and coastlines
states = NaturalEarthFeature(category='cultural', scale='50m', facecolor='none',
                             name='admin_1_states_provinces_shp')
ax.add_feature(states, linewidth=.5)
ax.coastlines('50m', linewidth=0.8)

# Make the contour outlines and filled contours for the smoothed sea level pressure.
plt.contour(to_np(lons), to_np(lats), to_np(LH[160,:,:]), 10, colors="black",
            transform=crs.PlateCarree())
plt.contourf(to_np(lons), to_np(lats), to_np(LH[160,:,:]), 10, transform=crs.PlateCarree(),
             cmap=get_cmap("jet"))

# Add a color bar
plt.colorbar(ax=ax, shrink=.92)

# Set the map limits.  Not really necessary, but used for demonstration.
ax.set_xlim(cartopy_xlim(LH[160,:,:]))
ax.set_ylim(cartopy_ylim(LH[160,:,:]))

# Add the gridlines
ax.gridlines(color="black", linestyle="dotted")

plt.title("LATENT HEAT FLUX AT THE SURFACE (W m-2)")

plt.savefig('LH.png')

# plt.show()
