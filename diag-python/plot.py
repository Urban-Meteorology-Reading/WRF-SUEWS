import salem
import numpy as np
import pandas as pd
# from salem.utils import get_demo_file
from salem import geogrid_simulator, DataLevels
import matplotlib.pyplot as plt

# get map settings
fig_res, ax_res = plt.subplots(2, 2, sharex=True, sharey=True)
# fig_res.subplots_adjust(wspace=0, hspace=0)


# load results
fn = '../test-run/nest-run/wrfout_d01_2014-07-01_00:00:00'
ds = salem.open_xr_dataset(fn)
t2 = ds.T2


# get a base map
cmap_res = ''  # 'jet'
data_plot = t2[:100:10]
dl = DataLevels(data_plot, cmap=cmap_res)
kargs_dl = {k: getattr(dl, k) for k in ['vmin', 'vmax', 'levels', 'nlevels']}
map_base = ds.T2[0].salem.get_map()
map_base.set_lonlat_contours(.6)

# add map plots to axes
# for x in ax_res.flat:
#     x.clear()
for x_ax, x_res in zip(ax_res.flat, data_plot):
    map_base.set_data(x_res)
    map_base.set_plot_params(**kargs_dl)
    map_base.set_cmap(cmap_res)
    map_base.visualize(ax=x_ax, addcbar=False)
    x_ax.set_aspect('auto')
    # label with datetime
    ts = pd.to_datetime(str(x_res.XTIME.values))
    x_ax.text(.5, .9, ts,
              horizontalalignment='center',
              transform=x_ax.transAxes)
    for tick in x_ax.get_xticklabels():
        tick.set_rotation(60)

# add a shared color bar
fig_res.subplots_adjust(right=0.8, wspace=0, hspace=.01)
cbar_ax = fig_res.add_axes([0.85, 0.12, 0.05, 0.7])
dl.colorbarbase(cbar_ax)
cbar_ax.set_title('T2 (K)')
# adjust figure size
fig_res.set_size_inches(np.array([1, 1]) * 6)

# save file
fig_res.savefig('T2_m.pdf')


# load map settings from wps namelist
fig_map, ax_map = plt.subplots(1, 1)
fpath = '/Users/sunt05/Dropbox/with_Zhenkun/nest-run/namelist.wps'
g, maps = geogrid_simulator(fpath)
maps[0].set_rgb(natural_earth='hr')
maps[0].visualize(ax=ax_map, title='Domains 1 to 3')

maps[0]
# export the figure
fig_map.set_size_inches(6, 6)
fig_map.savefig('map.pdf')
