#!/usr/bin/env python
import os
import salem
from glob import glob
import pandas as pd
import numpy as np
import wrf
import netCDF4 as nc
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns
from IQR_plot import plot_day_clm

# load map settings from wps namelist
fig_map, ax_map = plt.subplots(1, 1)
fpath = '../wrf_input/London-3domains/namelist.wps-london-3'
g, maps = salem.geogrid_simulator(fpath)
maps[0].set_rgb(natural_earth='hr')
maps[0].visualize(ax=ax_map, title='Domains')

# export the figure
fig_map.set_size_inches(6, 6)
fig_map.savefig('fig/map.png')


# load WRF results
fl_WRF = sorted(glob('wrfout*'))
ds = salem.open_mf_wrf_dataset(fl_WRF)


dswrf = nc.Dataset(fl_WRF[0])
x_pos, y_pos = wrf.ll_to_xy(dswrf, latitude=[51.], longitude=[-0.1])

# ds.HFX[:, y_pos, x_pos].time
ds.HFX[:, y_pos, x_pos].plot.line(add_legend=True)
ds_sel = ds[['HFX', 'LH', 'GRDFLX', 'SWDOWN', 'GLW']].resample(
    time='1h', label='left').mean()

# Facet plotting
ds_grp_clm = ds_sel.HFX.load().groupby('time.hour').median(axis=0)
fig_spatial_clm = ds_grp_clm.plot(
    x='west_east', y='south_north', col='hour',
    col_wrap=4, robust=True).fig

fig_spatial_clm.savefig('figures/QH_map.pdf')

# print ds_sel.resample.__doc__

da_sel = ds_sel.to_array(name='flux')
da_sel_pos = da_sel[:, :, y_pos, x_pos]
df_sel_pos = da_sel_pos.to_pandas().T
# plotting SEB components
da_sel_pos.loc[['HFX', 'LH', 'GRDFLX'], :].plot(x='time', hue='variable')

fig_flx_ts = da_sel_pos.loc[['HFX', 'LH'], :].plot(
    x='time', hue='variable')[0].figure
fig_flx_ts.axes[0].set_title('')
fig_flx_ts.tight_layout()
fig_flx_ts.savefig('QH-QE.pdf')
# climatology of diurnal cycles
grp_sel_pos_clm = df_sel_pos[['HFX', 'LH']].groupby(
    [df_sel_pos.index.hour.rename('hr'),
     df_sel_pos.index.minute.rename('min')])

# id_30min = pd.timedelta_range(start='0. hr', periods=48, freq='30min')
idx = pd.date_range('20140101', '20140101T23:30', periods=48)

quar_sel_pos_clm = grp_sel_pos_clm.quantile(
    [.75, .5, .25]).unstack()[['HFX', 'LH']].set_index(idx)


fig_x = plot_day_clm(df_sel_pos[['HFX', 'LH']])
fig_x.tight_layout()
fig_x.savefig('QH-QE-climatology.pdf')


# load KSSW observations
def func_parse_date(year, doy, hour, min):
    # dt = datetime.datetime.strptime(
    #     ' '.join([year, doy, hour, min]), '%Y %j %H %M')
    dt = pd.to_datetime(' '.join(
        [str(k) for k in [year, doy, hour, min]]),
        format='%Y %j %H %M')
    return dt


path_obs = os.path.join(
    '/Users/sunt05/Dropbox/8-Research/98.ReadingWork/20180813WRF-SUEWS-London',
    'London_KCL_obs_1h.txt')
res_obs = pd.read_csv(path_obs, sep=' ',
                      parse_dates={'datetime': [0, 1, 2, 3]},
                      keep_date_col=True,
                      date_parser=func_parse_date)

df_obs = res_obs.set_index('datetime')

df_obs.loc['2015 8', 'QE'].plot()

# determin QH&QE-availavle periods
df_obs[['QN','QH','QE']].loc['2015'].resample('1M').count().plot()


df_QH_comp = pd.concat([df_obs, df_sel_pos[['HFX', 'LH']]], axis=1,
                       join='inner').loc[:, ['HFX', 'QH']]

df_QH_comp.plot()

fig_comp = sns.regplot(x='Obs', y='Sim',
                       data=df_QH_comp.rename(
                           columns={'QH': 'Obs', 'HFX': 'Sim'}),
                       fit_reg=True).figure

ax = fig_comp.axes[0]
x0, x1 = ax.get_xlim()
y0, y1 = ax.get_ylim()
ax.set_aspect(abs(x1 - x0) / abs(y1 - y0))
fig_comp.savefig('comp.pdf')
