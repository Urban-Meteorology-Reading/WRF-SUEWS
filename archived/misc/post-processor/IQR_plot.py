import pandas as pd
# import numpy as np
# import wrf
# import netCDF4 as nc
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
# import seaborn as sns


# IQR filling plot:
def plot_day_clm(df_var):
    """Short summary.

    Parameters
    ----------
    df_var : pd.DataFrame
        DataFrame containing variables to plot with datetime as index

    Returns
    -------
    MPL.figure
        figure showing median lines and IQR in shadings

    """
    # group by hour and minute
    grp_sdf_var = df_var.groupby(
        [df_var.index.hour.rename('hr'),
         df_var.index.minute.rename('min')])
    # get index
    # idx_len = grp_sdf_var.ngroups
    # idx_keys = sorted(grp_sdf_var.groups.keys())
    # dt_start=grp_sdf_var.groups[idx_keys[0]]
    idx = [pd.datetime(2014, 1, 1, h, m)
           for h, m in sorted(grp_sdf_var.groups.keys())]
    idx = pd.date_range(idx[0], idx[-1], freq='1h')
    # calculate quartiles
    quar_sel_pos_clm = grp_sdf_var.quantile(
        [.75, .5, .25]).unstack().set_index(idx)
    fig, ax = plt.subplots(1)

    for var in quar_sel_pos_clm.columns.levels[0]:
        df_x = quar_sel_pos_clm.loc[:, var]
        y0 = df_x[0.5]
        y1, y2 = df_x[0.75], df_x[0.25]
        y0.plot(ax=ax, label=var).fill_between(
            quar_sel_pos_clm.index, y1, y2, alpha=0.3)
    # add legend
    ax.legend(title='variable')
    # adjust xticks formar
    # ax.xaxis.set_major_locator(mdates.HourLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%H:%M'))

    return fig
