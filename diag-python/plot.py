import salem
# from salem.utils import get_demo_file
from salem import geogrid_simulator
import matplotlib.pyplot as plt

fn = '../test-run/nest-run/wrfout_d01_2014-07-01_00:00:00'


ds = salem.open_xr_dataset(fn)
t2 = ds.T2.isel(Time=[1, 3, 5])
t2[2].salem.quick_map()


fpath = '/Users/sunt05/Dropbox/with_Zhenkun/nest-run/namelist.wps'
g, maps = geogrid_simulator(fpath)
maps[0].set_rgb(natural_earth='mr')
maps[0].visualize(ax=ax1, title='Domains 1 to 3')

f, ax1 = plt.subplots(1, 1)
maps[0].visualize(ax=ax1)

f.savefig('map.pdf')
