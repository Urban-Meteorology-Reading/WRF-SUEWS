import supy as sp
from shutil import copyfile
from pathlib import Path


forcing_name=sp.util.gen_forcing_era5(lat_x=6.939585,lon_x=79.860580,
                         start='2016 01 01',end='2017 01 01',
                        dir_save='./ERA5/')
print(forcing_name)
copyfile(forcing_name[0], "./runs/run_Colombo/Input/Cl_2016_data_60.txt")