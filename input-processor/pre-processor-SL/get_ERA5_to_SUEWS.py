import supy as sp
from shutil import copyfile
from pathlib import Path



forcing_name=sp.util.gen_forcing_era5(lat_x=6.939585,lon_x=79.860580,
                         start='2015 12 31',end='2017 01 01',
                        dir_save='./ERA5/')
print(forcing_name)
years=[2015,2016,2017]
for id,file in enumerate(forcing_name):
    copyfile(file, f"./runs/run_Colombo/Input/Cl_{years[id]}_data_60.txt")