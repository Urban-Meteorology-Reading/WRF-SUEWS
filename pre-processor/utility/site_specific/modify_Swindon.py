import xarray as xr

def mod_landusef(ds_base):
    # this if for Swindon
    xx = 45
    yy = 38
    ds_base["LANDUSEF"].values[0, 12, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.49
    ds_base["LANDUSEF"].values[0, 0, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.01 / 3
    ds_base["LANDUSEF"].values[0, 1, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.01 / 3
    ds_base["LANDUSEF"].values[0, 4, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.01 / 3
    ds_base["LANDUSEF"].values[0, 2:4, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.08 / 2
    ds_base["LANDUSEF"].values[0, 5:10, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.36 / 7
    ds_base["LANDUSEF"].values[0, 11, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.36 / 7
    ds_base["LANDUSEF"].values[0, 13, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.36 / 7
    ds_base["LANDUSEF"].values[0, 15, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.06 / 4
    ds_base["LANDUSEF"].values[0, 17:20, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.06 / 4
    ds_base["LANDUSEF"].values[0, 10, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.0 / 2
    ds_base["LANDUSEF"].values[0, 16, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.0 / 2

    return ds_base, xx, yy


def modify_Swindon(path_dir_output,path_dir_data,file_to_change):

    x_file=path_dir_output / file_to_change
    ds_base = xr.open_dataset(x_file)


    print("modifying the landusef around the site for Swindon . . .")
    ds_base, xx, yy = mod_landusef(ds_base)
    ds_base["PAVED_RATIO"].values[0, xx - 2 : xx + 2, yy - 2 : yy + 2] = 0.67


    ds_merged = ds_base.update(ds_base)

    for var in ds_merged.data_vars.keys():
        if 'coordinates' in ds_merged[var].attrs:
            del ds_merged[var].attrs['coordinates']

    file_out = x_file.parent / (x_file.name+'.new')

    ds_merged.to_netcdf(file_out,
                        mode='w', format='NETCDF3_64BIT')
    print('SUEWS input has been added to:' + str(file_out))