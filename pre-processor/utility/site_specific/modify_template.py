import xarray as xr


def modify_template(path_dir_output,path_dir_data):
    '''
    # Reading the original input file
        x_file=path_dir_output / 'final/wrfinput_d04.suews'
        ds_base = xr.open_dataset(x_file)
    #################

    # Do all the modification on "ds_base" here

    #################

    # Saving the modified input file
        ds_merged = ds_base.update(ds_base)
        for var in ds_merged.data_vars.keys():
            if 'coordinates' in ds_merged[var].attrs:
                del ds_merged[var].attrs['coordinates']

        file_out = x_file.parent / (x_file.name+'.new')
        ds_merged.to_netcdf(file_out,
                            mode='w', format='NETCDF3_64BIT')
    '''