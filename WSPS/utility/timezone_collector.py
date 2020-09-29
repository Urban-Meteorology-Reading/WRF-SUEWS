from glob import glob
import xarray as xr
import datetime
import pytz
import sys
from timezonefinder import TimezoneFinder
from tqdm import tqdm


def get_timezone(lat, lon, str_first_day):
    tf = TimezoneFinder(in_memory=True)
    timezone_str = tf.closest_timezone_at(lng=lon, lat=lat)
    timezone = pytz.timezone(timezone_str)
    date_d = str_first_day.split('-')
    dt = datetime.datetime(int(date_d[0]),
                           int(date_d[1]),
                           int(date_d[2])
                           )
    offset = timezone.utcoffset(dt)
    offset_hour = offset.total_seconds()/3600
    return offset_hour


def set_timezone(path_dir_output, str_first_day):
    print('This step might take some time . . .')
    x_files = sorted(glob(str(path_dir_output) +
                          '/2-parameters_changed/wrfinput_d0*'))
    for x_file in x_files:
        print('working on '+x_file.split(str(path_dir_output) +
                                         '/2-parameters_changed/')[1]+'. . .')
        ds_base = xr.open_dataset(x_file)

        wrf_LAT = ds_base.XLAT.values[0, :, :]
        wrf_LON = ds_base.XLONG.values[0, :, :]

        counter = 0
        for i in tqdm(range(wrf_LAT.shape[0])):
            for j in range(wrf_LON.shape[0]):
                lat = wrf_LAT[i, j]
                lon = wrf_LON[i, j]
                offset = get_timezone(lat, lon, str_first_day)
                try:
                    ds_base['timezone_SUEWS'.upper()].values[0, i, j] = offset
                except:
                    print(
                        'some exception happend! most probably, the timezone is not found and you need to add it to the input files manually . .')
                    sys.exit(0)
                if counter == 0:
                    print(
                        f'Time offset for the first grid is {get_timezone(lat,lon,str_first_day)} hour')
                    counter = 1

        print('Saving the new file . . .')
        ds_merged = ds_base.update(ds_base)

        for var in ds_merged.data_vars.keys():
            if 'coordinates' in ds_merged[var].attrs:
                del ds_merged[var].attrs['coordinates']

        file_out = path_dir_output / '3-timezone_changed' / \
            x_file.split(str(path_dir_output / '2-parameters_changed/'))[1]

        ds_merged.to_netcdf(file_out,
                            mode='w', format='NETCDF3_64BIT')
        print('SUEWS input has been added to:' + file_out)
