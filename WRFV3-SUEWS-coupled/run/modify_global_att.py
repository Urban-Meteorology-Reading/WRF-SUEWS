#!/usr/bin/env python
import netCDF4 as nc4
import os

filename = os.path.join('/Users/zhenkunli/work/2018/WRFV3/run', 'wrfinput_d01' )
print 'Processing file %s...' % filename
nc = nc4.Dataset(filename, 'a')
nc.SF_SURFACE_PHYSICS = 9
nc.close()
