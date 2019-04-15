C     This is part of the netCDF package.
C     Copyright 2006 University Corporation for Atmospheric Research/Unidata.
C     See COPYRIGHT file for conditions of use.

C     This is an example which reads some 4D pressure and
C     temperatures. The data file read by this program is produced by
C     the companion program pres_temp_4D_wr.f. It is intended to
C     illustrate the use of the netCDF Fortran 77 API.

C     This program is part of the netCDF tutorial:
C     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

C     Full documentation of the netCDF Fortran 77 API can be found at:
C     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f77

C     $Id: pres_temp_4D_rd.f,v 1.11 2007/01/24 19:45:09 russ Exp $

      program pres_temp_4D_rd
      implicit none
      include 'netcdf.inc'

C     This is the name of the data file we will read.
      character*(*) FILE_NAME
      parameter (FILE_NAME='wrfinput_d01.new.nc')
      integer ncid

C     We are reading 4D data, a 2 x 6 x 12 lvl-lat-lon grid, with 2
C     timesteps of data.
      integer NDIMS, NRECS
      parameter (NDIMS = 4, NRECS = 2)
      integer NLVLS, NLATS, NLONS
      parameter (NLVLS = 2, NLATS = 6, NLONS = 12)
      character*(*) LVL_NAME, LAT_NAME, LON_NAME, REC_NAME
      parameter (LVL_NAME = 'level')
      parameter (LAT_NAME = 'latitude', LON_NAME = 'longitude')
      parameter (REC_NAME = 'time')
      integer lvl_dimid, lon_dimid, lat_dimid, rec_dimid

C     The start and count arrays will tell the netCDF library where to
C     read our data.
      integer start(NDIMS), count(NDIMS)

C     In addition to the latitude and longitude dimensions, we will also
C     create latitude and longitude variables which will hold the actual
C     latitudes and longitudes. Since they hold data about the
C     coordinate system, the netCDF term for these is: "coordinate
C     variables."
      real lats(NLATS), lons(NLONS)
      integer lon_varid, lat_varid

C     We will read surface temperature and pressure fields. In netCDF
C     terminology these are called "variables."
      character*(*) PRES_NAME, TEMP_NAME
      parameter (PRES_NAME='LAI_SUEWS')
      parameter (TEMP_NAME='STATE_SUEWS')
      integer pres_varid, temp_varid
      integer dimids(NDIMS)

C     We recommend that each variable carry a "units" attribute.
C      character*(*) UNITS
C      parameter (UNITS = 'units')
C      character*(*) PRES_UNITS, TEMP_UNITS, LAT_UNITS, LON_UNITS
C      parameter (PRES_UNITS = 'hPa', TEMP_UNITS = 'celsius')
C      parameter (LAT_UNITS = 'degrees_north')
C      parameter (LON_UNITS = 'degrees_east')

C     Program variables to hold the data we will read in. We will only
C     need enough space to hold one timestep of data; one record.
C      real pres_in(NLONS, NLATS, NLVLS)
C      real temp_in(NLONS, NLATS, NLVLS)
C      real SAMPLE_PRESSURE
C      parameter (SAMPLE_PRESSURE = 900.0)
C      real SAMPLE_TEMP
C      parameter (SAMPLE_TEMP = 9.0)

C     Use these to calculate the values we expect to find.
C      integer START_LAT, START_LON
C      parameter (START_LAT = 25.0, START_LON = -125.0)

C     Loop indices.
      integer lvl, lat, lon, rec, i

C     Error handling.
      integer retval

C     Open the file.
      retval = nf_open(FILE_NAME, nf_nowrite, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Get the varids of the latitude and longitude coordinate variables.
C      retval = nf_inq_varid(ncid, LAT_NAME, lat_varid)
C      if (retval .ne. nf_noerr) call handle_err(retval)
C      retval = nf_inq_varid(ncid, LON_NAME, lon_varid)
C      if (retval .ne. nf_noerr) call handle_err(retval)

C     Read the latitude and longitude data.
C      retval = nf_get_var_real(ncid, lat_varid, lats)
C      if (retval .ne. nf_noerr) call handle_err(retval)
C      retval = nf_get_var_real(ncid, lon_varid, lons)
C      if (retval .ne. nf_noerr) call handle_err(retval)

C     Check to make sure we got what we expected.
C      do lat = 1, NLATS
C         if (lats(lat) .ne. START_LAT + (lat - 1) * 5.0) stop 2
C      end do
C      do lon = 1, NLONS
C         if (lons(lon) .ne. START_LON + (lon - 1) * 5.0) stop 2
C      end do

C     Get the varids of the pressure and temperature netCDF variables.
      retval = nf_inq_varid(ncid, PRES_NAME, pres_varid)
      print*, PRES_NAME
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_inq_varid(ncid, TEMP_NAME, temp_varid)
      print*, TEMP_NAME
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Read 1 record of NLVLS*NLATS*NLONS values, starting at the beginning
C     of the record (the (1, 1, 1, rec) element in the netCDF file).
C      count(1) = NLONS
C      count(2) = NLATS
C      count(3) = NLVLS
C      count(4) = 1
C      start(1) = 1
C      start(2) = 1
C      start(3) = 1

C     Read the surface pressure and temperature data from the file, one
C     record at a time.
C      do rec = 1, NRECS
C         start(4) = rec
C         retval = nf_get_vara_real(ncid, pres_varid, start, count,
C     $        pres_in)
C         if (retval .ne. nf_noerr) call handle_err(retval)
C         retval = nf_get_vara_real(ncid, temp_varid, start, count,
C     $        temp_in)
C         if (retval .ne. nf_noerr) call handle_err(retval)
C
C         i = 0
C         do lvl = 1, NLVLS
C            do lat = 1, NLATS
C               do lon = 1, NLONS
C                  if (pres_in(lon, lat, lvl) .ne. SAMPLE_PRESSURE + i)
C     $                 stop 2
C                  if (temp_in(lon, lat, lvl) .ne. SAMPLE_TEMP + i)
C     $                 stop 2
C                  i = i + 1
C               end do
C            end do
C         end do
C     next record
C      end do

C     Close the file. This frees up any internal netCDF resources
C     associated with the file.
      retval = nf_close(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     If we got this far, everything worked as expected. Yipee!
      print *,'*** SUCCESS reading example file:', FILE_NAME
      end

      subroutine handle_err(errcode)
      implicit none
      include 'netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end
