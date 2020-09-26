! This is part of the netCDF package.
! Copyright 2006 University Corporation for Atmospheric Research/Unidata.
! See COPYRIGHT file for conditions of use.

! This is an example which reads some 4D pressure and
! temperatures. The data file read by this program is produced by
! the companion program pres_temp_4D_wr.f90. It is intended to
! illustrate the use of the netCDF Fortran 90 API.

! This program is part of the netCDF tutorial:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial

! Full documentation of the netCDF Fortran 90 API can be found at:
! http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90

! $Id: pres_temp_4D_rd.f90,v 1.6 2006/12/09 18:44:58 russ Exp $

PROGRAM pres_temp_4D_rd
  USE netcdf
  IMPLICIT NONE

  ! This is the name of the data file we will read.
  CHARACTER (len = *), PARAMETER :: FILE_NAME = "wrfinput_d01.new.nc"
  INTEGER :: ncid

  ! We are reading 4D data, a 2 x 6 x 12 lvl-lat-lon grid, with 2
  ! timesteps of data.
  INTEGER, PARAMETER :: NDIMS = 4, NRECS = 2
  INTEGER, PARAMETER :: NLVLS = 2, NLATS = 6, NLONS = 12
  CHARACTER (len = *), PARAMETER :: LVL_NAME = "level"
  CHARACTER (len = *), PARAMETER :: LAT_NAME = "latitude"
  CHARACTER (len = *), PARAMETER :: LON_NAME = "longitude"
  CHARACTER (len = *), PARAMETER :: REC_NAME = "time"
  INTEGER :: lvl_dimid, lon_dimid, lat_dimid, rec_dimid

  ! The start and count arrays will tell the netCDF library where to
  ! read our data.
  INTEGER :: start(NDIMS), COUNT(NDIMS)

  ! In addition to the latitude and longitude dimensions, we will also
  ! create latitude and longitude variables which will hold the actual
  ! latitudes and longitudes. Since they hold data about the
  ! coordinate system, the netCDF term for these is: "coordinate
  ! variables."
  REAL :: lats(NLATS), lons(NLONS)
  INTEGER :: lon_varid, lat_varid

  ! We will read surface temperature and pressure fields. In netCDF
  ! terminology these are called "variables."
  CHARACTER (len = *), PARAMETER :: PRES_NAME="pressure"
  CHARACTER (len = *), PARAMETER :: TEMP_NAME="temperature"
  CHARACTER (len = 40) :: varname
  INTEGER :: pres_varid, temp_varid
  INTEGER :: dimids(NDIMS)

  ! We recommend that each variable carry a "units" attribute.
  CHARACTER (len = *), PARAMETER :: UNITS = "units"
  CHARACTER (len = *), PARAMETER :: PRES_UNITS = "hPa", TEMP_UNITS = "celsius"
  CHARACTER (len = *), PARAMETER :: LAT_UNITS = "degrees_north"
  CHARACTER (len = *), PARAMETER :: LON_UNITS = "degrees_east"

  ! Program variables to hold the data we will read in. We will only
  ! need enough space to hold one timestep of data; one record.
  REAL :: pres_in(NLONS, NLATS, NLVLS)
  REAL :: temp_in(NLONS, NLATS, NLVLS)
  REAL, PARAMETER :: SAMPLE_PRESSURE = 900.0
  REAL, PARAMETER :: SAMPLE_TEMP = 9.0

  ! Use these to calculate the values we expect to find.
  REAL, PARAMETER :: START_LAT = 25.0, START_LON = -125.0

  ! Loop indices
  INTEGER :: lvl, lat, lon, rec, i

  ! To check the units attributes.
  CHARACTER*80 pres_units_in, temp_units_in
  CHARACTER*80 lat_units_in, lon_units_in

  ! Open the file.
  CALL check( nf90_open(FILE_NAME, nf90_nowrite, ncid) )

  varname='LAI_SUEWS'
  CALL check(nf90_inq_varid(ncid, trim(varname), pres_varid))
  print*, varname, pres_varid

  varname='state_SUEWS'
  CALL check(nf90_inq_varid(ncid, trim(varname), pres_varid))
  print*, varname, pres_varid

  varname='soilmoist_SUEWS'
  CALL check(nf90_inq_varid(ncid, trim(varname), pres_varid))
  print*, varname, pres_varid

  ! ! Get the varids of the latitude and longitude coordinate variables.
  ! CALL check( nf90_inq_varid(ncid, LAT_NAME, lat_varid) )
  ! CALL check( nf90_inq_varid(ncid, LON_NAME, lon_varid) )
  !
  ! ! Read the latitude and longitude data.
  ! CALL check( nf90_get_var(ncid, lat_varid, lats) )
  ! CALL check( nf90_get_var(ncid, lon_varid, lons) )
  !
  ! ! Check to make sure we got what we expected.
  ! DO lat = 1, NLATS
  !    IF (lats(lat) /= START_LAT + (lat - 1) * 5.0) STOP 2
  ! END DO
  ! DO lon = 1, NLONS
  !    IF (lons(lon) /= START_LON + (lon - 1) * 5.0) STOP 2
  ! END DO
  !
  ! ! Get the varids of the pressure and temperature netCDF variables.
  ! CALL check( nf90_inq_varid(ncid, PRES_NAME, pres_varid) )
  ! CALL check( nf90_inq_varid(ncid, TEMP_NAME, temp_varid) )
  !
  ! ! Read 1 record of NLVLS*NLATS*NLONS values, starting at the beginning
  ! ! of the record (the (1, 1, 1, rec) element in the netCDF file).
  ! count = (/ NLONS, NLATS, NLVLS, 1 /)
  ! start = (/ 1, 1, 1, 1 /)
  !
  ! ! Read the surface pressure and temperature data from the file, one
  ! ! record at a time.
  ! DO rec = 1, NRECS
  !    start(4) = rec
  !    CALL check( nf90_get_var(ncid, pres_varid, pres_in, start = start, &
  !         count = count) )
  !    CALL check( nf90_get_var(ncid, temp_varid, temp_in, start, count) )
  !
  !    i = 0
  !    DO lvl = 1, NLVLS
  !       DO lat = 1, NLATS
  !          DO lon = 1, NLONS
  !             IF (pres_in(lon, lat, lvl) /= SAMPLE_PRESSURE + i) STOP 2
  !             IF (temp_in(lon, lat, lvl) /= SAMPLE_TEMP + i) STOP 2
  !             i = i + 1
  !          END DO
  !       END DO
  !    END DO
  !    ! next record
  ! END DO

  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file.
  CALL check( nf90_close(ncid) )

  ! If we got this far, everything worked as expected. Yipee!
  PRINT *,"*** SUCCESS reading example file ", FILE_NAME, "!"

CONTAINS
  SUBROUTINE check(status)
    INTEGER, INTENT ( in) :: status

    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       STOP "Stopped"
    END IF
  END SUBROUTINE check
END PROGRAM pres_temp_4D_rd
