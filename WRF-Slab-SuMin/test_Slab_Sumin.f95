program test_Slab_Sumin

  use module_sf_SUEWS
  USE SUEWS_Driver,ONLY: nsurf, nvegsurf, ndays

  implicit none

  integer, parameter :: NLCAT = 20
  integer            :: ids = 1, ide = 1, jds = 1, jde = 1, kds = 1, kde = 1
  integer, parameter :: ims = 1, ime = 2, jms = 1, jme = 2, kms = 1, kme = 2
  integer            :: its = 1, ite = 2, jts = 1, jte = 2, kts = 1, kte = 2
  integer            :: year = 2018, day = 20, hour = 3, minute = 5
  real, dimension(ims:ime, kms:kme, jms:jme)  :: QV3D, P3D, T3D, U3D, V3D, DZ3D
  real, dimension(ims:ime, jms:jme)           :: SWDOWN, PSFC, PREC, ht
  REAL   :: DT = 300, DX = 100
  REAL, DIMENSION(ims:ime, NLCAT, jms:jme)    :: LANDUSEF
  REAL, DIMENSION(ims:ime, jms:jme)           :: XLONG, XLAT
  REAL,DIMENSION( ims:ime, jms:jme )          :: HFX, QFX, LH, TSK, QSFC

  ! SUEWS specific variables:
  REAL,DIMENSION(ims:ime,jms:jme, 360)               :: qn1_store_SUEWS
  REAL,DIMENSION(ims:ime,jms:jme, 2*360+1)           :: qn1_av_store_SUEWS
  REAL,DIMENSION(ims:ime,jms:jme,-4:ndays, nvegsurf) :: LAI_SUEWS      !LAI for each veg surface [m2 m-2]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays)           :: albDecTr_SUEWS !Albedo of deciduous trees [-]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays)           :: albEveTr_SUEWS !Albedo of evergreen trees [-]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays)           :: albGrass_SUEWS !Albedo of grass[-]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays)           :: DecidCap_SUEWS !Storage capacity of deciduous trees [mm]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays)           :: porosity_SUEWS !Porosity of deciduous trees [-]
  REAL,DIMENSION(ims:ime,jms:jme, 0:ndays, 5)        :: GDD_SUEWS      !Growing Degree Days (see SUEWS_DailyState.f95)
  REAL,DIMENSION(ims:ime,jms:jme,-4:ndays, 6)        :: HDD_SUEWS      !Heating Degree Days (see SUEWS_DailyState.f95)
  REAL,DIMENSION(ims:ime,jms:jme,nsurf)              :: state_SUEWS
  REAL,DIMENSION(ims:ime,jms:jme,nsurf)              :: soilmoist_SUEWS
  REAL,DIMENSION(ims:ime,jms:jme,nsurf)              :: surf_var_SUEWS
  integer :: i, j, k, l
  real :: lbound, ubound

  do k = 1, 2
     call random_array2d(2, 2, QV3D(:, k, :), 0., 1.)
     call random_array2d(2, 2, P3D(:, k, :), 900., 1000.)
     call random_array2d(2, 2, T3D(:, k, :), 280., 300.)
     call random_array2d(2, 2, U3D(:, k, :), 5., 10.)
     call random_array2d(2, 2, V3D(:, k, :), 5., 10.)
     call random_array2d(2, 2, DZ3D(:, k, :), 20., 100.)
  end do

  call random_array2d(2, 2, SWDOWN, 200., 300.)
  call random_array2d(2, 2, PSFC, 1000., 1100.)
  call random_array2d(2, 2, PREC, 1., 5.)
  call random_array2d(2, 2, ht, 100., 200.)

  LANDUSEF(:, :, :) = 0.05

  do j = 1, 2
     do i = 1, 2
        XLONG(i, j) = 120. + 0.1 * i + 0.1 * j
        XLAT(i, j) = 30. + 0.1 * i + 0.1 * j
     end do
  end do

  call random_array2d(2, 2, HFX, 100., 200.)
  call random_array2d(2, 2, QFX, 100., 200.)
  call random_array2d(2, 2, LH, 100., 200.)
  call random_array2d(2, 2, TSK, 100., 200.)
  call random_array2d(2, 2, QSFC, 100., 200.)

  do k = 1, 360
     call random_array2d(2, 2, qn1_store_SUEWS(:,:,k), 100., 200.)
  end do

  do k = 1, 2*360+1
     call random_array2d(2, 2, qn1_av_store_SUEWS(:,:,k), 100., 200.)
  end do

  do l = 1, nvegsurf
     do k = -4, ndays
       call random_array2d(2, 2, LAI_SUEWS(:,:,k,l), 0., 3.)
     end do
  end do

  do k = 0, ndays
     call random_array2d(2, 2, albDecTr_SUEWS(:,:,k), 0., 1.)
     call random_array2d(2, 2, albEveTr_SUEWS(:,:,k), 0., 1.)
     call random_array2d(2, 2, albGrass_SUEWS(:,:,k), 0., 1.)
     call random_array2d(2, 2, DecidCap_SUEWS(:,:,k), 1., 10.)
     call random_array2d(2, 2, porosity_SUEWS(:,:,k), 0., 1.)
  end do

  do l = 1, 5
     do k = 0, ndays
        call random_array2d(2, 2, GDD_SUEWS(:,:,k,l), 0., 100.)
     end do
  end do

  do l = 1, 6
     do k = -4, ndays
        call random_array2d(2, 2, HDD_SUEWS(:,:,k,l), 0., 100.)
     end do
  end do

  do k = 1, nsurf
    call random_array2d(2, 2, state_SUEWS(:,:,k), 0., 100.)
    call random_array2d(2, 2, soilmoist_SUEWS(:,:,k), 0., 1.)
    call random_array2d(2, 2, surf_var_SUEWS(:,:,k), 0., 100.)
  end do

  call suewsdrv(year, day, hour, minute,                     &
                T3D, QV3D, P3D, U3D, V3D, DZ3d, SWDOWN,      &
                PSFC, PREC, NLCAT, LANDUSEF, ht,             &
                HFX, QFX, LH, TSK, QSFC,&
                LAI_SUEWS,&
                qn1_store_SUEWS,&
                qn1_av_store_SUEWS,&
                albDecTr_SUEWS,&
                albEveTr_SUEWS,&
                albGrass_SUEWS,&
                DecidCap_SUEWS,&
                porosity_SUEWS,&
                GDD_SUEWS,&
                HDD_SUEWS,&
                state_SUEWS,&
                soilmoist_SUEWS,&
                surf_var_SUEWS,&
                XLONG, XLAT, DT, DX,  &
                ids, ide, jds, jde, kds, kde,                             &
                ims, ime, jms, jme, kms, kme,                             &
                its, ite, jts, jte, kts, kte)
   print*, qn1_store_SUEWS

end program test_Slab_Sumin

subroutine random_array2d(nx, ny, arr2d, ubound, lbound)
  implicit none
  integer, INTENT(in) :: nx, ny
  real, INTENT(out)   :: arr2d(nx, ny)
  real, INTENT(in)    :: ubound, lbound
  real :: len
  real :: rand
  integer :: i, j

  len = ubound -lbound
  do j = 1, ny
     do i = 1, nx
        call random_number(rand)
        arr2d(i, j) = lbound + len * rand
     end do
  end do
end subroutine random_array2d
