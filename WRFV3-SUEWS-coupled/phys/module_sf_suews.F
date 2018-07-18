!WRF:MODEL_LAYER:PHYSICS
!
! TS, ZL, 25 Apr 2018: initial coupling version
! TS, ZL, 27 Apr 2018: multiple modifications


MODULE module_sf_SUEWS
  USE SuMin_Module,ONLY:SuMin

  REAL,DIMENSION(8,4,3)                        :: OHM_coef_s
  REAL,DIMENSION(8,6)                          :: WaterDist_s
  REAL,DIMENSION(24,2)                         :: AHProf_24hr
  REAL,DIMENSION(24,2)                         :: HumActivity_24hr
  REAL,DIMENSION(24,2)                         :: PopProf_24hr
  REAL,DIMENSION(24,2)                         :: TraffProf_24hr
  REAL,DIMENSION(24,2)                         :: WUProfA_24hr
  REAL,DIMENSION(24,2)                         :: WUProfM_24hr
  INTEGER                                      :: snowUse
  INTEGER                                      :: RoughLenHeatMethod
  INTEGER                                      :: RoughLenMomMethod
  INTEGER                                      :: EmissionsMethod
  INTEGER                                      :: NetRadiationMethod
  INTEGER                                      :: StorageHeatMethod
  INTEGER                                      :: OHMIncQF
  INTEGER                                      :: AerodynamicResistanceMethod
  INTEGER,parameter                            :: num_ind_max = 10
  INTEGER,parameter                            :: ind_undef = -999
  INTEGER,DIMENSION(8, num_ind_max)            :: suews_cat_ind
  REAL,DIMENSION(8, num_ind_max)               :: suews_cat_frac

CONTAINS

  !----------------------------------------------------------------
  SUBROUTINE suewsdrv(year, day, hour, minute, second, xtime, &
       T3D, QV3D, P3D, U3D, V3D, DZ3d, SWDOWN,  &
       PSFC, PREC, ht,                          &
       HFX, QFX, LH, TSK, QSFC, chklowq,        &
       LAI_SUEWS,                               &
       albDecTr_SUEWS,                          &
       albEveTr_SUEWS,                          &
       albGrass_SUEWS,                          &
       DecidCap_SUEWS,                          &
       porosity_SUEWS,                          &
       GDD_SUEWS,                               &
       HDD_SUEWS,                               &
       state_SUEWS,                             &
       soilmoist_SUEWS,                         &
       surf_var_SUEWS,                          &
       landusef_SUEWS,                          &
       alb_SUEWS,                               &
       emis_SUEWS,                              &
       qn1_av_SUEWS,                            &
       qn1_s_SUEWS,                             &
       dqndt_SUEWS,                             &
       dqnsdt_SUEWS,                            &
       MeltWaterStore,                          &
       SnowAlb,                                 &
       WUDay,                                   &
       z0m_in,                                  &
       zdm_in,                                  &
       xlong, xlat, DT, DT_PREV, DX,            &
       ids, ide, jds, jde, kds, kde,            &
       ims, ime, jms, jme, kms, kme,            &
       its, ite, jts, jte, kts, kte)
    !should be call from module_surface_driver as :
    ! call suewsdrv(year, day, hour, minute,                               &
    !               t_phy, qv_curr, p8w, u_phy, v_phy, dz8w, swdown,       &
    !               psfc, rainbl, NLCAT, LANDUSEF, ht, XLONG, XLAT         &
    !               ids,ide, jds,jde, kds,kde,                             &
    !               ims,ime, jms,jme, kms,kme,                             &
    !               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts, kte)

    !----------------------------------------------------------------
    IMPLICIT NONE
    !----------------------------------------------------------------
    !
    !     SUBROUTINE SLAB CALCULATES THE GROUND TEMPERATURE TENDENCY
    !     ACCORDING TO THE RESIDUAL OF THE SURFACE ENERGY BUDGET
    !     (BLACKADAR, 1978B).
    !
    !     CHANGES:
    !          FOR SOIL SUB-TIMESTEPS UPDATE SURFACE HFX AND QFX AS TG
    !          CHANGES TO PREVENT POSSIBLE INSTABILITY FOR LONG MODEL
    !          STEPS (DT > ~200 SEC).
    !
    !          PUT SNOW COVER CHECK ON SOIL SUB-TIMESTEPS
    !
    !          MAKE UPPER LIMIT ON SOIL SUB-STEP LENGTH MORE CONSERVATIVE
    !
    !----------------------------------------------------------------
    !-- T3D         temperature (K)
    !-- QV3D        3D water vapor mixing ratio (Kg/Kg)
    !-- P3D         3D pressure (Pa)
    !-- FLHC        exchange coefficient for heat (m/s)
    !-- FLQC        exchange coefficient for moisture (m/s)
    !-- PSFC        surface pressure (Pa)
    !-- XLAND       land mask (1 for land, 2 for water)
    !-- TMN         soil temperature at lower boundary (K)
    !-- HFX         upward heat flux at the surface (W/m^2)
    !-- QFX         upward moisture flux at the surface (kg/m^2/s)
    !-- LH          latent heat flux at the surface (W/m^2)
    !-- TSK         surface temperature (K)
    !-- GSW         downward short wave flux at ground surface (W/m^2)
    !-- GLW         downward long wave flux at ground surface (W/m^2)
    !-- CAPG        heat capacity for soil (J/K/m^3)
    !-- THC         thermal inertia (Cal/cm/K/s^0.5)
    !-- SNOWC       flag indicating snow coverage (1 for snow cover)
    !-- EMISS       surface emissivity (between 0 and 1)
    !-- DELTSM      time step (second)
    !-- ROVCP       R/CP
    !-- XLV         latent heat of melting (J/kg)
    !-- DTMIN       time step (minute)
    !-- IFSNOW      ifsnow=1 for snow-cover effects
    !-- SVP1        constant for saturation vapor pressure (kPa)
    !-- SVP2        constant for saturation vapor pressure (dimensionless)
    !-- SVP3        constant for saturation vapor pressure (K)
    !-- SVPT0       constant for saturation vapor pressure (K)
    !-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
    !-- EP2         constant for specific humidity calculation
    !               (R_d/R_v) (dimensionless)
    !-- KARMAN      Von Karman constant
    !-- EOMEG       angular velocity of earth's rotation (rad/s)
    !-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
    !-- TSLB        soil temperature in 5-layer model
    !-- ZS          depths of centers of soil layers
    !-- DZS         thicknesses of soil layers
    !-- num_soil_layers   the number of soil layers
    !-- ids         start index for i in domain
    !-- ide         end index for i in domain
    !-- jds         start index for j in domain
    !-- jde         end index for j in domain
    !-- kds         start index for k in domain
    !-- kde         end index for k in domain
    !-- ims         start index for i in memory
    !-- ime         end index for i in memory
    !-- jms         start index for j in memory
    !-- jme         end index for j in memory
    !-- kms         start index for k in memory
    !-- kme         end index for k in memory
    !-- its         start index for i in tile
    !-- ite         end index for i in tile
    !-- jts         start index for j in tile
    !-- jte         end index for j in tile
    !-- kts         start index for k in tile
    !-- kte         end index for k in tile
    !----------------------------------------------------------------
    INTEGER, INTENT(IN)    ::     year, day, hour, minute, second
    REAL, INTENT(IN   )    ::     xtime

    REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)   ::  &
         QV3D, P3D, T3D, U3D, V3D, DZ3D
    !
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)    ::  SWDOWN, PSFC, PREC, ht

    REAL,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) ::   &
         HFX, QFX, LH, TSK, QSFC, chklowq
    REAL , INTENT(IN)    :: DT, dt_prev, DX
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)    ::  XLONG, XLAT
    INTEGER, INTENT(IN)    ::     ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte

    ! SUEWS specific variables:
    REAL,DIMENSION(ims:ime,3,jms:jme),INTENT(INOUT)  :: LAI_SUEWS      !LAI for each veg surface [m2 m-2]
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: albDecTr_SUEWS !Albedo of deciduous trees [-]
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: albEveTr_SUEWS !Albedo of evergreen trees [-]
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: albGrass_SUEWS !Albedo of grass[-]
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: DecidCap_SUEWS !Storage capacity of deciduous trees [mm]
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: porosity_SUEWS !Porosity of deciduous trees [-]
    REAL,DIMENSION(ims:ime,5,jms:jme),INTENT(INOUT)  :: GDD_SUEWS      !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL,DIMENSION(ims:ime,12,jms:jme),INTENT(INOUT) :: HDD_SUEWS      !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: state_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: soilmoist_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: surf_var_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: landusef_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: alb_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: emis_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: qn1_av_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: qn1_s_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: dqndt_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: dqnsdt_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)  :: MeltWaterStore
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)    :: SnowAlb
    REAL,DIMENSION(ims:ime,9,jms:jme),INTENT(INOUT)  :: WUDay
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(IN)       :: z0m_in
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(IN)       :: zdm_in

    ! LOCAL VARS
    REAL,DIMENSION(7)                 :: landusef_suews1d
    REAL :: QV1D, P1D, T1D, U1D, V1D, DZ1D
    REAL :: SWDOWN1D, PSFC1D, PREC1D, ht1d, XLONG1D, XLAT1D
    REAL(KIND(1d0)) :: qh_out, qe_out, qsfc_out, tsk_out, CHKLOWQ_out
    REAL(KIND(1d0)) :: timezone

    REAL(KIND(1d0)),DIMENSION(3)                     :: LAI_id      !LAI for each veg surface [m2 m-2]
    REAL(KIND(1d0))                                  :: albDecTr_id
    REAL(KIND(1d0))                                  :: albEveTr_id
    REAL(KIND(1d0))                                  :: albGrass_id
    REAL(KIND(1d0))                                  :: DecidCap_id
    REAL(KIND(1d0))                                  :: porosity_id
    REAL(KIND(1d0)),DIMENSION(5)                     :: GDD_id      !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(12)                    :: HDD_id      !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(7)                     :: state_id
    REAL(KIND(1d0)),DIMENSION(7)                     :: soilmoist_id
    REAL(KIND(1d0)),DIMENSION(7)                     :: surf_var_id
    REAL(KIND(1d0)),DIMENSION(7)                     :: alb_id
    REAL(KIND(1d0)),DIMENSION(7)                     :: emis_id
    REAL(KIND(1d0))                                  :: qn1_av_id
    REAL(KIND(1d0))                                  :: qn1_s_id
    REAL(KIND(1d0))                                  :: dqndt_id
    REAL(KIND(1d0))                                  :: dqnsdt_id
    REAL(KIND(1d0)),DIMENSION(7)                     :: MeltWaterStore_id
    REAL(KIND(1d0))                                  :: SnowAlb_id
    REAL(KIND(1d0)),DIMENSION(9)                     :: WUDay_id
    REAL(KIND(1d0))                                  :: z0m_in_id
    REAL(KIND(1d0))                                  :: zdm_in_id


    INTEGER :: dt_since_start ! time since simulation starts [s]

    INTEGER ::  I, J

    dt_since_start = INT(xtime * 60)
    PRINT *, 'year = ', year, 'day = ', day, 'hour = ', hour, 'minute = ', minute
    PRINT *, 'minutesSinceStart = ', xtime, 'secondsSinceStart = ', dt_since_start
    PRINT *, 'dt = ', dt, 'dt_prev = ', dt_prev

    ! CALL MODIScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, landusef_suews)

    DO J = jts, jte

       DO I = its, ite

          T1D = T3D(i,1,j)
          QV1D = QV3D(i,1,j)
          P1D  = P3D(i,1,j)
          U1D = U3D(i,1,j)
          V1D = V3D(i,1,j)
          DZ1D = DZ3D(i,1,j)


          SWDOWN1D = SWDOWN(i,j)
          PSFC1D = PSFC(i,j)
          PREC1D = PREC(i,j)
          landusef_suews1d = landusef_suews(I,:,J)
          ht1d = ht(i,j)
          XLAT1D = XLAT(i,j)
          XLONG1D = XLONG(i,j)

          LAI_id          = LAI_SUEWS(I,:,J)
          albDecTr_id     = albDecTr_SUEWS(I,J)
          albEveTr_id     = albEveTr_SUEWS(I,J)
          albGrass_id     = albGrass_SUEWS(I,J)
          DecidCap_id     = DecidCap_SUEWS(I,J)
          porosity_id     = porosity_SUEWS(I,J)
          GDD_id          = GDD_SUEWS(I,:,J)
          HDD_id          = HDD_SUEWS(I,:,J)
          state_id        = state_SUEWS(I,:,J)
          soilmoist_id    = soilmoist_SUEWS(I,:,J)
          surf_var_id     = surf_var_SUEWS(I,:,J)
          alb_id          = alb_SUEWS(I,:,J)
          emis_id         = emis_SUEWS(I,:,J)
          qn1_av_id       = qn1_av_SUEWS(I,J)
          qn1_s_id        = qn1_s_SUEWS(I,J)
          dqndt_id        = dqndt_SUEWS(I,J)
          dqnsdt_id        = dqnsdt_SUEWS(I,J)
          MeltWaterStore_id  = MeltWaterStore(I,:,J)
          SnowAlb_id = SnowAlb(I,J)
          WUDay_id   = WUDay(I,:,J)
          z0m_in_id  = z0m_in(I,J)
          zdm_in_id  = zdm_in(I,J)

          ! the indices to the PSFC argument in the following call look
          ! wrong; however, it is correct to call with its (and not ims)
          ! because of the way PSFC is defined in SUEWS1D. Whether *that*
          ! is a good idea or not, this commenter cannot comment. JM

          timezone=0 ! NB: fix it for now
          ! print*, 'landusef_suews1d = ',landusef_suews1d

          CALL SUEWS1D(&
                                ! model configuration:
               I,J,DT,DT_PREV,year,day,hour,minute,second,dt_since_start,timezone,&
                                ! forcing:
               SWDOWN1D,QV1D,U1D,V1D,T1D,PSFC1D,PREC1D,&
                                ! surface properties (temporally invariant):
               landusef_suews1d,ht1d,XLAT1D,XLONG1D,DZ1D,DX,&
                                ! surface properties/states (temporally updated):
               LAI_id,albDecTr_id,albEveTr_id,albGrass_id,DecidCap_id,porosity_id,GDD_id,HDD_id,&
               state_id,soilmoist_id,surf_var_id,alb_id,emis_id,qn1_av_id,qn1_s_id,&
               dqndt_id,dqnsdt_id,MeltWaterStore_id,SnowAlb_id,WUDay_id,z0m_in_id,zdm_in_id,&

                                ! modelled outout:
               qh_out,qe_out,qsfc_out,tsk_out,CHKLOWQ_out,&
                                ! grid layout:
               ids,ide, jds,jde, kds,kde,&
               ims,ime, jms,jme, kms,kme,&
               its,ite, jts,jte, kts,kte)

          qn1_av_SUEWS(I,J)      = qn1_av_id
          qn1_s_SUEWS(I,J)       = qn1_s_id
          LAI_SUEWS(I,:,J)       = LAI_id
          albDecTr_SUEWS(I,J)    = albDecTr_id
          albEveTr_SUEWS(I,J)    = albEveTr_id
          albGrass_SUEWS(I,J)    = albGrass_id
          DecidCap_SUEWS(I,J)    = DecidCap_id
          porosity_SUEWS(I,J)    = porosity_id
          GDD_SUEWS(I,:,J)       = GDD_id
          HDD_SUEWS(I,:,J)       = HDD_id
          state_SUEWS(I,:,J)     = state_id
          soilmoist_SUEWS(I,:,J) = soilmoist_id
          surf_var_SUEWS(I,:,J)  = surf_var_id
          alb_SUEWS(I,:,J)       = alb_id
          emis_SUEWS(I,:,J)      = emis_id
          dqndt_SUEWS(I,J)       = dqndt_id
          dqnsdt_SUEWS(I,J)      = dqnsdt_id
          MeltWaterStore(I,:,J)  = MeltWaterStore_id
          SnowAlb(I,J)           = SnowAlb_id
          WUDay(I,:,J)           = WUDay_id
!          z0m_in(I,J)            = z0m_in_id
!          zdm_in(I,J)            = zdm_in_id


          ! print *, 'tsk = ', tsk_out
          tsk(i,j) = tsk_out
          ! update fluxes
          PRINT *, 'qh_out = ', qh_out
          PRINT *, 'qe_out = ', qe_out
          ! HFX(I,J)=min(max(qh_out,50.),50.)
          ! LH(I,J)=min(max(qe_out,50.),50.)
          ! QFX(I,J)=min(max(qsfc_out,2.e-3),2.e-3)
          HFX(I,J)=qh_out
          LH(I,J)=qe_out
          PRINT *, 'HFX(I,J) = ', HFX(I,J)
          PRINT *, 'LH(I,J) = ', LH(I,J)

          QFX(I,J)=qsfc_out
          chklowq(I,J)=CHKLOWQ_out

       ENDDO
    ENDDO

  END SUBROUTINE suewsdrv

  !----------------------------------------------------------------
  SUBROUTINE SUEWS1D(&
                                ! model configuration:
       I,J,DT,DT_PREV,iy,id,it,imin,isec,dt_since_start,timezone,&

                                ! forcing:
       SWDOWN1D,QV1D,U1D,V1D,T1D,PSFC,PREC1D,&
                                ! surface properties (temporally invariant):
       landusef_suews1d,ht,XLAT,XLONG,dz8w,DX,&
                                ! surface properties/states (temporally updated):
       LAI_id,albDecTr_id,albEveTr_id,albGrass_id,DecidCap_id,porosity_id,GDD_id,HDD_id,&
       state_id,soilmoist_id,surf_var_id,alb_id,emis_id,qn1_av_id,qn1_s_id,&
       dqndt_id,dqnsdt_id,MeltWaterStore_id,SnowAlb_id,WUDay_id,z0m_in_id,zdm_in_id,&

                                ! modelled outout:
       qh,qe,qsfc,tsk,CHKLOWQ,&
                                ! grid layout:
       ids,ide, jds,jde, kds,kde,&
       ims,ime, jms,jme, kms,kme,&
       its,ite, jts,jte, kts,kte)
    !----------------------------------------------------------------
    IMPLICIT NONE

    ! REAL, DIMENSION(:) ,INTENT(IN ) :: albedo         ! from WRF
    ! REAL, DIMENSION(:) ,INTENT(IN ) :: emiss
    INTEGER, INTENT(IN):: i, j
    REAL, INTENT(IN ) :: DT ! time step in s
    REAL, INTENT(IN ) :: DT_PREV ! time step of previous step
    ! 5. time-related:

    INTEGER,INTENT(in)::iy ! year
    INTEGER,INTENT(in)::id ! day of year
    INTEGER,INTENT(in)::it ! hour
    INTEGER,INTENT(in)::imin ! minute
    INTEGER,INTENT(in)::isec ! second
    INTEGER,INTENT(in)::dt_since_start
    REAL(KIND(1d0)),INTENT(in)::timezone   !NB:Timezone (GMT=0), assuming ZERO, SHOULD BE ALTERED

    REAL,INTENT(in) :: SWDOWN1D
    REAL,INTENT(in) :: QV1D
    REAL,INTENT(in) :: U1D, V1D
    REAL,INTENT(in) :: T1D
    REAL,INTENT(in) :: PSFC !note: unit is Pa
    REAL,INTENT(in) :: PREC1D ! precipitation amount

    REAL, DIMENSION(7) ,INTENT(IN ) :: landusef_suews1d !add in WRFcat2SUEWScat.f95
    REAL, INTENT(IN ) :: ht ! elevation
    REAL, INTENT(IN ) :: XLAT
    REAL, INTENT(IN ) :: XLONG
    REAL, INTENT(IN ) :: dz8w
    REAL, INTENT(IN ) :: DX ! horizontal space interval (m)


    REAL(KIND(1d0)),DIMENSION(3),INTENT(INOUT):: LAI_id      !LAI for each veg surface [m2 m-2]
    REAL(KIND(1d0)),INTENT(INOUT) :: albDecTr_id !Albedo of deciduous trees [-]
    REAL(KIND(1d0)),INTENT(INOUT) :: albEveTr_id !Albedo of evergreen trees [-]
    REAL(KIND(1d0)),INTENT(INOUT) :: albGrass_id !Albedo of grass[-]
    REAL(KIND(1d0)),INTENT(INOUT) :: DecidCap_id !Storage capacity of deciduous trees [mm]
    REAL(KIND(1d0)),INTENT(INOUT) :: porosity_id !Porosity of deciduous trees [-]
    REAL(KIND(1d0)),DIMENSION(5),INTENT(INOUT)       :: GDD_id      !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(12),INTENT(INOUT)      :: HDD_id      !Growing Degree Days (see SUEWS_DailyState.f95)
    ! 6. water balance:
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)       :: state_id          !Wetness status of each surface type [mm]
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)       :: soilmoist_id      !Soil moisture of each surface type [mm]
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)       :: surf_var_id   !variable to store the current states
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)       :: alb_id
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)       :: emis_id

    REAL(KIND(1d0)),INTENT(INOUT) :: dqndt_id
    REAL(KIND(1d0)),INTENT(INOUT) :: dqnsdt_id

    ! 10. OHM related:
    ! REAL(KIND(1d0)):: a1,a2,a3   !OHM coefficients, a1 [-]; a2 [h]; a3 [W m-2]
    ! REAL(KIND(1d0)),DIMENSION(3600/tstep):: qn1_store   !Q* values for each timestep over previous hr
    ! REAL(KIND(1d0)),DIMENSION(3600/tstep):: qn1_av_store  !Hourly Q* values for each timestep over previous 2 hr
    ! REAL(KIND(1d0)),DIMENSION(360), INTENT(inout):: qn1_store   !Q* values for each timestep over previous hr
    ! REAL(KIND(1d0)),DIMENSION(2*360+1), INTENT(inout):: qn1_av_store  !Hourly Q* values for each timestep over previous 2 hr
    REAL(KIND(1d0)),INTENT(INOUT)                         :: qn1_av_id
    REAL(KIND(1d0)),INTENT(INOUT)                         :: qn1_s_id
    REAL(KIND(1d0)),DIMENSION(7),INTENT(INOUT)            :: MeltWaterStore_id
    REAL(KIND(1d0)),INTENT(INOUT)                         :: SnowAlb_id
    REAL(KIND(1d0)),DIMENSION(9),INTENT(INOUT)            :: WUDay_id
    REAL(KIND(1d0)),INTENT(IN)                         :: z0m_in_id
    REAL(KIND(1d0)),INTENT(IN)                         :: zdm_in_id

    ! 11. output
    REAL(KIND(1d0)), INTENT(out) ::qh !QH for output
    REAL(KIND(1d0)), INTENT(out) ::qe ! QE for output
    REAL(KIND(1d0)), INTENT(out) ::qsfc ! QE for output
    REAL(KIND(1d0)), INTENT(out) ::tsk ! TSK for output
    REAL(KIND(1d0)), INTENT(out) ::CHKLOWQ ! TSK for output


    INTEGER, INTENT(IN)::     &
         ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte


    ! let's assign the values:
    ! parameters used in SUEWS for now:
    ! REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: SoilStoreCap=[150., 150., 150., 150., 150., 150., 0.]        !Capacity of soil store for each surface [mm]
    !
    ! REAL(KIND(1d0)),PARAMETER:: AlbMin_DecTr=0.12   !Min albedo for deciduous trees [-]
    ! REAL(KIND(1d0)),PARAMETER:: AlbMax_DecTr=0.18   !Max albedo for deciduous trees [-]
    ! REAL(KIND(1d0)),PARAMETER:: AlbMin_EveTr=0.11   !Min albedo for evergreen trees [-]
    ! REAL(KIND(1d0)),PARAMETER:: AlbMax_EveTr=0.12   !Max albedo for evergreen trees [-]
    ! REAL(KIND(1d0)),PARAMETER:: AlbMin_Grass=0.18   !Min albedo for grass [-]
    ! REAL(KIND(1d0)),PARAMETER:: AlbMax_Grass=0.21    !Max albedo for grass [-]
    !
    ! REAL(KIND(1d0)),PARAMETER:: CapMin_dec=0.3   !Min storage capacity for deciduous trees [mm] (from input information)
    ! REAL(KIND(1d0)),PARAMETER:: CapMax_dec=0.8   !Max storage capacity for deciduous trees [mm] (from input information)
    ! REAL(KIND(1d0)),PARAMETER:: PorMin_dec=0.2   !Min porosity for deciduous trees
    ! REAL(KIND(1d0)),PARAMETER:: PorMax_dec=0.6   !Max porosity for deciduous trees
    !
    ! REAL(KIND(1d0)),PARAMETER:: FAIbldg=0.                   !Frontal area fraction of buildings
    ! REAL(KIND(1d0)),PARAMETER:: FAIEveTree=0.                !Frontal area fraction of evergreen trees
    ! REAL(KIND(1d0)),PARAMETER:: FAIDecTree=0.                !Frontal area fraction of deciduous trees
    !
    ! REAL (KIND(1d0)),PARAMETER :: bldgH =10   !Mean building height
    ! REAL (KIND(1d0)),PARAMETER :: EveTreeH =10!Height of evergreen trees
    ! REAL (KIND(1d0)),PARAMETER :: DecTreeH =10!Height of deciduous trees
    !
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: BaseT          = [5,5,5]          !Base temperature for growing degree days [degC]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: BaseTe         = [11,11,11]       !Base temperature for senescence degree days [degC]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: GDDFull        = [300,300,300]    !Growing degree days needed for full capacity [degC]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: SDDFull        = [-450,-450,-450] !Senescence degree days needed to initiate leaf off [degC]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: LaiMin         = [4.,1.,1.6]      !Min LAI [m2 m-2]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: LaiMax         = [5.1,5.5,5.9]    !Max LAI [m2 m-2]
    ! REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: MaxConductance = [7.4,11.7,30.1]  !Max conductance [mm s-1]
    !
    ! REAL(KIND(1d0)),DIMENSION(4,nvegsurf),PARAMETER:: LaiPower=RESHAPE(&       !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
    !      [[0.03,0.03,0.03],&
    !      [0.0005,0.0005,0.0005],&
    !      [0.03,0.03,0.03],&
    !      [0.0005,0.0005,0.0005]],&
    !      [4,nvegsurf])
    !
    ! INTEGER,DIMENSION(nvegsurf),PARAMETER:: LAIType=1     !LAI equation to use: original (0) or new (1)
    INTEGER,PARAMETER::startDLS=85   !DOY when daylight saving starts
    INTEGER,PARAMETER::endDLS=302   !DOY when daylight saving ends

    ! REAL (KIND(1D0)),PARAMETER ::DRAINRT       = 0.25 !Drainage rate of the water bucket [mm hr-1]
    ! REAL (KIND(1D0)),PARAMETER ::RAINCOVER     = 1
    ! REAL (KIND(1D0)),PARAMETER ::RAINMAXRES    = 10   !Maximum water bucket reservoir [mm]
    ! REAL (KIND(1d0)),PARAMETER ::FlowChange    = 0    !Difference between the input and output flow in the water body
    ! REAL (KIND(1d0)),PARAMETER ::PipeCapacity  = 100  !Capacity of pipes to transfer water
    ! REAL (KIND(1d0)),PARAMETER ::RunoffToWater = 0.1  !Fraction of surface runoff going to water body
    !
    ! REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: StateLimit=[0.48, 0.25, 1.3, 0.8, 1.9, 1.0, 30000.]     !Limit for state of each surface type [mm] (specified in input files)

    ! REAL(KIND(1d0)),DIMENSION(nsurf+1,nsurf-1),PARAMETER::WaterDist=& !Within-grid water distribution to other surfaces and runoff/soil store [-]
    !      RESHAPE(&
    !      [[0.,0.1,0.1,0.1,0.,0.],&
    !      [0.,0.,0.,0.,0.,0.],&
    !      [0.,0.,0.,0.,0.,0.],&
    !      [0.,0.,0.,0.,0.,0.],&
    !      [0.02,0.,0.,0.,0.,0.],&
    !      [0.,0.,0.,0.,0.,0.],&
    !      [0.,0.,0.,0.,0.,0.],&
    !      [0.98,0.9,0.9,0.9,1.,1.]],&
    !      [nsurf+1,nsurf-1])
    ! REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: WetThresh = [0.48, 0.25, 1.3, 0.8, 1.9, 1., 0.5]     !When State > WetThresh, rs=0 limit in SUEWS_evap [mm] (specified in input files)

    ! ---- Drainage characteristics ----------------------------------------------------------------
    ! 1 - min storage capacity [mm]
    ! 2 - Drainage equation to use
    ! 3 - Drainage coeff 1 [units depend on choice of eqn]
    ! 4 - Drainage coeff 2 [units depend on choice of eqn]
    ! 5 - max storage capacity [mm]
    ! REAL(KIND(1d0)),DIMENSION(5,7),PARAMETER:: surf_attr=RESHAPE(&   ! variable to store the above five properties
    !      [[ 0.48 ,  0.25 ,  1.3  ,  0.3  ,  1.9  ,  0.8  ,  0.5  ],&
    !      [ 3.   ,  3.   ,  2.   ,  2.   ,  2.   ,  3.   ,  0.   ],&
    !      [10.   , 10.   ,  0.013,  0.013,  0.013, 10.   ,  0.   ],&
    !      [ 3.   ,  3.   ,  1.71 ,  1.71 ,  1.71 ,  3.   ,  0.   ],&
    !      [ 0.48 ,  0.25 ,  1.3  ,  0.8  ,  1.9  ,  0.8  ,  0.5  ]],&
    !      [5,7],order=[2,1])
    !
    !
    ! ! these will be assigned locally as data
    ! ! use gsodel=2 as in Ward et al. (2016)
    ! REAL (KIND(1d0)),PARAMETER::th   = 40   !Maximum temperature limit
    ! REAL (KIND(1d0)),PARAMETER::tl   = -10  !Minimum temperature limit
    ! REAL (KIND(1d0)),PARAMETER::Kmax = 1200 !Annual maximum hourly solar radiation
    ! REAL (KIND(1d0)),PARAMETER::g1   = 3.5  !Fitted parameters related to
    ! REAL (KIND(1d0)),PARAMETER::g2   = 200
    ! REAL (KIND(1d0)),PARAMETER::g3   = 0.1
    ! REAL (KIND(1d0)),PARAMETER::g4   = 0.7
    ! REAL (KIND(1d0)),PARAMETER::g5   = 30
    ! REAL (KIND(1d0)),PARAMETER::g6   = 0.05 !Fitted parameters related to
    ! REAL (KIND(1d0)),PARAMETER::s1   = 5.56
    ! REAL (KIND(1d0)),PARAMETER::s2   = 0    !surface res. calculations
    !
    !
    ! ! REAL(KIND(1d0)),DIMENSION(nsurf+1,4,3):: OHM_coef   !Array for OHM coefficients
    !
    !
    ! REAL(KIND(1d0)),DIMENSION(nsurf+1):: OHM_threshSW = [10,10,10,10,10,10,10,10]         !Arrays for OHM thresholds
    ! REAL(KIND(1d0)),DIMENSION(nsurf+1):: OHM_threshWD = [0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9] !Arrays for OHM thresholds
    !
    ! REAL (KIND(1d0)),PARAMETER::  BaseTHDD=18.9  !Base temperature for QF

    ! INTEGER,PARAMETER::OHMIncQF=0             !OHM calculation uses Q* only (0) or Q*+QF (1)

    ! variables:
    ! 1. static properties of land covers:
    ! TODO: this should be handled by WPS/ improved to load user specified values
    ! REAL(KIND(1d0)),DIMENSION(7),PARAMETER:: alb0=[0.12,0.15,0.1,0.18,0.138403,0.18,0.1]    !Albedo of each surface type [-]
    ! REAL(KIND(1d0)),DIMENSION(7),PARAMETER:: emis0=[0.95,0.91,0.98,0.98,0.988388,0.94,0.95]   !Emissivity of each surface type [-]
    ! REAL(KIND(1d0)),DIMENSION(nsurf)::alb,emis
    ! 2. site info:
    REAL(KIND(1d0)),DIMENSION(7):: sfr   !Surface fractions [-]

    REAL (KIND(1d0)) :: alt         !Altitude [m]
    REAL (KIND(1d0)) :: lat         !Latitude
    REAL (KIND(1d0)) :: lng         !Longitude
    REAL (KIND(1d0)) :: z           !Windspeed height [m]
    REAL (KIND(1d0)) :: SurfaceArea !Surface area of the study area [m2]

    ! 3. forcing variables:
    REAL (KIND(1d0))                :: avkdn     !Average downwelling shortwave radiation
    REAL (KIND(1d0))                :: avrh      !Average relative humidity
    REAL (KIND(1d0))                :: avu1      !Average wind speed
    REAL (KIND(1d0))                :: Temp_C    !Air temperature
    REAL (KIND(1d0))                :: Press_hPa !Station air pressure in hPa
    REAL (KIND(1d0))                :: Precip    !Precipitation per timestep [mm]

    REAL(KIND(1d0))::dectime ! decimal time of year

    INTEGER::tstep    !Timestep [s] at which the model is run (set in RunControl)
    INTEGER::tstep_prev    !Timestep [s] at which the model is run (set in RunControl)

    ! ---- Drainage characteristics ----------------------------------------------------------------
    REAL(KIND(1d0)),DIMENSION(6,7):: surf   !Storage capacities and drainage equation info for each surface
    ! 1 - min storage capacity [mm]
    ! 2 - Drainage equation to use
    ! 3 - Drainage coeff 1 [units depend on choice of eqn]
    ! 4 - Drainage coeff 2 [units depend on choice of eqn]
    ! 5 - max storage capacity [mm]
    ! 6 - current storage capacity [mm]
    !-----------------------------------------------------------------------------------------------
    REAL(KIND(1d0)),DIMENSION(8,4,3)                        :: OHM_coef_d
    REAL(KIND(1d0)),DIMENSION(8,6)                          :: WaterDist_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: AHProf_24hr_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: HumActivity_24hr_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: PopProf_24hr_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: TraffProf_24hr_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: WUProfA_24hr_d
    REAL(KIND(1d0)),DIMENSION(24,2)                         :: WUProfM_24hr_d
    ! OHM_coef=RESHAPE(SPREAD(SPREAD(OHMcoef, 2, 4), dim=2, ncopies=8),&
    !      shape=(/8,4,3/), order=(/3,2,1/))

    ! processing variables for SuMin
    ! surf(1:5,:)=surf_attr
    ! surf(6,:)=surf_var_id(:)


    ! get coordinates:
    lat=XLAT
    lng=XLONG
    ! get elevation
    alt=ht

    ! get measurement height of windspeed
    z= 10 !assume 10 m for now. TODO: this SHOULD be equal to the height of first model level

    ! get estimate of surface area
    SurfaceArea=dx*dx

    ! PASS forcing variables
    avkdn=SWDOWN1D
    avu1=SQRT(U1D**2+V1D**2)
    Temp_C=T1D-273.15
    Precip=PREC1D

    ! convert unit from Pa to hPa
    Press_hPa=PSFC/100.

    ! estimate relative humidity
    avRh=q2rh(QV1D,T1D,REAL(Press_hPa))*100 !TODO:convert to relative humidity
    avRh=MAX(5.,avRh)

    ! convert data type from real to int
    tstep=INT(DT)
    tstep_prev=INT(DT_PREV)


    ! surface properties:
    ! ! TODO: this should be dynamic
    ! alb=alb0
    ! emis=emis0

    dectime=id-1+(it+imin/60.)/24

    sfr=landusef_suews1d

    OHM_coef_d = OHM_coef_s
    WaterDist_d = WaterDist_s
    AHProf_24hr_d = AHProf_24hr
    HumActivity_24hr_d = HumActivity_24hr
    PopProf_24hr_d = PopProf_24hr
    TraffProf_24hr_d = TraffProf_24hr
    WUProfA_24hr_d = WUProfA_24hr
    WUProfM_24hr_d = WUProfM_24hr

    CALL SuMin(&
         snowUse,EmissionsMethod,NetRadiationMethod,RoughLenHeatMethod,&! model options
         RoughLenMomMethod,StorageHeatMethod,AerodynamicResistanceMethod,OHMIncQF,&! model options
         iy,id,it,imin,isec,dt_since_start,tstep,tstep_prev,startDLS,endDLS,&! time-related input
         alt,lat,lng,Z,timezone,SurfaceArea,sfr,&! site-specific geographical settings
         z0m_in_id,zdm_in_id,&! roughness related settings
         alb_id,emis_id,SnowAlb_id,OHM_coef_d,WaterDist_d,&
         AHProf_24hr_d,HumActivity_24hr_d,PopProf_24hr_d,TraffProf_24hr_d,WUProfA_24hr_d,WUProfM_24hr_d,&
         qn1_av_id,dqndt_id,qn1_s_id,dqnsdt_id,&
         surf_var_id,DecidCap_id,albDecTr_id,albEveTr_id,albGrass_id,porosity_id,&
         GDD_id,HDD_id,LAI_id,WUDay_id,soilmoist_id,state_id,MeltWaterStore_id,&
         avkdn,avRh,avU1,Press_hPa,Temp_C,Precip,& ! forcing variables
         qh,qe,qsfc,tsk,CHKLOWQ)!output

    ! CHKLOWQ_out = 0.02
    ! tsk_out=tsk_out+273.15

  END SUBROUTINE SUEWS1D

  !================================================================
  SUBROUTINE suewsinit(                           &
         lu_state,                                &
         MMINLU,                                  &
         julday,                                  &
         cen_lat,                                 &
         NLCAT,                                   &
         landusef,                                &
         landusef_SUEWS,                          &
         alb_SUEWS,                               &
         emis_SUEWS,                              &
         qn1_av_SUEWS,                            &
         qn1_s_SUEWS,                             &
         dqndt_SUEWS,dqnsdt_SUEWS,                &
         restart, allowed_to_read,                &
         ids,ide, jds,jde, kds,kde,               &
         ims,ime, jms,jme, kms,kme,               &
         its,ite, jts,jte, kts,kte                )
    !----------------------------------------------------------------
    USE module_wrf_error
    IMPLICIT NONE
    !----------------------------------------------------------------
    REAL,DIMENSION(:),INTENT(INOUT)                                 :: lu_state
    CHARACTER(LEN=*),INTENT(IN)                                     :: MMINLU
    INTEGER, INTENT(IN)                                             :: julday
    REAL, INTENT(IN)                                                :: cen_lat
    INTEGER,INTENT(IN)                                              :: NLCAT
    REAL,DIMENSION(ims:ime, NLCAT, jms:jme), INTENT(IN)             :: LANDUSEF
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)                 :: landusef_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)                 :: alb_SUEWS
    REAL,DIMENSION(ims:ime,7,jms:jme),INTENT(INOUT)                 :: emis_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: qn1_av_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: qn1_S_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: dqndt_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: dqnsdt_SUEWS

    INTEGER, INTENT(IN)         :: ids,ide, jds,jde, kds,kde, &
                                   ims,ime, jms,jme, kms,kme, &
                                   its,ite, jts,jte, kts,kte

    LOGICAL, INTENT(IN)         :: restart, allowed_to_read

    !  LOCAR VAR
    INTEGER                            :: i, j, k, itf, jtf
    CHARACTER(len = 1024)              :: message
    LOGICAL, EXTERNAL                  :: wrf_dm_on_monitor
    CHARACTER(len = 256)               :: filename ! local file name
    INTEGER                            :: namelist_unit ! file unit
    INTEGER                            :: ierr ! error code
    INTEGER                            :: ISN
    CHARACTER(len = 256)               :: LUTYPE
    INTEGER                            :: landuse_unit, ls, lc, li, cats, seas, curs
    INTEGER                            :: LUCATS, LUSEAS
    INTEGER , PARAMETER :: max_cats = 100 , max_seas = 12
    REAL    , DIMENSION( max_cats, max_seas ) :: ALBD, SLMO, SFEM, SFZ0, THERIN, SFHC
    REAL    , DIMENSION( max_cats )    :: SCFX
    LOGICAL                            :: found_lu

    REAL,DIMENSION(3,4,8)              :: OHM_coef
    REAL,DIMENSION(6,8)                :: WaterDist

    !----------------------------------------------------------------

    NAMELIST /coeff/   OHM_coef, WaterDist, AHProf_24hr, HumActivity_24hr, PopProf_24hr, TraffProf_24hr, &
                       WUProfA_24hr, WUProfM_24hr
    NAMELIST /method/  snowUse, RoughLenHeatMethod, RoughLenMomMethod, EmissionsMethod, NetRadiationMethod, &
                       StorageHeatMethod, OHMIncQF, AerodynamicResistanceMethod
    NAMELIST /landuse/ suews_cat_ind, suews_cat_frac

    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE SUEWS NAMELIST' )

       IF ( wrf_dm_on_monitor() ) THEN
          filename = 'namelist.suews'
          namelist_unit = 10
          OPEN(namelist_unit, file=filename, form="formatted", status='old', action='read', iostat=ierr)
          IF (ierr /= 0 ) THEN
            WRITE(message, FMT='(A)') 'module_sf_suews.F: suewsinit: open failure for namelist.suews'
            CLOSE(namelist_unit)
            CALL wrf_error_fatal ( message )
          END IF
          READ(namelist_unit, nml=coeff, iostat=ierr)
          IF (ierr /= 0) THEN
            CLOSE(namelist_unit)
            CALL wrf_error_fatal( "ERROR reading sector coeff of namelist.suews" )
          ENDIF
          OHM_coef_s = RESHAPE(OHM_coef, (/8,4,3/), ORDER = (/3,2,1/))
          WaterDist_s = RESHAPE(WaterDist, (/8,6/), ORDER = (/2,1/))
          READ(namelist_unit, nml=method, iostat=ierr)
          IF (ierr /= 0) THEN
            CLOSE(namelist_unit)
            CALL wrf_error_fatal( "ERROR reading sector method of namelist.suews" )
          ENDIF
          suews_cat_ind = ind_undef
          suews_cat_frac = 0.
          READ(namelist_unit, nml=landuse, iostat=ierr)
          IF (ierr /= 0) THEN
            CLOSE(namelist_unit)
            CALL wrf_error_fatal( "ERROR reading sector landuse of namelist.suews" )
          ENDIF
          CLOSE(namelist_unit)
          DO k = 1, 8
             IF ( any(suews_cat_ind(k, :) > NLCAT) ) THEN
               WRITE(message,*) " suews_cat_ind(k, :) > ", NLCAT, " check namelist.suews"
               CALL wrf_error_fatal( message )
             END IF
          END DO
       END IF

!NOTE: All the real type namelist variables are double precision, but only single precision
!      real variables have WORDSIZE in WRF, so if the following lines needed, we shoulde redefine
!      them in single precision and modify the interface in SUEWS1D.
       CALL wrf_dm_bcast_bytes (OHM_coef, 8 * 4 * 3  * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (WaterDist, 8 * 6 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (AHProf_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (HumActivity_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (PopProf_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (TraffProf_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (WUProfA_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (WUProfM_24hr, 24 * 2 * RWORDSIZE)
       CALL wrf_dm_bcast_bytes (snowUse, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (RoughLenHeatMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (RoughLenMomMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (EmissionsMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (NetRadiationMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (StorageHeatMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (OHMIncQF, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (AerodynamicResistanceMethod, IWORDSIZE)
       CALL wrf_dm_bcast_bytes (suews_cat_ind, 8 * 10 * IWORDSIZE)
       CALL wrf_dm_bcast_bytes (suews_cat_frac, 8 * 10 * RWORDSIZE)

    END IF
    print *, 'suews_cat_ind(1) = ', suews_cat_ind(1, :)

    ! initialize suews related table
    IF ( 6*(max_cats*max_seas)+1*max_cats .GT. 7501 ) THEN
      WRITE(message,*)'suewsinit: lu_state overflow. Make Registry dimspec p > ',6*(max_cats*max_seas)+1*max_cats
      CALL wrf_error_fatal ( message )
    END IF
    curs = 1
    DO cats = 1, max_cats
       SCFX(cats) =           lu_state(curs)         ; curs = curs + 1
       DO seas = 1, max_seas
          ALBD(cats,seas) =    lu_state(curs)         ; curs = curs + 1
          SLMO(cats,seas) =    lu_state(curs)         ; curs = curs + 1
          SFEM(cats,seas) =    lu_state(curs)         ; curs = curs + 1
          SFZ0(cats,seas) =    lu_state(curs)         ; curs = curs + 1
          SFHC(cats,seas) =    lu_state(curs)         ; curs = curs + 1
          THERIN(cats,seas) =  lu_state(curs)         ; curs = curs + 1
       END DO
    END DO

    ISN = 1
    IF(JULDAY <105 .OR. JULDAY > 288) ISN = 2
    IF(CEN_LAT < 0.0) ISN = 3 - ISN

    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE SUEWS RELATED TABLE' )
       landuse_unit = 29

       IF ( wrf_dm_on_monitor() ) THEN

          OPEN(landuse_unit, FILE='LANDUSE.TBL', FORM='FORMATTED', STATUS='OLD', IOSTAT=ierr)
          IF ( ierr /= 0 ) THEN
             WRITE(message, FMT='(A)') 'module_sf_suews.F: suewsinit: open failure for LANDUSE.TBL'
             CALL wrf_error_fatal ( message )
          END IF
          FOUND_LU = .FALSE.

          DO

             READ (landuse_unit, *, iostat=ierr) LUTYPE
             IF (ierr /= 0 ) EXIT
             FOUND_LU = LUTYPE == MMINLU

             READ (landuse_unit, *, iostat=ierr) LUCATS, LUSEAS
             IF (ierr /= 0 ) EXIT

             DO ls = 1, LUSEAS
                READ (landuse_unit,*)
                DO lc = 1, LUCATS
                   IF (FOUND_LU) THEN
                      READ (landuse_unit, *) li,ALBD(lc,ls),SLMO(lc,ls),SFEM(lc,ls), &
                           SFZ0(lc,ls),THERIN(lc,ls),SCFX(lc),SFHC(lc,ls)
                      IF (lc /= li) THEN
                         CLOSE (landuse_unit)
                         CALL wrf_error_fatal ( 'module_start: MISSING LANDUSE UNIT ' )
                      END IF
                   ELSE
                      READ (landuse_unit, *)
                   END IF
                END DO
             END DO

             IF (FOUND_LU) EXIT

          END DO

          CLOSE (landuse_unit)

          IF (FOUND_LU) THEN
             IF (LUSEAS == 1) ISN = 1
          ELSE
             CALL wrf_error_fatal ( 'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE: TABLE NOT USED' )
          END IF

          ! rescale loaded values
          albd = albd/100.
          sfz0 = sfz0/100.
          therin = therin/100.

       END IF

       CALL wrf_dm_bcast_bytes (ISN,    IWORDSIZE )
       CALL wrf_dm_bcast_bytes (albd,   max_cats * max_seas * RWORDSIZE )
       CALL wrf_dm_bcast_bytes (slmo,   max_cats * max_seas * RWORDSIZE )
       CALL wrf_dm_bcast_bytes (sfem,   max_cats * max_seas * RWORDSIZE )
       CALL wrf_dm_bcast_bytes (sfz0,   max_cats * max_seas * RWORDSIZE )
       CALL wrf_dm_bcast_bytes (therin, max_cats * max_seas * RWORDSIZE )
       CALL wrf_dm_bcast_bytes (scfx,   max_cats *            RWORDSIZE )
       CALL wrf_dm_bcast_bytes (sfhc,   max_cats * max_seas * RWORDSIZE )

    END IF



    IF (.NOT. restart) THEN

       itf = min0(ite, ide-1)
       jtf = min0(jte, jde-1)

       DO j = jts,jtf
          DO i = its,itf
             qn1_av_SUEWS(i,j) = 0.
             qn1_S_SUEWS(i,j)  = 0.
             dqndt_SUEWS(i,j)  = 0.
             dqnsdt_SUEWS(i,j) = 0.
          ENDDO
       ENDDO

       ! IF (TRIM(MMINLU) == 'MODIFIED_IGBP_MODIS_NOAH') THEN
       !    CALL MODIScat2SUEWScat(its, itf, NLCAT, jts, jtf, landusef(its:itf, :, jts:jtf), ISN, max_cats, max_seas, albd, sfem, landusef_SUEWS(its:itf, :, jts:jtf), alb_SUEWS(its:itf, :, jts:jtf), emis_SUEWS(its:itf, :, jts:jtf))
       ! ELSE IF (TRIM(MMINLU) == 'USGS') THEN
       !    CALL USGScat2SUEWScat(its, itf, NLCAT, jts, jtf, landusef(its:itf, :, jts:jtf), ISN, max_cats, max_seas, albd, sfem, landusef_SUEWS(its:itf, :, jts:jtf), alb_SUEWS(its:itf, :, jts:jtf), emis_SUEWS(its:itf, :, jts:jtf))
       ! ELSE
       !    CALL wrf_error_fatal ('THE Land Use Categories NOT SUPPORTED By SUEWS Model NOW' )
       ! END IF
       CALL toSUEWScat(its, itf, NLCAT, jts, jtf, landusef(its:itf, :, jts:jtf), ISN, max_cats, max_seas, albd, sfem, landusef_SUEWS(its:itf, :, jts:jtf), alb_SUEWS(its:itf, :, jts:jtf), emis_SUEWS(its:itf, :, jts:jtf))

    END IF

  END SUBROUTINE suewsinit
  !-------------------------------------------------------------------

  !----------------------------------------------------------------------
  !    Public subroutine to convert USGS Land Use Categories to SUEWS
  !    surface types
  !----------------------------------------------------------------------
  SUBROUTINE toSUEWScat(its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_SUEWS, alb_SUEWS, emis_SUEWS)

    IMPLICIT NONE

    !------------------------------------------------------------------
    !       dummy arguments
    !------------------------------------------------------------------
    INTEGER, INTENT(in)                ::  its, itf, NLCAT, jts, jtf
    REAL, INTENT(in)                   ::  landusef(its:itf, NLCAT, jts:jtf)
    INTEGER, INTENT(in)                ::  ISN, LUCATS, LUSEAS
    REAL, INTENT(in)                   ::  albd(LUCATS, LUSEAS), sfem(LUCATS, LUSEAS)
    REAL, INTENT(out)                  ::  landusef_SUEWS(its:itf, 7, jts:jtf)
    REAL, INTENT(out)                  ::  alb_SUEWS(its:itf, 7, jts:jtf)
    REAL, INTENT(out)                  ::  emis_SUEWS(its:itf, 7, jts:jtf)

    !------------------------------------------------------------------
    !       local variables
    !------------------------------------------------------------------
    INTEGER                            ::  i, j
    INTEGER                            ::  nind !number of indices
    INTEGER,ALLOCATABLE,DIMENSION(:)   ::  ind  !indices array
    REAL,ALLOCATABLE,DIMENSION(:)      ::  frac !fraction array

    landusef_SUEWS = 0.
    alb_SUEWS = 0.1
    emis_SUEWS = 0.9

    DO i = 1, 7

      nind = count(suews_cat_ind(i,:) /= ind_undef)
      IF (ALLOCATED(ind)) DEALLOCATE(ind)
      IF (ALLOCATED(frac)) DEALLOCATE(frac)
      ALLOCATE(ind(nind), frac(nind))

      ind = pack(suews_cat_ind(i, :), suews_cat_ind(i, :) /= ind_undef)
      frac = pack(suews_cat_frac(i, :), suews_cat_ind(i, :) /= ind_undef)

      call weightMean(nind, ind, frac, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,i,:), alb_suews(:,i,:), emis_suews(:,i,:))
      DEALLOCATE(ind, frac)

    END DO

      ! SELECT CASE( k )
      ! CASE( 1 )
      !   landusef_SUEWS(:, k, :) = landusef(:, 1, :) * 0.5
      !   alb_SUEWS(:, k, :) = albd(1, ISN)
      !   emis_SUEWS(:, k, :) = sfem(1, ISN)
      ! CASE( 2 )
      !   landusef_SUEWS(:, k, :) = landusef(:, 1, :) * 0.5
      !   alb_SUEWS(:, k, :) = albd(1, ISN)
      !   emis_SUEWS(:, k, :) = sfem(1, ISN)
      ! CASE( 3 )
      !   nind = 3
      !   IF (ALLOCATED(ind)) DEALLOCATE(ind)
      !   ALLOCATE(ind(nind))
      !   ind = (/13, 14, 15/)
      !   call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
      !   DEALLOCATE(ind)
      ! CASE( 4 )
      !   nind = 2
      !   IF (ALLOCATED(ind)) DEALLOCATE(ind)
      !   ALLOCATE(ind(nind))
      !   ind = (/11, 12/)
      !   call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
      !   DEALLOCATE(ind)
      ! CASE( 5 )
      !   nind = 9
      !   IF (ALLOCATED(ind)) DEALLOCATE(ind)
      !   ALLOCATE(ind(nind))
      !   ind = (/2, 3, 4, 5, 6, 7, 8, 9, 10/)
      !   call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
      !   DEALLOCATE(ind)
      ! CASE( 6 )
      !   nind = 5
      !   IF (ALLOCATED(ind)) DEALLOCATE(ind)
      !   ALLOCATE(ind(nind))
      !   ind = (/19, 20, 21, 22, 23/)
      !   call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
      !   DEALLOCATE(ind)
      ! CASE( 7 )
      !   nind = 3
      !   IF (ALLOCATED(ind)) DEALLOCATE(ind)
      !   ALLOCATE(ind(nind))
      !   ind = (/16, 17, 18/)
      !   call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
      !   DEALLOCATE(ind)
      ! END SELECT

    ! DO i = its, itf
    !    DO j = jts, jtf
    !       landusef_SUEWS(i, :, j) = 0.
    !       alb_SUEWS(i, :, j) = 0.
    !       emis_SUEWS(i, :, j) = 0.
    !       DO k = 1, 7

             ! SELECT CASE( k )
             ! CASE( 1 )
             !    landusef_SUEWS(i, 1, j) = landusef_SUEWS(i, 1, j) + landusef(i, k, j) * 0.5
             !    landusef_SUEWS(i, 2, j) = landusef_SUEWS(i, 2, j) + landusef(i, k, j) * 0.5
             ! CASE( 2:10 )
             !    landusef_SUEWS(i, 5, j) = landusef_SUEWS(i, 5, j) + landusef(i, k, j)
             ! CASE( 11, 12 )
             !    landusef_SUEWS(i, 4, j) = landusef_SUEWS(i, 4, j) + landusef(i, k, j)
             ! CASE( 13:15 )
             !    landusef_SUEWS(i, 3, j) = landusef_SUEWS(i, 3, j) + landusef(i, k, j)
             ! CASE( 16:18 )
             !    landusef_SUEWS(i, 7, j) = landusef_SUEWS(i, 7, j) + landusef(i, k, j)
             ! CASE( 19:23 )
             !    landusef_SUEWS(i, 6, j) = landusef_SUEWS(i, 6, j) + landusef(i, k, j)
             ! CASE( 24 )
             !    !TODO
             ! END SELECT

    !       END DO
    !    END DO
    ! END DO

  END SUBROUTINE toSUEWScat

  !----------------------------------------------------------------------
  !    Public subroutine to convert MODIS Land Use Categories to SUEWS
  !    surface types
  !----------------------------------------------------------------------
  ! SUBROUTINE MODIScat2SUEWScat(its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_SUEWS, alb_SUEWS, emis_SUEWS)

    ! IMPLICIT NONE

    !------------------------------------------------------------------
    !       dummy arguments
    !------------------------------------------------------------------
    ! INTEGER, INTENT(in)                ::  its, itf, NLCAT, jts, jtf
    ! REAL, INTENT(in)                   ::  landusef(its:itf, NLCAT, jts:jtf)
    ! INTEGER, INTENT(in)                ::  ISN, LUCATS, LUSEAS
    ! REAL, INTENT(in)                   ::  albd(LUCATS, LUSEAS), sfem(LUCATS, LUSEAS)
    ! REAL, INTENT(out)                  ::  landusef_SUEWS(its:itf, 7, jts:jtf)
    ! REAL, INTENT(out)                  ::  alb_SUEWS(its:itf, 7, jts:jtf)
    ! REAL, INTENT(out)                  ::  emis_SUEWS(its:itf, 7, jts:jtf)

    !------------------------------------------------------------------
    !       local variables
    !------------------------------------------------------------------
    ! INTEGER                            ::  k
    ! INTEGER                            ::  nind !number of indices
    ! INTEGER,ALLOCATABLE,DIMENSION(:)   ::  ind  !indices array

    ! landusef_SUEWS = 0.
    ! alb_SUEWS = 0.1
    ! emis_SUEWS = 0.9

    ! DO k = 1, 7

      ! SELECT CASE( k )
      ! CASE( 1 )
        ! landusef_SUEWS(:, k, :) = landusef(:, 13, :) * 0.5
        ! alb_SUEWS(:, k, :) = albd(13, ISN)
        ! emis_SUEWS(:, k, :) = sfem(13, ISN)
      ! CASE( 2 )
        ! landusef_SUEWS(:, k, :) = landusef(:, 13, :) * 0.5
        ! alb_SUEWS(:, k, :) = albd(13, ISN)
        ! emis_SUEWS(:, k, :) = sfem(13, ISN)
      ! CASE( 3 )
        ! nind = 3
        ! IF (ALLOCATED(ind)) DEALLOCATE(ind)
        ! ALLOCATE(ind(nind))
        ! ind = (/1, 2, 5/)
        ! call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
        ! DEALLOCATE(ind)
      ! CASE( 4 )
        ! nind = 2
        ! IF (ALLOCATED(ind)) DEALLOCATE(ind)
        ! ALLOCATE(ind(nind))
        ! ind = (/3, 4/)
        ! call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
        ! DEALLOCATE(ind)
      ! CASE( 5 )
        ! nind = 7
        ! IF (ALLOCATED(ind)) DEALLOCATE(ind)
        ! ALLOCATE(ind(nind))
        ! ind = (/6, 7, 8, 9, 10, 12, 14/)
        ! call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
        ! DEALLOCATE(ind)
      ! CASE( 6 )
        ! nind = 4
        ! IF (ALLOCATED(ind)) DEALLOCATE(ind)
        ! ALLOCATE(ind(nind))
        ! ind = (/16, 18, 19, 20/)
        ! call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
        ! DEALLOCATE(ind)
      ! CASE( 7 )
        ! nind = 2
        ! IF (ALLOCATED(ind)) DEALLOCATE(ind)
        ! ALLOCATE(ind(nind))
        ! ind = (/11, 17/)
        ! call weightMean(nind, ind, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_suews(:,k,:), alb_suews(:,k,:), emis_suews(:,k,:))
        ! DEALLOCATE(ind)
      ! END SELECT

    ! END DO

    ! DO i = its, itf
    !    DO j = jts, jtf
    !      print*, 'WRF landuse',landusef(i, :, j)
    !       landusef_SUEWS(i, :, j) = 0.
    !       alb_SUEWS(i, :, j) = 0.1
    !       emis_SUEWS(i, :, j) = 0.9
    !       DO k = 1, 7

             ! SELECT CASE( k )
             ! CASE( 1, 2, 5 )
             !    landusef_SUEWS(i, 3, j) = landusef_SUEWS(i, 3, j) + landusef(i, k, j)
             ! CASE( 3:4 )
             !    landusef_SUEWS(i, 4, j) = landusef_SUEWS(i, 4, j) + landusef(i, k, j)
             ! CASE( 6:10 )
             !    landusef_SUEWS(i, 5, j) = landusef_SUEWS(i, 5, j) + landusef(i, k, j)
             ! CASE( 11, 17 )
             !    landusef_SUEWS(i, 7, j) = landusef_SUEWS(i, 7, j) + landusef(i, k, j)
             ! CASE( 12, 14 )
             !    landusef_SUEWS(i, 5, j) = landusef_SUEWS(i, 5, j) + landusef(i, k, j)
             ! CASE( 13 )
             !    landusef_SUEWS(i, 1, j) = landusef_SUEWS(i, 1, j) + landusef(i, k, j) * 0.5
             !    landusef_SUEWS(i, 2, j) = landusef_SUEWS(i, 2, j) + landusef(i, k, j) * 0.5
             ! CASE( 15 )
             !    !TODO
             ! CASE( 16 )
             !    landusef_SUEWS(i, 6, j) = landusef_SUEWS(i, 6, j) + landusef(i, k, j)
             ! CASE( 18:20 )
             !    landusef_SUEWS(i, 6, j) = landusef_SUEWS(i, 6, j) + landusef(i, k, j)
             ! END SELECT

    !       END DO
    !       print*, 'now get'
    !       print*, landusef_SUEWS(i, :, j)
    !       print*, alb_SUEWS(i, :, j)
    !       print*, emis_SUEWS(i, :, j)
    !    END DO
    ! END DO

  ! END SUBROUTINE MODIScat2SUEWScat

  SUBROUTINE weightMean(nind, ind, frac, its, itf, NLCAT, jts, jtf, landusef, ISN, LUCATS, LUSEAS, albd, sfem, landusef_SUEWS, alb_SUEWS, emis_SUEWS )

    IMPLICIT NONE

    !------------------------------------------------------------------
    !       dummy arguments
    !------------------------------------------------------------------
    INTEGER, INTENT(in)                ::  nind
    INTEGER, INTENT(in)                ::  ind(nind)
    REAL, INTENT(in)                   ::  frac(nind)
    INTEGER, INTENT(in)                ::  its, itf, NLCAT, jts, jtf
    REAL, INTENT(in)                   ::  landusef(its:itf, NLCAT, jts:jtf)
    INTEGER, INTENT(in)                ::  ISN, LUCATS, LUSEAS
    REAL, INTENT(in)                   ::  albd(LUCATS, LUSEAS), sfem(LUCATS, LUSEAS)
    REAL, INTENT(inout)                ::  landusef_SUEWS(its:itf, jts:jtf)
    REAL, INTENT(inout)                ::  alb_SUEWS(its:itf, jts:jtf)
    REAL, INTENT(inout)                ::  emis_SUEWS(its:itf, jts:jtf)

    !------------------------------------------------------------------
    !       local variables
    !------------------------------------------------------------------
    REAL                               ::  temp(nind)
    INTEGER                            ::  i, j

    DO i = its, itf
       DO j = jts, jtf
          landusef_SUEWS(i, j) = DOT_PRODUCT(landusef(i, ind, j), frac)
          temp = landusef(i, ind, j) * frac
          IF ( landusef_SUEWS(i, j) > 0. ) THEN
             alb_SUEWS(i, j) = DOT_PRODUCT(albd(ind, ISN), temp)/landusef_SUEWS(i, j)
             emis_SUEWS(i, j) = DOT_PRODUCT(sfem(ind, ISN), temp)/landusef_SUEWS(i, j)
          END IF
       END DO
    END DO

  END SUBROUTINE weightMean

  REAL FUNCTION  q2rh(q, Ta, p)
    IMPLICIT NONE
    REAL, INTENT(in)  :: q, Ta ,p
    REAL :: e, es

    es=esat(Ta)
    e=(500*p*q)/(311+189*q)
    q2rh=e/es

  END FUNCTION q2rh


  REAL FUNCTION esat(T)
    IMPLICIT NONE
    REAL, INTENT(in):: T
    REAL :: a, b, c, d, est, f, h, Ts
    a=-7.90298
    b=5.02808
    c=-(1.3816/10**7)
    d=11.344
    est=1013.25
    f=8.1328/10**3
    h=-3.49149
    Ts=373.16

    esat=est*10**(a*(Ts/T-1) &
         + b*LOG10(Ts/T) &
         + c*(10**(d*(1-T/Ts))-1)+ f*(10**(h*(Ts/T-1))-1))

  END FUNCTION esat

END MODULE module_sf_suews
