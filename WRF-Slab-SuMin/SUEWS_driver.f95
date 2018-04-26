!========================================================================================
! SUEWS driver subroutines
! TS 31 Aug 2017: initial version
! TS 02 Oct 2017: added `SUEWS_cal_Main` as the generic wrapper
! TS 03 Oct 2017: added `SUEWS_cal_AnthropogenicEmission`
! TS 24 Apr 2018: a mini version of SUEWS
MODULE SUEWS_Driver
  USE AtmMoist_module,ONLY:LUMPS_cal_AtmMoist,qsatf
  USE NARP_MODULE,ONLY:NARP_cal_SunPosition,RadMethod,NARP
  ! USE AnOHM_module,ONLY:AnOHM
  ! USE ESTM_module,ONLY:ESTM
  ! USE Snow_module,ONLY:SnowCalc,Snow_cal_MeltHeat
  USE DailyState_module,ONLY:SUEWS_cal_DailyState,update_DailyState
  USE WaterDist_module,ONLY:&
       drainage,soilstore,SUEWS_cal_SoilMoist,&
       SUEWS_update_SoilMoist,ReDistributeWater


  IMPLICIT NONE
  INTEGER, PARAMETER:: ndays = 366   !Max no. days in a year used to specify size of daily arrays
  ! ---- Surface types ---------------------------------------------------------------------------
  INTEGER, PARAMETER:: nsurf=7                !Total number of surfaces
  INTEGER, PARAMETER:: NVegSurf=3             !Number of surfaces that are vegetated
  INTEGER, PARAMETER:: nsurfIncSnow=nsurf+1   !Number of surfaces + snow

  INTEGER,PARAMETER:: PavSurf   = 1,&   !When all surfaces considered together (1-7)
       BldgSurf  = 2,&
       ConifSurf = 3,&
       DecidSurf = 4,&
       GrassSurf = 5,&   !New surface classes: Grass = 5th/7 surfaces
       BSoilSurf = 6,&   !New surface classes: Bare soil = 6th/7 surfaces
       WaterSurf = 7,&
       ExcessSurf= 8,&   !Runoff or subsurface soil in WGWaterDist
       NSurfDoNotReceiveDrainage=0,&   !Number of surfaces that do not receive drainage water (green roof)
       ivConif = 1,&     !When only vegetated surfaces considered (1-3)
       ivDecid = 2,&
       ivGrass = 3

  ! ---- Set number of columns in output files ---------------------------------------------------
  INTEGER, PARAMETER:: ncolumnsDataOutSUEWS=84,&    !Main output file (_5.txt). dataOutSUEWS created in SUEWS_Calculations.f95
       ncolumnsDataOutSnow=102,&
       ncolumnsdataOutSOL=31,&
       ncolumnsdataOutBL=22,&
       ncolumnsDataOutESTM=32,&
       ncolumnsDataOutDailyState=46



CONTAINS
  ! ===================MAIN CALCULATION WRAPPER FOR ENERGY AND WATER FLUX===========
  SUBROUTINE SUEWS_cal_Main(&
       AerodynamicResistanceMethod,AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,& ! input&inout in alphabetical order
       AH_SLOPE_Heating,alb,albDecTr,albEveTr,albGrass,alBMax_DecTr,&
       alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       alpha_bioCO2,alpha_enh_bioCO2,alt,avkdn,avRh,avU1,BaseT,BaseTe,&
       BaseTHDD,beta_bioCO2,beta_enh_bioCO2,bldgH,CapMax_dec,CapMin_dec,&
       chAnOHM,cpAnOHM,CRWmax,CRWmin,DayWat,DayWatPer,&
       DecidCap,dectime,DecTreeH,Diagnose,DiagQN,DiagQS,DRAINRT,&
       EF_umolCO2perJ,emis,EmissionsMethod,EnEF_v_Jkm,endDLS,EveTreeH,FAIBldg,&
       FAIDecTree,FAIEveTree,Faut,FcEF_v_kgkm,fcld_obs,FlowChange,&
       FrFossilFuel_Heat,FrFossilFuel_NonHeat,G1,G2,G3,G4,G5,G6,GDD,&
       GDDFull,Gridiv,gsModel,HDD,HumActivity_tstep,&
       IceFrac,id,id_prev_t,Ie_a,Ie_end,Ie_m,Ie_start,imin,&
       InternalWaterUse_h,IrrFracConif,IrrFracDecid,IrrFracGrass,it,ity,&
       iy,iy_prev_t,kkAnOHM,Kmax,LAI,LAICalcYes,LAIMax,LAIMin,LAI_obs,&
       LAIPower,LAIType,lat,ldown_obs,lng,MaxConductance,MaxQFMetab,&
       MeltWaterStore,MetForcingData_grid,MinQFMetab,min_res_bioCO2,&
       NARP_EMIS_SNOW,NARP_TRANS_SITE,NetRadiationMethod,&
       NumCapita,OHM_coef,OHMIncQF,OHM_threshSW,&
       OHM_threshWD,PipeCapacity,PopDensDaytime,&
       PopDensNighttime,PopProf_tstep,PorMax_dec,PorMin_dec,porosity,&
       Precip,PrecipLimit,PrecipLimitAlb,Press_hPa,QF0_BEU,Qf_A,Qf_B,&
       Qf_C,qh_obs,qn1_av_store,qn1_obs,qn1_S_av_store,qn1_S_store,&
       qn1_store,RadMeltFact,RAINCOVER,RainMaxRes,resp_a,resp_b,&
       RoughLenHeatMethod,RoughLenMomMethod,RunoffToWater,S1,S2,&
       SatHydraulicConduct,SDDFull,sfr,SMDMethod,SnowAlb,SnowAlbMax,&
       SnowAlbMin,snowD,SnowDens,SnowDensMax,SnowDensMin,SnowfallCum,snowFrac,&
       SnowLimBuild,SnowLimPaved,snow_obs,SnowPack,SnowProf,snowUse,SoilDepth,&
       soilmoist,soilstoreCap,StabilityMethod,startDLS,state,StateLimit,&
       StorageHeatMethod,surf,SurfaceArea,Tair24HR,tau_a,tau_f,tau_r,&
       T_CRITIC_Cooling,T_CRITIC_Heating,Temp_C,TempMeltFact,TH,&
       theta_bioCO2,timezone,TL,TrafficRate,TrafficUnits,&
       TraffProf_tstep,Ts5mindata_ir,tstep,veg_type,&
       WaterDist,WaterUseMethod,WetThresh,WU_Day,WUProfA_tstep,&
       WUProfM_tstep,xsmd,Z,&
       datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&!output
       DailyStateLine)!output

    IMPLICIT NONE

    INTEGER,INTENT(IN)::AerodynamicResistanceMethod
    INTEGER,INTENT(IN)::Diagnose
    INTEGER,INTENT(IN)::DiagQN
    INTEGER,INTENT(IN)::DiagQS
    INTEGER,INTENT(IN)::startDLS
    INTEGER,INTENT(IN)::endDLS
    INTEGER,INTENT(IN)::EmissionsMethod
    INTEGER,INTENT(IN)::Gridiv
    INTEGER,INTENT(IN)::gsModel
    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::id_prev_t
    INTEGER,INTENT(IN)::Ie_end
    INTEGER,INTENT(IN)::Ie_start
    INTEGER,INTENT(IN)::imin
    INTEGER,INTENT(IN)::it
    INTEGER,INTENT(IN)::ity
    INTEGER,INTENT(IN)::iy
    INTEGER,INTENT(IN)::iy_prev_t
    INTEGER,INTENT(IN)::LAICalcYes
    INTEGER,INTENT(IN)::NetRadiationMethod
    INTEGER,INTENT(IN)::OHMIncQF
    INTEGER,INTENT(IN)::RoughLenHeatMethod
    INTEGER,INTENT(IN)::RoughLenMomMethod
    INTEGER,INTENT(IN)::SMDMethod
    INTEGER,INTENT(IN)::snowUse
    INTEGER,INTENT(IN)::StabilityMethod
    INTEGER,INTENT(IN)::StorageHeatMethod
    INTEGER,INTENT(IN)::tstep
    INTEGER,INTENT(IN)::veg_type
    INTEGER,INTENT(IN)::WaterUseMethod

    REAL(KIND(1D0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1D0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1D0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1D0)),INTENT(IN)::alt
    REAL(KIND(1D0)),INTENT(IN)::avkdn
    REAL(KIND(1D0)),INTENT(IN)::avRh
    REAL(KIND(1D0)),INTENT(IN)::avU1
    REAL(KIND(1D0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1D0)),INTENT(IN)::bldgH
    REAL(KIND(1D0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1D0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1D0)),INTENT(IN)::CRWmax
    REAL(KIND(1D0)),INTENT(IN)::CRWmin
    REAL(KIND(1D0)),INTENT(IN)::dectime
    REAL(KIND(1D0)),INTENT(IN)::DecTreeH
    REAL(KIND(1D0)),INTENT(IN)::DRAINRT
    REAL(KIND(1D0)),INTENT(IN)::EF_umolCO2perJ
    REAL(KIND(1D0)),INTENT(IN)::EnEF_v_Jkm
    REAL(KIND(1D0)),INTENT(IN)::EveTreeH
    REAL(KIND(1D0)),INTENT(IN)::FAIBldg
    REAL(KIND(1D0)),INTENT(IN)::FAIDecTree
    REAL(KIND(1D0)),INTENT(IN)::FAIEveTree
    REAL(KIND(1D0)),INTENT(IN)::Faut
    REAL(KIND(1D0)),INTENT(IN)::FcEF_v_kgkm
    REAL(KIND(1D0)),INTENT(IN)::fcld_obs
    REAL(KIND(1D0)),INTENT(IN)::FlowChange
    REAL(KIND(1D0)),INTENT(IN)::FrFossilFuel_Heat
    REAL(KIND(1D0)),INTENT(IN)::FrFossilFuel_NonHeat
    REAL(KIND(1D0)),INTENT(IN)::G1
    REAL(KIND(1D0)),INTENT(IN)::G2
    REAL(KIND(1D0)),INTENT(IN)::G3
    REAL(KIND(1D0)),INTENT(IN)::G4
    REAL(KIND(1D0)),INTENT(IN)::G5
    REAL(KIND(1D0)),INTENT(IN)::G6
    REAL(KIND(1D0)),INTENT(IN)::InternalWaterUse_h
    REAL(KIND(1D0)),INTENT(IN)::IrrFracConif
    REAL(KIND(1D0)),INTENT(IN)::IrrFracDecid
    REAL(KIND(1D0)),INTENT(IN)::IrrFracGrass
    REAL(KIND(1D0)),INTENT(IN)::Kmax
    REAL(KIND(1D0)),INTENT(IN)::LAI_obs
    REAL(KIND(1D0)),INTENT(IN)::lat
    REAL(KIND(1D0)),INTENT(IN)::ldown_obs
    REAL(KIND(1D0)),INTENT(IN)::lng
    REAL(KIND(1D0)),INTENT(IN)::MaxQFMetab
    REAL(KIND(1D0)),INTENT(IN)::MinQFMetab
    REAL(KIND(1D0)),INTENT(IN)::NARP_EMIS_SNOW
    REAL(KIND(1D0)),INTENT(IN)::NARP_TRANS_SITE
    REAL(KIND(1D0)),INTENT(IN)::NumCapita
    REAL(KIND(1D0)),INTENT(IN)::PipeCapacity
    REAL(KIND(1D0)),INTENT(IN)::PopDensDaytime
    REAL(KIND(1D0)),INTENT(IN)::PopDensNighttime
    REAL(KIND(1D0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1D0)),INTENT(IN)::PorMin_dec
    REAL(KIND(1D0)),INTENT(IN)::Precip
    REAL(KIND(1D0)),INTENT(IN)::PrecipLimit
    REAL(KIND(1D0)),INTENT(IN)::PrecipLimitAlb
    REAL(KIND(1D0)),INTENT(IN)::Press_hPa
    REAL(KIND(1D0)),INTENT(IN)::qh_obs
    REAL(KIND(1D0)),INTENT(IN)::qn1_obs
    REAL(KIND(1D0)),INTENT(IN)::RadMeltFact
    REAL(KIND(1D0)),INTENT(IN)::RAINCOVER
    REAL(KIND(1D0)),INTENT(IN)::RainMaxRes
    REAL(KIND(1D0)),INTENT(IN)::RunoffToWater
    REAL(KIND(1D0)),INTENT(IN)::S1
    REAL(KIND(1D0)),INTENT(IN)::S2
    REAL(KIND(1D0)),INTENT(IN)::SnowAlbMax
    REAL(KIND(1D0)),INTENT(IN)::SnowAlbMin
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMax
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMin
    REAL(KIND(1D0)),INTENT(IN)::SnowLimBuild
    REAL(KIND(1D0)),INTENT(IN)::SnowLimPaved
    REAL(KIND(1D0)),INTENT(IN)::snow_obs
    REAL(KIND(1D0)),INTENT(IN)::SurfaceArea
    REAL(KIND(1D0)),INTENT(IN)::tau_a
    REAL(KIND(1D0)),INTENT(IN)::tau_f
    REAL(KIND(1D0)),INTENT(IN)::tau_r
    REAL(KIND(1D0)),INTENT(IN)::Temp_C
    REAL(KIND(1D0)),INTENT(IN)::TempMeltFact
    REAL(KIND(1D0)),INTENT(IN)::TH
    REAL(KIND(1D0)),INTENT(IN)::timezone
    REAL(KIND(1D0)),INTENT(IN)::TL
    REAL(KIND(1D0)),INTENT(IN)::TrafficUnits
    REAL(KIND(1D0)),INTENT(IN)::xsmd
    REAL(KIND(1D0)),INTENT(IN)::Z

    INTEGER,DIMENSION(NVEGSURF),INTENT(IN)::LAIType

    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::AH_MIN
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::AH_SLOPE_Cooling
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::AH_SLOPE_Heating
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::QF0_BEU
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::Qf_A
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::Qf_B
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::Qf_C
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::T_CRITIC_Cooling
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::T_CRITIC_Heating
    REAL(KIND(1D0)),DIMENSION(2),INTENT(IN)               ::TrafficRate
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN)               ::Ie_a
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN)               ::Ie_m
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN)               ::MaxConductance
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::AHProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::HumActivity_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::PopProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::TraffProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::WUProfA_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2),INTENT(IN) ::WUProfM_tstep
    REAL(KIND(1D0)),DIMENSION(7),INTENT(IN)               ::DayWat
    REAL(KIND(1D0)),DIMENSION(7),INTENT(IN)               ::DayWatPer
    REAL(KIND(1D0)),DIMENSION(nsurf+1),INTENT(IN)         ::OHM_threshSW
    REAL(KIND(1D0)),DIMENSION(nsurf+1),INTENT(IN)         ::OHM_threshWD
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::chAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::cpAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::emis
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::kkAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::SatHydraulicConduct
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::sfr
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::snowD
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::SoilDepth
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::soilstoreCap
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::StateLimit
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::WetThresh
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::alpha_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::alpha_enh_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::BaseT
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::BaseTe
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::beta_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::beta_enh_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::GDDFull
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::LAIMax
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::LAIMin
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::min_res_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::resp_a
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::resp_b
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::SDDFull
    REAL(KIND(1D0)),DIMENSION(0:23,2),INTENT(in)          ::snowProf
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::theta_bioCO2
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in)               ::Ts5mindata_ir
    REAL(KIND(1D0)),DIMENSION(NSURF+1,NSURF-1),INTENT(IN) ::WaterDist
    REAL(KIND(1D0)),DIMENSION(nsurf+1,4,3),INTENT(IN)     ::OHM_coef
    REAL(KIND(1D0)),DIMENSION(4,NVEGSURF),INTENT(IN)      ::LAIPower
    REAL(KIND(1D0)),DIMENSION(:,:),INTENT(IN)             ::MetForcingData_grid

    REAL(KIND(1D0)),INTENT(INOUT) ::SnowfallCum
    REAL(KIND(1D0)),INTENT(INOUT)                             ::SnowAlb
    ! INTEGER,DIMENSION(0:NDAYS,3),INTENT(INOUT)                ::DayofWeek
    REAL(KIND(1d0)),DIMENSION(24*3600/tstep),INTENT(inout)    ::Tair24HR
    REAL(KIND(1D0)),DIMENSION(2*3600/tstep+1),INTENT(INOUT)   ::qn1_av_store
    REAL(KIND(1D0)),DIMENSION(2*3600/tstep+1),INTENT(INOUT)   ::qn1_S_av_store
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albDecTr
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albEveTr
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albGrass
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::DecidCap
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::porosity
    REAL(KIND(1D0)),DIMENSION(0:NDAYS,5),INTENT(INOUT)        ::GDD
    REAL(KIND(1D0)),DIMENSION(0:NDAYS,9),INTENT(INOUT)        ::WU_Day
    REAL(KIND(1D0)),DIMENSION(6,NSURF),INTENT(INOUT)          ::surf
    REAL(KIND(1D0)),DIMENSION(-4:NDAYS,6),INTENT(INOUT)       ::HDD
    REAL(KIND(1D0)),DIMENSION(-4:NDAYS,NVEGSURF),INTENT(INOUT)::LAI
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::alb
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::IceFrac
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::MeltWaterStore
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::SnowDens
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::snowFrac
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::SnowPack
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::soilmoist
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::state
    REAL(KIND(1D0)),DIMENSION(3600/tstep),INTENT(INOUT)       ::qn1_S_store
    REAL(KIND(1D0)),DIMENSION(3600/tstep),INTENT(INOUT)       ::qn1_store

    REAL(KIND(1D0)),DIMENSION(5),INTENT(OUT)                           ::datetimeLine
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSUEWS-5),INTENT(OUT)      ::dataOutLineSUEWS
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(OUT)       ::dataOutLineSnow
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(OUT)       ::dataOutLineESTM
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5),INTENT(OUT) ::DailyStateLine

    REAL(KIND(1D0))::a1
    REAL(KIND(1D0))::a2
    REAL(KIND(1D0))::a3
    REAL(KIND(1D0))::AdditionalWater
    REAL(KIND(1D0))::avU10_ms
    REAL(KIND(1D0))::azimuth
    REAL(KIND(1D0))::chSnow_per_interval

    REAL(KIND(1D0))::dens_dry
    REAL(KIND(1d0))::deltaLAI
    REAL(KIND(1D0))::drain_per_tstep
    REAL(KIND(1D0))::Ea_hPa
    REAL(KIND(1D0))::E_mod
    REAL(KIND(1D0))::es_hPa
    REAL(KIND(1D0))::ev
    REAL(KIND(1D0))::ev_per_tstep
    REAL(KIND(1D0))::ext_wu
    REAL(KIND(1D0))::Fc
    REAL(KIND(1D0))::Fc_anthro
    REAL(KIND(1D0))::Fc_biogen
    REAL(KIND(1D0))::Fc_build
    REAL(KIND(1D0))::fcld
    REAL(KIND(1D0))::Fc_metab
    REAL(KIND(1D0))::Fc_photo
    REAL(KIND(1D0))::Fc_respi
    REAL(KIND(1D0))::Fc_traff
    REAL(KIND(1D0))::fwh
    REAL(KIND(1D0))::gsc
    REAL(KIND(1D0))::H_mod
    REAL(KIND(1D0))::int_wu
    REAL(KIND(1D0))::kclear
    REAL(KIND(1D0))::kup
    REAL(KIND(1D0))::ldown
    REAL(KIND(1D0))::lup
    REAL(KIND(1D0))::L_mod
    REAL(KIND(1D0))::mwh
    REAL(KIND(1D0))::mwstore
    REAL(KIND(1D0))::NWstate_per_tstep
    REAL(KIND(1D0))::planF
    REAL(KIND(1D0))::p_mm
    REAL(KIND(1D0))::psim
    REAL(KIND(1D0))::q2_gkg
    REAL(KIND(1D0))::qeOut
    REAL(KIND(1D0))::qe_per_tstep
    REAL(KIND(1D0))::qf
    REAL(KIND(1D0))::QF_SAHP
    REAL(KIND(1D0))::qh
    REAL(KIND(1D0))::qh_r
    REAL(KIND(1D0))::Qm
    REAL(KIND(1D0))::QmFreez
    REAL(KIND(1D0))::QmRain
    REAL(KIND(1D0))::qn1
    REAL(KIND(1D0))::qn1_S
    REAL(KIND(1D0))::qn1_SF
    REAL(KIND(1D0))::qs
    REAL(KIND(1D0))::RA
    REAL(KIND(1D0))::ResistSurf
    REAL(KIND(1D0))::rss
    REAL(KIND(1d0))::runoffAGveg
    REAL(KIND(1d0))::runoffAGimpervious
    REAL(KIND(1D0))::runoff_per_tstep
    REAL(KIND(1D0))::runoffPipes
    REAL(KIND(1D0))::runoffPipes_m3
    REAL(KIND(1D0))::runoffSoil_per_tstep
    REAL(KIND(1D0))::runoffwaterbody
    REAL(KIND(1D0))::runoffWaterBody_m3
    REAL(KIND(1D0))::smd
    REAL(KIND(1D0))::SoilState
    REAL(KIND(1D0))::state_per_tstep
    REAL(KIND(1D0))::surf_chang_per_tstep
    REAL(KIND(1D0))::swe
    REAL(KIND(1D0))::t2_C
    REAL(KIND(1D0))::TempVeg
    REAL(KIND(1D0))::tot_chang_per_tstep
    REAL(KIND(1D0))::Tstar
    REAL(KIND(1D0))::tsurf
    REAL(KIND(1D0))::UStar
    REAL(KIND(1D0))::VPD_Pa
    REAL(KIND(1D0))::WUAreaDecTr_m2
    REAL(KIND(1D0))::WUAreaEveTr_m2
    REAL(KIND(1D0))::WUAreaGrass_m2
    REAL(KIND(1D0))::WUAreaTotal_m2
    REAL(KIND(1D0))::wu_DecTr
    REAL(KIND(1D0))::wu_EveTr
    REAL(KIND(1D0))::wu_Grass
    REAL(KIND(1D0))::wu_m3
    REAL(KIND(1D0))::Z0m
    REAL(KIND(1D0))::Zdm
    REAL(KIND(1D0))::ZENITH_deg
    REAL(KIND(1D0))::Zh


    REAL(KIND(1D0)),DIMENSION(2)::SnowRemoval
    REAL(KIND(1D0)),DIMENSION(NSURF)::chang
    REAL(KIND(1D0)),DIMENSION(NSURF)::changSnow
    REAL(KIND(1D0)),DIMENSION(NSURF)::evap
    REAL(KIND(1D0)),DIMENSION(NSURF)::ev_snow
    REAL(KIND(1D0)),DIMENSION(NSURF)::FreezMelt
    REAL(KIND(1d0)),DIMENSION(nsurf)::kup_ind_snow
    REAL(KIND(1D0)),DIMENSION(NSURF)::mw_ind
    REAL(KIND(1D0)),DIMENSION(NSURF)::Qm_freezState
    REAL(KIND(1D0)),DIMENSION(NSURF)::Qm_melt
    REAL(KIND(1D0)),DIMENSION(NSURF)::Qm_rain
    REAL(KIND(1D0)),DIMENSION(NSURF)::qn1_ind_snow
    REAL(KIND(1D0)),DIMENSION(NSURF)::rainOnSnow
    REAL(KIND(1D0)),DIMENSION(NSURF)::rss_nsurf
    REAL(KIND(1D0)),DIMENSION(NSURF)::runoff
    REAL(KIND(1D0)),DIMENSION(NSURF)::runoffSnow
    REAL(KIND(1D0)),DIMENSION(NSURF)::runoffSoil
    REAL(KIND(1D0)),DIMENSION(NSURF)::smd_nsurf
    REAL(KIND(1D0)),DIMENSION(NSURF)::SnowToSurf
    REAL(KIND(1D0)),DIMENSION(NSURF)::snowDepth

    REAL(KIND(1d0)),DIMENSION(nsurf)::Tsurf_ind_snow

    INTEGER,DIMENSION(NSURF)::snowCalcSwitch
    INTEGER,DIMENSION(3)    ::dayofWeek_id
    INTEGER::DLS

    REAL(KIND(1D0))::avcp
    REAL(KIND(1D0))::avdens
    REAL(KIND(1D0))::dq
    REAL(KIND(1D0))::lv_J_kg
    REAL(KIND(1D0))::lvS_J_kg
    REAL(KIND(1D0))::psyc_hPa
    REAL(KIND(1D0))::qe
    REAL(KIND(1D0))::RAsnow
    REAL(KIND(1D0))::rb
    REAL(KIND(1D0))::runoff_per_interval
    REAL(KIND(1D0))::s_hPa
    REAL(KIND(1D0))::sIce_hpa
    REAL(KIND(1D0))::SoilMoistCap
    REAL(KIND(1D0))::veg_fr
    REAL(KIND(1D0))::VegPhenLumps
    REAL(KIND(1D0))::VPd_hpa
    REAL(KIND(1D0))::vsmd
    REAL(KIND(1D0))::ZZD

    REAL(KIND(1D0)),DIMENSION(NSURF)::deltaQi
    REAL(KIND(1D0)),DIMENSION(NSURF)::drain
    REAL(KIND(1D0)),DIMENSION(NSURF)::FreezState
    REAL(KIND(1D0)),DIMENSION(NSURF)::FreezStateVol
    REAL(KIND(1D0)),DIMENSION(NSURF)::soilmoistOld
    REAL(KIND(1D0)),DIMENSION(NSURF)::stateOld
    REAL(KIND(1D0)),DIMENSION(NSURF)::tsurf_ind

    ! TODO: TS 25 Oct 2017
    ! the `add-*` variables are not used currently as grid-to-grid connection is NOT set up.
    ! set these variables as zero.
    REAL(KIND(1D0))::addImpervious=0
    REAL(KIND(1D0))::addPipes=0
    REAL(KIND(1D0))::addVeg=0
    REAL(KIND(1D0))::addWaterBody=0
    REAL(KIND(1D0)),DIMENSION(NSURF)::AddWater=0
    REAL(KIND(1D0)),DIMENSION(NSURF)::AddWaterRunoff=0

    ! values that are derived from tstep
    INTEGER::nsh ! number of timesteps per hour
    REAL(KIND(1D0))::nsh_real ! nsh in type real
    REAL(KIND(1D0))::tstep_real ! tstep in type real

    ! values that are derived from sfr (surface fractions)
    REAL(KIND(1D0))::VegFraction
    REAL(KIND(1D0))::ImpervFraction
    REAL(KIND(1D0))::PervFraction
    REAL(KIND(1D0))::NonWaterFraction

    ! calculate tstep related VARIABLES
    CALL SUEWS_cal_tstep(&
         tstep,& ! input
         nsh, nsh_real, tstep_real) ! output

    ! calculate surface fraction related VARIABLES
    CALL SUEWS_cal_surf(&
         sfr,& !input
         VegFraction,ImpervFraction,PervFraction,NonWaterFraction) ! output

    ! calculate dayofweek information
    CALL SUEWS_cal_weekday(&
         iy,id,lat,& !input
         dayofWeek_id) !output

    ! calculate dayofweek information
    CALL SUEWS_cal_DLS(&
         id,startDLS,endDLS,& !input
         DLS) !output


    !==============main calculation start=======================
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_RoughnessParameters...'
    ! CALL SUEWS_cal_RoughnessParameters(Gridiv) ! Added by HCW 11 Nov 2014
    CALL SUEWS_cal_RoughnessParameters(&
         RoughLenMomMethod,sfr,&!input
         bldgH,EveTreeH,DecTreeH,&
         porosity(id),FAIBldg,FAIEveTree,FAIDecTree,Z,&
         planF,&!output
         Zh,Z0m,Zdm,ZZD)

    ! Calculate sun position
    IF(Diagnose==1) WRITE(*,*) 'Calling NARP_cal_SunPosition...'
    CALL NARP_cal_SunPosition(&
         REAL(iy,KIND(1d0)),&!input:
         dectime-tstep/2,&! sun position at middle of timestep before
         timezone,lat,lng,alt,&
         azimuth,zenith_deg)!output:


    !Call the SUEWS_cal_DailyState routine to get surface characteristics ready
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_DailyState...'
    CALL SUEWS_cal_DailyState(&
         iy,id,it,imin,tstep,DayofWeek_id,&!input
         WaterUseMethod,snowUse,Ie_start,Ie_end,&
         LAICalcYes,LAIType,&
         nsh_real,avkdn,Temp_C,Precip,BaseTHDD,&
         lat,Faut,LAI_obs,tau_a,tau_f,tau_r,&
         SnowDensMax,SnowDensMin,SnowAlbMin,&
         alBMax_DecTr,alBMax_EveTr,alBMax_Grass,&
         AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
         CapMax_dec,CapMin_dec,PorMax_dec,PorMin_dec,&
         Ie_a,Ie_m,DayWatPer,DayWat,SnowPack,&
         BaseT,BaseTe,GDDFull,SDDFull,LAIMin,LAIMax,LAIPower,&
         SnowAlb,DecidCap,albDecTr,albEveTr,albGrass,&!inout
         porosity,GDD,HDD,SnowDens,LAI,WU_Day,&
         deltaLAI)!output


    !Calculation of density and other water related parameters
    IF(Diagnose==1) WRITE(*,*) 'Calling LUMPS_cal_AtmMoist...'
    CALL LUMPS_cal_AtmMoist(&
         Temp_C,Press_hPa,avRh,dectime,&! input:
         lv_J_kg,lvS_J_kg,&! output:
         es_hPa,Ea_hPa,VPd_hpa,VPD_Pa,dq,dens_dry,avcp,avdens)


    !======== Calculate soil moisture =========
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_update_SoilMoist...'
    CALL SUEWS_update_SoilMoist(&
         NonWaterFraction,&!input
         soilstoreCap,sfr,soilmoist,&
         SoilMoistCap,SoilState,&!output
         vsmd,smd)


    ! ===================NET ALLWAVE RADIATION================================
    CALL SUEWS_cal_Qn(&
         NetRadiationMethod,snowUse,id,&!input
         Diagnose,snow_obs,ldown_obs,fcld_obs,&
         dectime,ZENITH_deg,avKdn,Temp_C,avRH,Ea_hPa,qn1_obs,&
         SnowAlb,DiagQN,&
         NARP_TRANS_SITE,NARP_EMIS_SNOW,IceFrac,sfr,emis,&
         alb,albDecTr,DecidCap,albEveTr,albGrass,surf,&!inout
         snowFrac,ldown,fcld,&!output
         qn1,qn1_SF,qn1_S,kclear,kup,lup,tsurf,&
         qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)


    ! ===================ANTHROPOGENIC HEAT FLUX================================
    CALL SUEWS_cal_AnthropogenicEmission(&
         AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,AH_SLOPE_Heating,alpha_bioCO2,&
         alpha_enh_bioCO2,avkdn,beta_bioCO2,beta_enh_bioCO2,DayofWeek_id,&
         Diagnose,DLS,EF_umolCO2perJ,EmissionsMethod,EnEF_v_Jkm,Fc,Fc_anthro,Fc_biogen,&
         Fc_build,FcEF_v_kgkm,Fc_metab,Fc_photo,Fc_respi,Fc_traff,FrFossilFuel_Heat,&
         FrFossilFuel_NonHeat,HDD,HumActivity_tstep,id,imin,it,LAI, LaiMax,LaiMin,&
         MaxQFMetab,MinQFMetab,min_res_bioCO2,nsh,NumCapita,&
         PopDensDaytime,PopDensNighttime,PopProf_tstep,QF,QF0_BEU,Qf_A,Qf_B,Qf_C,QF_SAHP,&
         resp_a,resp_b,sfr,snowFrac,T_CRITIC_Cooling,T_CRITIC_Heating,Temp_C,&
         theta_bioCO2,TrafficRate,TrafficUnits,TraffProf_tstep)


    ! =================STORAGE HEAT FLUX=======================================
    CALL SUEWS_cal_Qs(&
         StorageHeatMethod,OHMIncQF,Gridiv,&!input
         id,tstep,Diagnose,sfr,&
         OHM_coef,OHM_threshSW,OHM_threshWD,&
         soilmoist,soilstoreCap,state,nsh,SnowUse,DiagQS,&
         HDD,MetForcingData_grid,Ts5mindata_ir,qf,qn1,&
         avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
         bldgh,alb,emis,cpAnOHM,kkAnOHM,chAnOHM,EmissionsMethod,&
         Tair24HR,qn1_store,qn1_S_store,&!inout
         qn1_av_store,qn1_S_av_store,surf,&
         qn1_S,snowFrac,dataOutLineESTM,qs,&!output
         deltaQi,a1,a2,a3)


    !==================Energy related to snow melting/freezing processes=======
    IF(Diagnose==1) WRITE(*,*) 'Calling MeltHeat'
    CALL Snow_cal_MeltHeat(&
         snowUse,&!input
         lvS_J_kg,lv_J_kg,tstep_real,RadMeltFact,TempMeltFact,SnowAlbMax,&
         SnowDensMin,Temp_C,Precip,PrecipLimit,PrecipLimitAlb,&
         nsh_real,sfr,Tsurf_ind,Tsurf_ind_snow,state,qn1_ind_snow,&
         kup_ind_snow,Meltwaterstore,deltaQi,&
         SnowPack,snowFrac,SnowAlb,SnowDens,SnowfallCum,&!inout
         mwh,fwh,Qm,QmFreez,QmRain,&! output
         veg_fr,snowCalcSwitch,Qm_melt,Qm_freezState,Qm_rain,FreezMelt,&
         FreezState,FreezStateVol,rainOnSnow,SnowDepth,mw_ind,&
         dataOutLineSnow)!output



    !==========================Turbulent Fluxes================================
    IF(Diagnose==1) WRITE(*,*) 'Calling LUMPS_cal_QHQE...'
    !Calculate QH and QE from LUMPS
    CALL LUMPS_cal_QHQE(&
         veg_type,& !input
         snowUse,id,qn1,qf,qs,Qm,Temp_C,Veg_Fr,avcp,Press_hPa,lv_J_kg,&
         tstep_real,DRAINRT,nsh_real,&
         Precip,RainMaxRes,RAINCOVER,sfr,LAI,LAImax,LAImin,&
         H_mod,E_mod,psyc_hPa,s_hPa,sIce_hpa,TempVeg,VegPhenLumps)!output


    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_WaterUse...'
    !Gives the external and internal water uses per timestep
    CALL SUEWS_cal_WaterUse(&
         nsh_real,& ! input:
         SurfaceArea,sfr,&
         IrrFracConif,IrrFracDecid,IrrFracGrass,&
         dayofWeek_id,WUProfA_tstep,WUProfM_tstep,&
         InternalWaterUse_h,HDD(id-1,:),WU_Day(id-1,:),&
         WaterUseMethod,NSH,it,imin,DLS,&
         WUAreaEveTr_m2,WUAreaDecTr_m2,& ! output:
         WUAreaGrass_m2,WUAreaTotal_m2,&
         wu_EveTr,wu_DecTr,wu_Grass,wu_m3,int_wu,ext_wu)


    !===============Resistance Calculations=======================
    CALL SUEWS_cal_Resistance(&
         StabilityMethod,&!input:
         Diagnose,AerodynamicResistanceMethod,RoughLenHeatMethod,snowUse,&
         id,it,gsModel,SMDMethod,&
         qh_obs,avdens,avcp,h_mod,qn1,dectime,zzd,z0M,zdm,&
         avU1,Temp_C,UStar,VegFraction,avkdn,&
         Kmax,&
         g1,g2,g3,g4,&
         g5,g6,s1,s2,&
         th,tl,&
         dq,xsmd,vsmd,MaxConductance,LAIMax,LAI(id-1,:),snowFrac,sfr,&
         Tstar,L_mod,&!output
         psim,gsc,ResistSurf,RA,RAsnow,rb)


    !============= calculate water balance =============
    CALL SUEWS_cal_Water(&
         Diagnose,&!input
         snowUse,NonWaterFraction,addPipes,addImpervious,addVeg,addWaterBody,&
         state,soilmoist,sfr,surf,WaterDist,nsh_real,&
         drain_per_tstep,&  !output
         drain,AddWaterRunoff,&
         AdditionalWater,runoffPipes,runoff_per_interval,&
         AddWater,stateOld,soilmoistOld)
    !============= calculate water balance end =============

    !======== Evaporation and surface state ========
    CALL SUEWS_cal_QE(&
         Diagnose,&!input
         id,tstep,imin,it,ity,snowCalcSwitch,DayofWeek_id,CRWmin,CRWmax,&
         nsh_real,dectime,lvS_J_kg,lv_j_kg,avdens,avRh,Press_hPa,Temp_C,&
         RAsnow,psyc_hPa,avcp,sIce_hPa,&
         PervFraction,vegfraction,addimpervious,qn1_SF,qf,qs,vpd_hPa,s_hPa,&
         ResistSurf,ra,rb,tstep_real,snowdensmin,precip,PipeCapacity,RunoffToWater,&
         NonWaterFraction,wu_EveTr,wu_DecTr,wu_Grass,addVeg,addWaterBody,SnowLimPaved,SnowLimBuild,&
         SurfaceArea,FlowChange,drain,WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
         freezmelt,freezstate,freezstatevol,Qm_Melt,Qm_rain,Tsurf_ind,sfr,&
         StateLimit,AddWater,addwaterrunoff,surf,snowD,&
         runoff_per_interval,state,soilmoist,SnowPack,snowFrac,MeltWaterStore,&! inout:
         iceFrac,SnowDens,&
         snowProf,& ! output:
         runoffSnow,runoff,runoffSoil,chang,changSnow,&
         snowDepth,SnowToSurf,ev_snow,SnowRemoval,&
         evap,rss_nsurf,p_mm,rss,qe,state_per_tstep,NWstate_per_tstep,qeOut,&
         swe,ev,chSnow_per_interval,ev_per_tstep,qe_per_tstep,runoff_per_tstep,&
         surf_chang_per_tstep,runoffPipes,mwstore,runoffwaterbody,&
         runoffAGveg,runoffAGimpervious,runoffWaterBody_m3,runoffPipes_m3)
    !======== Evaporation and surface state end========

    !============ Sensible heat flux ===============
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_QH...'
    CALL SUEWS_cal_QH(&
         1,&
         qn1,qf,QmRain,qeOut,qs,QmFreez,qm,avdens,avcp,tsurf,Temp_C,ra,&
         qh,qh_r)!output
    !============ Sensible heat flux end===============

    !=== Horizontal movement between soil stores ===
    ! Now water is allowed to move horizontally between the soil stores
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_HorizontalSoilWater...'
    CALL SUEWS_cal_HorizontalSoilWater(&
         sfr,&! input: ! surface fractions
         SoilStoreCap,&!Capacity of soil store for each surface [mm]
         SoilDepth,&!Depth of sub-surface soil store for each surface [mm]
         SatHydraulicConduct,&!Saturated hydraulic conductivity for each soil subsurface [mm s-1]
         SurfaceArea,&!Surface area of the study area [m2]
         NonWaterFraction,&! sum of surface cover fractions for all except water surfaces
         tstep_real,& !tstep cast as a real for use in calculations
         SoilMoist,&! inout:!Soil moisture of each surface type [mm]
         runoffSoil,&!Soil runoff from each soil sub-surface [mm]
         runoffSoil_per_tstep&!  output:!Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
         )

    !========== Calculate soil moisture ============
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_SoilMoist...'
    CALL SUEWS_cal_SoilMoist(&
         SMDMethod,xsmd,NonWaterFraction,SoilMoistCap,&!input
         SoilStoreCap,surf_chang_per_tstep,&
         soilmoist,soilmoistOld,sfr,&
         smd,smd_nsurf,tot_chang_per_tstep,SoilState)!output


    !============ surface-level diagonostics ===============
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_Diagnostics...'
    CALL SUEWS_cal_Diagnostics(&
         tsurf,qh,&!input
         Press_hPa,qe,&
         UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real,&
         RoughLenHeatMethod,StabilityMethod,&
         avU10_ms,t2_C,q2_gkg)!output
    !============ surface-level diagonostics end ===============


    !==============main calculation end=======================

    !==============translation of  output variables into output array===========
    CALL SUEWS_update_outputLine(&
         AdditionalWater,alb,avkdn,avU10_ms,azimuth,&!input
         chSnow_per_interval,dectime,&
         drain_per_tstep,E_mod,ev_per_tstep,ext_wu,Fc,Fc_build,fcld,&
         Fc_metab,Fc_photo,Fc_respi,Fc_traff,FlowChange,&
         h_mod,id,id_prev_t,imin,int_wu,it,iy,iy_prev_t,&
         kup,LAI,ldown,l_mod,lup,mwh,MwStore,&
         nsh_real,NWstate_per_tstep,Precip,q2_gkg,&
         qeOut,qf,qh,QH_r,Qm,QmFreez,&
         QmRain,qn1,qn1_S,qn1_SF,qs,RA,&
         resistsurf,runoffAGimpervious,runoffAGveg,&
         runoff_per_tstep,runoffPipes,runoffSoil_per_tstep,&
         runoffWaterBody,sfr,smd,smd_nsurf,SnowAlb,SnowRemoval,&
         state,state_per_tstep,surf_chang_per_tstep,swe,t2_C,&
         tot_chang_per_tstep,tsurf,UStar,wu_DecTr,&
         wu_EveTr,wu_Grass,z0m,zdm,zenith_deg,&
         datetimeLine,dataOutLineSUEWS)!output

    ! model state:

    ! daily state:
    CALL update_DailyState(&
         iy,id,it,imin,nsh_real,&!input
         GDD,HDD,LAI,&
         DecidCap,albDecTr,albEveTr,albGrass,porosity,&
         WU_Day,&
         deltaLAI,VegPhenLumps,&
         SnowAlb,SnowDens,&
         a1,a2,a3,&
         DailyStateLine)!out

    !==============translation end ================

  END SUBROUTINE SUEWS_cal_Main
  ! ================================================================================

  ! ===================ANTHROPOGENIC HEAT + CO2 FLUX================================
  SUBROUTINE SUEWS_cal_AnthropogenicEmission(&
       AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,AH_SLOPE_Heating,alpha_bioCO2,&
       alpha_enh_bioCO2,avkdn,beta_bioCO2,beta_enh_bioCO2,dayofWeek_id,&
       Diagnose,DLS,EF_umolCO2perJ,EmissionsMethod,EnEF_v_Jkm,Fc,Fc_anthro,Fc_biogen,&
       Fc_build,FcEF_v_kgkm,Fc_metab,Fc_photo,Fc_respi,Fc_traff,FrFossilFuel_Heat,&
       FrFossilFuel_NonHeat,HDD,HumActivity_tstep,id,imin,it,LAI, LaiMax,LaiMin,&
       MaxQFMetab,MinQFMetab,min_res_bioCO2,nsh,NumCapita,&
       PopDensDaytime,PopDensNighttime,PopProf_tstep,QF,QF0_BEU,Qf_A,Qf_B,Qf_C,QF_SAHP,&
       resp_a,resp_b,sfr,snowFrac,T_CRITIC_Cooling,T_CRITIC_Heating,Temp_C,&
       theta_bioCO2,TrafficRate,TrafficUnits,TraffProf_tstep)

    IMPLICIT NONE

    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::EmissionsMethod
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::it
    INTEGER,INTENT(in)::imin
    INTEGER,INTENT(in)::DLS
    INTEGER,INTENT(in)::nsh
    ! INTEGER,INTENT(in)::notUsedI
    INTEGER,DIMENSION(3),INTENT(in)::dayofWeek_id
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(in)::HDD
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::Qf_A
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::Qf_B
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::Qf_C
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::AH_MIN
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::AH_SLOPE_Heating
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::AH_SLOPE_Cooling
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::T_CRITIC_Heating
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::T_CRITIC_Cooling
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::TrafficRate
    REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::QF0_BEU
    REAL(KIND(1d0)),DIMENSION(24*nsh,2),INTENT(in)::AHProf_tstep
    REAL(KIND(1d0)),DIMENSION(24*nsh,2),INTENT(in)::HumActivity_tstep
    REAL(KIND(1d0)),DIMENSION(24*nsh,2),INTENT(in)::TraffProf_tstep
    REAL(KIND(1d0)),DIMENSION(24*nsh,2),INTENT(in)::PopProf_tstep
    REAL(KIND(1D0)),INTENT(in)::EF_umolCO2perJ
    REAL(KIND(1D0)),INTENT(in)::FcEF_v_kgkm
    REAL(KIND(1D0)),INTENT(in)::EnEF_v_Jkm
    REAL(KIND(1D0)),INTENT(in)::TrafficUnits
    REAL(KIND(1D0)),INTENT(in)::FrFossilFuel_Heat
    REAL(KIND(1D0)),INTENT(in)::FrFossilFuel_NonHeat
    REAL(KIND(1D0)),INTENT(in)::MinQFMetab
    REAL(KIND(1D0)),INTENT(in)::MaxQFMetab
    REAL(KIND(1D0)),INTENT(in)::NumCapita
    REAL(KIND(1D0)),INTENT(in)::PopDensDaytime
    REAL(KIND(1D0)),INTENT(in)::PopDensNighttime
    REAL(KIND(1D0)),INTENT(in)::Temp_C
    REAL(KIND(1D0)),INTENT(out)::QF
    REAL(KIND(1D0)),INTENT(out)::QF_SAHP
    REAL(KIND(1D0)),INTENT(out)::Fc_anthro
    REAL(KIND(1D0)),INTENT(out)::Fc_metab
    REAL(KIND(1D0)),INTENT(out)::Fc_traff
    REAL(KIND(1D0)),INTENT(out)::Fc_build
    REAL(KIND(1d0)),INTENT(in)::avkdn
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowFrac
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(in)::LAI
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::LaiMin
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in):: LaiMax
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::alpha_bioCO2
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::beta_bioCO2
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::theta_bioCO2
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::alpha_enh_bioCO2
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::beta_enh_bioCO2
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::resp_a
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::resp_b
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::min_res_bioCO2
    REAL(KIND(1D0)),INTENT(out)::Fc_biogen
    REAL(KIND(1D0)),INTENT(out)::Fc_respi
    REAL(KIND(1D0)),INTENT(out)::Fc_photo
    REAL(KIND(1D0)),INTENT(out)::Fc

    INTEGER,PARAMETER :: notUsedI=-999
    REAL(KIND(1D0)),PARAMETER::notUsed=-999


    !ih=it-DLS           !Moved to subroutine AnthropogenicEmissions MH 29 June 2017
    !IF(ih<0) ih=23

    IF(EmissionsMethod>0 .AND. EmissionsMethod<=6)THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AnthropogenicEmissions...'
       CALL AnthropogenicEmissions(&
            EmissionsMethod,&
            id,it,imin,DLS,nsh,dayofWeek_id,ndays,&
            EF_umolCO2perJ,FcEF_v_kgkm,EnEF_v_Jkm,TrafficUnits,&
            FrFossilFuel_Heat,FrFossilFuel_NonHeat,&
            MinQFMetab,MaxQFMetab,&
            NumCapita,PopDensDaytime,PopDensNighttime,&
            Temp_C,HDD,Qf_A,Qf_B,Qf_C,&
            AH_MIN,AH_SLOPE_Heating,AH_SLOPE_Cooling,&
            T_CRITIC_Heating,T_CRITIC_Cooling,&
            TrafficRate,&
            QF0_BEU,QF_SAHP,&
            Fc_anthro,Fc_metab,Fc_traff,Fc_build,&
            AHProf_tstep,HumActivity_tstep,TraffProf_tstep,PopProf_tstep,&
            notUsed,notUsedI)

       !  qn1_bup=qn1
       Fc_anthro=0
       Fc_metab=0
       Fc_traff=0
       Fc_build=0
       Fc_biogen=0
       Fc_respi=0
       Fc_photo=0
    ELSEIF(EmissionsMethod>=11)THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AnthropogenicEmissions...'
       CALL AnthropogenicEmissions(&
            EmissionsMethod,&
            id,it,imin,DLS,nsh,dayofWeek_id,ndays,&
            EF_umolCO2perJ,FcEF_v_kgkm,EnEF_v_Jkm,TrafficUnits,&
            FrFossilFuel_Heat,FrFossilFuel_NonHeat,&
            MinQFMetab,MaxQFMetab,&
            NumCapita,PopDensDaytime,PopDensNighttime,&
            Temp_C,HDD,Qf_A,Qf_B,Qf_C,&
            AH_MIN,AH_SLOPE_Heating,AH_SLOPE_Cooling,&
            T_CRITIC_Heating,T_CRITIC_Cooling,&
            TrafficRate,&
            QF0_BEU,QF_SAHP,&
            Fc_anthro,Fc_metab,Fc_traff,Fc_build,&
            AHProf_tstep,HumActivity_tstep,TraffProf_tstep,PopProf_tstep,&
            notUsed,notUsedI)

       !  qn1_bup=qn1
    ELSEIF(EmissionsMethod==0)THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AnthropogenicEmissions...'
       CALL AnthropogenicEmissions(&
            EmissionsMethod,&
            id,it,imin,DLS,nsh,dayofWeek_id,ndays,&
            EF_umolCO2perJ,FcEF_v_kgkm,EnEF_v_Jkm,TrafficUnits,&
            FrFossilFuel_Heat,FrFossilFuel_NonHeat,&
            MinQFMetab,MaxQFMetab,&
            NumCapita,PopDensDaytime,PopDensNighttime,&
            Temp_C,HDD,Qf_A,Qf_B,Qf_C,&
            AH_MIN,AH_SLOPE_Heating,AH_SLOPE_Cooling,&
            T_CRITIC_Heating,T_CRITIC_Cooling,&
            TrafficRate,&
            QF0_BEU,QF_SAHP,&
            Fc_anthro,Fc_metab,Fc_traff,Fc_build,&
            AHProf_tstep,HumActivity_tstep,TraffProf_tstep,PopProf_tstep,&
            notUsed,notUsedI)

       !  qn1_bup=qn1
       !  qn1=qn1+qf
    ELSE
       CALL ErrorHint(73,'RunControl.nml:EmissionsMethod unusable',notUsed,notUsed,EmissionsMethod)
    ENDIF
    ! -- qn1 is now QSTAR+QF (net all-wave radiation + anthropogenic heat flux)
    ! -- qn1_bup is QSTAR only
    IF(EmissionsMethod>=1) qf = QF_SAHP

    IF(EmissionsMethod>=11) THEN
       ! Calculate CO2 fluxes from biogenic components
       IF(Diagnose==1) WRITE(*,*) 'Calling CO2_biogen...'
       CALL CO2_biogen(EmissionsMethod,id,ndays,ivConif,ivDecid,ivGrass,ConifSurf,DecidSurf,GrassSurf,BSoilSurf,&
            snowFrac,nsurf,NVegSurf,avkdn,Temp_C,sfr,LAI,LaiMin,LaiMax,&
            alpha_bioCO2,beta_bioCO2,theta_bioCO2,alpha_enh_bioCO2,beta_enh_bioCO2,&
            resp_a,resp_b,min_res_bioCO2,Fc_biogen,Fc_respi,Fc_photo,&
            notUsed,notUsedI)
    ENDIF
    ! Sum anthropogenic and biogenic CO2 flux components to find overall CO2 flux
    Fc = Fc_anthro + Fc_biogen

    ! =================STORAGE HEAT FLUX=======================================

  END SUBROUTINE SUEWS_cal_AnthropogenicEmission
  ! ================================================================================

  !=============net all-wave radiation=====================================
  SUBROUTINE SUEWS_cal_Qn(&
       NetRadiationMethod,snowUse,id,&!input
       Diagnose,snow_obs,ldown_obs,fcld_obs,&
       dectime,ZENITH_deg,avKdn,Temp_C,avRH,ea_hPa,qn1_obs,&
       SnowAlb,DiagQN,&
       NARP_TRANS_SITE,NARP_EMIS_SNOW,IceFrac,sfr,emis,&
       alb,albDecTr,DecidCap,albEveTr,albGrass,surf,&!inout
       snowFrac,ldown,fcld,&!output
       qn1,qn1_SF,qn1_S,kclear,kup,lup,tsurf,&
       qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)


    IMPLICIT NONE
    ! INTEGER,PARAMETER ::nsurf     = 7 ! number of surface types
    ! INTEGER,PARAMETER ::ConifSurf = 3 !New surface classes: Grass = 5th/7 surfaces
    ! INTEGER,PARAMETER ::DecidSurf = 4 !New surface classes: Grass = 5th/7 surfaces
    ! INTEGER,PARAMETER ::GrassSurf = 5

    INTEGER,INTENT(in)::NetRadiationMethod
    INTEGER,INTENT(in)::snowUse
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::DiagQN

    REAL(KIND(1d0)),INTENT(in)::snow_obs
    REAL(KIND(1d0)),INTENT(in)::ldown_obs
    REAL(KIND(1d0)),INTENT(in)::fcld_obs
    REAL(KIND(1d0)),INTENT(in)::dectime
    REAL(KIND(1d0)),INTENT(in)::ZENITH_deg
    REAL(KIND(1d0)),INTENT(in)::avKdn
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::avRH
    REAL(KIND(1d0)),INTENT(in)::ea_hPa
    REAL(KIND(1d0)),INTENT(in)::qn1_obs
    REAL(KIND(1d0)),INTENT(in)::SnowAlb
    REAL(KIND(1d0)),INTENT(in)::NARP_EMIS_SNOW
    REAL(KIND(1d0)),INTENT(in)::NARP_TRANS_SITE


    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in):: IceFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in):: sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in):: emis

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)  ::alb
    REAL(KIND(1d0)),DIMENSION(0:366),INTENT(inout)  ::albDecTr
    REAL(KIND(1d0)),DIMENSION(0:366),INTENT(inout)  ::DecidCap
    REAL(KIND(1d0)),DIMENSION(0:366),INTENT(inout)  ::albEveTr
    REAL(KIND(1d0)),DIMENSION(0:366),INTENT(inout)  ::albGrass
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(inout)::surf

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::snowFrac

    REAL(KIND(1d0)),INTENT(out)::ldown
    REAL(KIND(1d0)),INTENT(out)::fcld
    REAL(KIND(1d0)),INTENT(out)::qn1
    REAL(KIND(1d0)),INTENT(out)::qn1_SF
    REAL(KIND(1d0)),INTENT(out)::qn1_S
    REAL(KIND(1d0)),INTENT(out)::kclear
    REAL(KIND(1d0)),INTENT(out)::kup
    REAL(KIND(1d0)),INTENT(out)::lup
    REAL(KIND(1d0)),INTENT(out)::tsurf

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::qn1_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::kup_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::Tsurf_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: tsurf_ind


    REAL(KIND(1d0)),DIMENSION(nsurf):: lup_ind
    REAL(KIND(1d0)),DIMENSION(nsurf):: kup_ind
    REAL(KIND(1d0)),DIMENSION(nsurf):: qn1_ind

    REAL(KIND(1d0)),PARAMETER::NAN=-999
    INTEGER :: NetRadiationMethodX
    INTEGER::AlbedoChoice,ldown_option


    CALL RadMethod(&
         NetRadiationMethod,&!inout
         snowUse,&!input
         NetRadiationMethodX,AlbedoChoice,ldown_option)!output

    IF(NetRadiationMethodX>0)THEN

       ! IF (snowUse==0) snowFrac=snow_obs
       IF (snowUse==0) snowFrac=0

       IF(ldown_option==1) THEN !Observed ldown provided as forcing
          ldown=ldown_obs
       ELSE
          ldown=-9              !to be filled in NARP
       ENDIF

       IF(ldown_option==2) THEN !observed cloud fraction provided as forcing
          fcld=fcld_obs
       ENDIF

       !write(*,*) DecidCap(id), id, it, imin, 'Calc - near start'

       ! Update variables that change daily and represent seasonal variability
       alb(DecidSurf)    = albDecTr(id) !Change deciduous albedo
       surf(6,DecidSurf) = DecidCap(id) !Change current storage capacity of deciduous trees
       ! Change EveTr and Grass albedo too
       alb(ConifSurf) = albEveTr(id)
       alb(GrassSurf) = albGrass(id)

       IF(Diagnose==1) WRITE(*,*) 'Calling NARP...'


       CALL NARP(&
            nsurf,sfr,snowFrac,alb,emis,IceFrac,&! input:
            NARP_TRANS_SITE,NARP_EMIS_SNOW,&
            dectime,ZENITH_deg,avKdn,Temp_C,avRH,ea_hPa,qn1_obs,&
            SnowAlb,&
            AlbedoChoice,ldown_option,NetRadiationMethodX,DiagQN,&
            qn1,qn1_SF,qn1_S,kclear,kup,LDown,lup,fcld,tsurf,&! output:
            qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)

    ELSE ! NetRadiationMethod==0
       snowFrac  = snow_obs
       qn1       = qn1_obs
       qn1_sf    = qn1_obs
       qn1_s     = qn1_obs
       ldown     = NAN
       lup       = NAN
       kup       = NAN
       tsurf     = NAN
       lup_ind   = NAN
       kup_ind   = NAN
       tsurf_ind = NAN
       qn1_ind   = NAN
       Fcld      = NAN
    ENDIF

    IF(ldown_option==1) THEN
       Fcld = NAN
    ENDIF

  END SUBROUTINE SUEWS_cal_Qn
  !========================================================================

  !=============storage heat flux=========================================
  SUBROUTINE SUEWS_cal_Qs(&
       StorageHeatMethod,OHMIncQF,Gridiv,&!input
       id,tstep,Diagnose,sfr,&
       OHM_coef,OHM_threshSW,OHM_threshWD,&
       soilmoist,soilstoreCap,state,nsh,SnowUse,DiagQS,&
       HDD,MetForcingData_grid,Ts5mindata_ir,qf,qn1,&
       avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
       bldgh,alb,emis,cpAnOHM,kkAnOHM,chAnOHM,EmissionsMethod,&
       Tair24HR,qn1_store,qn1_S_store,&!inout
       qn1_av_store,qn1_S_av_store,surf,&
       qn1_S,snowFrac,dataOutLineESTM,qs,&!output
       deltaQi,a1,a2,a3)

    IMPLICIT NONE
    INTEGER,INTENT(in)::StorageHeatMethod
    INTEGER,INTENT(in)::OHMIncQF
    INTEGER,INTENT(in)::Gridiv
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::tstep
    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::nsh                ! number of timesteps in one hour
    INTEGER,INTENT(in)::SnowUse            ! option for snow related calculations
    INTEGER,INTENT(in)::DiagQS             ! diagnostic option
    INTEGER,INTENT(in):: EmissionsMethod !< AnthropHeat option [-]


    REAL(KIND(1d0)),INTENT(in)::OHM_coef(nsurf+1,4,3)                 ! OHM coefficients
    REAL(KIND(1d0)),INTENT(in)::OHM_threshSW(nsurf+1) ! OHM thresholds
    REAL(KIND(1d0)),INTENT(in)::OHM_threshWD(nsurf+1) ! OHM thresholds
    REAL(KIND(1d0)),INTENT(in)::soilmoist(nsurf)                ! soil moisture
    REAL(KIND(1d0)),INTENT(in)::soilstoreCap(nsurf)             ! capacity of soil store
    REAL(KIND(1d0)),INTENT(in)::state(nsurf) ! wetness status


    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(in)::HDD
    REAL(KIND(1d0)),INTENT(in)::qf
    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown
    REAL(KIND(1d0)),INTENT(in)::bldgh

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::alb  !< albedo [-]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::emis !< emissivity [-]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::cpAnOHM   !< heat capacity [J m-3 K-1]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::kkAnOHM   !< thermal conductivity [W m-1 K-1]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::chAnOHM   !< bulk transfer coef [J m-3 K-1]

    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::MetForcingData_grid !< met forcing array of grid

    REAL(KIND(1d0)),DIMENSION(:),INTENT(in)::Ts5mindata_ir

    REAL(KIND(1d0)),DIMENSION(24*nsh),INTENT(inout):: Tair24HR
    REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout) ::qn1_store
    REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout) ::qn1_S_store !< stored qn1 [W m-2]

    REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_av_store
    REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_S_av_store !< average net radiation over previous hour [W m-2]
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(inout)::surf
    ! REAL(KIND(1d0)),DIMENSION(ReadlinesMetdata,32,NumberOfGrids),INTENT(inout)::dataOutESTM

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::deltaQi ! storage heat flux of snow surfaces
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::snowFrac

    REAL(KIND(1d0)),DIMENSION(27),INTENT(out):: dataOutLineESTM
    REAL(KIND(1d0)),INTENT(out)::qn1_S
    REAL(KIND(1d0)),INTENT(out):: qs ! storage heat flux
    REAL(KIND(1d0)),INTENT(out):: a1 !< AnOHM coefficients of grid [-]
    REAL(KIND(1d0)),INTENT(out):: a2 !< AnOHM coefficients of grid [h]
    REAL(KIND(1d0)),INTENT(out):: a3 !< AnOHM coefficients of grid [W m-2]


    REAL(KIND(1d0))::HDDday ! HDDday=HDD(id-1,4) HDD at the begining of today (id-1)
    REAL(KIND(1d0))::qn1_use ! qn used in OHM calculations

    ! initialise output variables
    deltaQi=0
    snowFrac=0
    qn1_S=0
    dataOutLineESTM=-999
    qs=-999
    a1=-999
    a2=-999
    a3=-999


    ! calculate qn if qf should be included
    IF(OHMIncQF == 1) THEN
       qn1_use= qf+qn1
    ELSEIF(OHMIncQF == 0) THEN
       qn1_use= qn1
    ENDIF

    IF(StorageHeatMethod==1) THEN           !Use OHM to calculate QS
       HDDday=HDD(id-1,4)
       IF(Diagnose==1) WRITE(*,*) 'Calling OHM...'
       CALL OHM(qn1_use,qn1_store,qn1_av_store,&
            qn1_S,qn1_S_store,qn1_S_av_store,&
            nsh,&
            sfr,nsurf,&
            HDDday,&
            OHM_coef,&
            OHM_threshSW,OHM_threshWD,&
            soilmoist,soilstoreCap,state,&
            BldgSurf,WaterSurf,&
            SnowUse,SnowFrac,&
            DiagQS,&
            a1,a2,a3,qs,deltaQi)

    ENDIF

    ! use AnOHM to calculate QS, TS 14 Mar 2016
    IF (StorageHeatMethod==3) THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AnOHM...'
       CALL AnOHM(qn1_use,qn1_store,qn1_av_store,qf,&
            MetForcingData_grid,state/surf(6,:),&
            alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
            sfr,nsurf,nsh,EmissionsMethod,id,Gridiv,&
            a1,a2,a3,qs,deltaQi)

    END IF


    ! !Calculate QS using ESTM
    IF(StorageHeatMethod==4 .OR. StorageHeatMethod==14) THEN
       !    !CALL ESTM(QSestm,iMB)
       IF(Diagnose==1) WRITE(*,*) 'Calling ESTM...'
       CALL ESTM(&
            Gridiv,&!input
            nsh,tstep,&
            avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
            bldgh,Ts5mindata_ir,&
            Tair24HR,&!inout
            dataOutLineESTM,QS)!output
       !    CALL ESTM(QSestm,Gridiv,ir)  ! iMB corrected to Gridiv, TS 09 Jun 2016
       !    QS=QSestm   ! Use ESTM qs
    ENDIF

  END SUBROUTINE SUEWS_cal_Qs
  !=======================================================================

  !==========================water balance================================
  SUBROUTINE SUEWS_cal_Water(&
       Diagnose,&!input
       snowUse,NonWaterFraction,addPipes,addImpervious,addVeg,addWaterBody,&
       state,soilmoist,sfr,surf,WaterDist,nsh_real,&
       drain_per_tstep,&  !output
       drain,AddWaterRunoff,&
       AdditionalWater,runoffPipes,runoff_per_interval,&
       AddWater,stateOld,soilmoistOld)

    IMPLICIT NONE
    ! INTEGER,PARAMETER :: nsurf=7! number of surface types
    ! INTEGER,PARAMETER ::WaterSurf = 7
    INTEGER,INTENT(in) ::Diagnose
    INTEGER,INTENT(in) ::snowUse

    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in)::addPipes
    REAL(KIND(1d0)),INTENT(in)::addImpervious
    REAL(KIND(1d0)),INTENT(in)::addVeg
    REAL(KIND(1d0)),INTENT(in)::addWaterBody
    REAL(KIND(1d0)),INTENT(in)::nsh_real !nsh cast as a real for use in calculations

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::sfr
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(in)        ::surf
    REAL(KIND(1d0)),DIMENSION(nsurf+1,nsurf-1),INTENT(in)::WaterDist

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: drain         !Drainage of surface type "is" [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: AddWaterRunoff!Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: AddWater      !water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: stateOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: soilmoistOld

    REAL(KIND(1d0)),INTENT(out)::drain_per_tstep
    REAL(KIND(1d0)),INTENT(out)::AdditionalWater
    REAL(KIND(1d0)),INTENT(out)::runoffPipes
    REAL(KIND(1d0)),INTENT(out)::runoff_per_interval
    INTEGER:: is

    ! Retain previous surface state and soil moisture state
    stateOld     = state     !State of each surface [mm] for the previous timestep
    soilmoistOld = soilmoist !Soil moisture of each surface [mm] for the previous timestep


    !============= Grid-to-grid runoff =============
    ! Calculate additional water coming from other grids
    ! i.e. the variables addImpervious, addVeg, addWaterBody, addPipes
    !call RunoffFromGrid(GridFromFrac)  !!Need to code between-grid water transfer

    ! Sum water coming from other grids (these are expressed as depths over the whole surface)
    AdditionalWater = addPipes+addImpervious+addVeg+addWaterBody  ![mm]

    ! Initialise runoff in pipes
    runoffPipes         = addPipes !Water flowing in pipes from other grids. No need for scaling??
    !! CHECK p_i
    runoff_per_interval = addPipes !pipe plor added to total runoff.


    !================== Drainage ===================
    ! Calculate drainage for each soil subsurface (excluding water body)
    IF(Diagnose==1) WRITE(*,*) 'Calling Drainage...'

    IF (NonWaterFraction/=0) THEN !Soil states only calculated if soil exists. LJ June 2017
       DO is=1,nsurf-1

          CALL drainage(&
               is,&! input:
               state(is),&
               surf(6,is),&
               surf(2,is),&
               surf(3,is),&
               surf(4,is),&
               nsh_real,&
               drain(is))! output

          ! !HCW added and changed to surf(6,is) here 20 Feb 2015
          ! drain_per_tstep=drain_per_tstep+(drain(is)*sfr(is)/NonWaterFraction)   !No water body included
       ENDDO
       drain_per_tstep=DOT_PRODUCT(drain(1:nsurf-1),sfr(1:nsurf-1))/NonWaterFraction !No water body included
    ELSE
       drain(1:nsurf-1)=0
       drain_per_tstep=0
    ENDIF

    drain(WaterSurf) = 0  ! Set drainage from water body to zero

    ! Distribute water within grid, according to WithinGridWaterDist matrix (Cols 1-7)
    IF(Diagnose==1) WRITE(*,*) 'Calling ReDistributeWater...'
    ! CALL ReDistributeWater
    !Calculates AddWater(is)
    CALL ReDistributeWater(&
         nsurf,& ! input:
         WaterSurf, snowUse, WaterDist,  sfr,   Drain,&
         AddWaterRunoff,&  ! output:
         AddWater)

  END SUBROUTINE SUEWS_cal_Water
  !=======================================================================

  !===============initialize sensible heat flux============================
  SUBROUTINE SUEWS_init_QH(&
       qh_obs,avdens,avcp,h_mod,qn1,dectime,&!input
       H_init)!output

    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(in)::qh_obs
    REAL(KIND(1d0)),INTENT(in)::avdens
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::h_mod
    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::dectime
    REAL(KIND(1d0)),INTENT(out)::H_init


    REAL(KIND(1d0)),PARAMETER::NAN=-999
    INTEGER,PARAMETER::notUsedI=-999

    ! Calculate kinematic heat flux (w'T') from sensible heat flux [W m-2] from observed data (if available) or LUMPS
    IF(qh_obs/=NAN) THEN   !if(qh_obs/=NAN) qh=qh_obs   !Commented out by HCW 04 Mar 2015
       H_init=qh_obs/(avdens*avcp)  !Use observed value
    ELSE
       IF(h_mod/=NAN) THEN
          H_init = h_mod/(avdens*avcp)   !Use LUMPS value
       ELSE
          H_init=(qn1*0.2)/(avdens*avcp)   !If LUMPS has had a problem, we still need a value
          CALL ErrorHint(38,'LUMPS unable to calculate realistic value for H_mod.',h_mod, dectime, notUsedI)
       ENDIF
    ENDIF

  END SUBROUTINE SUEWS_init_QH
  !========================================================================

  !================latent heat flux and surface wetness===================
  ! TODO: optimise the structure of this function
  SUBROUTINE SUEWS_cal_QE(&
       Diagnose,&!input
       id,tstep,imin,it,ity,snowCalcSwitch,dayofWeek_id,CRWmin,CRWmax,&
       nsh_real,dectime,lvS_J_kg,lv_j_kg,avdens,avRh,Press_hPa,Temp_C,&
       RAsnow,psyc_hPa,avcp,sIce_hPa,&
       PervFraction,vegfraction,addimpervious,qn1_SF,qf,qs,vpd_hPa,s_hPa,&
       ResistSurf,ra,rb,tstep_real,snowdensmin,precip,PipeCapacity,RunoffToWater,&
       NonWaterFraction,wu_EveTr,wu_DecTr,wu_Grass,addVeg,addWaterBody,SnowLimPaved,SnowLimBuild,&
       SurfaceArea,FlowChange,drain,WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
       freezmelt,freezstate,freezstatevol,Qm_Melt,Qm_rain,Tsurf_ind,sfr,&
       StateLimit,AddWater,addwaterrunoff,surf,snowD,&
       runoff_per_interval,state,soilmoist,SnowPack,snowFrac,MeltWaterStore,&! inout:
       iceFrac,SnowDens,&
       snowProf,& ! output:
       runoffSnow,runoff,runoffSoil,chang,changSnow,&
       snowDepth,SnowToSurf,ev_snow,SnowRemoval,&
       evap,rss_nsurf,p_mm,rss,qe,state_per_tstep,NWstate_per_tstep,qeOut,&
       swe,ev,chSnow_per_interval,ev_per_tstep,qe_per_tstep,runoff_per_tstep,&
       surf_chang_per_tstep,runoffPipes,mwstore,runoffwaterbody,&
       runoffAGveg,runoffAGimpervious,runoffWaterBody_m3,runoffPipes_m3)

    IMPLICIT NONE

    INTEGER,INTENT(in) ::Diagnose
    INTEGER,INTENT(in) ::id
    INTEGER,INTENT(in) ::tstep
    INTEGER,INTENT(in) ::imin
    INTEGER,INTENT(in) ::it
    INTEGER,INTENT(in) ::ity !Evaporation calculated according to Rutter (1) or Shuttleworth (2)
    ! INTEGER,INTENT(in) ::snowfractionchoice

    INTEGER,DIMENSION(nsurf),INTENT(in)::snowCalcSwitch
    INTEGER,DIMENSION(3),INTENT(in)::dayofWeek_id

    REAL(KIND(1d0)),INTENT(in)::CRWmin
    REAL(KIND(1d0)),INTENT(in)::CRWmax
    REAL(KIND(1d0)),INTENT(in)::nsh_real
    REAL(KIND(1d0)),INTENT(in)::dectime
    REAL(KIND(1d0)),INTENT(in)::lvS_J_kg
    REAL(KIND(1d0)),INTENT(in)::lv_j_kg
    REAL(KIND(1d0)),INTENT(in)::avdens
    REAL(KIND(1d0)),INTENT(in)::avRh
    REAL(KIND(1d0)),INTENT(in)::Press_hPa
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::RAsnow
    REAL(KIND(1d0)),INTENT(in)::psyc_hPa
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::sIce_hPa
    REAL(KIND(1d0)),INTENT(in)::PervFraction
    REAL(KIND(1d0)),INTENT(in)::vegfraction
    REAL(KIND(1d0)),INTENT(in)::addimpervious
    REAL(KIND(1d0)),INTENT(in)::qn1_SF
    REAL(KIND(1d0)),INTENT(in)::qf
    REAL(KIND(1d0)),INTENT(in)::qs
    REAL(KIND(1d0)),INTENT(in)::vpd_hPa
    REAL(KIND(1d0)),INTENT(in)::s_hPa
    REAL(KIND(1d0)),INTENT(in)::ResistSurf
    REAL(KIND(1d0)),INTENT(in)::ra
    REAL(KIND(1d0)),INTENT(in)::rb
    REAL(KIND(1d0)),INTENT(in)::tstep_real
    REAL(KIND(1d0)),INTENT(in)::snowdensmin
    REAL(KIND(1d0)),INTENT(in)::precip
    REAL(KIND(1d0)),INTENT(in)::PipeCapacity
    REAL(KIND(1d0)),INTENT(in)::RunoffToWater
    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
    REAL(KIND(1d0)),INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
    REAL(KIND(1d0)),INTENT(in)::wu_Grass!Water use for grass [mm]
    REAL(KIND(1d0)),INTENT(in)::addVeg!Water from vegetated surfaces of other grids [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(in)::addWaterBody!Water from water surface of other grids [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(in)::SnowLimPaved
    REAL(KIND(1d0)),INTENT(in)::SnowLimBuild
    REAL(KIND(1d0)),INTENT(in)::SurfaceArea
    REAL(KIND(1d0)),INTENT(in)::FlowChange

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::drain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::WetThresh
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::stateOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::mw_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilstorecap
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::rainonsnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezmelt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezstate
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezstatevol
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Qm_Melt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Qm_rain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Tsurf_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowD
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::StateLimit !Limit for state of each surface type [mm] (specified in input files)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWater
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::addwaterrunoff
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(in)::surf
    REAL(KIND(1d0)), DIMENSION(0:23,2),INTENT(in):: snowProf

    !Updated status: input and output
    REAL(KIND(1d0)),INTENT(inout)::runoff_per_interval! Total water transported to each grid for grid-to-grid connectivity

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowPack
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::snowFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::MeltWaterStore

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::iceFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens
    REAL(KIND(1d0)),DIMENSION(2)    ::SurplusEvap        !Surplus for evaporation in 5 min timestep


    ! output:
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoffSnow !Initialize for runoff caused by snowmelting
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoff
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoffSoil
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::chang
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::changSnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::snowDepth
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::SnowToSurf
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::ev_snow
    REAL(KIND(1d0)),DIMENSION(2),INTENT(out)::SnowRemoval
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::evap
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::rss_nsurf

    REAL(KIND(1d0)),INTENT(out)::p_mm!Inputs to surface water balance
    REAL(KIND(1d0)),INTENT(out)::rss
    REAL(KIND(1d0)),INTENT(out)::qe ! latent heat flux [W m-2]
    REAL(KIND(1d0)),INTENT(out)::state_per_tstep
    REAL(KIND(1d0)),INTENT(out)::NWstate_per_tstep
    REAL(KIND(1d0)),INTENT(out)::qeOut
    REAL(KIND(1d0)),INTENT(out)::swe
    REAL(KIND(1d0)),INTENT(out)::ev
    REAL(KIND(1d0)),INTENT(out)::chSnow_per_interval
    REAL(KIND(1d0)),INTENT(out)::ev_per_tstep
    REAL(KIND(1d0)),INTENT(out)::qe_per_tstep
    REAL(KIND(1d0)),INTENT(out)::runoff_per_tstep
    REAL(KIND(1d0)),INTENT(out)::surf_chang_per_tstep
    REAL(KIND(1d0)),INTENT(out)::runoffPipes
    REAL(KIND(1d0)),INTENT(out)::mwstore
    REAL(KIND(1d0)),INTENT(out)::runoffwaterbody
    REAL(KIND(1d0)),INTENT(out)::runoffWaterBody_m3
    REAL(KIND(1d0)),INTENT(out)::runoffPipes_m3
    REAL(KIND(1d0)),INTENT(out)::runoffAGveg
    REAL(KIND(1d0)),INTENT(out)::runoffAGimpervious


    ! local:
    INTEGER:: is

    REAL(KIND(1d0))::surplusWaterBody
    REAL(KIND(1d0))::pin!Rain per time interval
    REAL(KIND(1d0))::sae
    REAL(KIND(1d0))::vdrc
    REAL(KIND(1d0))::sp
    REAL(KIND(1d0))::numPM
    REAL(KIND(1d0))::tlv
    REAL(KIND(1d0))::runoffAGimpervious_m3
    REAL(KIND(1d0))::runoffAGveg_m3


    tlv=lv_J_kg/tstep_real !Latent heat of vapourisation per timestep

    pin=MAX(0.,Precip)!Initiate rain data [mm]


    ! Initialize the output variables
    qe                   = 0
    ev                   = 0
    swe                  = 0
    ev_snow              = 0
    ev_per_tstep         = 0
    surf_chang_per_tstep = 0
    runoff_per_tstep     = 0
    state_per_tstep      = 0
    NWstate_per_tstep    = 0
    qeOut                = 0
    runoffwaterbody      = 0
    chSnow_per_interval  = 0
    mwstore              = 0
    runoffAGveg          = 0
    runoffAGimpervious   = 0
    surplusWaterBody     = 0
    runoffSoil           = 0
    runoff               = 0
    chang                = 0
    SurplusEvap          = 0
    SnowRemoval          = 0

    !========= these need to be wrapped================================
    sae   = s_hPa*(qn1_SF+qf-qs)    !s_haPa - slope of svp vs t curve. qn1 changed to qn1_SF, lj in May 2013
    vdrc  = vpd_hPa*avdens*avcp
    sp    = s_hPa/psyc_hPa
    numPM = sae+vdrc/ra
    !write(*,*) numPM, sae, vdrc/ra, s_hPA+psyc_hPa, NumPM/(s_hPA+psyc_hPa)
    !========= these need to be wrapped end================================

    IF(Diagnose==1) WRITE(*,*) 'Calling evap_SUEWS and SoilStore...'
    DO is=1,nsurf   !For each surface in turn
       IF (snowCalcSwitch(is)==1) THEN
          IF (sfr(is)/=0) THEN
             IF(Diagnose==1) WRITE(*,*) 'Calling SnowCalc...'
             CALL SnowCalc(&
                  id,& !input
                  tstep,imin,it,dectime,is,&
                  ity,CRWmin,CRWmax,nsh_real,lvS_J_kg,lv_j_kg,avdens,&
                  avRh,Press_hPa,Temp_C,RAsnow,psyc_hPa,avcp,sIce_hPa,&
                  PervFraction,vegfraction,addimpervious,&
                  numPM,s_hPa,ResistSurf,sp,ra,rb,tlv,snowdensmin,SnowProf,precip,&
                  PipeCapacity,RunoffToWater,runoffAGimpervious,runoffAGveg,&
                  addVeg,surplusWaterBody,SnowLimPaved,SnowLimBuild,FlowChange,drain,&
                  WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
                  freezmelt,freezstate,freezstatevol,&
                  Qm_Melt,Qm_rain,Tsurf_ind,sfr,dayofWeek_id,surf,snowD,&
                  AddWater,addwaterrunoff,&
                  SnowPack,SurplusEvap,&!inout
                  snowFrac,MeltWaterStore,iceFrac,SnowDens,&
                  runoffSnow,& ! output
                  runoff,runoffSoil,chang,changSnow,SnowToSurf,state,ev_snow,soilmoist,&
                  SnowDepth,SnowRemoval,swe,ev,chSnow_per_interval,&
                  ev_per_tstep,qe_per_tstep,runoff_per_tstep,surf_chang_per_tstep,&
                  runoffPipes,mwstore,runoffwaterbody)
          ELSE
             snowFrac(is) = 0
             SnowDens(is) = 0
             SnowPack(is) = 0
          ENDIF
       ELSE

          !Calculates ev [mm]
          CALL Evap_SUEWS(&
               ity,&! input: !Evaporation calculated according to Rutter (1) or Shuttleworth (2)
               state(is),& ! wetness status
               WetThresh(is),&!When State > WetThresh, rs=0 limit in SUEWS_evap [mm] (specified in input files)
               surf(6,is),& ! = surf(6,is), current storage capacity [mm]
               numPM,&!numerator of P-M eqn
               s_hPa,&!Vapour pressure versus temperature slope in hPa
               psyc_hPa,&!Psychometric constant in hPa
               ResistSurf,&!Surface resistance
               sp,&!Term in calculation of E
               ra,&!Aerodynamic resistance
               rb,&!Boundary layer resistance
               tlv,&!Latent heat of vaporization per timestep [J kg-1 s-1], (tlv=lv_J_kg/tstep_real)
               rss,&! output:
               ev,&
               qe) ! latent heat flux [W m-2]


          rss_nsurf(is) = rss !Store rss for each surface
          ! CALL soilstore    !Surface water balance and soil store updates (can modify ev, updates state)
          !Surface water balance and soil store updates (can modify ev, updates state)
          CALL soilstore(&
               is,& ! input: ! surface type
               sfr,&! surface fractions
               PipeCapacity,&!Capacity of pipes to transfer water
               RunoffToWater,&!Fraction of surface runoff going to water body
               pin,&!Rain per time interval
               wu_EveTr,&!Water use for evergreen trees/shrubs [mm]
               wu_DecTr,&!Water use for deciduous trees/shrubs [mm]
               wu_Grass,&!Water use for grass [mm]
               drain,&!Drainage of each surface type [mm]
               AddWater,&!Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
               addImpervious,&!Water from impervious surfaces of other grids [mm] for whole surface area
               nsh_real,&!nsh cast as a real for use in calculations
               stateOld,&!Wetness status of each surface type from previous timestep [mm]
               AddWaterRunoff,&!Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
               PervFraction,&! sum of surface cover fractions for impervious surfaces
               addVeg,&!Water from vegetated surfaces of other grids [mm] for whole surface area
               soilstoreCap,&!Capacity of soil store for each surface [mm]
               addWaterBody,&!Water from water surface of other grids [mm] for whole surface area
               FlowChange,&!Difference between the input and output flow in the water body
               StateLimit,&!Limit for state of each surface type [mm] (specified in input files)
               runoffAGimpervious,&!  inout:!Above ground runoff from impervious surface [mm] for whole surface area
               surplusWaterBody,&!Extra runoff that goes to water body [mm] as specified by RunoffToWater
               runoffAGveg,&!Above ground runoff from vegetated surfaces [mm] for whole surface area
               runoffPipes,&!Runoff in pipes [mm] for whole surface area
               ev,&!Evaporation
               soilmoist,&!Soil moisture of each surface type [mm]
               SurplusEvap,&!Surplus for evaporation in 5 min timestep
               runoffWaterBody,&!Above ground runoff from water surface [mm] for whole surface area
               runoff_per_interval,&! Total water transported to each grid for grid-to-grid connectivity
               p_mm,&!output: !Inputs to surface water balance
               chang,&!Change in state [mm]
               runoff,&!Runoff from each surface type [mm]
               state&!Wetness status of each surface type [mm]
               )

          evap(is)     = ev !Store ev for each surface

          ! Sum evaporation from different surfaces to find total evaporation [mm]
          ev_per_tstep = ev_per_tstep+evap(is)*sfr(is)

          ! Sum change from different surfaces to find total change to surface state
          surf_chang_per_tstep = surf_chang_per_tstep+(state(is)-stateOld(is))*sfr(is)
          ! Sum runoff from different surfaces to find total runoff
          runoff_per_tstep     = runoff_per_tstep+runoff(is)*sfr(is)
          ! Calculate total state (including water body)
          state_per_tstep      = state_per_tstep+(state(is)*sfr(is))
          ! Calculate total state (excluding water body)

          IF (NonWaterFraction/=0 .AND. is.NE.WaterSurf) THEN
             NWstate_per_tstep=NWstate_per_tstep+(state(is)*sfr(is)/NonWaterFraction)
          ENDIF

          ChangSnow(is)  = 0
          runoffSnow(is) = 0

       ENDIF
    ENDDO  !end loop over surfaces


    ! Convert evaporation to latent heat flux [W m-2]
    qe_per_tstep = ev_per_tstep*tlv
    qeOut        = qe_per_tstep

    ! Calculate volume of water that will move between grids
    ! Volume [m3] = Depth relative to whole area [mm] / 1000 [mm m-1] * SurfaceArea [m2]
    ! Need to use these volumes when converting back to addImpervious, AddVeg and AddWater
    runoffAGimpervious_m3 = runoffAGimpervious/1000 *SurfaceArea
    runoffAGveg_m3        = runoffAGveg/1000 *SurfaceArea
    runoffWaterBody_m3    = runoffWaterBody/1000 *SurfaceArea
    runoffPipes_m3        = runoffPipes/1000 *SurfaceArea

  END SUBROUTINE SUEWS_cal_QE
  !========================================================================

  !===============sensible heat flux======================================
  SUBROUTINE SUEWS_cal_QH(&
       QHMethod,&!input
       qn1,qf,QmRain,qeOut,qs,QmFreez,qm,avdens,avcp,tsurf,Temp_C,ra,&
       qh,qh_r)!output
    IMPLICIT NONE

    INTEGER,INTENT(in) :: QHMethod ! option for QH calculation: 1, residual; 2, resistance-based

    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::qf
    REAL(KIND(1d0)),INTENT(in)::QmRain
    REAL(KIND(1d0)),INTENT(in)::qeOut
    REAL(KIND(1d0)),INTENT(in)::qs
    REAL(KIND(1d0)),INTENT(in)::QmFreez
    REAL(KIND(1d0)),INTENT(in)::qm
    REAL(KIND(1d0)),INTENT(in)::avdens
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::tsurf
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::ra


    REAL(KIND(1d0)),INTENT(out)::qh
    REAL(KIND(1d0)),INTENT(out)::qh_r

    REAL(KIND(1d0)),PARAMETER::NAN=-999

    ! ! Calculate QH using resistance method (for testing HCW 06 Jul 2016)

    SELECT CASE (QHMethod)
    CASE (1)
       ! Calculate sensible heat flux as a residual (Modified by LJ in Nov 2012)
       qh=(qn1+qf+QmRain)-(qeOut+qs+Qm+QmFreez)     !qh=(qn1+qf+QmRain+QmFreez)-(qeOut+qs+Qm)

    CASE (2)
       ! Aerodynamic-Resistance-based method
       IF(ra/=0) THEN
          qh = avdens*avcp*(tsurf-Temp_C)/ra
       ELSE
          qh=NAN
       ENDIF

    END SELECT
    QH_R=qh

  END SUBROUTINE SUEWS_cal_QH
  !========================================================================

  !===============Resistance Calculations=======================
  SUBROUTINE SUEWS_cal_Resistance(&
       StabilityMethod,&!input:
       Diagnose,AerodynamicResistanceMethod,RoughLenHeatMethod,snowUse,&
       id,it,gsModel,SMDMethod,&
       qh_obs,avdens,avcp,h_mod,qn1,dectime,zzd,z0M,zdm,&
       avU1,Temp_C,UStar,VegFraction,&
       avkdn,Kmax,G1,G2,G3,G4,G5,G6,S1,S2,TH,TL,dq,&
       xsmd,vsmd,MaxConductance,LAIMax,LAI_id,snowFrac,sfr,&
       Tstar,L_mod,&!output
       psim,gsc,ResistSurf,RA,RAsnow,rb)

    IMPLICIT NONE

    INTEGER,INTENT(in)::StabilityMethod
    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::AerodynamicResistanceMethod
    INTEGER,INTENT(in)::RoughLenHeatMethod
    INTEGER,INTENT(in)::snowUse
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::it       !time: day of year and hour
    INTEGER,INTENT(in)::gsModel  !Choice of gs parameterisation (1 = Ja11, 2 = Wa16)
    INTEGER,INTENT(in)::SMDMethod!Method of measured soil moisture

    REAL(KIND(1d0)),INTENT(in)::qh_obs
    REAL(KIND(1d0)),INTENT(in)::avdens
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::h_mod
    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::dectime    !Decimal time
    REAL(KIND(1d0)),INTENT(in)::zzd        !Active measurement height (meas. height-displac. height)
    REAL(KIND(1d0)),INTENT(in)::z0M        !Aerodynamic roughness length
    REAL(KIND(1d0)),INTENT(in)::zdm        !Displacement height
    REAL(KIND(1d0)),INTENT(in)::avU1       !Average wind speed
    REAL(KIND(1d0)),INTENT(in)::Temp_C     !Air temperature
    REAL(KIND(1d0)),INTENT(in)::VegFraction!Fraction of vegetation
    REAL(KIND(1d0)),INTENT(in)::avkdn      !Average downwelling shortwave radiation
    REAL(KIND(1d0)),INTENT(in)::Kmax       !Annual maximum hourly solar radiation
    REAL(KIND(1d0)),INTENT(in)::G1         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::G2         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::G3         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::G4         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::G5         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::G6         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::S1         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::S2         !Fitted parameters related to surface res. calculations
    REAL(KIND(1d0)),INTENT(in)::TH         !Maximum temperature limit
    REAL(KIND(1d0)),INTENT(in)::TL         !Minimum temperature limit
    REAL(KIND(1d0)),INTENT(in)::dq         !Specific humidity deficit
    REAL(KIND(1d0)),INTENT(in)::xsmd       !Measured soil moisture deficit
    REAL(KIND(1d0)),INTENT(in)::vsmd       !Soil moisture deficit for vegetated surfaces only (what about BSoil?)

    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::MaxConductance!Max conductance [mm s-1]
    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::LAIMax        !Max LAI [m2 m-2]
    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::LAI_id        !=LAI_id(id-1,:), LAI for each veg surface [m2 m-2]

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowFrac      !Surface fraction of snow cover
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr           !Surface fractions [-]

    REAL(KIND(1d0)),INTENT(out)::Tstar     !T*
    REAL(KIND(1d0)),INTENT(out)::UStar     !Friction velocity
    REAL(KIND(1d0)),INTENT(out)::psim      !Stability function of momentum
    REAL(KIND(1d0)),INTENT(out)::gsc       !Surface Layer Conductance
    REAL(KIND(1d0)),INTENT(out)::ResistSurf!Surface resistance
    REAL(KIND(1d0)),INTENT(out)::RA        !Aerodynamic resistance [s m^-1]
    REAL(KIND(1d0)),INTENT(out)::RAsnow    !Aerodynamic resistance for snow [s m^-1]
    REAL(KIND(1d0)),INTENT(out)::rb        !boundary layer resistance shuttleworth
    REAL(KIND(1d0)),INTENT(out)::L_mod  !Obukhov length
    REAL(KIND(1d0))::H_init !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity


    ! Get first estimate of sensible heat flux. Modified by HCW 26 Feb 2015
    CALL SUEWS_init_QH(&
         qh_obs,avdens,avcp,h_mod,qn1,dectime,&
         H_init)

    IF(Diagnose==1) WRITE(*,*) 'Calling STAB_lumps...'
    !u* and Obukhov length out
    CALL STAB_lumps(&
         StabilityMethod,&  ! input
         dectime,& !Decimal time
         zzd,&     !Active measurement height (meas. height-displac. height)
         z0M,&     !Aerodynamic roughness length
         zdm,&     !Displacement height
         avU1,&    !Average wind speed
         Temp_C,&  !Air temperature
         H_init,& !Kinematic sensible heat flux [K m s-1] used to calculate friction velocity
         L_mod,&! output: !Obukhov length
         Tstar,& !T*
         UStar,& !Friction velocity
         psim)!Stability function of momentum

    IF(Diagnose==1) WRITE(*,*) 'Calling AerodynamicResistance...'
    CALL AerodynamicResistance(&
         ZZD,&! input:
         z0m,&
         AVU1,&
         L_mod,&
         UStar,&
         VegFraction,&
         AerodynamicResistanceMethod,&
         StabilityMethod,&
         RoughLenHeatMethod,&
         RA) ! output:

    IF (snowUse==1) THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AerodynamicResistance for snow...'
       CALL AerodynamicResistance(&
            ZZD,&! input:
            z0m,&
            AVU1,&
            L_mod,&
            UStar,&
            VegFraction,&
            AerodynamicResistanceMethod,&
            StabilityMethod,&
            3,&
            RAsnow)     ! output:
    ENDIF

    IF(Diagnose==1) WRITE(*,*) 'Calling SurfaceResistance...'
    ! CALL SurfaceResistance(id,it)   !qsc and surface resistance out
    CALL SurfaceResistance(&
         id,it,&! input:
         SMDMethod,snowFrac,sfr,avkdn,Temp_C,dq,xsmd,vsmd,MaxConductance,&
         LAIMax,LAI_id,gsModel,Kmax,&
         G1,G2,G3,G4,G5,G6,TH,TL,S1,S2,&
         gsc,ResistSurf)! output:

    IF(Diagnose==1) WRITE(*,*) 'Calling BoundaryLayerResistance...'
    CALL BoundaryLayerResistance(&
         zzd,&! input:     !Active measurement height (meas. height-displac. height)
         z0M,&     !Aerodynamic roughness length
         avU1,&    !Average wind speed
         UStar,&  ! input/output:
         rb)  ! output:

  END SUBROUTINE SUEWS_cal_Resistance
  !========================================================================

  !==============Update output arrays=========================
  SUBROUTINE SUEWS_update_outputLine(&
       AdditionalWater,alb,avkdn,avU10_ms,azimuth,&!input
       chSnow_per_interval,dectime,&
       drain_per_tstep,E_mod,ev_per_tstep,ext_wu,Fc,Fc_build,fcld,&
       Fc_metab,Fc_photo,Fc_respi,Fc_traff,FlowChange,&
       h_mod,id,id_prev_t,imin,int_wu,it,iy,iy_prev_t,&
       kup,LAI,ldown,l_mod,lup,mwh,&
       MwStore,&
       nsh_real,NWstate_per_tstep,Precip,q2_gkg,&
       qeOut,qf,qh,QH_r,Qm,QmFreez,&
       QmRain,qn1,qn1_S,qn1_SF,qs,RA,&
       resistsurf,runoffAGimpervious,runoffAGveg,&
       runoff_per_tstep,runoffPipes,runoffSoil_per_tstep,&
       runoffWaterBody,sfr,smd,smd_nsurf,SnowAlb,SnowRemoval,&
       state,state_per_tstep,surf_chang_per_tstep,swe,t2_C,&
       tot_chang_per_tstep,tsurf,UStar,wu_DecTr,&
       wu_EveTr,wu_Grass,z0m,zdm,zenith_deg,&
       datetimeLine,dataOutLineSUEWS)!output
    IMPLICIT NONE

    REAL(KIND(1d0)),PARAMETER :: NAN=-999
    INTEGER,INTENT(in) :: iy
    INTEGER,INTENT(in) :: iy_prev_t
    INTEGER,INTENT(in) :: id
    INTEGER,INTENT(in) :: id_prev_t
    INTEGER,INTENT(in) :: it
    INTEGER,INTENT(in) :: imin

    REAL(KIND(1d0)),INTENT(in) :: AdditionalWater
    REAL(KIND(1d0)),INTENT(in) :: alb(nsurf)
    REAL(KIND(1d0)),INTENT(in) :: avkdn
    REAL(KIND(1d0)),INTENT(in) :: avU10_ms
    REAL(KIND(1d0)),INTENT(in) :: azimuth
    REAL(KIND(1d0)),INTENT(in) :: chSnow_per_interval
    REAL(KIND(1d0)),INTENT(in) :: dectime
    REAL(KIND(1d0)),INTENT(in) :: drain_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: E_mod
    REAL(KIND(1d0)),INTENT(in) :: ev_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: ext_wu
    REAL(KIND(1d0)),INTENT(in) :: Fc
    REAL(KIND(1d0)),INTENT(in) :: Fc_build
    REAL(KIND(1d0)),INTENT(in) :: Fc_metab
    REAL(KIND(1d0)),INTENT(in) :: Fc_photo
    REAL(KIND(1d0)),INTENT(in) :: Fc_respi
    REAL(KIND(1d0)),INTENT(in) :: Fc_traff
    REAL(KIND(1d0)),INTENT(in) :: fcld
    REAL(KIND(1d0)),INTENT(in) :: FlowChange
    REAL(KIND(1d0)),INTENT(in) :: h_mod
    REAL(KIND(1d0)),INTENT(in) :: int_wu
    REAL(KIND(1d0)),INTENT(in) :: kup
    REAL(KIND(1d0)),INTENT(in) :: l_mod
    REAL(KIND(1d0)),INTENT(in) :: LAI(-4:ndays, nvegsurf)
    REAL(KIND(1d0)),INTENT(in) :: ldown
    REAL(KIND(1d0)),INTENT(in) :: lup
    REAL(KIND(1d0)),INTENT(in) :: mwh
    REAL(KIND(1d0)),INTENT(in) :: MwStore
    REAL(KIND(1d0)),INTENT(in) :: nsh_real
    REAL(KIND(1d0)),INTENT(in) :: NWstate_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: Precip
    REAL(KIND(1d0)),INTENT(in) :: q2_gkg
    REAL(KIND(1d0)),INTENT(in) :: qeOut
    REAL(KIND(1d0)),INTENT(in) :: qf
    REAL(KIND(1d0)),INTENT(in) :: qh
    REAL(KIND(1d0)),INTENT(in) :: QH_r
    REAL(KIND(1d0)),INTENT(in) :: Qm
    REAL(KIND(1d0)),INTENT(in) :: QmFreez
    REAL(KIND(1d0)),INTENT(in) :: QmRain
    REAL(KIND(1d0)),INTENT(in) :: qn1
    REAL(KIND(1d0)),INTENT(in) :: qn1_S
    REAL(KIND(1d0)),INTENT(in) :: qn1_SF
    REAL(KIND(1d0)),INTENT(in) :: qs
    REAL(KIND(1d0)),INTENT(in) :: RA
    REAL(KIND(1d0)),INTENT(in) :: resistsurf
    REAL(KIND(1d0)),INTENT(in) :: runoff_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: runoffAGimpervious
    REAL(KIND(1d0)),INTENT(in) :: runoffAGveg
    REAL(KIND(1d0)),INTENT(in) :: runoffPipes
    REAL(KIND(1d0)),INTENT(in) :: runoffSoil_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: runoffWaterBody
    REAL(KIND(1d0)),INTENT(in) :: sfr(nsurf)
    REAL(KIND(1d0)),INTENT(in) :: smd
    REAL(KIND(1d0)),INTENT(in) :: smd_nsurf(nsurf)
    REAL(KIND(1d0)),INTENT(in) :: SnowAlb
    REAL(KIND(1d0)),INTENT(in) :: SnowRemoval(2)
    REAL(KIND(1d0)),INTENT(in) :: state(nsurf)
    REAL(KIND(1d0)),INTENT(in) :: state_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: surf_chang_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: swe
    REAL(KIND(1d0)),INTENT(in) :: t2_C
    REAL(KIND(1d0)),INTENT(in) :: tot_chang_per_tstep
    REAL(KIND(1d0)),INTENT(in) :: tsurf
    REAL(KIND(1d0)),INTENT(in) :: UStar
    REAL(KIND(1d0)),INTENT(in) :: wu_DecTr
    REAL(KIND(1d0)),INTENT(in) :: wu_EveTr
    REAL(KIND(1d0)),INTENT(in) :: wu_Grass
    REAL(KIND(1d0)),INTENT(in) :: z0m
    REAL(KIND(1d0)),INTENT(in) :: zdm
    REAL(KIND(1d0)),INTENT(in) :: zenith_deg


    REAL(KIND(1D0)),DIMENSION(5),INTENT(OUT)::datetimeLine
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSUEWS-5),INTENT(out) :: dataOutLineSUEWS

    ! INTEGER:: is
    REAL(KIND(1d0)):: LAI_wt

    ! the variables below with '_x' endings stand for 'exported' values
    REAL(KIND(1d0))::ResistSurf_x
    REAL(KIND(1d0))::l_mod_x
    REAL(KIND(1d0))::bulkalbedo
    REAL(KIND(1d0))::smd_nsurf_x(nsurf)
    REAL(KIND(1d0))::state_x(nsurf)

    !====================== Prepare data for output ======================

    ! Remove non-existing surface type from surface and soil outputs   ! Added back in with NANs by HCW 24 Aug 2016
    state_x=UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr)), mask=(sfr<0.00001), field=state)
    smd_nsurf_x=UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr)), mask=(sfr<0.00001), field=smd_nsurf)

    ResistSurf_x=MIN(9999.,ResistSurf)
    l_mod_x=MAX(MIN(9999.,l_mod), -9999.)

    ! Calculate areally-weighted LAI
    IF(iy == (iy_prev_t+1) .AND. (id-1) == 0) THEN   !Check for start of next year and avoid using LAI(id-1) as this is at the start of the year
       !  LAI_wt=0
       !  DO is=1,nvegsurf
       !     LAI_wt=LAI_wt+LAI(id_prev_t,is)*sfr(is+2)
       !  ENDDO
       LAI_wt=DOT_PRODUCT(LAI(id_prev_t,:),sfr(1+2:nvegsurf+2))
    ELSE
       !  LAI_wt=0
       !  DO is=1,nvegsurf
       !     LAI_wt=LAI_wt+LAI(id-1,is)*sfr(is+2)
       !  ENDDO
       LAI_wt=DOT_PRODUCT(LAI(id-1,:),sfr(1+2:nvegsurf+2))
    ENDIF

    ! Calculate areally-weighted albedo
    bulkalbedo=DOT_PRODUCT(alb,sfr)

    !====================== update output line ==============================
    ! date & time:
    datetimeLine=[&
         REAL(iy,KIND(1D0)),REAL(id,KIND(1D0)),&
         REAL(it,KIND(1D0)),REAL(imin,KIND(1D0)),dectime]
    !Define the overall output matrix to be printed out step by step
    dataOutLineSUEWS=[&
         avkdn,kup,ldown,lup,tsurf,&
         qn1,qf,qs,qh,qeOut,&
         h_mod,e_mod,qh_r,&
         precip,ext_wu,ev_per_tstep,runoff_per_tstep,tot_chang_per_tstep,&
         surf_chang_per_tstep,state_per_tstep,NWstate_per_tstep,drain_per_tstep,smd,&
         FlowChange/nsh_real,AdditionalWater,&
         runoffSoil_per_tstep,runoffPipes,runoffAGimpervious,runoffAGveg,runoffWaterBody,&
         int_wu,wu_EveTr,wu_DecTr,wu_Grass,&
         smd_nsurf_x(1:nsurf-1),&
         state_x(1:nsurf),&
         zenith_deg,azimuth,bulkalbedo,Fcld,&
         LAI_wt,z0m,zdm,&
         UStar,l_mod,ra,ResistSurf,&
         Fc,&
         Fc_photo,Fc_respi,Fc_metab,Fc_traff,Fc_build,&
         qn1_SF,qn1_S,SnowAlb,&
         Qm,QmFreez,QmRain,swe,mwh,MwStore,chSnow_per_interval,&
         SnowRemoval(1:2),&
         t2_C,q2_gkg,avU10_ms& ! surface-level diagonostics
         ]
    ! set invalid values to NAN
    ! dataOutLineSUEWS=set_nan(dataOutLineSUEWS)


    !====================update output line end==============================

  END SUBROUTINE SUEWS_update_outputLine
  !========================================================================

  !========================================================================
  SUBROUTINE SUEWS_cal_Diagnostics(&
       tsurf,qh,&!input
       Press_hPa,qe,&
       UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real,&
       RoughLenHeatMethod,StabilityMethod,&
       avU10_ms,t2_C,q2_gkg)!output
    IMPLICIT NONE
    ! REAL(KIND(1d0)),INTENT(in) ::usurf,uflux
    REAL(KIND(1d0)),INTENT(in) ::tsurf,qh
    REAL(KIND(1d0)),INTENT(in) ::Press_hPa,qe
    REAL(KIND(1d0)),INTENT(in) :: UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real

    ! INTEGER,INTENT(in)         :: opt ! 0 for momentum, 1 for temperature, 2 for humidity
    INTEGER,INTENT(in)         :: RoughLenHeatMethod,StabilityMethod

    REAL(KIND(1d0)),INTENT(out):: avU10_ms,t2_C,q2_gkg
    REAL(KIND(1d0))::tlv
    REAL(KIND(1d0)),PARAMETER::k=0.4

    tlv=lv_J_kg/tstep_real !Latent heat of vapourisation per timestep
    ! wind speed:
    CALL diagSfc(0d0,0d0,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,avU10_ms,0,RoughLenHeatMethod,StabilityMethod)
    ! temperature:
    CALL diagSfc(tsurf,qh,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,t2_C,1,RoughLenHeatMethod,StabilityMethod)
    ! humidity:
    CALL diagSfc(qsatf(tsurf,Press_hPa)*1000,& ! Saturation specific humidity at surface in g/kg
         qe,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,q2_gkg,2,RoughLenHeatMethod,StabilityMethod)

  END SUBROUTINE SUEWS_cal_Diagnostics


  ! Calculate tstep-derived variables
  SUBROUTINE SUEWS_cal_tstep(&
       tstep,& ! input
       nsh, nsh_real, tstep_real) ! output
    IMPLICIT NONE
    INTEGER,INTENT(in)::tstep ! number of timesteps per hour
    ! values that are derived from tstep
    INTEGER,INTENT(out)::nsh ! number of timesteps per hour
    REAL(KIND(1D0)),INTENT(out)::nsh_real ! nsh in type real
    REAL(KIND(1D0)),INTENT(out)::tstep_real ! tstep in type real
    nsh=3600/tstep
    nsh_real=nsh*1.0
    tstep_real=tstep*1.0

  END SUBROUTINE SUEWS_cal_tstep

  SUBROUTINE SUEWS_cal_surf(&
       sfr,& !input
       vegfraction,ImpervFraction,PervFraction,NonWaterFraction) ! output
    IMPLICIT NONE

    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)::sfr
    REAL(KIND(1D0)),INTENT(OUT)::VegFraction
    REAL(KIND(1D0)),INTENT(OUT)::ImpervFraction
    REAL(KIND(1D0)),INTENT(OUT)::PervFraction
    REAL(KIND(1D0)),INTENT(OUT)::NonWaterFraction


    VegFraction=sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)
    ImpervFraction=sfr(PavSurf)+sfr(BldgSurf)
    PervFraction=1-ImpervFraction
    NonWaterFraction=1 - sfr(WaterSurf)

  END SUBROUTINE SUEWS_cal_surf


  SUBROUTINE SUEWS_cal_weekday(&
       iy,id,lat,& !input
       dayofWeek_id) !output
    IMPLICIT NONE

    INTEGER,INTENT(in) :: iy  ! year
    INTEGER,INTENT(in) :: id  ! day of year
    REAL(KIND(1d0)),INTENT(in):: lat

    INTEGER,DIMENSION(3),INTENT(OUT) ::dayofWeek_id

    INTEGER::wd
    INTEGER::mb
    INTEGER::date
    INTEGER::seas



    CALL day2month(id,mb,date,seas,iy,lat) !Calculate real date from doy
    CALL Day_of_Week(date,mb,iy,wd)        !Calculate weekday (1=Sun, ..., 7=Sat)

    dayofWeek_id(1)=wd      !Day of week
    dayofWeek_id(2)=mb      !Month
    dayofweek_id(3)=seas    !Season

  END SUBROUTINE SUEWS_cal_weekday


  SUBROUTINE SUEWS_cal_DLS(&
       id,startDLS,endDLS,& !input
       DLS) !output
    IMPLICIT NONE

    INTEGER, INTENT(in) :: id,startDLS,endDLS
    INTEGER, INTENT(out) :: DLS

    DLS=0
    IF ( id>startDLS .AND. id<endDLS ) dls=1

  END SUBROUTINE SUEWS_cal_DLS

  SUBROUTINE diagSfc(&
       xSurf,xFlux,us,VegFraction,z0m,L_mod,k,avdens,avcp,tlv,&
       xDiag,opt,RoughLenHeatMethod,StabilityMethod)
    ! TS 05 Sep 2017: improved interface
    ! TS 20 May 2017: calculate surface-level diagonostics


    IMPLICIT NONE

    REAL(KIND(1d0)),INTENT(in) :: xSurf,xFlux,us,VegFraction,z0m,L_mod,k,avdens,avcp,tlv
    REAL(KIND(1d0)),INTENT(out):: xDiag
    INTEGER,INTENT(in)         :: opt ! 0 for momentum, 1 for temperature, 2 for humidity
    INTEGER,INTENT(in)         :: RoughLenHeatMethod,StabilityMethod

    REAL(KIND(1d0))            :: &
         psymz2,psymz10,psymz0,psyhz2,psyhz0,& ! stability correction functions
         z0h,& ! Roughness length for heat
         z2zd,z10zd,&
         stab_fn_mom,stab_fn_heat !stability correction functions
    REAL(KIND(1d0)),PARAMETER :: muu=1.46e-5 !molecular viscosity
    REAL(KIND(1d0)),PARAMETER :: nan=-999



    !***************************************************************
    ! log-law based stability corrections:
    ! Roughness length for heat
    IF (RoughLenHeatMethod==1) THEN !Brutasert (1982) z0h=z0/10(see Grimmond & Oke, 1986)
       z0h=z0m/10
    ELSEIF (RoughLenHeatMethod==2) THEN ! Kawai et al. (2007)
       !z0h=z0m*exp(2-(1.2-0.9*veg_fr**0.29)*(us*z0m/muu)**0.25)
       ! Changed by HCW 05 Nov 2015 (veg_fr includes water; VegFraction = veg + bare soil)
       z0h=z0m*EXP(2-(1.2-0.9*VegFraction**0.29)*(us*z0m/muu)**0.25)
    ELSEIF (RoughLenHeatMethod==3) THEN
       z0h=z0m*EXP(-20.) ! Voogt and Grimmond, JAM, 2000
    ELSEIF (RoughLenHeatMethod==4) THEN
       z0h=z0m*EXP(2-1.29*(us*z0m/muu)**0.25) !See !Kanda and Moriwaki (2007),Loridan et al. (2010)
    ENDIF

    ! z0h=z0m/5


    ! zX-z0
    z2zd=2+z0h   ! set lower limit as z0h to prevent arithmetic error
    z10zd=10+z0m ! set lower limit as z0m to prevent arithmetic error

    ! stability correction functions
    ! momentum:
    psymz10=stab_fn_mom(StabilityMethod,z10zd/L_mod,z10zd/L_mod)
    psymz2=stab_fn_mom(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
    psymz0=stab_fn_mom(StabilityMethod,z0m/L_mod,z0m/L_mod)

    ! heat and vapor: assuming both are the same

    psyhz2=stab_fn_heat(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
    psyhz0=stab_fn_heat(StabilityMethod,z0h/L_mod,z0h/L_mod)
    !***************************************************************
    IF ( xSurf==nan ) THEN
       ! xSurf can be nan e.g. when TSurf is not calculated
       ! if so xDiag is set as nan as well
       xDiag=nan
    ELSE
       SELECT CASE (opt)
       CASE (0) ! wind (momentum) at 10 m
          xDiag=us/k*(LOG(z10zd/z0m)-psymz10+psymz0)

       CASE (1) ! temperature at 2 m
          xDiag=xSurf-xFlux/(k*us*avdens*avcp)*(LOG(z2zd/z0h)-psyhz2+psyhz0)

       CASE (2) ! humidity at 2 m
          xDiag=xSurf-xFlux/(k*us*avdens*tlv)*(LOG(z2zd/z0h)-psyhz2+psyhz0)

       END SELECT


    END IF

  END SUBROUTINE diagSfc



  !===============set variable of invalid value to NAN=====================
  ELEMENTAL FUNCTION set_nan(x) RESULT(xx)
    IMPLICIT NONE
    REAL(KIND(1d0)),PARAMETER::pNAN=9999
    REAL(KIND(1d0)),PARAMETER::NAN=-999
    REAL(KIND(1d0)),INTENT(in)::x
    REAL(KIND(1d0))::xx

    IF(ABS(x)>pNAN) THEN
       xx=NAN
    ELSE
       xx=x
    ENDIF

  END FUNCTION set_nan
  !========================================================================


  ! SUBROUTINE output_name_n(i,name,group,aggreg)
  !   ! used by f2py module `SuPy` to handle output names
  !   IMPLICIT NONE
  !   ! the dimension is potentially incorrect,
  !   ! which should be consistent with that in output module
  !   INTEGER,INTENT(in) :: i
  !   CHARACTER(len = 15),INTENT(out) :: name,group,aggreg
  !
  !   INTEGER :: n
  !   n=SIZE(varList, dim=1)
  !   IF ( i<n .AND.i>0  ) THEN
  !      name   = TRIM(varList(i)%header)
  !      group  = TRIM(varList(i)%group)
  !      aggreg = TRIM(varList(i)%aggreg)
  !   ELSE
  !      name   = ''
  !      group  = ''
  !      aggreg = ''
  !   END IF
  !
  !
  !   ! DO i = 1, SIZE(varList, dim=1), 1
  !   !    names(i)=TRIM(varList(i)%header)
  !   !    ! names(i,1)=trim(varList(i)%header)
  !   !    ! names(i,2)=trim(varList(i)%unit)
  !   !    ! ! names(i,3)=trim(varList(i)%fmt)
  !   !    ! names(i,3)=trim(varList(i)%longNm)
  !   !    ! names(i,4)=trim(varList(i)%aggreg)
  !   !    ! names(i,5)=trim(varList(i)%group)
  !   !    ! print*, names(i,:)
  !   ! END DO
  !   ! print*, varList
  !
  ! END SUBROUTINE output_name_n


  ! SUBROUTINE output_size(n)
  !   ! used by f2py module `SuPy` to get size of the output list
  !   IMPLICIT NONE
  !   ! the dimension is potentially incorrect,
  !   ! which should be consistent with that in output module
  !   INTEGER,INTENT(out) :: n
  !
  !
  !   n=SIZE(varList, dim=1)
  !
  ! END SUBROUTINE output_size

  ! SUBROUTINE output_names(names)
  !   ! used by f2py module `SuPy` to handle output names
  !   IMPLICIT NONE
  !   ! the dimension is potentially incorrect,
  !   ! which should be consistent with that in output module
  !   ! CHARACTER(len = 50),DIMENSION(300,5),INTENT(out) :: names
  !   ! CHARACTER(len = *),DIMENSION(300),INTENT(out) :: names
  !   CHARACTER(len = 12),INTENT(out) :: names(300)
  !
  !   INTEGER :: i,n,err,stat
  !   ! name=TRIM(varList(1)%header)
  !   n=SIZE(varList, dim=1)
  !   ! ALLOCATE(names(10,n), stat=err)
  !   ! IF (stat /= 0) PRINT *, "names: Allocation request denied"
  !
  !
  !   DO i = 1, n
  !      names(i)=TRIM(varList(i)%header)
  !      ! names(i,1)=trim(varList(i)%header)
  !      ! names(i,2)=trim(varList(i)%unit)
  !      ! ! names(i,3)=trim(varList(i)%fmt)
  !      ! names(i,3)=trim(varList(i)%longNm)
  !      ! names(i,4)=trim(varList(i)%aggreg)
  !      ! names(i,5)=trim(varList(i)%group)
  !      ! print*, names(i,:)
  !   END DO
  !   ! print*, varList
  !   ! IF (ALLOCATED(names)) DEALLOCATE(names)
  !   ! IF (stat /= 0) PRINT *, "names: Deallocation request denied"
  !
  ! END SUBROUTINE output_names


  ! a mini version of SUEWS
  SUBROUTINE SuMin(&
       alb,albDecTr,albEveTr,albGrass,alBMax_DecTr,&! input&inout in alphabetical order
       alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       alt,avkdn,avRh,avU1,BaseT,BaseTe,&
       BaseTHDD,bldgH,CapMax_dec,CapMin_dec,&
       DecidCap,dectime,DecTreeH,DRAINRT,&
       emis,endDLS,EveTreeH,FAIBldg,&
       FAIDecTree,FAIEveTree,FlowChange,&
       G1,G2,G3,G4,G5,G6,GDD,&
       GDDFull,HDD,&
       id,imin,it,iy,Kmax,LAI,LAIMax,LAIMin,&
       LAIPower,LAIType,lat,lng,MaxConductance,&
       OHM_coef,OHMIncQF,OHM_threshSW,&
       OHM_threshWD,PipeCapacity,PorMax_dec,PorMin_dec,porosity,&
       Precip,Press_hPa,&
       qn1_av_store,qn1_store,RAINCOVER,RainMaxRes,&
       RunoffToWater,S1,S2,&
       SDDFull,sfr,&
       soilmoist,soilstoreCap,startDLS,state,StateLimit,&
       surf,SurfaceArea,&
       Temp_C,TH,&
       timezone,TL,&
       tstep,&
       WaterDist,WetThresh,&
       Z,&
       qh,qe)!output

    INTEGER::AerodynamicResistanceMethod
    INTEGER::Diagnose
    INTEGER::DiagQN
    INTEGER::DiagQS
    INTEGER,INTENT(IN)::startDLS
    INTEGER,INTENT(IN)::endDLS
    INTEGER::EmissionsMethod
    INTEGER::Gridiv
    INTEGER::gsModel
    INTEGER,INTENT(IN)::id
    INTEGER::id_prev_t
    INTEGER,INTENT(IN)::Ie_end
    INTEGER,INTENT(IN)::Ie_start
    INTEGER,INTENT(IN)::imin
    INTEGER,INTENT(IN)::it
    INTEGER::ity
    INTEGER,INTENT(IN)::iy
    INTEGER::iy_prev_t
    INTEGER::LAICalcYes
    INTEGER::NetRadiationMethod
    INTEGER,INTENT(IN)::OHMIncQF
    INTEGER::RoughLenHeatMethod
    INTEGER::RoughLenMomMethod
    INTEGER::SMDMethod
    INTEGER::snowUse
    INTEGER::StabilityMethod
    INTEGER::StorageHeatMethod
    INTEGER,INTENT(IN)::tstep
    INTEGER::veg_type
    INTEGER::WaterUseMethod

    REAL(KIND(1D0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1D0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1D0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1D0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1D0)),INTENT(IN)::alt
    REAL(KIND(1D0)),INTENT(IN)::avkdn
    REAL(KIND(1D0)),INTENT(IN)::avRh
    REAL(KIND(1D0)),INTENT(IN)::avU1
    REAL(KIND(1D0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1D0)),INTENT(IN)::bldgH
    REAL(KIND(1D0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1D0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1D0))::CRWmax
    REAL(KIND(1D0))::CRWmin
    REAL(KIND(1D0)),INTENT(IN)::dectime
    REAL(KIND(1D0)),INTENT(IN)::DecTreeH
    REAL(KIND(1D0)),INTENT(IN)::DRAINRT
    REAL(KIND(1D0))::EF_umolCO2perJ
    REAL(KIND(1D0))::EnEF_v_Jkm
    REAL(KIND(1D0)),INTENT(IN)::EveTreeH
    REAL(KIND(1D0)),INTENT(IN)::FAIBldg
    REAL(KIND(1D0)),INTENT(IN)::FAIDecTree
    REAL(KIND(1D0)),INTENT(IN)::FAIEveTree
    REAL(KIND(1D0))::Faut
    REAL(KIND(1D0))::FcEF_v_kgkm
    REAL(KIND(1D0))::fcld_obs
    REAL(KIND(1D0)),INTENT(IN)::FlowChange
    REAL(KIND(1D0))::FrFossilFuel_Heat
    REAL(KIND(1D0))::FrFossilFuel_NonHeat
    REAL(KIND(1D0)),INTENT(IN)::G1
    REAL(KIND(1D0)),INTENT(IN)::G2
    REAL(KIND(1D0)),INTENT(IN)::G3
    REAL(KIND(1D0)),INTENT(IN)::G4
    REAL(KIND(1D0)),INTENT(IN)::G5
    REAL(KIND(1D0)),INTENT(IN)::G6
    REAL(KIND(1D0))::InternalWaterUse_h
    REAL(KIND(1D0))::IrrFracConif
    REAL(KIND(1D0))::IrrFracDecid
    REAL(KIND(1D0))::IrrFracGrass
    REAL(KIND(1D0)),INTENT(IN)::Kmax
    REAL(KIND(1D0))::LAI_obs
    REAL(KIND(1D0)),INTENT(IN)::lat
    REAL(KIND(1D0))::ldown_obs
    REAL(KIND(1D0)),INTENT(IN)::lng
    REAL(KIND(1D0))::MaxQFMetab
    REAL(KIND(1D0))::MinQFMetab
    REAL(KIND(1D0))::NARP_EMIS_SNOW
    REAL(KIND(1D0))::NARP_TRANS_SITE
    REAL(KIND(1D0))::NumCapita
    REAL(KIND(1D0)),INTENT(IN)::PipeCapacity
    REAL(KIND(1D0))::PopDensDaytime
    REAL(KIND(1D0))::PopDensNighttime
    REAL(KIND(1D0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1D0)),INTENT(IN)::PorMin_dec
    REAL(KIND(1D0)),INTENT(IN)::Precip
    REAL(KIND(1D0))::PrecipLimit
    REAL(KIND(1D0))::PrecipLimitAlb
    REAL(KIND(1D0)),INTENT(IN)::Press_hPa
    REAL(KIND(1D0))::qh_obs
    REAL(KIND(1D0))::qn1_obs
    REAL(KIND(1D0))::RadMeltFact
    REAL(KIND(1D0)),INTENT(IN)::RAINCOVER
    REAL(KIND(1D0)),INTENT(IN)::RainMaxRes
    REAL(KIND(1D0)),INTENT(IN)::RunoffToWater
    REAL(KIND(1D0)),INTENT(IN)::S1
    REAL(KIND(1D0)),INTENT(IN)::S2
    REAL(KIND(1D0))::SnowAlbMax
    REAL(KIND(1D0))::SnowAlbMin
    REAL(KIND(1D0))::SnowDensMax
    REAL(KIND(1D0))::SnowDensMin
    REAL(KIND(1D0))::SnowLimBuild
    REAL(KIND(1D0))::SnowLimPaved
    REAL(KIND(1D0))::snow_obs
    REAL(KIND(1D0)),INTENT(IN)::SurfaceArea
    ! REAL(KIND(1D0)),INTENT(IN)::tau_a
    ! REAL(KIND(1D0)),INTENT(IN)::tau_f
    ! REAL(KIND(1D0)),INTENT(IN)::tau_r
    REAL(KIND(1D0)),INTENT(IN)::Temp_C
    REAL(KIND(1D0))::TempMeltFact
    REAL(KIND(1D0)),INTENT(IN)::TH
    REAL(KIND(1D0)),INTENT(IN)::timezone
    REAL(KIND(1D0)),INTENT(IN)::TL
    REAL(KIND(1D0))::TrafficUnits
    REAL(KIND(1D0))::xsmd
    REAL(KIND(1D0)),INTENT(IN)::Z

    INTEGER,DIMENSION(NVEGSURF),INTENT(IN)::LAIType

    REAL(KIND(1D0)),DIMENSION(2)            ::AH_MIN
    REAL(KIND(1D0)),DIMENSION(2)            ::AH_SLOPE_Cooling
    REAL(KIND(1D0)),DIMENSION(2)            ::AH_SLOPE_Heating
    REAL(KIND(1D0)),DIMENSION(2)            ::QF0_BEU
    REAL(KIND(1D0)),DIMENSION(2)            ::Qf_A
    REAL(KIND(1D0)),DIMENSION(2)            ::Qf_B
    REAL(KIND(1D0)),DIMENSION(2)            ::Qf_C
    REAL(KIND(1D0)),DIMENSION(2)            ::T_CRITIC_Cooling
    REAL(KIND(1D0)),DIMENSION(2)            ::T_CRITIC_Heating
    REAL(KIND(1D0)),DIMENSION(2)            ::TrafficRate
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN) ::Ie_a
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN) ::Ie_m
    REAL(KIND(1D0)),DIMENSION(3),INTENT(IN) ::MaxConductance
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::AHProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::HumActivity_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::PopProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::TraffProf_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::WUProfA_tstep
    REAL(KIND(1D0)),DIMENSION(24*3600/tstep,2) ::WUProfM_tstep
    REAL(KIND(1D0)),DIMENSION(7)             ::DayWat
    REAL(KIND(1D0)),DIMENSION(7)           ::DayWatPer
    REAL(KIND(1D0)),DIMENSION(nsurf+1),INTENT(IN)         ::OHM_threshSW
    REAL(KIND(1D0)),DIMENSION(nsurf+1),INTENT(IN)         ::OHM_threshWD
    REAL(KIND(1D0)),DIMENSION(NSURF)           ::chAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF)          ::cpAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::emis
    REAL(KIND(1D0)),DIMENSION(NSURF)        ::kkAnOHM
    REAL(KIND(1D0)),DIMENSION(NSURF)          ::SatHydraulicConduct
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::sfr
    REAL(KIND(1D0)),DIMENSION(NSURF)           ::snowD
    REAL(KIND(1D0)),DIMENSION(NSURF)           ::SoilDepth
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::soilstoreCap
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::StateLimit
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(IN)           ::WetThresh
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::alpha_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::alpha_enh_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::BaseT
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::BaseTe
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::beta_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::beta_enh_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::GDDFull
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::LAIMax
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::LAIMin
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::min_res_bioCO2
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::resp_a
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)        ::resp_b
    REAL(KIND(1D0)),DIMENSION(NVEGSURF),INTENT(IN)        ::SDDFull
    REAL(KIND(1D0)),DIMENSION(0:23,2)          ::snowProf
    REAL(KIND(1D0)),DIMENSION(NVEGSURF)     ::theta_bioCO2
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE             ::Ts5mindata_ir
    REAL(KIND(1D0)),DIMENSION(NSURF+1,NSURF-1),INTENT(IN) ::WaterDist
    REAL(KIND(1D0)),DIMENSION(nsurf+1,4,3),INTENT(IN)     ::OHM_coef
    REAL(KIND(1D0)),DIMENSION(4,NVEGSURF),INTENT(IN)      ::LAIPower
    REAL(KIND(1D0)),DIMENSION(:,:),ALLOCATABLE          ::MetForcingData_grid

    REAL(KIND(1D0)) ::SnowfallCum
    REAL(KIND(1D0))                             ::SnowAlb
    ! INTEGER,DIMENSION(0:NDAYS,3),INTENT(INOUT)                ::DayofWeek
    REAL(KIND(1d0)),DIMENSION(24*3600/tstep)  ::Tair24HR
    REAL(KIND(1D0)),DIMENSION(2*3600/tstep+1),INTENT(INOUT)   ::qn1_av_store
    REAL(KIND(1D0)),DIMENSION(2*3600/tstep+1)  ::qn1_S_av_store
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albDecTr
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albEveTr
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::albGrass
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::DecidCap
    REAL(KIND(1D0)),DIMENSION(0:NDAYS),INTENT(INOUT)          ::porosity
    REAL(KIND(1D0)),DIMENSION(0:NDAYS,5),INTENT(INOUT)        ::GDD
    REAL(KIND(1D0)),DIMENSION(0:NDAYS,9)        ::WU_Day
    REAL(KIND(1D0)),DIMENSION(6,NSURF),INTENT(INOUT)          ::surf
    REAL(KIND(1D0)),DIMENSION(-4:NDAYS,6),INTENT(INOUT)       ::HDD
    REAL(KIND(1D0)),DIMENSION(-4:NDAYS,NVEGSURF),INTENT(INOUT)::LAI
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::alb
    REAL(KIND(1D0)),DIMENSION(NSURF)            ::IceFrac
    REAL(KIND(1D0)),DIMENSION(NSURF)            ::MeltWaterStore
    REAL(KIND(1D0)),DIMENSION(NSURF)           ::SnowDens
    REAL(KIND(1D0)),DIMENSION(NSURF)           ::snowFrac
    REAL(KIND(1D0)),DIMENSION(NSURF)        ::SnowPack
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::soilmoist
    REAL(KIND(1D0)),DIMENSION(NSURF),INTENT(INOUT)            ::state
    REAL(KIND(1D0)),DIMENSION(3600/tstep)      ::qn1_S_store
    REAL(KIND(1D0)),DIMENSION(3600/tstep),INTENT(INOUT)       ::qn1_store

    REAL(KIND(1D0)),DIMENSION(5)                           ::datetimeLine
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSUEWS-5)      ::dataOutLineSUEWS
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSnow-5)       ::dataOutLineSnow
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5)       ::dataOutLineESTM
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5) ::DailyStateLine

    REAL(KIND(1D0)),INTENT(out)::qh


    INTEGER,DIMENSION(NSURF)::snowCalcSwitch
    REAL(KIND(1D0)),INTENT(out)::qe


    Diagnose=0
    snowCalcSwitch=0
    snowUse=0
    DiagQN=0
    DiagQS=0
    WaterUseMethod=1 ! use observed, don't model it
    ity=2
    LAICalcYes=1
    RoughLenMomMethod=2
    EmissionsMethod=0
    NetRadiationMethod=3
    StorageHeatMethod=1
    LAI_obs=0
    ldown_obs=0
    fcld_obs=0
    snow_obs=0
    qn1_obs=0
    qh_obs=0

    MeltWaterStore=0

    SnowAlb=0
    WU_Day=0

    CALL SUEWS_cal_Main(&
         AerodynamicResistanceMethod,AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,& ! input&inout in alphabetical order
         AH_SLOPE_Heating,alb,albDecTr,albEveTr,albGrass,alBMax_DecTr,&
         alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
         alpha_bioCO2,alpha_enh_bioCO2,alt,avkdn,avRh,avU1,BaseT,BaseTe,&
         BaseTHDD,beta_bioCO2,beta_enh_bioCO2,bldgH,CapMax_dec,CapMin_dec,&
         chAnOHM,cpAnOHM,CRWmax,CRWmin,DayWat,DayWatPer,&
         DecidCap,dectime,DecTreeH,Diagnose,DiagQN,DiagQS,DRAINRT,&
         EF_umolCO2perJ,emis,EmissionsMethod,EnEF_v_Jkm,endDLS,EveTreeH,FAIBldg,&
         FAIDecTree,FAIEveTree,Faut,FcEF_v_kgkm,fcld_obs,FlowChange,&
         FrFossilFuel_Heat,FrFossilFuel_NonHeat,G1,G2,G3,G4,G5,G6,GDD,&
         GDDFull,Gridiv,gsModel,HDD,HumActivity_tstep,&
         IceFrac,id,id_prev_t,Ie_a,Ie_end,Ie_m,Ie_start,imin,&
         InternalWaterUse_h,IrrFracConif,IrrFracDecid,IrrFracGrass,it,ity,&
         iy,iy_prev_t,kkAnOHM,Kmax,LAI,LAICalcYes,LAIMax,LAIMin,LAI_obs,&
         LAIPower,LAIType,lat,ldown_obs,lng,MaxConductance,MaxQFMetab,&
         MeltWaterStore,MetForcingData_grid,MinQFMetab,min_res_bioCO2,&
         NARP_EMIS_SNOW,NARP_TRANS_SITE,NetRadiationMethod,&
         NumCapita,OHM_coef,OHMIncQF,OHM_threshSW,&
         OHM_threshWD,PipeCapacity,PopDensDaytime,&
         PopDensNighttime,PopProf_tstep,PorMax_dec,PorMin_dec,porosity,&
         Precip,PrecipLimit,PrecipLimitAlb,Press_hPa,QF0_BEU,Qf_A,Qf_B,&
         Qf_C,qh_obs,qn1_av_store,qn1_obs,qn1_S_av_store,qn1_S_store,&
         qn1_store,RadMeltFact,RAINCOVER,RainMaxRes,resp_a,resp_b,&
         RoughLenHeatMethod,RoughLenMomMethod,RunoffToWater,S1,S2,&
         SatHydraulicConduct,SDDFull,sfr,SMDMethod,SnowAlb,SnowAlbMax,&
         SnowAlbMin,snowD,SnowDens,SnowDensMax,SnowDensMin,SnowfallCum,snowFrac,&
         SnowLimBuild,SnowLimPaved,snow_obs,SnowPack,SnowProf,snowUse,SoilDepth,&
         soilmoist,soilstoreCap,StabilityMethod,startDLS,state,StateLimit,&
         StorageHeatMethod,surf,SurfaceArea,Tair24HR,tau_a,tau_f,tau_r,&
         T_CRITIC_Cooling,T_CRITIC_Heating,Temp_C,TempMeltFact,TH,&
         theta_bioCO2,timezone,TL,TrafficRate,TrafficUnits,&
         TraffProf_tstep,Ts5mindata_ir,tstep,veg_type,&
         WaterDist,WaterUseMethod,WetThresh,WU_Day,WUProfA_tstep,&
         WUProfM_tstep,xsmd,Z,&
         datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&!output
         DailyStateLine)!output

    qh=dataOutLineSUEWS(9)
    qe=dataOutLineSUEWS(10)

  END SUBROUTINE SuMin

END MODULE SUEWS_Driver


MODULE WaterDist_module

  IMPLICIT NONE
CONTAINS

  SUBROUTINE drainage(&
       is,& !input
       state_is,&
       StorCap,&
       DrainEq,&
       DrainCoef1,&
       DrainCoef2,&
       nsh_real,&
       drain_is)!output

    !Calculation of drainage for each land surface.
    !INPUT: Storage capacity, type of drainage equation used, drainage coefficients
    !       used in the equation
    !Modified by HCW 16 Feb 2015
    !  Removed option of Eq 4 (calculation needs to be checked before re-implementing).
    !  Code writes an error if calculated drainage exceeds surface state (but code continues).
    !  This may indicate inappropriate drainage equation, storage capacities or model tstep.
    !Modified by LJ in Aug 2011. Drainage cannot exceed the surface storage.
    !Modified LJ in 10/2010
    !------------------------------------------------------------------------------

    ! use allocateArray
    ! use gis_data
    ! use sues_data
    ! use time

    IMPLICIT NONE
    INTEGER,INTENT(in)::&
         is ! surface type number
    REAL (KIND(1d0)),INTENT(in)::&
         state_is,  &!Wetness status of surface type "is" [mm]
         StorCap,   &!current storage capacity [mm]
         DrainCoef1,&!Drainage coeff 1 [units depend on choice of eqn]
         DrainCoef2,&!Drainage coeff 2 [units depend on choice of eqn]
         DrainEq,   &!Drainage equation to use
         nsh_real    !nsh cast as a real for use in calculations
    REAL (KIND(1d0)),INTENT(out)::&
         drain_is!Drainage of surface type "is" [mm]


    !If surface is dry, no drainage occurs
    IF(state_is<0.000000001) THEN
       drain_is=0.0
    ELSE
       IF(INT(DrainEq)==1) THEN   !Falk and Niemczynowicz (1978): Drainage equation for paved, buildings and irrigated grass

          IF (state_is<StorCap) THEN
             drain_is=0   !No drainage if state is less than storage capacity
          ELSE
             drain_is=(DrainCoef1*(state_is-StorCap)**DrainCoef2)/nsh_real
          ENDIF

       ELSEIF(INT(DrainEq)==2) THEN   !Rutter eqn corrected for c=0, see Eq 9 of Calder & Wright 1986
          drain_is=(DrainCoef1*(EXP(DrainCoef2*state_is)-1))/nsh_real
          ! N.B. -1 is correct here but brackets are wrong in G&O 1991 Eq 5 & Ja11 Eq 18.

       ELSEIF(INT(DrainEq)==3) THEN   !Falk and Niemczynowicz (1978)
          drain_is=(DrainCoef1*(state_is**DrainCoef2))/nsh_real

          ! Option 4 removed by HCW 16 Feb 2015, as it is not used and appears to be problematic
          !elseif(int(DrainEq)==4) then    !Rutter eqn not corrected for c=0
          !   drain(is)=DrainCoef1*exp(DrainCoef2*(state(is)-StorCap))
          !   drain(is)=drain(is)*tstep_real/60 !i.e. multiply by no. mins per timestep  !Is this correct?? Why not divide by nsh_real?
       ENDIF

       ! Check value obtained is physically reasonable
       ! More water cannot drain than is in the surface state
       ! although high initial rate of drainage tries to drain more water than is in state within tstep
       ! May indicate shorter tstep needed, or a more suitable equation
       IF (drain_is>state_is) THEN
          !write(*,*) 'Drainage:', is, drain(is), state(is), drain(is)-state(is), DrainEq, DrainCoef1, DrainCoef2, nsh_real
          CALL ErrorHint(61,'SUEWS_drain: drain_is > state_is for surface is ',drain_is,state_is,is)
          drain_is=state_is   !All water in state is drained (but no more)
       ELSEIF(drain_is<0.0001) THEN
          drain_is=0
       ENDIF
    ENDIF

    RETURN

  END SUBROUTINE drainage
  !------------------------------------------------------------------------------

  SUBROUTINE soilstore(&
       is,& ! input: ! surface type
       sfr,&! surface fractions
       PipeCapacity,&!Capacity of pipes to transfer water
       RunoffToWater,&!Fraction of surface runoff going to water body
       pin,&!Rain per time interval
       wu_EveTr,&!Water use for evergreen trees/shrubs [mm]
       wu_DecTr,&!Water use for deciduous trees/shrubs [mm]
       wu_Grass,&!Water use for grass [mm]
       drain,&!Drainage of each surface type [mm]
       AddWater,&!Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
       addImpervious,&!Water from impervious surfaces of other grids [mm] for whole surface area
       nsh_real,&!nsh cast as a real for use in calculations
       stateOld,&!Wetness status of each surface type from previous timestep [mm]
       AddWaterRunoff,&!Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
       PervFraction,&! sum of surface cover fractions for impervious surfaces
       addVeg,&!Water from vegetated surfaces of other grids [mm] for whole surface area
       soilstoreCap,&!Capacity of soil store for each surface [mm]
       addWaterBody,&!Water from water surface of other grids [mm] for whole surface area
       FlowChange,&!Difference between the input and output flow in the water body
       StateLimit,&!Limit for state of each surface type [mm] (specified in input files)
       runoffAGimpervious,&!  inout:!Above ground runoff from impervious surface [mm] for whole surface area
       surplusWaterBody,&!Extra runoff that goes to water body [mm] as specified by RunoffToWater
       runoffAGveg,&!Above ground runoff from vegetated surfaces [mm] for whole surface area
       runoffPipes,&!Runoff in pipes [mm] for whole surface area
       ev,&!Evaporation
       soilmoist,&!Soil moisture of each surface type [mm]
       SurplusEvap,&!Surplus for evaporation in 5 min timestep
       runoffWaterBody,&!Above ground runoff from water surface [mm] for whole surface area
       runoff_per_interval,&! Total water transported to each grid for grid-to-grid connectivity
       p_mm,&!output: !Inputs to surface water balance
       chang,&!Change in state [mm]
       runoff,&!Runoff from each surface type [mm]
       state&!Wetness status of each surface type [mm]
       )
    !------------------------------------------------------------------------------
    !Calculation of storage change
    ! LJ 27 Jan 2016
    !   -Removed tabs and cleaned the code
    ! HCW 08 Dec 2015
    !   -Added if-loop check for no Paved surfaces
    ! LJ 6 May 2015
    !   - Calculations of the piperunoff exceedings moved to separate subroutine updateFlood.
    !   - Now also called from snow subroutine
    !   - Evaporation is modified using EvapPart
    !   - when no water on impervious surfaces, evap occurs above pervious surfaces instead
    ! Rewritten by HCW 12 Feb 2015
    !   - Old variable 'p' for water input to the surface renamed to 'p_mm'
    !   - All water now added to p_mm first, before threshold checks or other calculations
    !   - Water from other grids now added to p_mm (instead of state for impervious surfaces)
    !   - Removed division of runoff by nsh, as whole model now runs at the same timestep
    !   - Adjusted transfer of ev between surfaces to conserve mass (not depth)
    !   - Volumes used for water transport between grids to account for SurfaceArea changing between grids
    !   - Added threshold check for state(WaterSurf) - was going negative
    ! Last modified HCW 09 Feb 2015
    !   - Removed StorCap input because it is provided by module allocateArray
    !   - Tidied and commented code
    ! Modified by LJ in November 2012:
    !   - P>10 was not taken into account for impervious surfaces - Was fixed.
    !   - Above impervious surfaces possibility of the state to exceed max capacity was limited
    !     although this should be possible - was fixed
    ! Modified by LJ 10/2010
    ! Rewritten mostly by LJ in 2010
    ! To do:
    !   - Finish area normalisation for RG2G & finish coding GridConnections
    !   - What is the 10 mm hr-1 threshold for?
    !  - Decide upon and correct storage capacities here & in evap subroutine
    !  - FlowChange units should be mm hr-1 - need to update everywhere
    !   - Add SurfaceFlood(is)?
    !   - What happens if sfr(is) = 0 or 1?
    !   - Consider how irrigated trees actually works...
    !------------------------------------------------------------------------------

    IMPLICIT NONE

    !Stores flood water when surface state exceeds storage capacity [mm]
    !real(kind(1d0)),dimension(nsurf):: SurfaceFlood
    INTEGER, PARAMETER:: nsurf=7                !Total number of surfaces
    ! INTEGER, PARAMETER:: NVegSurf=3             !Number of surfaces that are vegetated
    ! INTEGER, PARAMETER:: nsurfIncSnow=nsurf+1   !Number of surfaces + snow

    INTEGER:: PavSurf   = 1,&   !When all surfaces considered together (1-7)
         BldgSurf  = 2,&
         ConifSurf = 3,&
         DecidSurf = 4,&
         GrassSurf = 5,&   !New surface classes: Grass = 5th/7 surfaces
         BSoilSurf = 6,&   !New surface classes: Bare soil = 6th/7 surfaces
         WaterSurf = 7
    !  ExcessSurf= 8,&   !Runoff or subsurface soil in WGWaterDist
    !  NSurfDoNotReceiveDrainage=0,&   !Number of surfaces that do not receive drainage water (green roof)
    !  ivConif = 1,&     !When only vegetated surfaces considered (1-3)
    !  ivDecid = 2,&
    !  ivGrass = 3

    INTEGER,INTENT(in)::is ! surface type


    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr(nsurf)! surface fractions
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWater(nsurf)!Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::stateOld(nsurf)!Wetness status of each surface type from previous timestep [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWaterRunoff(nsurf)!Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilstoreCap(nsurf)!Capacity of soil store for each surface [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::StateLimit(nsurf)!Limit for state of each surface type [mm] (specified in input files)

    REAL(KIND(1d0)),INTENT(in)::PipeCapacity!Capacity of pipes to transfer water
    REAL(KIND(1d0)),INTENT(in)::RunoffToWater!Fraction of surface runoff going to water body
    REAL(KIND(1d0)),INTENT(in)::pin!Rain per time interval
    REAL(KIND(1d0)),INTENT(in)::wu_EveTr!Water use for evergreen trees/shrubs [mm]
    REAL(KIND(1d0)),INTENT(in)::wu_DecTr!Water use for deciduous trees/shrubs [mm]
    REAL(KIND(1d0)),INTENT(in)::wu_Grass!Water use for grass [mm]
    REAL(KIND(1d0)),INTENT(in)::addImpervious!Water from impervious surfaces of other grids [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(in)::nsh_real!nsh cast as a real for use in calculations
    REAL(KIND(1d0)),INTENT(in)::PervFraction! sum of surface cover fractions for impervious surfaces
    REAL(KIND(1d0)),INTENT(in)::addVeg!Water from vegetated surfaces of other grids [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(in)::addWaterBody!Water from water surface of other grids [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(in)::FlowChange!Difference between the input and output flow in the water body


    REAL(KIND(1d0)),INTENT(inout)::runoffAGimpervious!Above ground runoff from impervious surface [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(inout)::surplusWaterBody!Extra runoff that goes to water body [mm] as specified by RunoffToWater
    REAL(KIND(1d0)),INTENT(inout)::runoffAGveg!Above ground runoff from vegetated surfaces [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(inout)::runoffPipes!Runoff in pipes [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(inout)::ev!Evaporation
    REAL(KIND(1d0)),INTENT(inout)::runoffWaterBody!Above ground runoff from water surface [mm] for whole surface area
    REAL(KIND(1d0)),INTENT(inout)::runoff_per_interval! Total water transported to each grid for grid-to-grid connectivity

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::soilmoist  !Soil moisture of each surface type [mm]
    REAL(KIND(1d0)),DIMENSION(2),INTENT(inout)    ::SurplusEvap!Surplus for evaporation in 5 min timestep

    REAL(KIND(1d0)),INTENT(out)::p_mm!Inputs to surface water balance

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::chang !Change in state [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoff!Runoff from each surface type [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::drain !Drainage of each surface type [mm]
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::state !Wetness status of each surface type [mm]

    !Extra evaporation [mm] from impervious surfaces which cannot happen due to lack of water
    REAL(KIND(1d0)):: EvPart
    REAL(KIND(1d0)),PARAMETER:: NotUsed=-55.5,&
         IPThreshold_mmhr=10

    !Initialise extra evaporation to zero
    EvPart=0

    !SurfaceFlood(is) = 0 !!This probably needs to be carried over between timesteps, but reset for now

    !==================================================================
    ! Combine water inputs to the current surface
    ! Add external water use for each surface type
    IF(is==ConifSurf) THEN
       p_mm=pin+wu_EveTr
    ELSEIF(is==DecidSurf) THEN
       p_mm=pin+wu_DecTr
    ELSEIF(is==GrassSurf) THEN
       p_mm=pin+wu_Grass
    ELSE
       p_mm=pin
    ENDIF

    ! Add water from other surfaces within the same grid (RS2S) ----
    ! AddWater is the water supplied to the current surface from other surfaces
    !  i.e. drain*WaterDist (see SUEWS_ReDistributeWater)
    p_mm=p_mm+AddWater(is)

    !==== Impervious surfaces (Paved, Buildings) ======================
    IF(is==PavSurf.OR.is==BldgSurf) THEN

       ! Add water from neighbouring grids (RG2G)
       ! Add to PavSurf only, as water cannot flow onto buildings
       IF (is==PavSurf) THEN
          IF(sfr(PavSurf)/=0) THEN   ! If loop added HCW 08 Dec 2015
             p_mm=p_mm+addImpervious/sfr(PavSurf)
          ENDIF
       ENDIF

       ! Calculate change in surface state (inputs - outputs)
       chang(is)=p_mm-(drain(is)+ev)

       ! If p_mm is too large, excess goes to runoff (i.e. the rate of water supply is too fast)
       ! and does not affect state
       IF(p_mm>IPThreshold_mmhr/nsh_real) THEN
          runoff(is)=runoff(is)+(p_mm-IPThreshold_mmhr/nsh_real)
          chang(is)=IPThreshold_mmhr/nsh_real-(drain(is)+ev)
       ENDIF

       ! Calculate updated state using chang
       state(is)=state(is)+chang(is)

       ! Check state is within physical limits between zero (dry) and max. storage capacity
       IF(state(is)<0.0) THEN   ! Cannot have a negative surface state
          ! If there is not sufficient water on the surface, then don't allow this evaporation to happen
          ! Allow evaporation only until surface is dry (state(is)=0); additional evaporation -> evaporation surplus
          SurplusEvap(is)=ABS(state(is))   !Surplus evaporation is that which tries to evaporate non-existent water
          ev = ev-SurplusEvap(is)          !Limit evaporation according to water availability
          state(is)=0.0                    !Now surface is dry
          ! elseif (state(is)>surf(6,is)) then   !!This should perhaps be StateLimit(is)
          !    !! If state exceeds the storage capacity, then the excess goes to surface flooding
          !    !SurfaceFlood(is)=SurfaceFlood(is)+(state(is)-surf(6,is))   !!Need to deal with this properly
          !    runoff(is)=runoff(is)+(state(is)-surf(6,is))   !!needs to go to flooding
          !    state(is)=surf(6,is)              !Now surface state is at max (storage) capacity
       ENDIF

       ! Recalculate change in surface state from difference with previous timestep
       chang(is) = state(is)-stateOld(is)

       ! Runoff -------------------------------------------------------
       ! For impervious surfaces, some of drain(is) becomes runoff
       runoff(is)=runoff(is)+drain(is)*AddWaterRunoff(is)   !Drainage (that is not flowing to other surfaces) goes to runoff

       !So, up to this point, runoff(is) can have contributions if
       ! p_mm > ipthreshold (water input too fast)
       ! state > surf(6,is) (net water exceeds storage capacity)
       ! WaterDist specifies some fraction of drain(is) -> runoff

       !==== Pervious surfaces (Conif, Decid, Grass, BSoil, Water) =======
    ELSEIF(is>=3) THEN

       ! Transfer evaporation surplus from impervious surfaces to pervious surfaces
       IF(PervFraction/=0) THEN   !If pervious surfaces exist
          EvPart=(SurplusEvap(PavSurf)*sfr(PavSurf)+SurplusEvap(BldgSurf)*sfr(BldgSurf))/PervFraction
       ELSE         !If no pervious surface, SurplusEvap cannot be transferred and this evap cannot
          EvPart=0  !happen (will increase QHinstead)
       ENDIF

       ! Add surplus evaporation to ev for pervious surfaces
       ev=ev+EvPart

       !==== For Conif, Decid, Grass, BSoil surfaces ==================
       IF (is/=WaterSurf) THEN

          ! ---- Add water from neighbouring grids (RG2G) ----
          ! Add to Grass and BSoil only, as water cannot flow onto trees
          IF (is==GrassSurf.OR.is==BSoilSurf) THEN
             IF ((sfr(GrassSurf)+sfr(BSoilSurf))/=0) THEN
                p_mm=p_mm+addVeg/(sfr(GrassSurf)+sfr(BSoilSurf))
             ENDIF
          ENDIF

          ! Calculate change in surface state (inputs - outputs)
          chang(is)=p_mm-(drain(is)+ev)

          ! If p_mm is too large, excess goes to runoff (i.e. the rate of water supply is too fast)
          !  and does not affect state
          IF (p_mm>IPThreshold_mmhr/nsh_real) THEN
             runoff(is)=runoff(is)+(p_mm-IPThreshold_mmhr/nsh_real)
             chang(is)=IPThreshold_mmhr/nsh_real-(drain(is)+ev)
          ENDIF

          ! Calculate updated state using chang
          state(is)=state(is)+chang(is)

          ! Check state is within physical limits between zero (dry) and max. storage capacity
          IF(state(is)<0.0) THEN   ! Cannot have a negative surface state
             ! If there is not sufficient water on the surface, then remove water from soilstore
             ! Allow evaporation until soilmoist is depleted and surface is dry
             IF((soilmoist(is)+state(is))>=0) THEN
                soilmoist(is)=soilmoist(is)+state(is)
                state(is)=0.0
                ! If there is not sufficient water on the surface or soilstore, then don't allow this evaporation to happen
             ELSE
                ev=ev-ABS(state(is))   !Limit evaporation according to water availability
                state(is)=0.0          !Now surface is dry
             ENDIF
             !! What about if there is some water in soilstore, but not enough to provide all the water for evaporation??
             !! Is this saying water can be evaporated from the soilstore as easily as from the surface??
             !elseif (state(is)>surf(6,is)) then   !!This should perhaps be StateLimit(is)
             !   !! If state exceeds the storage capacity, then the excess goes to surface flooding
             !   !SurfaceFlood(is)=SurfaceFlood(is)+(state(is)-surf(6,is))   !!Need to deal with this properly
             !   runoff(is)=runoff(is)+(state(is)-surf(6,is))   !!needs to go to flooding
             !   state(is)=surf(6,is)              !Now surface state is at max (storage) capacity
          ENDIF

          ! Recalculate change in surface state from difference with previous timestep
          chang(is) = state(is)-stateOld(is)

          !Where should this go? Used to be before previous part!!
          ! Soilmoist -------------------------------------------------
          ! For pervious surfaces (not water), some of drain(is) goes to soil storage
          ! Drainage (that is not flowing to other surfaces) goes to soil storages
          soilmoist(is)=soilmoist(is)+drain(is)*AddWaterRunoff(is)

          ! If soilstore is full, the excess will go to runoff
          IF(soilmoist(is)>soilstoreCap(is)) THEN              !! Should this also go to flooding of some sort?
             runoff(is)=runoff(is)+(soilmoist(is)-soilstoreCap(is))
             soilmoist(is)=soilstoreCap(is)
          ELSEIF (soilmoist(is)<0) THEN   !!But where does this lack of water go? !!Can this really happen here??
             CALL ErrorHint(62,'SUEWS_store: soilmoist(is) < 0 ',soilmoist(is),NotUsed,is)
             ! Code this properly - soilmoist(is) < 0 shouldn't happen given the above loops
             !soilmoist(is)=0   !Groundwater / deeper soil should kick in
          ENDIF

          !==== Water surface ========================================
       ELSEIF (is==WaterSurf) THEN

          IF(sfr(WaterSurf)/=0)THEN

             ! ---- Add water from neighbouring grids (RG2G) ----
             p_mm=p_mm+addWaterBody/sfr(WaterSurf)

             ! Calculate change in surface state (inputs - outputs)
             ! No drainage for water surface
             ! FlowChange is the difference in input and output flows [mm hr-1]   !!Should this really be a constant??
             chang(is)=p_mm+FlowChange/nsh_real-(ev)

             ! Calculate updated state using chang
             state(is)=state(is)+chang(is)

             ! Check state is within physical limits between zero (dry) and max. storage capacity
             IF(state(is)<0.0) THEN   ! Cannot have a negative surface state
                ! If there is not sufficient water on the surface, then don't allow this evaporation to happen
                ev=ev-ABS(state(is))   !Limit evaporation according to water availability
                state(is)=0.0          !Now surface is dry
                !elseif (state(is)>surf(6,is)) then   !!This should perhaps be StateLimit(is)
                !   !! If state exceeds the storage capacity, then the excess goes to surface flooding
                !   !SurfaceFlood(is)=SurfaceFlood(is)+(state(is)-surf(6,is))   !!Need to deal with this properly
                !   runoff(is)=runoff(is)+(state(is)-surf(6,is))   !!needs to go to flooding
                !   state(is)=surf(6,is)              !Now surface state is at max (storage) capacity
             ENDIF

             ! Recalculate change in surface state from difference with previous timestep
             chang(is) = state(is)-stateOld(is)

             ! If state exceeds limit, then excess goes to runoff (currently applies to water surf only)
             IF (state(WaterSurf)>StateLimit(WaterSurf)) THEN
                runoff(WaterSurf)=runoff(WaterSurf)+(state(WaterSurf)-StateLimit(WaterSurf))
                state(WaterSurf)=StateLimit(WaterSurf)
                runoffWaterBody=runoffWaterBody+runoff(WaterSurf)*sfr(WaterSurf)
             ELSE
                state(WaterSurf)=state(WaterSurf)+surplusWaterBody
                IF (state(WaterSurf)>StateLimit(WaterSurf)) THEN
                   runoffWaterBody=runoffWaterBody+(state(WaterSurf)-StateLimit(WaterSurf))*sfr(WaterSurf)
                   state(WaterSurf)=StateLimit(WaterSurf)
                ENDIF
             ENDIF

             ! Recalculate change in surface state from difference with previous timestep
             chang(is) = state(is)-stateOld(is)
          ENDIF

       ENDIF   !end of WaterSurf

    ENDIF   !end of different surfaces

    !==================================================================
    !==== RUNOFF ======================================================

    ! Need to consider areas here - SurfaceArea may vary between grids too
    ! - also implement where water for next surface is calculated (RunoffFromGrid subroutine)
    ! Calculations of the piperunoff exceedensances moved to separate subroutine so that from snow same
    ! calculations can be made. LJ in May 2015

    IF(is<WaterSurf) THEN   !Not for water body

       ! Add runoff to pipes
       runoffPipes=runoffPipes+(runoff(is)*sfr(is))
       !  CALL updateFlood
       CALL updateFlood(&
                                ! input:
            nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf,&
            sfr,PipeCapacity,RunoffToWater,&
                                ! inout:
            runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes&
            )
    ENDIF

    runoff_per_interval=runoff_per_interval+(runoff(is)*sfr(is)) !The total runoff from the area !!Check (HCW)

  END SUBROUTINE soilstore
  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  SUBROUTINE updateFlood(&

                                ! input:
       nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf,&
       sfr,PipeCapacity,RunoffToWater,&
                                ! inout:
       runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes&
       )

    ! USE allocateArray
    ! USE sues_data

    IMPLICIT NONE
    INTEGER, INTENT(in) :: nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf
    REAL(KIND(1d0)), INTENT(in) :: sfr(nsurf),PipeCapacity,RunoffToWater
    REAL(KIND(1d0)), INTENT(inout) :: runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes

    ! If pipe capacity is full, surface runoff occurs
    ! N.B. this will happen each loop (replicates pipes filling up)
    IF(runoffPipes>PipeCapacity) THEN

       !------Paved and building surface
       IF(is==PavSurf.OR.is==BldgSurf) THEN
          IF(sfr(WaterSurf)>0.0000001) THEN
             ! If there is some water present, the water surface will take some of the flood water (fraction RunoffToWater)
             ! RunoffToWater is specified in SUEWS_SiteSelect.txt
             runoffAGimpervious=runoffAGimpervious+(runoffPipes-PipeCapacity)*(1-RunoffToWater)
             surplusWaterBody=surplusWaterBody+(runoffPipes-PipeCapacity)*RunoffToWater
          ELSE
             ! Otherwise, all flood water must go to runoff
             runoffAGimpervious=runoffAGimpervious+(runoffPipes-PipeCapacity)
          ENDIF
          !------other surfaces
       ELSEIF(is>=ConifSurf.AND.is<=BSoilSurf) THEN
          IF(sfr(WaterSurf)>0.0000001) THEN
             ! If there is some water present, the water surface will take some of the flood water (fraction RunoffToWater)
             runoffAGveg=runoffAGveg+(runoffPipes-PipeCapacity)*(1-RunoffToWater)
             surplusWaterBody=surplusWaterBody+(runoffPipes-PipeCapacity)*RunoffToWater
          ELSE
             ! Otherwise, all flood water must go to runoff
             runoffAGveg=runoffAGveg+(runoffPipes-PipeCapacity)
          ENDIF
       ENDIF

       runoffPipes=PipeCapacity   !Pipes are at their max capacity

    ENDIF   !If runoff exceed pipe capacity

  END SUBROUTINE updateFlood

  SUBROUTINE ReDistributeWater(&
                                ! input:
       nsurf,& ! surface type number
       WaterSurf,&
       snowUse,&
       WaterDist,  &
       sfr,   &!
       Drain,&
                                ! output:
       AddWaterRunoff,&
       AddWater&
       )
    !Drainage moves into different parts defined by WaterDistSS_YYYY.txt. LJ 2010
    !AddWater(is) is that amount of water that is gained for each surface
    !Latest update takes snow into account. 22/03/2013 LJ
    !-------------------------------------------------------------------

    ! use allocateArray
    ! use data_in
    ! use gis_data
    ! use sues_data

    IMPLICIT NONE

    INTEGER,INTENT(in)::nsurf ! number of surface types
    INTEGER,INTENT(in)::WaterSurf!=7, water surface code
    INTEGER,INTENT(in)::snowUse!Snow part used (1) or not used (0)

    REAL (KIND(1d0)),INTENT(in)::WaterDist(nsurf+1,nsurf-1) !Within-grid water distribution to other surfaces and runoff/soil store [-]
    REAL (KIND(1d0)),INTENT(in)::sfr(nsurf)                !Surface fractions [-]
    REAL (KIND(1d0)),INTENT(in)::Drain(nsurf)               !Drainage of each surface type [mm]

    REAL (KIND(1d0)),INTENT(out)::AddWaterRunoff(nsurf)!Fraction of water going to runoff/sub-surface soil (WGWaterDist) [-]
    REAL (KIND(1d0)),INTENT(out)::AddWater(nsurf)        !Water from other surfaces (WGWaterDist in SUEWS_ReDistributeWater.f95) [mm]

    INTEGER::ii,jj,&
         NSurfDoNotReceiveDrainage=0!Number of surfaces that do not receive drainage water (green roof)

    !Fractions that go to runoff from each surface
    DO ii=1,nsurf-1   !not water in the calculation
       AddWaterRunoff(ii)=WaterDist(8,ii)
    ENDDO
    AddWaterRunoff(WaterSurf)=0
    AddWater=0

    DO ii=1,nsurf-NSurfDoNotReceiveDrainage !go through surfaces from 1 to 7. These gain water through drainage
       DO jj=1,nsurf-(NSurfDoNotReceiveDrainage+1) !From where surface ii can gain water - can't gain water from itself

          IF (sfr(ii)/=0) THEN !Water movement takes place only if surface fraction exists

             !No snow calculations!
             IF (snowUse==0) THEN
                AddWater(ii)=AddWater(ii)+(Drain(jj)*sfr(jj)/sfr(ii))*WaterDist(ii,jj) !Original

                !Snow included, This needs to be fixed at some point. LJ Mar 2013
             ELSE
                AddWaterRunoff(jj)=AddWaterRunoff(jj)+WaterDist(ii,jj) !No receiving surface -> runoff
             ENDIF

          ELSE
             AddWaterRunoff(jj)=AddWaterRunoff(jj)+WaterDist(ii,jj) !If no receiving surface exists,
             !water fraction goes to AddWaterRunoff
          ENDIF
       ENDDO
    ENDDO
  END SUBROUTINE ReDistributeWater


  SUBROUTINE SUEWS_update_SoilMoist(&
       NonWaterFraction,&!input
       soilstoreCap,sfr,soilmoist,&
       SoilMoistCap,SoilState,&!output
       vsmd,smd)
    IMPLICIT NONE
    INTEGER,PARAMETER ::nsurf     = 7 ! number of surface types
    INTEGER,PARAMETER ::ConifSurf = 3 !New surface classes: Grass = 5th/7 surfaces
    INTEGER,PARAMETER ::DecidSurf = 4 !New surface classes: Grass = 5th/7 surfaces
    INTEGER,PARAMETER ::GrassSurf = 5

    ! INTEGER,INTENT(in)::nsurf,ConifSurf,DecidSurf,GrassSurf
    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in),DIMENSION(nsurf)::soilstoreCap,sfr,soilmoist

    REAL(KIND(1d0)),INTENT(out)::SoilMoistCap,SoilState
    REAL(KIND(1d0)),INTENT(out)::vsmd,smd

    INTEGER :: is


    SoilMoistCap=0   !Maximum capacity of soil store [mm] for whole surface
    SoilState=0      !Area-averaged soil moisture [mm] for whole surface

    IF (NonWaterFraction/=0) THEN !Soil states only calculated if soil exists. LJ June 2017
       DO is=1,nsurf-1   !No water body included
          SoilMoistCap=SoilMoistCap+(soilstoreCap(is)*sfr(is)/NonWaterFraction)
          SoilState=SoilState+(soilmoist(is)*sfr(is)/NonWaterFraction)
       ENDDO
    ENDIF

    !If loop removed HCW 26 Feb 2015
    !if (ir==1) then  !Calculate initial smd
    smd=SoilMoistCap-SoilState
    !endif

    ! Calculate soil moisture for vegetated surfaces only (for use in surface conductance)
    vsmd=0
    DO is=ConifSurf,GrassSurf  !Vegetated surfaces only
       IF ( sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) ==0 ) THEN
          vsmd=0
       ELSE
          vsmd=vsmd+(soilstoreCap(is) - soilmoist(is))*sfr(is)/(sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf))
       END IF
       !write(*,*) is, vsmd, smd
    ENDDO

  END SUBROUTINE SUEWS_update_SoilMoist


  !========== Calculate soil moisture ============
  SUBROUTINE SUEWS_cal_SoilMoist(&
       SMDMethod,xsmd,NonWaterFraction,SoilMoistCap,&!input
       SoilStoreCap,surf_chang_per_tstep,&
       soilmoist,soilmoistOld,sfr,&
       smd,smd_nsurf,tot_chang_per_tstep,SoilState)!output

    IMPLICIT NONE
    INTEGER,PARAMETER::nsurf=7


    INTEGER,INTENT(in) ::SMDMethod
    REAL(KIND(1d0)),INTENT(in)::xsmd
    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in)::SoilMoistCap

    REAL(KIND(1d0)),INTENT(in)::surf_chang_per_tstep
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilmoistOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::SoilStoreCap        !Capacity of soil store for each surface [mm]

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::smd_nsurf
    REAL(KIND(1d0)),INTENT(out)::SoilState
    REAL(KIND(1d0)),INTENT(out)::smd
    REAL(KIND(1d0)),INTENT(out)::tot_chang_per_tstep

    REAL(KIND(1d0)),PARAMETER::NotUsed=-999
    REAL(KIND(1d0)),PARAMETER::NAN=-999
    INTEGER :: is

    SoilState=0       !Area-averaged soil moisture [mm] for whole surface
    IF (NonWaterFraction/=0) THEN !Fixed for water surfaces only
       DO is=1,nsurf-1   !No water body included
          SoilState=SoilState+(soilmoist(is)*sfr(is)/NonWaterFraction)
          IF (SoilState<0) THEN
             CALL ErrorHint(62,'SUEWS_Calculations: total SoilState < 0 (just added surface is) ',SoilState,NotUsed,is)
          ELSEIF (SoilState>SoilMoistCap) THEN
             CALL ErrorHint(62,'SUEWS_Calculations: total SoilState > capacity (just added surface is) ',SoilState,NotUsed,is)
             !SoilMoist_state=SoilMoistCap !What is this LJ 10/2010 - SM exceeds capacity, but where does extra go?HCW 11/2014
          ENDIF
       ENDDO  !end loop over surfaces
    ENDIF

    ! Calculate soil moisture deficit
    smd=SoilMoistCap-SoilState   !One value for whole surface
    smd_nsurf=SoilstoreCap-soilmoist   !smd for each surface

    ! Soil stores can change after horizontal water movements
    ! Calculate total change in surface and soil state
    tot_chang_per_tstep = surf_chang_per_tstep   !Change in surface state
    DO is=1,(nsurf-1)   !No soil for water surface (so change in soil moisture is zero)
       tot_chang_per_tstep = tot_chang_per_tstep + ((SoilMoist(is)-soilmoistOld(is))*sfr(is))   !Add change in soil state
    ENDDO

    IF (SMDMethod>0) THEN
       !  smd_nsurf=NAN
       smd_nsurf=NAN
       smd=xsmd
    ENDIF


  END SUBROUTINE SUEWS_cal_SoilMoist


END MODULE WaterDist_module

MODULE NARP_MODULE
  !==============================================================================
  !NET ALL WAVE RADIATION PARAMETERIZATION ROUTINES
  !B. OFFERLE
  !DEPT OF GEOGRAPHY
  !INDIANA UNIVERSITY
  !bofferle@indiana.edu
  !
  !MODIFIED: 19 DEC 2002
  !CURRENTLY THE SMITH GRID IS ONLY VALID FOR THE N. HEMISPHERE
  !
  !Thomas Loridan, May 2008
  !4.1: MODIFICATION FOR CLOUD FRACTION PARAMETERIZATION AT NIGHT USING THE RATE OF COOLING.
  !     EOSHIFT INTRINSIC FUNCTION WAS ALSO REMOVED BECAUSE IT IS COMPILER DEPENDENT.
  !
  !6.0  T. Loridan - June 2009
  !     Different longwave down options (ldown_option)
  ! 1 - LDOWN Observed (add data as last column in met file)
  ! 2 - LDOWN modelled from observed FCLD (add data as last column in met file)
  ! 3 - LDOWN modelled from FCLD(RH,TA)
  ! 4 - LDOWN modelled from FCLD(Kdown); i.e. day FCLD only
  !     cloud fraction is kept constant throught the night (Offerle et al. 2003, JAM)
  ! 5 - Option 3 at night and 4 during the day (might cause discontinuities in LDOWN)

  !SUEWS   L. Jarvi - Oct 2010
  !Currently LDOWN options 4 and 5 commented out in order to reduce input files.
  !Last modified:
  ! TS 06 Aug 2017 - interface modified to receive explict input and output arguments
  ! LJ 27 Jan 2016 - Removal of tabs, cleaning of the code
  ! FL July 2014 - Variables are moved to modules in NARP subroutine. Snow related should also in future.
  ! FL Nov 2013 - A new sun postion algorithm added
  ! LJ May 2013 - Main program NARP changed to take subsurfaces and snow into account here and not
  ! in the main program
  ! LJ Oct 2012 - Zenith angle change in the calculation of albedo added
  ! sg feb 2012 - Allocatable array module added

  !==============================================================================================
  ! USE allocateArray

  IMPLICIT NONE

CONTAINS
  !==============================================================================
  SUBROUTINE RadMethod(&
       NetRadiationMethod,&!inout
       snowUse,&!input
       NetRadiationMethodX,AlbedoChoice,ldown_option)!output
    IMPLICIT NONE
    INTEGER,INTENT(in) :: NetRadiationMethod ! the one from RunControl setting
    INTEGER,INTENT(in) ::snowUse
    INTEGER,INTENT(out)::NetRadiationMethodX ! processed NetRadiationMethod to be used for other radiation calculations
    INTEGER,INTENT(out)::AlbedoChoice,ldown_option
    !Determine what should be done with respect to radiation
    ! TODO: this can be wrapped into a subroutine, TS 20 Oct 2017
    AlbedoChoice=0
    ldown_option=0
    IF(NetRadiationMethod==0)THEN    !Observed Q* from the met input file will be used
       NetRadiationMethodX=0
       !  ldown_option is not required if NetRadiationMethodX=0 as LDOWN calculations are skipped

       IF(snowUse==1) THEN            !If snow is modelled, NARP is needed for surface temperature
          ! NetRadiationMethod=3000
          NetRadiationMethodX=3000
          ldown_option=3              !LDOWN will be modelled
          !NetRadiationMethod=NetRadiationMethod/1000
       ENDIF

    ELSEIF(NetRadiationMethod>0)THEN  !Modelled Q* is used (NARP)
       AlbedoChoice=-9
       IF(NetRadiationMethod<10) THEN
          AlbedoChoice=0
          IF(NetRadiationMethod==1)ldown_option=1
          IF(NetRadiationMethod==2)ldown_option=2
          IF(NetRadiationMethod==3)ldown_option=3
          NetRadiationMethodX=NetRadiationMethod

       ELSEIF(NetRadiationMethod>=100.AND.NetRadiationMethod<1000) THEN
          AlbedoChoice=1
          IF(NetRadiationMethod==100)ldown_option=1
          IF(NetRadiationMethod==200)ldown_option=2
          IF(NetRadiationMethod==300)ldown_option=3
          ! NetRadiationMethod=NetRadiationMethod/100
          NetRadiationMethodX=NetRadiationMethod/100
       ENDIF

       !If bad NetRadiationMethod value
       IF(NetRadiationMethodX>3.OR. AlbedoChoice==-9)THEN
          WRITE(*,*) 'NetRadiationMethod=',NetRadiationMethodX
          WRITE(*,*) 'Value not usable'
          STOP
       ENDIF
    ENDIF


  END SUBROUTINE RadMethod




  !==============================================================================
  SUBROUTINE NARP(&
       nsurf,sfr,snowFrac,alb,emis,IceFrac,&! input:
       NARP_TRANS_SITE,NARP_EMIS_SNOW,&
       DTIME,ZENITH_deg,kdown,Temp_C,RH,Press_hPa,qn1_obs,&
       SnowAlb,&
       AlbedoChoice,ldown_option,&
       NetRadiationMethodX,DiagQN,&
       QSTARall,QSTAR_SF,QSTAR_S,kclear,KUPall,LDOWN,LUPall,fcld,TSURFall,&! output:
       qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)
    !KCLEAR,FCLD,DTIME,KDOWN,QSTARall,KUPall,LDOWN,LUPall,TSURFall,&
    !AlbedoChoice,ldown_option,Temp_C,Press_hPa,Ea_hPa,qn1_obs,RH,&
    !,zenith_degnetRadiationChoice,

    !SUBROUTINE NARP(QSTAR,DTIME,KDOWN,LDOWN,T,RH,PRESS,FCLD,SNOW)
    !returns estimate of Q* given the meteorological fields and prior
    !configuration call.

    !OUTPUT FIELDS
    !QSTARall (W/m2) = net all wave radiation
    !KCLEAR          = clear sky incoming solar radiation
    !KUPall          = reflect solar radiation
    !LDOWN           = incoming longwave radiation (observed or modelled depending on ldown_option)
    !LUPall          = outgoing longwaver radiation
    !FCLD            = estimated cloud fraction (used only for emissivity estimate)
    !                  FCLD USED AS INPUT ALSO
    !TSURFall (DEG C)= estimated surface temperature
    !QSTAR_SF        = net all wave radiation for snow free surface
    !QSTAR_S         = net all wave radiation for SnowPack

    !INPUT FIELDS
    !DTIME (days) = local time, not daylight savings
    !KDOWN (W/m2) = incoming solar radiation
    !T (DEG C)    = air temperature near model height
    !RH (%)       = relative humidity near model height
    !PRESS (mb)   = station pressure, use estimate if unavailable
    !qn1_obs      = Observed Q*


    !INTERNAL FIELDS
    ! TemP_K = air temperature in K
    ! ES_hPs = saturation vapor pressure (hPa)
    ! EA_Pa = vapor pressure (hPa)
    ! TD = dewpoint (C)
    ! ZENITH = solar zenith angle
    ! ALB0 = surface albedo
    ! EMIS0 = surface emissivity
    ! EMIS_A = atmospheric emissivity
    ! TRANS = atmospheric transmissivity
    ! LUPCORR = correction for surface heating by kdown (W/m2)
    ! SIGMATK4 = energy flux density
    ! KDOWN_HR = hourly average insolation
    ! DOY = day of year

    !Modified by LJ to calcuate snow free and SnowPack components (May 2013)
    !Modified to define variables in data_in module
    !-------------------------------------------------------------------------------
    ! USE allocateArray
    ! use gis_data
    ! use data_in ! Included 20140701, FL
    ! use moist   ! Included 20140701, FL
    ! use time    ! Included 20140701, FL
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::sfr
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::snowFrac
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::alb
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::emis
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) ::IceFrac
    ! REAL(KIND(1D0)),DIMENSION(365),INTENT(in) ::NARP_G

    REAL(KIND(1D0)),INTENT(in) ::DTIME
    REAL(KIND(1D0)),INTENT(in) ::ZENITH_deg
    REAL(KIND(1D0)),INTENT(in) ::kdown
    REAL(KIND(1D0)),INTENT(in) ::Temp_C
    REAL(KIND(1D0)),INTENT(in) ::RH
    REAL(KIND(1D0)),INTENT(in) ::Press_hPa
    REAL(KIND(1D0)),INTENT(in) ::qn1_obs
    REAL(KIND(1D0)),INTENT(in) ::SnowAlb
    REAL(KIND(1D0)),INTENT(in) ::NARP_TRANS_SITE
    REAL(KIND(1D0)),INTENT(in) ::NARP_EMIS_SNOW

    INTEGER,INTENT(in) ::nsurf
    INTEGER,INTENT(in) ::NetRadiationMethodX ! the one processed by RadMethod
    INTEGER,INTENT(in) ::AlbedoChoice
    INTEGER,INTENT(in) ::ldown_option
    INTEGER,INTENT(in) ::DiagQN

    REAL(KIND(1D0)),INTENT(out) ::QSTARall
    REAL(KIND(1D0)),INTENT(out) ::QSTAR_SF
    REAL(KIND(1D0)),INTENT(out) ::QSTAR_S
    REAL(KIND(1D0)),INTENT(out) ::kclear
    REAL(KIND(1D0)),INTENT(out) ::KUPall
    REAL(KIND(1D0)),INTENT(out) ::LDOWN
    REAL(KIND(1D0)),INTENT(out) ::LUPall
    REAL(KIND(1D0)),INTENT(out) ::fcld
    REAL(KIND(1D0)),INTENT(out) ::TSURFall

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::qn1_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::kup_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::Tsurf_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::Tsurf_ind

    REAL(KIND(1d0)),DIMENSION(nsurf) ::qn1_ind
    REAL(KIND(1d0)),DIMENSION(nsurf) ::kup_ind
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind


    REAL(KIND(1d0)),DIMENSION(nsurf) ::qn1_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::kup_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::Tsurf_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind_snow


    REAL(KIND(1D0)) ::Temp_K,TD,ZENITH,QSTAR,QSTAR_SNOW,KUP_SNOW,LUP_SNOW,TSURF_SNOW,KUP,LUP,TSURF
    REAL(KIND(1D0)) ::ALB0,EMIS0,EMIS_A,TRANS!,RH,DTIME,KDOWN
    REAL(KIND(1D0)) ::LUPCORR,SIGMATK4,KDOWN_HR=0.
    INTEGER         ::DOY, is

    REAL(KIND(1D0))::qn1_cum,kup_cum,lup_cum,tsurf_cum,&   !Cumulative radiation components
         qn1_is,kup_is,lup_is,tsurf_is,&       !Sub-surface radiation components
         SF_all,ALB1

    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292,&
                                !  RAD2DEG=57.29577951,&
         SIGMA_SB=5.67E-8

    ! NB: NARP_G is not assigned with a value in SUEWS_translate.
    ! 3.0 is used here as annual average for mid-latitude areas. TS 24 Oct 2017
    REAL(KIND(1D0)),DIMENSION(365),PARAMETER ::  NARP_G=3.0
    !Initialize variables
    ! RH=avrh
    ! DTIME=dectime
    ! KDOWN=avkdn
    Temp_K=Temp_C+273.16
    SIGMATK4=SIGMA_SB*Temp_K**4
    TD=DEWPOINT(Temp_C,RH)
    ! Sun postition is now calculated in the main loop, FL
    !ZENITH=SOLAR_ZENITH(NARP_LAT,NARP_LONG,NARP_TZ,DTIME)
    !call NARP_cal_SunPosition(NARP_YEAR,DTIME,NARP_TZ,NARP_LAT,NARP_LONG,Alt,AZIMUTH,ZENITH)
    ZENITH=ZENITH_deg*DEG2RAD
    DOY=INT(DTIME)
    IF(DOY==366)doy=365

    !===================================================================================
    !Calculate radiation for each sub-surface
    qn1_cum=0
    kup_cum=0
    lup_cum=0
    tsurf_cum=0

    QSTAR_SF=0
    QSTAR_S=0

    !Total snowfree surface fraction
    SF_all=0
    DO is = 1,nsurf
       IF (sfr(is)/=0) SF_all = SF_all + sfr(is)*(1-snowFrac(is))
    ENDDO

    DO is=1,nsurf
       IF(DiagQN==1) WRITE(*,*) 'is ',is

       EMIS_A=PRATA_EMIS(Temp_K,Press_hPa)

       !--------------------------------------------------
       !-------SNOW FREE SURFACE--------------------------

       IF (AlbedoChoice==1.AND.180*ZENITH/ACOS(0.0)<90) THEN
          ALB0=ALB(is)+0.5e-16*(180*ZENITH/ACOS(0.0))**8 !AIDA 1982
       ELSE
          ALB0=ALB(is)
       ENDIF
       EMIS0=EMIS(is)

       !Downward longwave radiation
       IF((ldown_option==4) .OR. (ldown_option==5)) THEN !Estimate FCLD from Kdown (Offerle et al. 2003)
          IF (ZENITH<1.5) THEN !DAYTIME CALCULATIONS
             TRANS=TRANSMISSIVITY(Press_hPa,TD,NARP_G(DOY),ZENITH)
             KCLEAR=ISURFACE(DOY,ZENITH)*TRANS*NARP_TRANS_SITE
             IF (KCLEAR>50.) THEN
                FCLD=CLOUD_FRACTION(KDOWN,KCLEAR)
                EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
             ELSE
                IF(ldown_option==5) THEN ! Use RH when Kdown can not be used
                   FCLD=WC_fraction(RH,Temp_C)
                   EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
                ELSE
                   !FCLD is left to the latest calculable value
                   EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
                ENDIF
             ENDIF
          ELSE !NIGHT TIME CALCULATIONS
             IF(ldown_option==4) THEN
                !FCLD is left to the latest calculable value
                EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
             ELSEIF((ldown_option==5)) THEN ! Use RH
                FCLD=WC_fraction(RH,Temp_C)
                EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
             ENDIF
          ENDIF
       ELSEIF(ldown_option==3) THEN !Always use RH
          FCLD=WC_fraction(RH,Temp_C)
          EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
       ELSEIF(ldown_option==2) THEN ! FCLD obs, came from input
          EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
       ENDIF
       IF(DiagQN==1) WRITE(*,*) 'ldown_option: ',ldown_option,'FCLD:',FCLD

       IF(ldown_option>1) THEN ! Forcing available if ldown_option=1, model otherwise
          LDOWN=EMIS_A*SIGMATK4
          IF(DiagQN==1) WRITE(*,*) 'EMIS_A: ',EMIS_A,'SIGMATK4:',SIGMATK4,'LDOWN: ',LDOWN
       ENDIF


       !----------------------------------------------------------------------------
       !Note that this is not averaged over the hour for cases where time step < 1hr
       KDOWN_HR=KDOWN
       IF (KDOWN_HR>0) THEN
          LUPCORR=(1-ALB0)*(0.08*KDOWN_HR)
       ELSE
          LUPCORR=0.
       ENDIF

       KUP=ALB0*KDOWN
       TSURF=((EMIS0*SIGMATK4+LUPCORR)/(EMIS0*SIGMA_SB))**0.25 !Eqs. (14) and (15),

       LUP=EMIS0*SIGMATK4+LUPCORR+(1-EMIS0)*LDOWN              !Eq (16) in Offerle et al. (2002)
       QSTAR=KDOWN-KUP+LDOWN-LUP
       TSURF=TSURF-273.16

       !======================================================================
       !Snow related parameters if snow pack existing
       IF (snowFrac(is)>0) THEN
          IF (AlbedoChoice==1.AND.180*ZENITH/ACOS(0.0)<90) THEN
             ALB1=SnowAlb+0.5e-16*(180*ZENITH/ACOS(0.0))**8 !AIDA 1982
          ELSE
             ALB1=SnowAlb
          ENDIF

          KUP_SNOW = (ALB1*(snowFrac(is)-snowFrac(is)*IceFrac(is))+ALB0*snowFrac(is)*IceFrac(is))*KDOWN
          TSURF_SNOW=((NARP_EMIS_SNOW*SIGMATK4)/(NARP_EMIS_SNOW*SIGMA_SB))**0.25 !Snow surface temperature

          !IF (TSURF_SNOW>273.16) TSURF_SNOW=min(273.16,Temp_K)!Set this to 2 degrees (melted water on top)
          !open(34,file='TestingSnowFrac.txt',position='append')
          !write(34,*) dectime,is,alb1,alb0,snowFrac(is),IceFrac(is),KDOWN,KUP_snow
          !close(34)

          LUP_SNOW = NARP_EMIS_SNOW*SIGMA_SB*TSURF_SNOW**4+(1-NARP_EMIS_SNOW)*LDOWN
          QSTAR_SNOW = KDOWN-KUP_SNOW+LDOWN-LUP_SNOW
          TSURF_SNOW = TSURF_SNOW-273.16

       ELSE
          KUP_SNOW = 0
          LUP_SNOW = 0
          TSURF_SNOW = 0
          QSTAR_SNOW = 0
          !QSTAR_ICE = 0
          !KUP_ICE = 0
       ENDIF

       qn1_ind_nosnow(is)=QSTAR          !Define sub-surface radiation components
       kup_ind_nosnow(is)=KUP
       lup_ind_nosnow(is)=LUP
       Tsurf_ind_nosnow(is)=TSURF

       qn1_ind_snow(is)=QSTAR_SNOW        !Define snow sub-surface radiation components
       kup_ind_snow(is)=KUP_SNOW
       lup_ind_snow(is)=LUP_SNOW
       Tsurf_ind_snow(is)=TSURF_SNOW


       IF (SF_all/=0)THEN
          QSTAR_SF = QSTAR_SF + QSTAR*sfr(is)*(1-snowFrac(is))/SF_all
       ELSE
          QSTAR_SF = QSTAR_SF + QSTAR*sfr(is)*(1-snowFrac(is))
       ENDIF

       IF ((1-SF_all)/=0)THEN
          QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr(is)*snowFrac(is)/(1-SF_all)
       ELSE
          QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr(is)*snowFrac(is)
       ENDIF

       !---------------------------------------------------------------------
       !Calculate weighted variables for each subsurface
       qn1_is = QSTAR*(1-snowFrac(is))+QSTAR_SNOW*snowFrac(is)
       kup_is = KUP*(1-snowFrac(is))+KUP_SNOW*snowFrac(is)
       lup_is = LUP*(1-snowFrac(is))+LUP_SNOW*snowFrac(is)
       tsurf_is = TSURF*(1-snowFrac(is))+TSURF_SNOW*snowFrac(is)

       IF(DiagQN==1) WRITE(*,*) 'QSTAR',QSTAR,'QSTAR_SNOW',QSTAR_SNOW,'snowFrac',snowFrac(is)

       qn1_cum=qn1_cum+(qn1_is*sfr(is))  !Calculate cumulative radiation components
       kup_cum=kup_cum+(kup_is*sfr(is))
       lup_cum=lup_cum+(lup_is*sfr(is))
       tsurf_cum=tsurf_cum+(tsurf_is*sfr(is))

       qn1_ind(is)=qn1_is                !Define sub-surface radiation components
       kup_ind(is)=kup_is
       lup_ind(is)=lup_is
       Tsurf_ind(is)=tsurf_is

       IF(DiagQN==1) WRITE(*,*) 'qn1_is: ',qn1_is

    ENDDO !End of the surface types



    !Set overall radiation components
    IF (NetRadiationMethodX/=3000) THEN !Observed Q* used and snow is modeled
       QSTARall=qn1_cum
    ELSE
       QSTARall=qn1_obs
    ENDIF

    KUPall=kup_cum
    LUPall=lup_cum
    TSURFall=tsurf_cum

    ! qn1=QSTARall
    ! kup=KUPall
    !LDOWN has same name
    ! LUP=LUPall
    tsurf=TSURFall
    !if (kup>500) then
    ! write(*,*) Kdown, kup, kup_ind(1),kup_ind(2),kup_ind(3),kup_ind(4),kup_ind(5),kup_ind(6),SnowAlb
    ! pause
    !endif

    IF(DiagQN==1) WRITE(*,*) 'kdown: ',kdown,'kup:',kup,'LDOWN: ',LDOWN,'LUP: ',LUP
    IF(DiagQN==1) WRITE(*,*) 'Qn: ',QSTARall

  END SUBROUTINE NARP


  !==============================================================================
  SUBROUTINE NARP_cal_SunPosition(year,idectime,UTC,locationlatitude,locationlongitude,locationaltitude,sunazimuth,sunzenith)
    IMPLICIT NONE

    REAL(KIND(1D0)),INTENT(in) :: year,idectime,UTC,locationlatitude,locationlongitude,locationaltitude
    REAL(KIND(1D0)),INTENT(out) ::sunazimuth,sunzenith

    REAL(KIND(1D0)):: sec
    INTEGER :: month,day,hour,min,seas,dayofyear

    REAL(KIND(1D0)) :: juliancentury,julianday,julianephemeris_century,julianephemeris_day,&
         julianephemeris_millenium
    REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude,earth_heliocentric_positionlongitude,&
         earth_heliocentric_positionradius
    REAL(KIND(1D0)) :: sun_geocentric_positionlatitude, sun_geocentric_positionlongitude
    REAL(KIND(1D0)) :: nutationlongitude,nutationobliquity
    REAL(KIND(1D0)) :: corr_obliquity
    REAL(KIND(1D0)) :: aberration_correction
    REAL(KIND(1D0)) :: apparent_sun_longitude
    REAL(KIND(1D0)) :: apparent_stime_at_greenwich
    REAL(KIND(1D0)) :: sun_rigth_ascension
    REAL(KIND(1D0)) :: sun_geocentric_declination
    REAL(KIND(1D0)) :: observer_local_hour
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension ,topocentric_sun_positionrigth_ascension_parallax
    REAL(KIND(1D0)) :: topocentric_sun_positiondeclination
    REAL(KIND(1D0)) :: topocentric_local_hour
    ! REAL(KIND(1D0)) :: sunazimuth,sunzenith

    ! This function compute the sun position (zenith and azimuth angle (in degrees) at the observer
    ! location) as a function of the observer local time and position.
    !
    ! Input lat and lng should be in degrees, alt in meters.
    !
    ! It is an implementation of the algorithm presented by Reda et Andreas in:
    ! Reda, I., Andreas, A. (2003) Solar position algorithm for solar
    ! radiation application. National Renewable Energy Laboratory (NREL)
    ! Technical report NREL/TP-560-34302.
    ! This document is avaLAIble at www.osti.gov/bridge
    ! Code is translated from matlab code by Fredrik Lindberg (fredrikl@gvc.gu.se)
    ! Last modified: LJ 27 Jan 2016 - Tabs removed


    ! Convert to timevectors from dectime and year
    CALL dectime_to_timevec(idectime,hour,min,sec)
    dayofyear=FLOOR(idectime)
    CALL day2month(dayofyear,month,day,seas,year,locationlatitude)

    ! 1. Calculate the Julian Day, and Century. Julian Ephemeris day, century
    ! and millenium are calculated using a mean delta_t of 33.184 seconds.
    CALL julian_calculation(year,month,day,hour,min,sec,UTC,juliancentury,julianday,julianephemeris_century,&
         julianephemeris_day,julianephemeris_millenium)

    ! 2. Calculate the Earth heliocentric longitude, latitude, and radius
    ! vector (L, B, and R)
    CALL earth_heliocentric_position_calculation(julianephemeris_millenium,earth_heliocentric_positionlatitude,&
         &earth_heliocentric_positionlongitude,earth_heliocentric_positionradius)

    ! 3. Calculate the geocentric longitude and latitude
    CALL sun_geocentric_position_calculation(earth_heliocentric_positionlongitude,earth_heliocentric_positionlatitude,&
         & sun_geocentric_positionlatitude, sun_geocentric_positionlongitude)

    ! 4. Calculate the nutation in longitude and obliquity (in degrees).
    CALL nutation_calculation(julianephemeris_century,nutationlongitude,nutationobliquity)

    ! 5. Calculate the true obliquity of the ecliptic (in degrees).
    CALL corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)

    ! 6. Calculate the aberration correction (in degrees)
    CALL abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)

    ! 7. Calculate the apparent sun longitude in degrees)
    CALL apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
         & aberration_correction, apparent_sun_longitude)

    ! 8. Calculate the apparent sideral time at Greenwich (in degrees)
    CALL apparent_stime_at_greenwich_calculation(julianday,juliancentury, nutationlongitude, &
         &corr_obliquity, apparent_stime_at_greenwich)

    ! 9. Calculate the sun rigth ascension (in degrees)
    CALL sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
         &sun_rigth_ascension)

    ! 10. Calculate the geocentric sun declination (in degrees). Positive or
    ! negative if the sun is north or south of the celestial equator.
    CALL sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
         &sun_geocentric_declination)

    ! 11. Calculate the observer local hour angle (in degrees, westward from south).
    CALL observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, sun_rigth_ascension, observer_local_hour)

    ! 12. Calculate the topocentric sun position (rigth ascension, declination and
    ! rigth ascension parallax in degrees)
    CALL topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension,&
         &topocentric_sun_positionrigth_ascension_parallax,topocentric_sun_positiondeclination,locationaltitude,&
         &locationlatitude,observer_local_hour,sun_rigth_ascension,sun_geocentric_declination,&
         &earth_heliocentric_positionradius)

    ! 13. Calculate the topocentric local hour angle (in degrees)
    CALL topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
         & topocentric_local_hour)

    ! 14. Calculate the topocentric zenith and azimuth angle (in degrees)
    CALL sun_topocentric_zenith_angle_calculate(locationlatitude , topocentric_sun_positiondeclination,&
         & topocentric_local_hour, sunazimuth,sunzenith)


  END SUBROUTINE NARP_cal_SunPosition


  !================================ Subfunction definitions ========================================================!
  SUBROUTINE julian_calculation(year,month,day,hour,min,sec,UTC,juliancentury,julianday,julianephemeris_century&
       &,julianephemeris_day,julianephemeris_millenium)
    IMPLICIT NONE

    REAL(KIND(1D0)) :: A,B,D,delta_t
    REAL(KIND(1D0)) :: juliancentury
    REAL(KIND(1D0)) :: julianday
    REAL(KIND(1D0)) :: julianephemeris_century
    REAL(KIND(1D0)) :: julianephemeris_day
    REAL(KIND(1D0)) :: julianephemeris_millenium
    REAL(KIND(1D0)) :: M,sec,year,UTC
    INTEGER :: day,hour,min,month
    !REAL(KIND(1D0)) :: time      !>
    REAL(KIND(1D0)) :: ut_time ,Y !tt,
    !
    ! This function compute the julian day and julian century from the local
    ! time and timezone information. Ephemeris are calculated with a delta_t=0
    ! seconds.

    IF (month == 1 .OR. month == 2) THEN
       Y = year - 1.
       M = month + 12
    ELSE
       Y = year
       M = month
    END IF
    ut_time = ((float(hour) - UTC)/24.) + (float(min)/(60.*24.)) + (sec/(60.*60.*24.)) ! time of day in UT time.
    D = day + ut_time ! Day of month in decimal time, ex. 2sd day of month at 12:30:30UT, D=2.521180556

    ! In 1582, the gregorian calendar was adopted
    IF (year == 1582.) THEN
       IF (month == 10) THEN
          IF (day <= 4) THEN ! The Julian calendar ended on October 4, 1582
             B = 0
          ELSE IF (day >= 15) THEN ! The Gregorian calendar started on October 15, 1582
             A = FLOOR(Y/100)
             B = 2 - A + FLOOR(A/4)
          ELSE
             !disp('This date never existed!. Date automatically set to October 4, 1582')
             month = 10
             day = 4
             B = 0
          END IF
       ELSE IF (month<10) THEN ! Julian calendar
          B = 0
       ELSE ! Gregorian calendar
          A = FLOOR(Y/100)
          B = 2 - A + FLOOR(A/4)
       END IF

    ELSE IF (year<1582.) THEN ! Julian calendar
       B = 0
    ELSE
       A = FLOOR(Y/100) ! Gregorian calendar
       B = 2 - A + FLOOR(A/4)
    END IF

    julianday = FLOOR(365.25*(Y+4716.)) + FLOOR(30.6001*(M+1)) + D + B - 1524.5

    delta_t = 0. ! 33.184;
    julianephemeris_day = julianday + (delta_t/86400)

    juliancentury = (julianday - 2451545.) / 36525.

    julianephemeris_century = (julianephemeris_day - 2451545.) / 36525.

    julianephemeris_millenium = julianephemeris_century / 10.

  END SUBROUTINE julian_calculation

  SUBROUTINE earth_heliocentric_position_calculation(julianephemeris_millenium,earth_heliocentric_positionlatitude&
       &,earth_heliocentric_positionlongitude,earth_heliocentric_positionradius)
    IMPLICIT NONE

    REAL(KIND(1D0)) :: julianephemeris_millenium      !>

    REAL(KIND(1D0)),DIMENSION(64) :: A0      !>
    REAL(KIND(1D0)),DIMENSION(34) :: A1      !>
    REAL(KIND(1D0)),DIMENSION(20) :: A2      !>
    REAL(KIND(1D0)),DIMENSION(7) :: A3      !>
    REAL(KIND(1D0)),DIMENSION(3) :: A4      !>
    REAL(KIND(1D0)) :: A5      !>
    REAL(KIND(1D0)),DIMENSION(64) :: B0      !>
    REAL(KIND(1D0)),DIMENSION(34) :: B1      !>
    REAL(KIND(1D0)),DIMENSION(20) :: B2      !>
    REAL(KIND(1D0)),DIMENSION(7) :: B3      !>
    REAL(KIND(1D0)),DIMENSION(3) :: B4      !>
    REAL(KIND(1D0)) :: B5      !>
    REAL(KIND(1D0)),DIMENSION(64) :: C0      !>
    REAL(KIND(1D0)),DIMENSION(34) :: C1      !>
    REAL(KIND(1D0)),DIMENSION(20) :: C2      !>
    REAL(KIND(1D0)),DIMENSION(7) :: C3      !>
    REAL(KIND(1D0)),DIMENSION(3) :: C4      !>
    REAL(KIND(1D0)) :: C5
    REAL(KIND(1D0)),DIMENSION(40) :: A0j      !>
    REAL(KIND(1D0)),DIMENSION(10) :: A1j     !>
    REAL(KIND(1D0)),DIMENSION(6) :: A2j      !>
    REAL(KIND(1D0)),DIMENSION(2) :: A3j      !>
    REAL(KIND(1D0)) :: A4j      !>
    REAL(KIND(1D0)),DIMENSION(40) :: B0j      !>
    REAL(KIND(1D0)),DIMENSION(10) :: B1j      !>
    REAL(KIND(1D0)),DIMENSION(6) :: B2j      !>
    REAL(KIND(1D0)),DIMENSION(2) :: B3j      !>
    REAL(KIND(1D0)) :: B4j      !>
    REAL(KIND(1D0)),DIMENSION(40) :: C0j      !>
    REAL(KIND(1D0)),DIMENSION(10) :: C1j      !>
    REAL(KIND(1D0)),DIMENSION(6) :: C2j      !>
    REAL(KIND(1D0)),DIMENSION(2) :: C3j      !>
    REAL(KIND(1D0)) :: C4j      !>
    REAL(KIND(1D0)),DIMENSION(5) ::  A0i
    REAL(KIND(1D0)),DIMENSION(5) ::  B0i
    REAL(KIND(1D0)),DIMENSION(5) ::  C0i
    REAL(KIND(1D0)),DIMENSION(2) ::  A1i
    REAL(KIND(1D0)),DIMENSION(2) ::  B1i
    REAL(KIND(1D0)),DIMENSION(2) ::   C1i
    REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude      !>
    REAL(KIND(1D0)) :: earth_heliocentric_positionlongitude      !>
    REAL(KIND(1D0)) :: earth_heliocentric_positionradius      !>
    REAL(KIND(1D0)) :: JME      !>
    REAL(KIND(1D0)) :: L0      !>
    !REAL(KIND(1D0)) :: L0_terms      !>
    REAL(KIND(1D0)) :: L1      !>
    !REAL(KIND(1D0)) :: L1_terms      !>
    REAL(KIND(1D0)) :: L2      !>
    !REAL(KIND(1D0)) :: L2_terms      !>
    REAL(KIND(1D0)) :: L3      !>
    !REAL(KIND(1D0)) :: L3_terms      !>
    REAL(KIND(1D0)) :: L4      !>
    !REAL(KIND(1D0)) :: L4_terms      !>
    REAL(KIND(1D0)) :: L5      !>
    !REAL(KIND(1D0)), dimension(3) :: L5_terms      !>
    !REAL(KIND(1D0)) :: R0_terms      !>
    !REAL(KIND(1D0)) :: R1_terms      !>
    !REAL(KIND(1D0)) :: R2_terms      !>
    !REAL(KIND(1D0)) :: R3_terms      !>
    !REAL(KIND(1D0)), dimension(3) :: R4_terms      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    ! This function compute the earth position relative to the sun, using
    ! tabulated values.

    A0=(/175347046,3341656,34894,3497,3418,3136,2676,2343,1324,1273,1199,990,902,857,780,753,505,&
         &492,357,317,284,271,243,206,205,202,156,132,126,115,103,102,102,99,98,86,85,85,80,79,71,&
         &74,74,70,62,61,57,56,56,52,52,51,49,41,41,39,37,37,36,36,33,30,30,25/)
    B0 = (/0.,4.669256800,4.626100,2.744100,2.828900,3.627700,4.418100,6.135200,0.7425000,2.037100,&
         &1.109600,5.233000,2.045000,3.508000,1.179000,2.533000,4.583000,4.205000,2.92,5.849000,&
         &1.899000,0.315,0.345,4.806000,1.869000,2.445800,0.833,3.411000,1.083000,0.645,0.636,0.976,&
         &4.267000,6.21,0.68,5.98,1.3,3.67,1.81,3.04,1.76,3.5,4.68,0.83,3.98,1.82,2.78,4.39,3.47,0.19,&
         &1.33,0.28,0.49,5.37,2.4,6.17,6.04,2.57,1.71,1.78,0.59,0.44,2.74,3.16/)
    C0 = (/0.,6283.075850,12566.15170,5753.384900,3.523100,77713.77150,7860.419400,3930.209700,&
         &11506.76980,529.6910,1577.343500,5884.927,26.29800,398.1490,5223.694,5507.553,&
         &18849.22800,775.5230,0.067,11790.62900,796.2980,10977.07900,5486.778,2544.314,&
         &5573.143,6069.777,213.2990,2942.463,20.77500,0.98,4694.003,15720.83900,7.114000,&
         &2146.170,155.4200,161000.6900,6275.960,71430.70,17260.15,12036.46,5088.630,3154.690,&
         &801.8200,9437.760,8827.390,7084.900,6286.600,14143.50,6279.550,12139.55,1748.020,&
         &5856.480,1194.450,8429.240,19651.05,10447.39,10213.29,1059.380,2352.870,6812.770,&
         &17789.85,83996.85,1349.870,4690.480/)
    A1 = (/628331966747.000,206059.,4303.,425.,119.,109.,93.,72.,68.,67.,59.,56.,45.,36.,29.,21.,19.,19.,17.,16.,&
         &16.,15.,12.,12.,12.,12.,11.,10.,10.,9.,9.,8.,6.,6./)
    B1 =(/ 0.,2.678235,2.635100,1.59,5.796000,2.966000,2.59,1.14,1.87,4.41,2.89,2.17,0.40,0.47,&
         &2.65,5.34,1.85,4.97,2.99,0.030,1.43,1.21,2.83,3.26,5.27,2.08,0.77,1.3,4.24,2.7,5.64,&
         &5.3,2.65,4.67/)
    C1 =(/ 0.,6283.075850,12566.15170,3.523000,26.29800,1577.344,18849.23,529.6900,398.1500,&
         &5507.550,5223.690,155.4200,796.3000,775.5200,7.11,0.98,5486.780,213.3000,6275.960,&
         &2544.310,2146.170,10977.08,1748.020,5088.630,1194.450,4694.,553.5700,3286.600,&
         &1349.870,242.7300,951.7200,2352.870,9437.760,4690.480/)
    A2 =(/ 52919,8720,309,27,16,16,10,9,7,5,4,4,3,3,3,3,3,3,2,2/)
    B2 = (/0.,1.072100,0.867,0.050,5.19,3.68,0.76,2.06,0.83,4.66,1.03,3.44,5.14,6.05,1.19,&
         &6.12,0.31,2.28,4.38,3.75/)
    C2 =(/ 0.,6283.075800,12566.15200,3.52,26.3,155.4200,18849.23,77713.77,775.5200,1577.340,&
         &7.11,5573.140,796.3000,5507.550,242.7300,529.6900,398.1500,553.5700,5223.690,0.98/)
    A3 = (/289,35,17,3,1,1,1/)
    B3 = (/5.8440,0.,5.4900,5.2000,4.7200,5.3000,5.9700/)
    C3 = (/6283.076,0.,12566.15,155.4200,3.52,18849.23,242.7300/)
    A4 = (/114,8,1/)
    B4 = (/3.1420,4.1300,3.8400/)
    C4 =  (/0.,6283.08,12566.15/)
    A5 =1.
    B5 =3.1400
    C5 =0.

    JME = julianephemeris_millenium

    ! Compute the Earth Heliochentric longitude from the tabulated values.
    L0 = SUM(A0 * COS(B0 + (C0 * JME)))
    L1 = SUM(A1 * COS(B1 + (C1 * JME)))
    L2 = SUM(A2 * COS(B2 + (C2 * JME)))
    L3 = SUM(A3 * COS(B3 + (C3 * JME)))
    L4 = SUM(A4 * COS(B4 + (C4 * JME)))
    L5 = A5 * COS(B5 + (C5 * JME))

    earth_heliocentric_positionlongitude = &
         &(L0 + (L1 * JME) + (L2 * JME**2) + (L3 * JME**3) + (L4 * JME**4) + (L5 * JME**5)) / 1e8
    ! Convert the longitude to degrees.
    earth_heliocentric_positionlongitude = earth_heliocentric_positionlongitude * 180./pi
    ! Limit the range to [0,360[;
    earth_heliocentric_positionlongitude=set_to_range(earth_heliocentric_positionlongitude)

    A0i = (/280,102,80,44,32/)
    B0i = (/3.19900000000000,5.42200000000000,3.88000000000000,3.70000000000000,4./)
    C0i = (/84334.6620000000,5507.55300000000,5223.69000000000,2352.87000000000,1577.34000000000/)
    A1i = (/9,6/)
    B1i =(/3.90000000000000,1.73000000000000/)
    C1i = (/5507.55000000000,5223.69000000000/)

    L0 = SUM(A0i * COS(B0i + (C0i * JME)))
    L1 = SUM(A1i * COS(B1i + (C1i * JME)))

    earth_heliocentric_positionlatitude = (L0 + (L1 * JME)) / 1e8
    ! Convert the latitude to degrees.
    earth_heliocentric_positionlatitude = earth_heliocentric_positionlatitude * 180/pi
    ! Limit the range to [0,360];
    earth_heliocentric_positionlatitude=set_to_range(earth_heliocentric_positionlatitude)

    A0j = (/100013989,1670700,13956,3084,1628,1576,925,542,472,346,329,307,243,212,186,175,110,&
         &98,86,86,85,63,57,56,49,47,45,43,39,38,37,37,36,35,33,32,32,28,28,26/)
    B0j = (/0.,3.09846350,3.05525000,5.1985,1.1739,2.8469,5.453,4.564,3.661,0.964,5.90,0.299,&
         &4.273,5.847,5.022,3.012,5.055,0.890,5.69,1.27,0.270,0.920,2.01,5.24,3.25,2.58,5.54,&
         &6.01,5.36,2.39,0.830,4.90,1.67,1.84,0.240,0.180,1.78,1.21,1.90,4.59/)
    C0j = (/0.,6283.07585,12566.1517,77713.7715,5753.38490,7860.41940,11506.7700,3930.21000,&
         &5884.92700,5507.55300,5223.69400,5573.14300,11790.6290,1577.34400,10977.0790,18849.2280,&
         &5486.77800,6069.78000,15720.8400,161000.690,17260.1500,529.69,83996.8500,71430.7000,&
         &2544.31000,775.52,9437.76000,6275.96000,4694.,8827.39000,19651.0500,12139.5500,&
         &12036.4600,2942.46000,7084.9,5088.63000,398.15,6286.6,6279.55000,10447.3900/)
    A1j = (/103019,1721,702,32,31,25,18,10,9,9/)
    B1j = (/1.10749000,1.0644,3.142,1.02,2.84,1.32,1.42,5.91,1.42,0.270/)
    C1j = (/6283.07585,12566.1517,0.,18849.2300,5507.55000,5223.69000,1577.34000,10977.0800,&
         &6275.96000,5486.78000/)
    A2j = (/4359,124,12,9,6,3/)
    B2j = (/5.7846,5.579,3.14,3.63,1.87,5.47/)
    C2j = (/6283.07580,12566.1520,0.,77713.7700,5573.14000,18849./)
    A3j = (/145,7/)
    B3j = (/4.273,3.92/)
    C3j = (/6283.07600,12566.1500/)
    A4j = 4
    B4j = 2.56
    C4j = 6283.08000

    ! Compute the Earth heliocentric radius vector
    L0 = SUM(A0j * COS(B0j + (C0j * JME)))
    L1 = SUM(A1j * COS(B1j + (C1j * JME)))
    L2 = SUM(A2j * COS(B2j + (C2j * JME)))
    L3 = SUM(A3j * COS(B3j + (C3j * JME)))
    L4 = A4j * COS(B4j + (C4j * JME))

    ! Units are in AU
    earth_heliocentric_positionradius = &
         &(L0 + (L1 * JME) + (L2 * JME**2) + (L3 * JME**3) + (L4 * JME**4)) / 1e8

  END SUBROUTINE earth_heliocentric_position_calculation

  SUBROUTINE sun_geocentric_position_calculation(earth_heliocentric_positionlongitude,&
       &earth_heliocentric_positionlatitude, sun_geocentric_positionlatitude, &
       &sun_geocentric_positionlongitude)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlongitude      !>
    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlatitude      !>
    REAL(KIND(1D0)) :: sun_geocentric_positionlatitude      !>
    REAL(KIND(1D0)) :: sun_geocentric_positionlongitude      !>

    ! This function compute the sun position relative to the earth.

    sun_geocentric_positionlongitude = earth_heliocentric_positionlongitude + 180.0
    ! Limit the range to [0,360];
    sun_geocentric_positionlongitude=set_to_range(sun_geocentric_positionlongitude)

    sun_geocentric_positionlatitude = -earth_heliocentric_positionlatitude
    ! Limit the range to [0,360]
    sun_geocentric_positionlatitude=set_to_range(sun_geocentric_positionlatitude)
  END SUBROUTINE sun_geocentric_position_calculation

  SUBROUTINE nutation_calculation(julianephemeris_century,nutationlongitude,nutationobliquity)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: julianephemeris_century      !>
    REAL(KIND(1D0)), DIMENSION(63) :: delta_longitude      !>
    REAL(KIND(1D0)), DIMENSION(63) :: delta_obliquity      !>
    REAL(KIND(1D0)) :: JCE      !>
    REAL(KIND(1D0)) :: nutationlongitude      !>
    REAL(KIND(1D0)) :: nutationobliquity      !>
    REAL(KIND(1D0)) , DIMENSION(4) :: p0,p1,p2,p3,p4
    REAL(KIND(1D0)), DIMENSION(63) ::tabulated_argument      !>
    REAL(KIND(1D0)) :: X0      !>
    REAL(KIND(1D0)) :: X1      !>
    REAL(KIND(1D0)) :: X2      !>
    REAL(KIND(1D0)) :: X3      !>
    REAL(KIND(1D0)) :: X4      !>
    REAL(KIND(1D0)), DIMENSION(5) :: Xi      !>
    INTEGER, DIMENSION(315) :: Y_terms1     !>
    INTEGER, DIMENSION(5,63) ::Y_terms
    REAL(KIND(1D0)), DIMENSION(252) :: nutation_terms1     !>
    REAL(KIND(1D0)), DIMENSION(4,63) ::nutation_terms
    INTEGER :: i
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    ! This function compute the nutation in longtitude and in obliquity, in
    ! degrees.

    ! All Xi are in degrees.
    JCE = julianephemeris_century

    ! 1. Mean elongation of the moon from the sun
    p0 = (/ (1/189474.),-0.0019142,445267.11148,297.85036 /)
    ! X0 = polyval(p, JCE);
    X0 = p0(1) * JCE**3 + p0(2) * JCE**2 + p0(3) * JCE + p0(4) ! This is faster than polyval...

    ! 2. Mean anomaly of the sun (earth)
    p1 = (/ -(1/300000.),-0.0001603,35999.05034,357.52772 /)
    ! X1 = polyval(p, JCE);
    X1 = p1(1) * JCE**3 + p1(2) * JCE**2 + p1(3) * JCE + p1(4)

    ! 3. Mean anomaly of the moon
    p2 = (/(1/56250.),0.0086972,477198.867398,134.96298 /)
    ! X2 = polyval(p, JCE);
    X2 = p2(1) * JCE**3 + p2(2) * JCE**2 + p2(3) * JCE + p2(4)

    ! 4. Moon argument of latitude
    p3 = (/ (1/327270.),-0.0036825,483202.017538,93.27191 /)
    ! X3 = polyval(p, JCE);
    X3 = p3(1) * JCE**3 + p3(2) * JCE**2 + p3(3) * JCE + p3(4)

    ! 5. Longitude of the ascending node of the moon's mean orbit on the
    ! ecliptic, measured from the mean equinox of the date
    p4 = (/ (1/450000.),0.0020708,-1934.136261,125.04452 /)
    ! X4 = polyval(p, JCE);
    X4 = p4(1) * JCE**3 + p4(2) * JCE**2 + p4(3) * JCE + p4(4)

    ! Y tabulated terms from the original code
    Y_terms1 =  (/0,0,0,0,1,-2,0,0,2,2,0,0,0,2,2,0,0,0,0,2,0,1,0,0,0,0,0,1,0,0,-2,1,0,2,2,0,0,0,2,1, &
         0,0,1,2,2,-2,-1,0,2,2,-2,0,1,0,0,-2,0,0,2,1,0,0,-1,2,2,2,0,0,0,0,0,0,1,0,1,2,0,-1,2,2,&
         0,0,-1,0,1,0,0,1,2,1,-2,0,2,0,0,0,0,-2,2,1,2,0,0,2,2,0,0,2,2,2,0,0,2,0,0,-2,0,1,2,2,0,&
         0,0,2,0,-2,0,0,2,0,0,0,-1,2,1,0,2,0,0,0,2,0,-1,0,1,-2,2,0,2,2,0,1,0,0,1,-2,0,1,0,1,0,-1,0,0,1,0,0,&
         2,-2,0,2,0,-1,2,1,2,0,1,2,2,0,1,0,2,2,-2,1,1,0,0,0,-1,0,2,2,2,0,0,2,1,2,0,1,0,0,-2,0,2,2,2,-2,0,1,&
         2,1,2,0,-2,0,1,2,0,0,0,1,0,-1,1,0,0,-2,-1,0,2,1,-2,0,0,0,1,0,0,2,2,1,-2,0,2,0,1,-2,1,0,2,1,0,0,1,-2,&
         0,-1,0,1,0,0,-2,1,0,0,0,1,0,0,0,0,0,0,1,2,0,0,0,-2,2,2,-1,-1,1,0,0,0,1,1,0,0,0,-1,1,2,2,2,-1,-1,2,2,&
         0,0,3,2,2,2,-1,0,2,2/)
    Y_terms=RESHAPE(Y_terms1,(/5,63/))
    nutation_terms1 = (/-171996.,-174.2,92025.,8.9,-13187.,-1.6,5736.,-3.1,-2274.,-0.2,977.,-0.5,2062.,0.2,-895.,0.5,&
         1426.,-3.4,54.,-0.1,712.,0.1,-7.,0.,-517.,1.2,224.,-0.6,-386.,-0.4,200.,0.,-301.,0.,129.,-0.1,&
         217.,-0.5,-95.,0.3,-158.,0.,0.,0.,129.,0.1,-70.,0.,123.,0.,-53.,0.,63.,0.,0.,0.,63.,0.1,-33.,0.,-59.,0.,26.,0.,&
         -58.,-0.1,32.,0.,-51.,0.,27.,0.,48.,0.,0.,0.,46.,0.,-24.,0.,-38.,0.,16.,0.,-31.,0.,13.,0.,29.,0.,&
         0.,0.,29.,0.,-12.,0.,26.,0.,0.,0.,-22.,0.,0.,0.,21.,0.,-10.,0.,17.,-0.1,0.,0.,16.,0.,-8.,0.,-16.,0.1,7.,0.,&
         -15.,0.,9.,0.,-13.,0.,7.,0.,-12.,0.,6.,0.,11.,0.,0.,0.,-10.,0.,5.,0.,-8.,0.,3.,0.,7.,0.,-3.,0.,-7.,0.,0.,0.,&
         -7.,0.,3.,0.,-7.,0.,3.,0.,6.,0.,0.,0.,6.,0.,-3.,0.,6.,0.,-3.,0.,-6.,0.,3.,0.,-6.,0.,3.,0.,5.,0.,0.,0.,-5.,0.,&
         3.,0.,-5.,0.,3.,0.,-5.,0.,3.,0.,4.,0.,0.,0.,4.,0.,0.,0.,4.,0.,0.,0.,-4.,0.,0.,0.,-4.,0.,0.,0.,-4.,0.,0.,0.,&
         3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0./)
    nutation_terms=RESHAPE(nutation_terms1,(/4,63/))
    ! Using the tabulated values, compute the delta_longitude and
    ! delta_obliquity.
    Xi = (/X0, X1, X2, X3, X4/)

    DO i=1,63
       tabulated_argument(i)=&
            &((Y_terms(1,i)*Xi(1))+(Y_terms(2,i)*Xi(2))+(Y_terms(3,i)*Xi(3))+(Y_terms(4,i)*Xi(4))+(Y_terms(5,i)*Xi(5)))*pi/180
    END DO

    delta_longitude = ((nutation_terms(1,:) + (nutation_terms(2,:) * JCE))) * SIN(tabulated_argument)
    delta_obliquity = ((nutation_terms(3,:) + (nutation_terms(4,:) * JCE))) * COS(tabulated_argument)

    ! Nutation in longitude
    nutationlongitude = SUM(delta_longitude) / 36000000.0

    ! Nutation in obliquity
    nutationobliquity = SUM(delta_obliquity) / 36000000.0

  END SUBROUTINE nutation_calculation

  SUBROUTINE corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: corr_obliquity      !>
    REAL(KIND(1D0)), INTENT(in) :: julianephemeris_millenium     !>
    REAL(KIND(1D0)), INTENT(in) :: nutationobliquity     !>
    REAL(KIND(1D0)) :: mean_obliquity      !>
    REAL(KIND(1D0)), DIMENSION(11) :: p      !>
    REAL(KIND(1D0)) :: U      !>

    ! This function compute the true obliquity of the ecliptic.


    p = (/ 2.45,5.79,27.87,7.12,-39.05,-249.67,-51.38,1999.25,-1.55,-4680.93,84381.448 /)
    ! mean_obliquity = polyval(p, julian.ephemeris_millenium/10);

    U = julianephemeris_millenium/10
    mean_obliquity =&
         &p(1)*U**10 + p(2)*U**9 + p(3)*U**8 + p(4)*U**7 + p(5)*U**6 + p(6)*U**5 + p(7)*U**4 + &
         &p(8)*U**3 + p(9)*U**2 + p(10)*U + p(11)

    corr_obliquity = (mean_obliquity/3600) + nutationobliquity
  END SUBROUTINE corr_obliquity_calculation

  SUBROUTINE abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: aberration_correction      !>
    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius     !>

    ! This function compute the aberration_correction, as a function of the
    ! earth-sun distance.

    aberration_correction = -20.4898/(3600*earth_heliocentric_positionradius)

  END SUBROUTINE abberation_correction_calculation

  SUBROUTINE apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
       & aberration_correction, apparent_sun_longitude)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: aberration_correction      !>
    REAL(KIND(1D0)), INTENT(out) :: apparent_sun_longitude      !>
    REAL(KIND(1D0)), INTENT(in) :: nutationlongitude      !>
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlongitude      !>

    ! This function compute the sun apparent longitude

    apparent_sun_longitude = sun_geocentric_positionlongitude + nutationlongitude + aberration_correction

  END SUBROUTINE apparent_sun_longitude_calculation

  SUBROUTINE apparent_stime_at_greenwich_calculation(julianday,juliancentury, nutationlongitude,&
       & corr_obliquity, apparent_stime_at_greenwich)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: apparent_stime_at_greenwich      !>
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      !>
    REAL(KIND(1D0)), INTENT(in) :: julianday     !>
    REAL(KIND(1D0)), INTENT(in) :: juliancentury     !>
    REAL(KIND(1D0)), INTENT(in) :: nutationlongitude      !>
    REAL(KIND(1D0)) :: JC      !>
    REAL(KIND(1D0)) :: JD      !>
    REAL(KIND(1D0)) :: mean_stime      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.14159265358979d+0

    ! This function compute the apparent sideral time at Greenwich.

    JD = julianday
    JC = juliancentury

    ! Mean sideral time, in degrees
    mean_stime = 280.46061837d+0 + (360.98564736629d+0*(JD-2451545.0d+0)) + (0.000387933d+0*JC**2) - (JC**3/38710000.0d+0)

    ! Limit the range to [0-360];
    mean_stime=set_to_range(mean_stime)

    apparent_stime_at_greenwich = mean_stime + (nutationlongitude * COS(corr_obliquity * pi/180))
  END SUBROUTINE apparent_stime_at_greenwich_calculation

  SUBROUTINE sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, &
       &sun_geocentric_positionlatitude, sun_rigth_ascension)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude      !>
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      !>
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude      !>
    REAL(KIND(1D0)), INTENT(out) :: sun_rigth_ascension      !>
    REAL(KIND(1D0)) :: argument_denominator      !>
    REAL(KIND(1D0)) :: argument_numerator      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    ! This function compute the sun rigth ascension.

    argument_numerator = (SIN(apparent_sun_longitude * pi/180.0) * COS(corr_obliquity * pi/180.0)) - &
         (TAN(sun_geocentric_positionlatitude * pi/180.0) * SIN(corr_obliquity * pi/180.0))
    argument_denominator = COS(apparent_sun_longitude * pi/180.0)

    sun_rigth_ascension = ATAN2(argument_numerator, argument_denominator) * 180.0/pi
    ! Limit the range to [0,360];
    sun_rigth_ascension=set_to_range(sun_rigth_ascension)
  END SUBROUTINE sun_rigth_ascension_calculation

  SUBROUTINE sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, &
       &sun_geocentric_positionlatitude, sun_geocentric_declination)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude      !>
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      !>
    REAL(KIND(1D0)), INTENT(out) :: sun_geocentric_declination      !>
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude     !>
    REAL(KIND(1D0)) :: argument      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    argument = (SIN(sun_geocentric_positionlatitude * pi/180.0) * COS(corr_obliquity * pi/180.0)) + &
         (COS(sun_geocentric_positionlatitude * pi/180.0) * SIN(corr_obliquity * pi/180) * SIN(apparent_sun_longitude * pi/180.0))

    sun_geocentric_declination = ASIN(argument) * 180.0/pi
  END SUBROUTINE sun_geocentric_declination_calculation

  SUBROUTINE observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, &
       &sun_rigth_ascension, observer_local_hour)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_stime_at_greenwich      !>
    REAL(KIND(1D0)), INTENT(in) :: locationlongitude     !>
    REAL(KIND(1D0)), INTENT(out) :: observer_local_hour      !>
    REAL(KIND(1D0)), INTENT(in) :: sun_rigth_ascension      !>


    observer_local_hour = apparent_stime_at_greenwich + locationlongitude - sun_rigth_ascension
    ! Set the range to [0-360]
    observer_local_hour=set_to_range(observer_local_hour)
  END SUBROUTINE observer_local_hour_calculation

  SUBROUTINE topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension &
       &,topocentric_sun_positionrigth_ascension_parallax,topocentric_sun_positiondeclination,&
       &locationaltitude,locationlatitude,observer_local_hour,sun_rigth_ascension,&
       &sun_geocentric_declination,earth_heliocentric_positionradius)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius
    REAL(KIND(1D0)), INTENT(in) :: locationlatitude      !>
    REAL(KIND(1D0)), INTENT(in) :: locationaltitude
    REAL(KIND(1D0)), INTENT(in) :: observer_local_hour      !>
    REAL(KIND(1D0)),  INTENT(in) :: sun_geocentric_declination      !>
    REAL(KIND(1D0)),  INTENT(in) :: sun_rigth_ascension      !>
    REAL(KIND(1D0)) :: denominator      !>
    REAL(KIND(1D0)) :: eq_horizontal_parallax      !>
    REAL(KIND(1D0)) :: nominator      !>
    REAL(KIND(1D0)) :: sun_rigth_ascension_parallax      !>
    REAL(KIND(1D0)) :: topocentric_sun_positiondeclination      !>
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension      !>
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension_parallax      !>
    REAL(KIND(1D0)) :: u      !>
    REAL(KIND(1D0)) :: x      !>
    REAL(KIND(1D0)) :: y      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    ! topocentric_sun_positionrigth_ascension_parallax
    ! This function compute the sun position (rigth ascension and declination)
    ! with respect to the observer local position at the Earth surface.

    ! Equatorial horizontal parallax of the sun in degrees
    eq_horizontal_parallax = 8.794 / (3600 * earth_heliocentric_positionradius)

    ! Term u, used in the following calculations (in radians)
    u = ATAN(0.99664719 * TAN(locationlatitude * pi/180))

    ! Term x, used in the following calculations
    x = COS(u) + ((locationaltitude/6378140) * COS(locationaltitude * pi/180))

    ! Term y, used in the following calculations
    y = (0.99664719d+0 * SIN(u)) + ((locationaltitude/6378140) * SIN(locationlatitude * pi/180))

    ! Parallax in the sun rigth ascension (in radians)
    nominator = -x * SIN(eq_horizontal_parallax * pi/180.0) * SIN(observer_local_hour * pi/180.0)
    denominator = COS(sun_geocentric_declination * pi/180.0) - &
         (x * SIN(eq_horizontal_parallax * pi/180.0) * COS(observer_local_hour * pi/180.0))
    sun_rigth_ascension_parallax = ATAN2(nominator, denominator)
    ! Conversion to degrees.
    topocentric_sun_positionrigth_ascension_parallax = sun_rigth_ascension_parallax * 180.0/pi

    ! Topocentric sun rigth ascension (in degrees)
    topocentric_sun_positionrigth_ascension = sun_rigth_ascension + (sun_rigth_ascension_parallax * 180.0/pi)

    ! Topocentric sun declination (in degrees)
    nominator = (SIN(sun_geocentric_declination * pi/180.0) - (y*SIN(eq_horizontal_parallax * pi/180.0)))&
         & * COS(sun_rigth_ascension_parallax)
    denominator = COS(sun_geocentric_declination * pi/180.0) - (y*SIN(eq_horizontal_parallax * pi/180.0))&
         & * COS(observer_local_hour * pi/180.0)
    topocentric_sun_positiondeclination = ATAN2(nominator, denominator) * 180.0/pi
  END SUBROUTINE topocentric_sun_position_calculate

  SUBROUTINE topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
       & topocentric_local_hour)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: observer_local_hour      !>
    REAL(KIND(1D0)), INTENT(out) :: topocentric_local_hour      !>
    REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positionrigth_ascension_parallax     !>

    ! This function compute the topocentric local jour angle in degrees

    topocentric_local_hour = observer_local_hour - topocentric_sun_positionrigth_ascension_parallax
  END SUBROUTINE topocentric_local_hour_calculate

  SUBROUTINE sun_topocentric_zenith_angle_calculate(locationlatitude , topocentric_sun_positiondeclination, &
       &topocentric_local_hour, sunazimuth,sunzenith)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: locationlatitude     !>
    REAL(KIND(1D0)), INTENT(in) :: topocentric_local_hour      !>
    REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positiondeclination
    REAL(KIND(1D0)) :: corr_elevation      !>
    REAL(KIND(1D0)) :: apparent_elevation      !>
    REAL(KIND(1D0)) :: argument      !>
    REAL(KIND(1D0)) :: denominator      !>
    REAL(KIND(1D0)) :: nominator      !>
    REAL(KIND(1D0)) :: refraction_corr      !>
    REAL(KIND(1D0)) :: sunazimuth      !>
    REAL(KIND(1D0)) :: sunzenith      !>
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0
    ! This function compute the sun zenith angle, taking into account the
    ! atmospheric refraction. A default temperature of 283K and a
    ! default pressure of 1010 mbar are used.

    ! Topocentric elevation, without atmospheric refraction
    argument = (SIN(locationlatitude * pi/180.0) * SIN(topocentric_sun_positiondeclination * pi/180.0)) + &
         (COS(locationlatitude * pi/180.0) * COS(topocentric_sun_positiondeclination * pi/180.0) * &
         &COS(topocentric_local_hour * pi/180.0))
    corr_elevation = ASIN(argument) * 180.0/pi

    ! Atmospheric refraction correction (in degrees)
    argument = corr_elevation + (10.3/(corr_elevation + 5.11))
    refraction_corr = 1.02 / (60 * TAN(argument * pi/180.0))

    ! For exact pressure and temperature correction, use this,
    ! with P the pressure in mbar amd T the temperature in Kelvins:
    ! refraction_corr = (P/1010) * (283/T) * 1.02 / (60 * tan(argument * pi/180));

    ! Apparent elevation
    apparent_elevation = corr_elevation + refraction_corr

    sunzenith = 90.0 - apparent_elevation

    ! Topocentric azimuth angle. The +180 conversion is to pass from astronomer
    ! notation (westward from south) to navigation notation (eastward from
    ! north);
    nominator = SIN(topocentric_local_hour * pi/180.0)
    denominator = (COS(topocentric_local_hour * pi/180.0) * SIN(locationlatitude * pi/180.0)) - &
         (TAN(topocentric_sun_positiondeclination * pi/180.0) * COS(locationlatitude * pi/180.0))
    sunazimuth = (ATAN2(nominator, denominator) * 180.0/pi) + 180.0
    ! Set the range to [0-360]
    sunazimuth=set_to_range(sunazimuth)

  END SUBROUTINE sun_topocentric_zenith_angle_calculate

  FUNCTION set_to_range(var) RESULT(vari)
    ! This function make sure the variable is in the specified range.

    REAL(KIND(1D0)) :: max_interval      !>
    REAL(KIND(1D0)) :: min_interval      !>
    REAL(KIND(1D0)) :: var
    REAL(KIND(1D0)) :: vari
    !
    max_interval=360.0
    min_interval=0.0

    vari = var - max_interval * FLOOR(var/max_interval)

    IF (vari<min_interval) THEN
       vari = vari + max_interval
    END IF

  END FUNCTION set_to_range

  !==============================================================================
  FUNCTION dewpoint(Temp_C,rh) RESULT(td)
    ! ea = vapor pressure (hPa)
    ! td = dewpoint (oC)
    ! calculates dewpoint in degC from
    ! http://www.atd.ucar.edu/weather_fl/dewpoint.html
    ! dewpoint = (237.3 * ln(e_vp/6.1078)) / (17.27 - (ln(e_vp/6.1078)))

    REAL(KIND(1d0))::rh,td,Temp_C,g
    !http://en.wikipedia.org/wiki/Dew_point
    g=((17.27*Temp_C)/(237.7+Temp_C))+LOG(rh/100)
    Td=(237.7*g)/(17.27-g)
    !td = (237.3 * LOG(ea_hPa/6.1078)) / (17.27 - (LOG(ea_hPa/6.1078)))
  END FUNCTION dewpoint
  !===============================================================================
  FUNCTION PRATA_EMIS(Temp_K,EA_hPa) RESULT(EMIS_A)
    ! clear sky emissivity function Prata 1996
    REAL(KIND(1d0))::Temp_K,ea_hPa,EMIS_A
    REAL(KIND(1d0))::W

    W=46.5*(ea_hPa/Temp_K)
    EMIS_A=1.-(1.+W)*EXP(-SQRT(1.2+3.*W))
  END FUNCTION PRATA_EMIS
  !===============================================================================
  FUNCTION EMIS_CLOUD(EMIS_A,FCLD) RESULT(em_adj)
    !calculates adjusted emissivity due to clouds
    REAL(KIND(1d0))::EMIS_A,FCLD,em_adj
    !T. Loridan, removing the square for FCLD in the emissivity correction
    !em_adj=EMIS_A+(1.-EMIS_A)*FCLD*FCLD
    em_adj=EMIS_A+(1.-EMIS_A)*FCLD
  END FUNCTION EMIS_CLOUD
  !===============================================================================
  FUNCTION EMIS_CLOUD_SQ(EMIS_A,FCLD) RESULT(em_adj)
    !calculates adjusted emissivity due to clouds
    REAL(KIND(1d0))::EMIS_A,FCLD,em_adj
    em_adj=EMIS_A+(1.-EMIS_A)*FCLD*FCLD
  END FUNCTION EMIS_CLOUD_SQ
  !===============================================================================
  FUNCTION cloud_fraction(KDOWN,KCLEAR) RESULT(FCLD)
    REAL(KIND(1d0))::KDOWN,KCLEAR,FCLD

    FCLD=1.-KDOWN/KCLEAR
    IF(FCLD>1.) FCLD=1.
    IF(FCLD<0.) FCLD=0.
  END FUNCTION cloud_fraction
  !===============================================================================
  FUNCTION WC_fraction(RH,Temp) RESULT(FWC)
    ! Thomas Loridan, King's College London: June 2009
    ! Parameterisation of fraction water content using the relative humidity
    REAL(KIND(1d0)),INTENT(in)   :: RH      !Relative Humidity in %
    REAL(KIND(1d0)),INTENT(in)   :: Temp    !Temperature in degre C

    REAL(KIND(1d0))              :: FWC     !Fraction water content between 0 and 1
    REAL(KIND(1d0))              :: A, B    !Parameters in the expo

    !Parameters
    !A=0.078
    !B=0.026

    A=0.185
    B=0.00019*Temp+0.015

    !FWC parameterization
    FWC=A * (EXP(B * RH)-1)
    IF(FWC>1.) FWC=1.
    IF(FWC<0.) FWC=0.
  END FUNCTION WC_fraction

  !===============================================================================
  FUNCTION ISURFACE(doy,zenith) RESULT(Isurf)
    ! Calculates ground level solar irradiance clear sky
    ! assuming transmissivity = 1
    ! let it report zero if zenith >= 90
    REAL(KIND(1d0))::zenith,Isurf
    INTEGER::doy
    REAL(KIND(1d0))::Rmean, Rse, cosZ,Itoa
    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292

    Rmean = 149.6                 !Stull 1998
    Rse=solar_ESdist(doy)
    IF(zenith<90.*DEG2RAD) THEN
       cosZ = COS(zenith)
       Itoa = 1370.*(Rmean/Rse)**2  !top of the atmosphere
       Isurf = Itoa*cosZ            !ground level solar irradiance in W/m2
    ELSE
       Isurf = 0.
    ENDIF

  END FUNCTION ISURFACE

  !===============================================================================
  FUNCTION solar_ESdist(doy) RESULT(Rse)
    !from Stull, 1998   Keep! called from SOLWEIG_clearnessindex_2013b
    INTEGER          ::doy
    REAL(KIND(1d0))             ::Rse
    REAL(KIND(1d0)) ::MA,nu,e,a

    e = 0.0167
    a = 146.457

    MA = 2.*3.141592654*(doy-3)/365.25463 !Mean anomaly
    nu=MA+0.0333988*SIN(MA)+.0003486*SIN(2.*MA)+5e-6*SIN(3.*MA) !true anomaly
    Rse = a*(1-e*e)/(1+e*COS(nu))

  END FUNCTION solar_ESdist

  !===============================================================================
  FUNCTION SmithLambda(lat) RESULT(G)
    USE FileName
    USE defaultnotUsed
    !read kriged data based on Smith 1966 (JAM)
    ! Smith, William L.
    ! "Note on the relationship between total precipitable water and surface dew point."
    ! Journal of Applied Meteorology 5.5 (1966): 726-727.
    INTEGER :: lat,ios,ilat
    REAL(KIND(1d0)),DIMENSION(365):: G

    !open(99,file="Smith1966.grd",access="direct",action="read",recl=365*4,iostat=ios)
    !read(99,rec=lat+1,iostat=ios) G
    OPEN(99,file=smithFile,iostat=ios)
    DO ilat=1,lat
       READ(99,*)
    ENDDO
    READ(99,*,iostat=ios)ilat, G
    IF (ios/=0) THEN
       CALL  ErrorHint(11,'reading Smith1966.grd (ios).',notUsed,notUsed,ios)
    ENDIF
    CLOSE(99)
  END FUNCTION SmithLambda

  !===============================================================================
  FUNCTION transmissivity(Press_hPa,Temp_C_dew,G,zenith) RESULT(trans)
    ! bulk atmospheric transmissivity (Crawford and Duchon, 1999)
    ! P = pressure (hPa)
    ! Td = dewpoint (C)
    ! G parameter is empirical value from Smith 1966 (JAM)
    ! zenith in radians
    ! if zenith > 80 use the value for 80.

    REAL(KIND(1d0)) ::Press_hPa,TemP_C_dew,zenith,G,trans
    REAL(KIND(1d0))::m,TrTpg,u,Tw,Ta,cosZ
    REAL(KIND(1d0))::Tdf
    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292


    IF (zenith>80.*DEG2RAD) THEN
       cosZ=COS(80.*DEG2RAD)
    ELSE
       cosZ=COS(zenith)
    ENDIF

    Tdf = TemP_C_dew*1.8+32. !celsius to fahrenheit
    !	Transmission coefficients
    m = 35*cosZ/SQRT(1224.*cosZ*cosZ+1)            !optical air mass at p=1013 mb
    !Rayleigh & permanent gases
    TrTpg = 1.021-0.084*SQRT(m*(0.000949*Press_hPa+0.051)) !first two trans coeff
    u = EXP(0.113-LOG(G+1)+0.0393*Tdf)             !precipitable water
    Tw = 1-0.077*(u*m)**0.3             !vapor transmission coe3ff.
    Ta = 0.935**m                       !4th trans coeff
    trans = TrTpg*Tw*Ta                 !bulk atmospheric transmissivity
  END FUNCTION transmissivity
  !===============================================================================
END MODULE NARP_MODULE


MODULE AtmMoist_module
  IMPLICIT NONE

CONTAINS
  !.c!! For Lumps Version 2 - no stability calculations
  ! Latent heat of sublimation when air temperature below zero added. LJ Nov 2012
  ! explict interface added to all subroutines, TS 08 Aug 2017
  SUBROUTINE LUMPS_cal_AtmMoist(&
       Temp_C,Press_hPa,avRh,dectime,&! input:
       lv_J_kg,lvS_J_kg,&! output:
       es_hPa,Ea_hPa,VPd_hpa,VPD_Pa,dq,dens_dry,avcp,air_dens)


    IMPLICIT NONE
    REAL(KIND(1d0))::vap_dens

    REAL(KIND(1d0)),INTENT(in)::&
         Temp_C,&
         Press_hPa,&
         avRh,dectime
    REAL(KIND(1d0)),INTENT(out)::&
         lv_J_kg,&!Latent heat of vaporization in [J kg-1]
         lvS_J_kg,&!Latent heat of sublimation in J/kg
         es_hPa,&!Saturation vapour pressure over water in hPa
         Ea_hPa,&!Vapour pressure of water in hPa
         VPd_hpa,& !vapour pressure deficit in hPa
         VPD_Pa,& !vapour pressure deficit in Pa
         dq,&!Specific humidity deficit in g/kg
         dens_dry,& !Vap density or absolute humidity	 (kg/m3)
         avcp,&!specific heat capacity in J kg-1 K-1
         air_dens!Air density in kg/m3

     !REAL(KIND(1d0))::sat_vap_press,spec_heat_beer,lat_vap,lat_vapSublim,SPEC_HUM_DEF   ! functions


    REAL (KIND(1d0)),PARAMETER:: &
         gas_ct_dry    = 8.31451/0.028965,&  !j/kg/k=dry_gas/molar
         gas_ct_wv     = 8.31451/0.0180153 !j/kg/kdry_gas/molar_wat_vap
    !  waterDens     = 999.8395            !Density of water in 0 cel deg
    INTEGER::from=1

    !Saturation vapour pressure over water in hPa
    es_hPa = sat_vap_press(Temp_C,Press_hPa,from,dectime) ! dectime is more or less unnecessary here

    !Vapour pressure of water in hPa
    Ea_hPa=avRh/100*es_hPa

    ! if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'%',Temp_C

    VPd_hpa=es_hPa-ea_hpa           !vapour pressure deficit in hPa
    VPD_Pa=(es_hPa*100)-(Ea_hPa*100)!vapour pressure deficit in Pa

    dq=(spec_hum_def(vpd_hPa,Press_hPa)) !Specific humidity deficit in g/kg

    !Vap density or absolute humidity	 (kg/m3)
    vap_dens=(Ea_hPa*100/((Temp_C+273.16)*gas_ct_wv))

    !density Dry Air Beer(1990)	kg/m3
    dens_dry=((Press_hPa-Ea_hPa)*100)/(gas_ct_dry*(273.16+Temp_C))

    !Air density in kg/m3
    air_dens=(Press_hPa*100)/(gas_ct_dry*(Temp_C+273.16))

    !Calculate specific heat capacity in J kg-1 K-1
    avcp=spec_heat_beer(Temp_C,avRh,vap_dens,dens_dry)

    !Latent heat of vaporization in [J kg-1]
    lv_J_kg=lat_vap(Temp_C,Ea_hPa,Press_hPa,avcp,dectime)

    !Latent heat of sublimation in J/kg
    IF(Temp_C<0.000) THEN
       lvS_J_kg=lat_vapSublim(Temp_C,Ea_hPa,Press_hPa,avcp)
    ELSE
       lvS_J_kg = 2834000
    ENDIF
    !if(debug)write(*,*)lv_J_kg,Temp_C,'lv2'
    IF(press_hPa<900) THEN
       CALL ErrorHint(46, 'Function LUMPS_cal_AtmMoist',press_hPa,-55.55, -55)
    ENDIF
    RETURN
  END SUBROUTINE LUMPS_cal_AtmMoist

  !=====================================================================
  ! sg sept 99 f90
  ! Uses eqns from Buck (1981) JAM 20, 1527-1532
  ! units T (deg C) e (mb) P (mb)
  ! f corrects for the fact that we are not dealing with pure water
  ! LJ Feb 2010
  !Changed to use the updated version (Buck research manual, 1996) from Buck (1981)
  !For water different equations in cold and warm temperatures

  FUNCTION sat_vap_press(Temp_c,PRESS_hPa,from,dectime) RESULT(es_hPa)
    ! USE time
    ! USE defaultnotUsed
    IMPLICIT NONE

    REAL(KIND(1d0))::temp_C,press_hpa,dectime!,pw
    REAL(KIND(1d0))::e_mb,f,press_kpa,es_hPA
    INTEGER:: from,iv
    INTEGER,PARAMETER::notUsedI=-55

    !If air temperature between -0.001 -
    IF(ABS(temp_C)<0.001000)THEN
       IF(from==1) THEN  ! not from determining Tw
          iv=INT(press_Hpa)
          CALL errorHint(29,'Function sat_vap_press: temp_C, dectime,press_Hpa = ',temp_C, dectime,iv)

       ENDIF
       temp_C=0.001000
    ENDIF

    Press_kPa=Press_hPa/10

    IF(Temp_C<50.AND.Temp_C>-40)THEN
       !e_mb=6.1121*EXP(((18.729-Temp_C/227.3)*Temp_C)/(Temp_C+257.87)) !Old one
       !f=1.00072+Press_hPa*(3.2E-6+5.9D-10*Temp_C**2)

       IF (Temp_C>=0.001000) THEN
          e_mb=6.1121*EXP(((18.678-Temp_C/234.5)*Temp_C)/(Temp_C+257.14))
          f=1.00072+Press_kPa*(3.2E-6+5.9E-10*Temp_C**2)
          es_hPa=e_mb*f

       ELSEIF (Temp_C<=-0.001000) THEN
          e_mb=6.1115*EXP(((23.036-Temp_C/333.7)*Temp_C)/(Temp_C+279.82))
          f=1.00022+Press_kPa*(3.83E-6+6.4E-10*Temp_C**2)
          es_hPa=e_mb*f
       ENDIF

    ELSE
       CALL ErrorHint(28,'FUNCTION sat_vap_press: [Temperature is out of range], Temp_C,dectime',Temp_C,dectime,notUsedI)

    ENDIF

    RETURN
  END FUNCTION sat_vap_press


  FUNCTION sat_vap_pressIce(Temp_c,PRESS_hPa,from,dectime) RESULT(es_hPa)
    ! USE time
    ! USE defaultnotUsed
    IMPLICIT NONE

    REAL(KIND(1d0))::e_mb,f,temp_C,press_hpa,press_kpa,es_hPA,dectime!,pw
    INTEGER:: from,iv
    INTEGER,PARAMETER::notUsedI=-55

    !If air temperature between -0.001 -
    IF(ABS(temp_C)<0.001000)THEN
       IF(from==1) THEN  ! not from determining Tw
          iv=INT(press_Hpa)
          CALL errorHint(29,'Function sat_vap_press: temp_C, dectime,press_Hpa = ',temp_C, dectime,iv)

       ENDIF
       temp_C=0.001000
    ENDIF

    Press_kPa=Press_hPa/10

    IF(Temp_C<50.AND.Temp_C>-40)THEN
       e_mb=6.1115*EXP(((23.036-Temp_C/333.7)*Temp_C)/(Temp_C+279.82))
       f=1.00022+Press_kPa*(3.83E-6+6.4E-10*Temp_C**2) !In hPa
       es_hPa=e_mb*f

    ELSE
       CALL ErrorHint(28,'FUNCTION sat_vap_press: [Temperature is out of range], Temp_C,dectime',Temp_C,dectime,notUsedI)

    ENDIF

    RETURN
  END FUNCTION sat_vap_pressIce

  !==========================================================
  !Output: specific humidity deficit in g/kg
  !Input: Dry air density and air pressure in hPa
  FUNCTION spec_hum_def(vpd_hPa,press_hPa) RESULT(dq)
    ! USE gas
    IMPLICIT NONE
    REAL(KIND(1d0))           :: press_hPa,vpd_hPa,dq
    REAL(KIND(1d0)),PARAMETER :: epsil_gkg = 621.97 !ratio molecular weight of water vapor/dry air in g/kg
    dq=epsil_gkg*vpd_hPa/press_hPa ! Phd Thesis II.13 p 196
  END FUNCTION spec_hum_def

  ! ==============================================================================
  FUNCTION spec_heat_beer(Temp_C,rh,rho_v,rho_d) RESULT (cp)
    ! Input: Air temperature, relative humidity, water vapour and dry air densities
    ! Output: heat capacity in units J kg-1 K-1
    ! Reference: Tom Beer, CSIRO, 1990. Applied Environmetrics Meteorological Tables.
    ! Can be found from SG:s office from Atmmos Moist map
    !-------------------------------------------------------------------------------

    ! USE defaultnotUsed
    IMPLICIT NONE

    REAL(KIND(1d0))::cp,cpd,cpm,rho_v,rho_d,rh,temp_C

    !Garratt equation a20 (1992)
    CPd = 1005.0+((Temp_C+23.16)**2)/3364.0 !Changed from 23.15 to 23.16

    !Beer (1990) for water vapor
    cpm = 1859 + 0.13*rH+ (19.3+0.569*rH)*(Temp_C/100.) + &
         (10.+0.5*rH)*(Temp_C/100.)**2

    IF(ABS(rho_d)<0.000100.OR.ABS(rho_v)<0.000100.OR.ABS(rho_d+rho_v)<0.000100)THEN
       CALL ErrorHint(42,'spec-heat_beer',rho_v,rho_d,INT(Temp_C))
    ENDIF

    cp=cpd*(rho_d/(rho_d+rho_v))+cpm*(rho_v/(rho_d+rho_v))

    !   print*,"cp: ",cp,cpd,rho_d,rho_v,cpm,rh
  END FUNCTION spec_heat_beer

  !==========================================================
  !Latent_heat.f sg nov 96
  !sg sep 99 converted f90 FUNCTION
  !Added calcualation of latent heat of sublimation, LJ June 2012

  FUNCTION Lat_vap(Temp_C,Ea_hPa,Press_hPa,cp,dectime) RESULT (lv_J_kg)
    !Input: Air temperature, Water vapour pressure, Air pressure, heat capacity
    !Output: latent heat of vaporization

    IMPLICIT NONE
    REAL(KIND(1d0))::cp,lv_J_kg,ea_fix,tw,&
         incr,es_tw,psyc,ea_est,press_hPa,ea_HPa, temp_C,dectime!,Temp_K
    ! REAL(KIND(1d0))::sat_vap_press,psyc_const ! functions

    LOGICAL:: switch1=.FALSE.,switch2=.FALSE.!,debug=.true.
    INTEGER:: ii,from=2
    REAL(KIND(1d0)),PARAMETER::notUsed=-55.55

    ea_fix=ea_hPa
    !if(debug) write(*,*)Temp_C, 'LV'
    !Temp_K=temp_C+273.16

    !lv=1.91846E6*(Temp_K/(Temp_K-33.91))**2

    lv_J_kg=(2500.25-2.365*temp_C)*1000  !First guess for lv in units J/kg


    tw=Temp_C/2.  !First estimate for wet bulb temperature
    incr=3.
    DO ii=1,100
       IF(Press_hPa<900) THEN
          CALL ErrorHint(45,'function Lat_vap',Press_hPA,notUsed,ii)
       ENDIF

       ! if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'% 1',Tw

       es_tw=sat_vap_press(Tw,Press_hPa,from,dectime)  !Calculate saturation vapour pressure in hPa

       !if(debug.and.dectime>55.13.and.dectime<55.2)write(35,*)'% 2',Tw

       IF(Press_hPa<900) THEN
          CALL ErrorHint(45,'function Lat_vap - 2',Press_hPA,notUsed,ii)
       ENDIF

       psyc=psyc_const(cp,Press_hPa,lv_J_kg) !in units hPa/K

       IF(Press_hPa<900) THEN
          CALL ErrorHint(45,'function Lat _vap -31',Press_hPA,notUsed,ii)
       ENDIF

       ea_est=es_tw-psyc*(temp_C-tw)

       lv_J_kg=(2500.25-2.365*tw)*1e3

       IF(switch1.AND.switch2)THEN
          incr=incr/10.
          switch1=.FALSE.
          switch2=.FALSE.
       ENDIF
       IF(ABS(ea_est-ea_fix)<0.001000)THEN
          RETURN
       ELSEIF(ea_est > ea_fix)THEN
          tw=tw-incr
          switch1=.TRUE.
       ELSEIF(ea_est< ea_fix)THEN
          tw=tw+incr
          switch2=.TRUE.
       ENDIF
    ENDDO

    RETURN
  END FUNCTION Lat_vap


  FUNCTION Lat_vapSublim(Temp_C,Ea_hPa,Press_hPa,cp) RESULT (lvS_J_kg)
    !Input: Air temperature, Water vapour pressure, Air pressure, heat capacity
    !Output: latent heat of sublimation in units J/kg

    ! USE time

    IMPLICIT NONE

    REAL(KIND(1d0))::lvS_J_kg,temp_C,tw,incr,Ea_hPa,Press_hPa,cp

    lvS_J_kg=(2834.1-0.29*temp_C)*1e3 !First guess for Ls in J/kg

    tw=Temp_C/2.  !First estimate for wet bulb temperature
    incr=3.
    Press_hPa=Press_hPa
    Ea_hPa=Ea_hPa
    cp=cp

  END FUNCTION Lat_vapSublim



  !=====================================================================
  !psyc_const.f   sg   nov 96
  !sg june 99 f90
  !calculate psyc - psychrometic constant Fritschen and Gay (1979)

  FUNCTION psyc_const(cp,Press_hPa,lv_J_kg) RESULT(psyc_hPa) !In units hPa/K
    USE gas

    IMPLICIT NONE
    REAL (KIND(1d0))::cp,lv_J_kg,press_hPa,psyc_hpa

    ! cp for moist air (shuttleworth p 4.13)
    IF(cp*press_hPa<900.OR.lv_J_kg<10000)THEN
       CALL errorHint(19,'in psychrometric constant calculation:  cp [J kg-1 K-1], p [hPa], Lv [J kg-1]',cp,Press_hPa,INT(lv_J_kg))
    ENDIF

    psyc_hPa=(cp*press_hPa)/(epsil*lv_J_kg)
    !    if(debug)write(*,*)psyc_hpa, 'g',cp,press_HPa,lv
    ! LV MJKg-1
    !www.cimis.water.ca.gov/infoEotPmEquation.jsp
    !psyc_hPa=(0.00163*(Press_hPa/10)/LV)*10
    ! write(*,*)psyc_hpa
    !psyc=psyc*100.! convert into Pa
  END FUNCTION psyc_const

  !==========================================================

  FUNCTION dewpoint(ea_hPa) RESULT(Temp_C_dew)
    ! ea = vapor pressure (hPa)
    ! td = dewpoint (oC)
    !calculates dewpoint in degC from
    ! http://www.atd.ucar.edu/weather_fl/dewpoint.html
    !     dewpoint = (237.3 * ln(e_vp/6.1078)) / (17.27 - (ln(e_vp/6.1078)))

    REAL(KIND(1d0))::ea_hPa,temp_C_dew
    Temp_C_dew = (237.3 * LOG(ea_hPa/6.1078)) / (17.27 - (LOG(ea_hPa/6.1078)))
  END FUNCTION dewpoint
  !===============================================================================
  FUNCTION slope_svp(temp_C) RESULT(s_hPa)
    !COEFFICENTS FOR CALCULATING desat/dT
    !Slope of the saturation vapor pressure vst air temperature curve

    IMPLICIT  NONE

    REAL (KIND(1d0)):: b1,b2,b3,b4,b5,b6,b7,S_hPa,temp_C
    B1=4.438099984D-1
    B2=2.857002636D-2
    B3=7.938054040D-4
    B4=1.215215065D-5
    B5=1.036561403D-7
    B6=3.532421810D-10
    B7=-7.090244804D-13

    !     s - slope of saturation vapour pressure curve - Lowe (1977) -T (K)
    !     mb /K
    S_hPa=B1+temp_C*(B2+temp_C*(B3+temp_C*(B4+temp_C*(B5+temp_C*(B6+B7*temp_C)))))
    ! write(*,*)'s',s_hpa,temp_C
    !s_Pa=s_Pa*100  ! Pa/K
    !www.cimis.water.ca.gov/infoEotPmEquation.jsp
    ! s_hPa=(((4099 *(es_hPa/10))/(Temp_C+273.3)**2))*10
    ! if(debug)write(*,*)s_hpa
    RETURN
  END FUNCTION slope_svp

  !===============================================================================
  FUNCTION slopeIce_svp(temp_C) RESULT(s_hPa)
    !COEFFICENTS FOR CALCULATING desat/dT
    !Slope of the saturation vapor pressure vst air temperature curve

    IMPLICIT  NONE

    REAL (KIND(1d0)):: b1,b2,b3,b4,b5,b6,b7,S_hPa,temp_C

    B1=5.030305237D-1
    B2=3.773255020D-2
    B3=1.267995369D-3
    B4=2.477563108D-5
    B5=3.005693132D-7
    B6=2.158542548D-9
    B7=7.131097725D-12

    ! s - slope of saturation vapour pressure curve - Lowe (1977) -T (K)
    ! mb /K
    S_hPa=B1+temp_C*(B2+temp_C*(B3+temp_C*(B4+temp_C*(B5+temp_C*(B6+B7*temp_C)))))

    RETURN
  END FUNCTION slopeIce_svp

  !------------------------------------------------------------------------
  FUNCTION qsatf(T,PMB) RESULT(qsat)
    !       MRR, 1987
    ! AT TEMPERATURE T (DEG C) AND PRESSURE PMB (MB), GET SATURATION SPECIFIC
    !       HUMIDITY (KG/KG) FROM TETEN FORMULA

    REAL (KIND(1D0))::T,es,qsat,PMB

    REAL (KIND(1D0)),PARAMETER::&

                                !Teten coefficients
         A=6.106,&
         B=17.27,&
         C=237.3,&
         molar=0.028965,& !Dry air molar fraction in kg/mol
         molar_wat_vap=0.0180153 !Molar fraction of water vapor in kg/mol


    IF(t.GT.55)THEN
       CALL ErrorHint(34,'Function qsatf',T,0.00D0,-55)
    ENDIF

    ES = A*dEXP(B*T/(C+T))
    qsat = (molar_wat_vap/molar)*ES/PMB!(rmh2o/rmair)*ES/PMB
  END FUNCTION qsatf

END MODULE AtmMoist_module


MODULE DailyState_module
  USE allocateArray,ONLY:&
       ndays,nsurf,nvegsurf,ivConif,ivDecid,ivGrass,ncolumnsDataOutDailyState


  IMPLICIT NONE

CONTAINS

  ! Calculation of daily state variables
  ! Responds to what has happened in the past (temperature, rainfall, etc)
  ! Updates each time step, but for many variables, correct values are calculated only at the end of each day!
  ! --> for these variables, the rest of the code MUST use values from the previous day
  ! N.B. Some of this code is repeated in SUEWS_Initial
  ! --> so if changes are made here, SUEWS_Initial may also need to be updated accordingly
  ! N.B. Currently, daily variables are calculated using 00:00-23:55 timestamps (for 5-min resolution); should use 00:05-00:00
  !
  ! Last modified:
  !  TS 18 Sep 2017  - Added explicit interface
  !  TS 07 Jun 2017  - Improve the format of output with more friendly alignment
  !  HCW 04 Jul 2016 - GridID can now be up to 10 digits long
  !  HCW 25 May 2016 - Added extra columns to daily state file (albedo for EveTr and Grass)
  !  HCW 24 May 2016 - Bug fixed in naming of SUEWS_cal_DailyState file (now uses GridIDmatrix(Gridiv) rather than Gridiv)
  !  LJ 27 Jan 2016  - Removal of tabs
  !  HCW 20 Aug 2015 - Sign of the porosity change corrected so that porosity is greatest when LAI is smallest
  !  HCW 03 Jul 2015 - Increased output resolution of P/day in SUEWS_cal_DailyState file to avoid rounding errors.
  !                    Albedo of EveTr and Grass now adjusted based on change in LAI for EveTr and Grass
  !                    (rather than DecTr)
  !  HCW 29 Jun 2015 - Added albChange for EveTr and Grass surfaces
  !  HCW 11 Jun 2015 - Bug fix from 05 Jun now fixed in a different way -
  !                    DecidCap is now treated the same as DecidAlb so should be able to cope with multiple grids.
  !  HCW 05 Jun 2015 - Bug fix - set all current storage capacities (surf(6,)) to min. value, then set for DecTr
  !  LJ 11 Mar 2015  - Removed switch as no longer necessary
  !  HCW 06 Mar 2015 - iy used instead of year which does not have a value here
  !  HCW 20 Feb 2015 - Added surf(6,is) for the current storage capacity
  !  Updated and corrected SUEWS_cal_DailyState output file
  !  LJ 05 Feb 2015  - SUEWS_cal_DailyState saving fixed. Now header is printed and the file closed and opened as suggested.
  ! N.B. Bug in daily Precip - needs fixing!!! - HCW thinks this is fixed 20 Feb 2015
  !  HCW 26 Jan 2015 - sfr and IrrFracs deleted from WU_Day calculations, so that WU_Day is not spread over
  !  the total area
  !  HCW 23 Jan 2015 - WU_Day now has 9 columns (EveTr, DecTr, Grass; automatic, manual, total)
  !  HCW 27 Nov 2014 - Handles values for different grids (Gridiv & ir arguments)
  ! Added the calculation of surface temperature
  !  LJ 22 Feb 2013  - Snow albedo aging and calculation of snow density added,
  !  LJ 22 Jul 2013  - Calculation of LAI senescence from previous day length added
  ! sg feb 2012 - rewritten from LUMPS_LAI so done in real time
  !
  ! To Do
  !   - Account for change of year in 5-day running mean?
  !   - Check LAI calcs (N/S hemisphere similarities; use of day length)
  !       - Take out doy limits (140,170, etc) and code as parameters
  !   - Could add different coefficients (Ie_m, Ie_a) for each vegetation type
  !==============================================================================
  SUBROUTINE SUEWS_cal_DailyState(&
       iy,id,it,imin,tstep,DayofWeek_id,&!input
       WaterUseMethod,snowUse,Ie_start,Ie_end,&
       LAICalcYes,LAIType,&
       nsh_real,avkdn,Temp_C,Precip,BaseTHDD,&
       lat,Faut,LAI_obs,tau_a,tau_f,tau_r,&
       SnowDensMax,SnowDensMin,SnowAlbMin,&
       alBMax_DecTr,alBMax_EveTr,alBMax_Grass,&
       AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       CapMax_dec,CapMin_dec,PorMax_dec,PorMin_dec,&
       Ie_a,Ie_m,DayWatPer,DayWat,SnowPack,&
       BaseT,BaseTe,GDDFull,SDDFull,LAIMin,LAIMax,LAIPower,&
       SnowAlb,DecidCap,albDecTr,albEveTr,albGrass,&!inout
       porosity,GDD,HDD,SnowDens,LAI,WU_Day,&
       deltaLAI)!output


    IMPLICIT NONE

    INTEGER,INTENT(IN)::iy
    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::it
    INTEGER,INTENT(IN)::imin

    ! INTEGER,INTENT(IN)::Gridiv
    INTEGER,INTENT(IN)::tstep
    INTEGER,INTENT(IN)::WaterUseMethod
    INTEGER,INTENT(IN)::snowUse
    INTEGER,INTENT(IN)::Ie_start   !Starting time of water use (DOY)
    INTEGER,INTENT(IN)::Ie_end       !Ending time of water use (DOY)
    ! INTEGER,INTENT(IN)::ReadLinesMetdata
    ! INTEGER,INTENT(IN)::ncolumnsDataOutSUEWS
    ! INTEGER,INTENT(IN)::NumberOfGrids
    INTEGER,INTENT(IN)::LAICalcYes


    INTEGER,DIMENSION(nvegsurf),INTENT(IN):: LAIType                  !LAI equation to use: original (0) or new (1)

    ! INTEGER,DIMENSION(MaxNumberOfGrids),INTENT(IN):: GridIDmatrix         !Array containing GridIDs in SiteSelect after sorting

    REAL(KIND(1d0)),INTENT(IN)::nsh_real
    REAL(KIND(1d0)),INTENT(IN)::avkdn
    REAL(KIND(1d0)),INTENT(IN)::Temp_C
    REAL(KIND(1d0)),INTENT(IN)::Precip
    REAL(KIND(1d0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::Faut
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs
    REAL(KIND(1D0)),INTENT(IN)::tau_a
    REAL(KIND(1D0)),INTENT(IN)::tau_f
    REAL(KIND(1D0)),INTENT(IN)::tau_r
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMax
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMin
    REAL(KIND(1D0)),INTENT(IN)::SnowAlbMin
    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec
    ! REAL(KIND(1d0)),INTENT(IN) ::VegPhenLumps

    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN) ::Ie_a
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN) ::Ie_m !Coefficients for automatic and manual irrigation models
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN) ::DayWatPer !% of houses following daily water
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN) ::DayWat !Days of watering allowed


    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(IN)      ::SnowPack
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::BaseT !Base temperature for growing degree days [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::BaseTe !Base temperature for senescence degree days [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::GDDFull !Growing degree days needed for full capacity [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::SDDFull !Senescence degree days needed to initiate leaf off [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::LAIMin !Min LAI [m2 m-2]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::LAIMax !Max LAI [m2 m-2]
    REAL(KIND(1d0)),DIMENSION(4,nvegsurf),INTENT(IN) ::LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

    REAL(KIND(1d0)),INTENT(INOUT)::SnowAlb

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::porosity
    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT):: GDD !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(INOUT):: HDD          !Heating Degree Days (see SUEWS_DailyState.f95)

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(INOUT)::SnowDens
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT):: LAI !LAI for each veg surface [m2 m-2]
    INTEGER,DIMENSION(3),INTENT(in)::DayofWeek_id

    !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(INOUT):: WU_Day
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI
    ! REAL(KIND(1d0)),INTENT(OUT)::xBo

    INTEGER::date

    ! --------------------------------------------------------------------------------
    ! ------------- Key to daily arrays ----------------------------------------------
    ! HDD(,1) ---- Heating         [degC] ! GDD(,1) ---- Growing      [degC]
    ! HDD(,2) ---- Cooling         [degC] ! GDD(,2) ---- Senescence   [degC]
    ! HDD(,3) ---- Daily mean temp     [degC] ! GDD(,3) ---- Daily min temp   [degC]
    ! HDD(,4) ---- 5-day running mean temp [degC] ! GDD(,4) ---- Daily max temp   [degC]
    ! HDD(,5) ---- Daily precip total  [mm]   ! GDD(,5) ---- Daytime hours    [h]
    ! HDD(,6) ---- Days since rain     [d]
    !
    ! LAI(,1:3) -- LAI for each veg surface [m2 m-2]
    !
    ! WU_Day(,1) - Daily water use total for Irr EveTr (automatic+manual) [mm]
    ! WU_Day(,2) - Automatic irrigation for Irr EveTr             [mm]
    ! WU_Day(,3) - Manual irrigation for Irr EveTr            [mm]
    ! WU_Day(,4) - Daily water use total for Irr DecTr (automatic+manual) [mm]
    ! WU_Day(,5) - Automatic irrigation for Irr DecTr             [mm]
    ! WU_Day(,6) - Manual irrigation for Irr DecTr            [mm]
    ! WU_Day(,7) - Daily water use total for Irr Grass (automatic+manual) [mm]
    ! WU_Day(,8) - Automatic irrigation for Irr Grass                 [mm]
    ! WU_Day(,9) - Manual irrigation for Irr Grass            [mm]
    ! --------------------------------------------------------------------------------

    CALL init_DailyState(&
         id,& !input
         avkdn,&
         Temp_C,&
         Precip,&
         BaseTHDD,&
         nsh_real,&
         GDD,&!inout
         HDD)


    ! Update snow density, albedo surface fraction
    IF (snowUse==1) CALL SnowUpdate(&
         nsurf,tstep,Temp_C,tau_a,tau_f,tau_r,&!input
         SnowDensMax,SnowDensMin,SnowAlbMin,SnowPack,&
         SnowAlb,SnowDens)!inout

    ! ================================================================================
    ! This next part occurs only on the first or last timestep of each day ===========

    ! --------------------------------------------------------------------------------
    ! On first timestep of each day, define whether the day each a workday or weekend
    IF (it==0.AND.imin==0) THEN
       ! CALL Cal_DailyStateStart(&
       !      id,iy,lat,&!input
       !      DayofWeek_id)!output

       ! --------------------------------------------------------------------------------
       ! On last timestep, perform the daily calculations -------------------------------
       ! Daily values not correct until end of each day,
       !  so main program should use values from the previous day
    ELSEIF (it==23 .AND. imin==(nsh_real-1)/nsh_real*60) THEN
       CALL Cal_DailyStateEnd(&
            id,it,imin,tstep,&!input
            LAIType,Ie_end,Ie_start,LAICalcYes,&
            WaterUseMethod,DayofWeek_id,&
            alBMax_DecTr,alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
            BaseT,BaseTe,CapMax_dec,CapMin_dec,DayWat,DayWatPer,Faut,GDDFull,&
            Ie_a,Ie_m,LAIMax,LAIMin,LAIPower,lat,PorMax_dec,PorMin_dec,SDDFull,LAI_obs,&
            albDecTr,albEveTr,albGrass,porosity,DecidCap,&!inout
            GDD,HDD,LAI,WU_Day,&!inout
            deltaLAI)!output
       ! ,xBo)!output
    ENDIF   !End of section done only at the end of each day (i.e. only once per day)

    RETURN

  END SUBROUTINE SUEWS_cal_DailyState


  SUBROUTINE Cal_DailyStateEnd(&
       id,it,imin,tstep,&!input
       LAIType,Ie_end,Ie_start,LAICalcYes,&
       WaterUseMethod,DayofWeek_id,&
       alBMax_DecTr,alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       BaseT,BaseTe,CapMax_dec,CapMin_dec,DayWat,DayWatPer,Faut,GDDFull,&
       Ie_a,Ie_m,LAIMax,LAIMin,LAIPower,lat,PorMax_dec,PorMin_dec,SDDFull,LAI_obs,&
       albDecTr,albEveTr,albGrass,porosity,DecidCap,&!inout
       GDD,HDD,LAI,WU_Day,&!inout
       deltaLAI)!output
    IMPLICIT NONE
    ! INTEGER,PARAMETER::ndays    = 366
    ! INTEGER,PARAMETER::nvegsurf = 3


    ! INTEGER,INTENT(IN)::ncolumnsDataOutSUEWS
    ! INTEGER,INTENT(IN)::NumberOfGrids
    ! INTEGER,INTENT(IN)::ReadLinesMetdata
    ! INTEGER,INTENT(IN)::Gridiv
    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::it
    INTEGER,INTENT(IN)::imin
    INTEGER,INTENT(IN)::tstep
    INTEGER,INTENT(IN)::LAIType(nvegsurf)
    INTEGER,INTENT(IN)::Ie_end
    INTEGER,INTENT(IN)::Ie_start
    INTEGER,INTENT(IN)::LAICalcYes
    INTEGER,INTENT(IN)::WaterUseMethod
    INTEGER,INTENT(in)::DayofWeek_id(3)

    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::BaseT(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::BaseTe(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    ! REAL(KIND(1d0)),INTENT(IN)::dataOutSUEWS(ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids)
    REAL(KIND(1d0)),INTENT(IN)::DayWat(7)
    REAL(KIND(1d0)),INTENT(IN)::DayWatPer(7)
    REAL(KIND(1d0)),INTENT(IN)::Faut
    REAL(KIND(1d0)),INTENT(IN)::GDDFull(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::Ie_a(3)
    REAL(KIND(1d0)),INTENT(IN)::Ie_m(3)
    REAL(KIND(1d0)),INTENT(IN)::LAIMax(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAIMin(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAIPower(4,nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec
    REAL(KIND(1d0)),INTENT(IN)::SDDFull(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs

    ! REAL(KIND(1d0)),INTENT(INOUT)::a1
    ! REAL(KIND(1d0)),INTENT(INOUT)::a2
    ! REAL(KIND(1d0)),INTENT(INOUT)::a3
    REAL(KIND(1d0)),INTENT(INOUT)::albDecTr( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::albEveTr( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::albGrass( 0:ndays)
    ! REAL(KIND(1d0)),INTENT(INOUT)::tstepcount
    REAL(KIND(1d0)),INTENT(INOUT)::porosity( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::DecidCap( 0:ndays)
    ! REAL(KIND(1d0)),INTENT(INOUT)::xmAH
    REAL(KIND(1d0)),INTENT(INOUT)::GDD( 0:ndays, 5)
    REAL(KIND(1d0)),INTENT(INOUT)::HDD(-4:ndays, 6)
    REAL(KIND(1d0)),INTENT(INOUT)::LAI(-4:ndays, nvegsurf)

    REAL(KIND(1d0)),INTENT(INOUT):: WU_Day(0:ndays,9)
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI
    ! REAL(KIND(1d0)),INTENT(OUT):: xBo


    !write(*,*) 'Last timestep of day'

    CALL update_HDD(&
         id,it,imin,tstep,& !input
         HDD) !inout

    ! IF(Gridiv == NumberOfGrids) tstepcount=0  !Set to zero only after last grid has run

    ! Calculate modelled daily water use ------------------------------------------
    CALL update_WaterUse(&
         id,WaterUseMethod,DayofWeek_id,lat,Faut,HDD,&!input
         Ie_a,Ie_m,Ie_start,Ie_end,DayWatPer,DayWat,&
         WU_Day) !inout

    !------------------------------------------------------------------------------
    ! Calculation of LAI from growing degree days
    ! This was revised and checked on 16 Feb 2014 by LJ
    !------------------------------------------------------------------------------
    CALL update_GDDLAI(&
         id,LAICalcYes,& !input
         lat,LAI_obs,&
         BaseT,&
         BaseTe,&
         GDDFull,&
         SDDFull,&
         LAIMin,&
         LAIMax,&
         LAIPower,LAIType,&
         GDD,LAI) !inout

    CALL update_Veg(&
         id,&!input
         LAImax,&
         LAIMin,&
         alBMax_DecTr,&
         alBMax_EveTr,&
         alBMax_Grass,&
         AlbMin_DecTr,&
         AlbMin_EveTr,&
         AlbMin_Grass,&
         CapMax_dec,&
         CapMin_dec,&
         PorMax_dec,&
         PorMin_dec,&
         DecidCap,&!inout
         albDecTr,&
         albEveTr,&
         albGrass,&
         porosity,&
         LAI,&
         deltaLAI)!output

    ! CALL update_AnOHM(&
    !      Gridiv,id,& !input
    !      ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids,dataOutSUEWS,&
    !                             !  a1,a2,a3,&!inout
    !      xBo,xmAH) !output


  END SUBROUTINE Cal_DailyStateEnd


  SUBROUTINE init_DailyState(&
       id,& !input
       avkdn,&
       Temp_C,&
       Precip,&
       BaseTHDD,&
       nsh_real,&
       GDD,&!inout
       HDD)
    IMPLICIT NONE
    ! INTEGER,PARAMETER::ndays    = 366

    INTEGER,INTENT(IN)::id
    ! INTEGER,INTENT(IN)::it
    ! INTEGER,INTENT(IN)::imin
    ! INTEGER,INTENT(IN)::tstep
    ! INTEGER,INTENT(IN)::Gridiv
    REAL(KIND(1d0)),INTENT(IN)::avkdn
    REAL(KIND(1d0)),INTENT(IN)::Temp_C
    REAL(KIND(1d0)),INTENT(IN)::Precip
    REAL(KIND(1d0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1d0)),INTENT(IN)::nsh_real

    ! REAL(KIND(1d0))::tstepcount
    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT):: GDD !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(INOUT):: HDD          !Heating Degree Days (see SUEWS_DailyState.f95)

    INTEGER::gamma1
    INTEGER::gamma2
    !! Initialization -----------------------------------------------------------------
    !! These variables don't seem to be needed (commented out HCW 27 Nov 2014)
    !! If required, they will need updating for a non-hourly timestep
    !runT(it)=Temp_C      !runT has been initialized in SUEWS_initial to the previous day average
    !avT_h=sum(runT)/24   !Average daily temperature
    !runP(it)=Precip   !Same for precipitation
    !totP_h=sum(runP)     !Daily sum for precipitation

    ! Daily min and max temp (these get updated through the day) ---------------------
    GDD(id,3) = MIN(Temp_C,GDD(id,3))     !Daily min T in column 3
    GDD(id,4) = MAX(Temp_C,GDD(id,4))     !Daily max T in column 4
    IF (avkdn>10) THEN
       GDD(id,5) = GDD(id,5)+1/nsh_real   !Cumulate daytime hours !Divide by nsh (HCW 01 Dec 2014)
    ENDIF

    ! Calculations related to heating and cooling degree days (HDD) ------------------
    ! See Sailor & Vasireddy (2006) EMS Eq 1,2 (theirs is hourly timestep)
    IF ((BaseTHDD-Temp_C)>=0) THEN   !Heating
       gamma1=1
    ELSE
       gamma1=0
    ENDIF

    IF ((Temp_C-BaseTHDD)>=0) THEN   !Cooling
       gamma2=1
    ELSE
       gamma2=0
    ENDIF

    ! count of timesteps performed during day `id`
    ! tstepcount=(it*60+imin)*60/tstep
    ! IF(Gridiv == 1) tstepcount=tstepcount+1   !Add 1 to tstepcount only once for all grids

    HDD(id,1)=HDD(id,1) + gamma1*(BaseTHDD-Temp_C)   !Heating
    HDD(id,2)=HDD(id,2) + gamma2*(Temp_C-BaseTHDD)   !Cooling
    HDD(id,3)=HDD(id,3) + Temp_C                     !Will become daily average temperature
    !      4 ------------------------------------!   !5-day running mean
    HDD(id,5)=HDD(id,5) + Precip                     !Daily precip total
    !      6 ------------------------------------!   !Days since rain

  END SUBROUTINE init_DailyState


  SUBROUTINE update_AnOHM(&
       Gridiv,id,& !input
       ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids,dataOutSUEWS,&
                                !  a1,a2,a3,&!inout
       xBo,xmAH) !output
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: Gridiv
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::ReadLinesMetdata
    INTEGER,INTENT(IN) ::ncolumnsDataOutSUEWS
    INTEGER,INTENT(IN) ::NumberOfGrids

    REAL(KIND(1d0)),DIMENSION(ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids),INTENT(IN)::dataOutSUEWS

    ! REAL(KIND(1d0)),INTENT(INOUT)::a1
    ! REAL(KIND(1d0)),INTENT(INOUT)::a2
    ! REAL(KIND(1d0)),INTENT(INOUT)::a3

    REAL(KIND(1d0)),INTENT(OUT)::xBo
    REAL(KIND(1d0)),INTENT(OUT)::xmAH
    !   local variables:
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE :: subMet ! subset array of daytime series

    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE :: xQH
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE ::xQE
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE ::xAH
    REAL(KIND(1d0))::mxQH
    REAL(KIND(1d0))::mxQE
    ! REAL(KIND(1d0))::xa1
    ! REAL(KIND(1d0))::xa2
    ! REAL(KIND(1d0))::xa3

    INTEGER:: err
    INTEGER:: lenMetData
    INTEGER::nVar


    LOGICAL, ALLOCATABLE :: metMask(:)


    ! construct mask
    IF (ALLOCATED(metMask)) DEALLOCATE(metMask, STAT=err)
    ALLOCATE(metMask(SIZE(dataOutSUEWS, dim=1)))
    metMask=(dataOutSUEWS(:,2,Gridiv)==id & ! day=xid
         .AND. dataOutSUEWS(:,4,Gridiv)==0)! tmin=0

    ! determine the length of subset
    lenMetData = COUNT(metMask)

    ! construct array for time and met variables
    nVar=3! number of variables to retrieve
    ! print*, 'good 1'
    ! allocate subMet:
    IF (ALLOCATED(subMet)) DEALLOCATE(subMet, STAT=err)
    ALLOCATE(subMet(lenMetData,nVar))
    subMet=RESHAPE(PACK(dataOutSUEWS(:,(/14,15,16/),Gridiv),&! QH,QE,AH
         SPREAD(metMask, dim=2, ncopies=nVar)),& ! replicate mask vector to 2D array
         (/lenMetData,nVar/)) ! convert to target shape

    ! re-allocate arrays as their sizes may change during passing
    IF (ALLOCATED(xQH)) DEALLOCATE(xQH, STAT=err)
    ALLOCATE(xQH(lenMetData))
    IF (ALLOCATED(xQE)) DEALLOCATE(xQE, STAT=err)
    ALLOCATE(xQE(lenMetData))
    IF (ALLOCATED(xAH)) DEALLOCATE(xAH, STAT=err)
    ALLOCATE(xAH(lenMetData))

    xQH  = subMet(:,1)
    xQE  = subMet(:,2)
    mxQH = SUM(xQH(10:16))/7
    mxQE = SUM(xQE(10:16))/7
    ! handle extreme dry condition to prevent NAN
    IF ( ABS(mxQE) < 0.1 ) mxQE = 0.1
    xBo  = mxQH/mxQE

    !   calculate daily mean AH
    xAH  = subMet(:,3)
    xmAH  = SUM(xAH(:))/24

    ! xa1=a1
    ! xa2=a2
    ! xa3=a3

  END SUBROUTINE update_AnOHM


  SUBROUTINE update_Veg(&
       id,&!input
       LAImax,&
       LAIMin,&
       alBMax_DecTr,&
       alBMax_EveTr,&
       alBMax_Grass,&
       AlbMin_DecTr,&
       AlbMin_EveTr,&
       AlbMin_Grass,&
       CapMax_dec,&
       CapMin_dec,&
       PorMax_dec,&
       PorMin_dec,&
       DecidCap,&!inout
       albDecTr,&
       albEveTr,&
       albGrass,&
       porosity,&
       LAI,&
       deltaLAI)!output

    IMPLICIT NONE

    INTEGER,INTENT(IN)::id
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)::LAImax
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)::LAIMin

    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::porosity

    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT)::LAI
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI

    INTEGER::iv

    REAL(KIND(1d0))::albChangeDecTr
    REAL(KIND(1d0))::albChangeEveTr
    REAL(KIND(1d0))::albChangeGrass
    REAL(KIND(1d0))::CapChange

    REAL(KIND(1d0))::deltaLAIEveTr
    REAL(KIND(1d0))::deltaLAIGrass
    REAL(KIND(1d0))::porChange
    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ! Calculate the development of vegetation cover
    ! Albedo changes with LAI for each vegetation type
    ! Storage capacity and porosity are updated based on DecTr LAI only (seasonal variation in Grass and EveTr assumed small)
    ! If only LUMPS is used, set deciduous capacities to 0
    ! Assume porosity Change based on GO99- Heisler??
    deltaLAI=0
    deltaLAIEveTr=0
    deltaLAIGrass=0
    CapChange=0
    porChange=0
    albChangeDecTr=0
    albChangeEveTr=0
    albChangeGrass=0

    iv=ivDecid
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAI=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeDecTr=(alBMax_DecTr-AlbMin_DecTr)* deltaLAI
       CapChange=(CapMin_dec-CapMax_dec)* deltaLAI
       porChange=(PorMin_dec-PorMax_dec)* deltaLAI
    ENDIF

    iv=ivConif
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAIEveTr=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeEveTr=(alBMax_EveTr-AlbMin_EveTr)* deltaLAIEveTr    !!N.B. Currently uses deltaLAI for deciduous trees only!!
    ENDIF

    iv=ivGrass
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAIGrass=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeGrass=(alBMax_Grass-AlbMin_Grass)* deltaLAIGrass    !!N.B. Currently uses deltaLAI for deciduous trees only!!
    ENDIF

    iv=ivDecid

    !write(*,*) deltaLAI, deltaLAIEveTr, deltaLAIGrass

    DecidCap(id) = DecidCap(id-1) - CapChange
    albDecTr(id) = albDecTr(id-1) + albChangeDecTr
    porosity(id) = porosity(id-1) + porChange !- changed to + by HCW 20 Aug 2015 (porosity greatest when LAI smallest)
    !Also update albedo of EveTr and Grass surfaces
    albEveTr(id) = albEveTr(id-1) + albChangeEveTr
    albGrass(id) = albGrass(id-1) + albChangeGrass

  END SUBROUTINE update_Veg



  SUBROUTINE update_GDDLAI(&
       id,LAICalcYes,& !input
       lat,LAI_obs,&
       BaseT,&
       BaseTe,&
       GDDFull,&
       SDDFull,&
       LAIMin,&
       LAIMax,&
       LAIPower,LAIType,&
       GDD,LAI) !inout
    IMPLICIT NONE

    !------------------------------------------------------------------------------
    ! Calculation of LAI from growing degree days
    ! This was revised and checked on 16 Feb 2014 by LJ
    !------------------------------------------------------------------------------
    ! INTEGER, PARAMETER:: ndays = 366   !Max no. days in a year used to specify size of daily arrays
    ! INTEGER, PARAMETER:: nsurf=7                !Total number of surfaces
    ! INTEGER, PARAMETER:: NVegSurf=3             !Number of surfaces that are vegetated
    ! INTEGER, PARAMETER:: nsurfIncSnow=nsurf+1   !Number of surfaces + snow
    ! INTEGER, PARAMETER:: MaxNumberOfGrids=2000   !Max no. grids   !HCW changed to 2000 from 10000 so prog can run on windows (2GB lim)
    ! INTEGER, PARAMETER:: MaxLinesMet=8640        !Max no. lines to read in one go (for all grids, ie MaxLinesMet/NumberOfGrids each)

    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::LAICalcYes

    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs

    ! --- Vegetation phenology ---------------------------------------------------------------------
    ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: BaseT          !Base temperature for growing degree days [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: BaseTe         !Base temperature for senescence degree days [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: GDDFull        !Growing degree days needed for full capacity [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: SDDFull        !Senescence degree days needed to initiate leaf off [degC]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: LAIMin         !Min LAI [m2 m-2]
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: LAIMax         !Max LAI [m2 m-2]
    REAL(KIND(1d0)),DIMENSION(4,nvegsurf),INTENT(IN):: LAIPower       !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
    !! N.B. currently DecTr only, although input provided for all veg types
    INTEGER,DIMENSION(nvegsurf),INTENT(IN):: LAIType                  !LAI equation to use: original (0) or new (1)

    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT)       :: GDD !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT):: LAI !LAI for each veg surface [m2 m-2]

    REAL(KIND(1d0)):: no   !Switches and checks for GDD
    REAL(KIND(1d0))::yes   !Switches and checks for GDD
    REAL(KIND(1d0))::indHelp   !Switches and checks for GDD

    INTEGER:: critDays
    INTEGER::iv


    critDays=50   !Critical limit for GDD when GDD or SDD is set to zero

    ! Loop through vegetation types (iv)
    DO iv=1,NVegSurf
       ! Calculate GDD for each day from the minimum and maximum air temperature
       yes =((GDD(id,3)+GDD(id,4))/2-BaseT(iv))    !Leaf on
       no  =((GDD(id,3)+GDD(id,4))/2-BaseTe(iv))   !Leaf off

       indHelp = 0   !Help switch to allow GDD to go to zero in sprint-time !!What does this mean?? HCW

       IF(yes<0) THEN   !GDD cannot be negative
          indHelp=yes   !Amount of negative GDD
          yes=0
       ENDIF

       IF(no>0) no=0    !SDD cannot be positive

       ! Calculate cumulative growing and senescence degree days
       GDD(id,1) = GDD(id-1,1)+yes
       GDD(id,2) = GDD(id-1,2)+no

       ! Possibility for cold spring
       IF(GDD(id,2)<=SDDFull(iv).AND.indHelp<0) THEN
          GDD(id,1)=0
       ENDIF

       IF(GDD(id,1)>=GDDFull(iv)) THEN   !Start senescence
          GDD(id,1)=GDDFull(iv)          !Leaves should not grow so delete yes from earlier
          IF(GDD(id,2)<-critDays) GDD(id,1)=0
       ENDIF

       IF (GDD(id,2)<=SDDFull(iv)) THEN   !After senescence now start growing leaves
          GDD(id,2)=SDDFull(iv)           !Leaves off so add back earlier
          IF(GDD(id,1)>critDays) GDD(id,2)=0
       ENDIF

       ! With these limits SDD, GDD is set to zero
       IF(GDD(id,2)<-critDays.AND.GDD(id,2)>SDDFull(iv))  GDD(id,1)=0
       IF(GDD(id,1)> critDays.AND.GDD(id,1)<GDDFull(iv))  GDD(id,2)=0

       ! Now calculate LAI itself
       IF(lat>=0) THEN   !Northern hemispere
          IF (id==140.AND.GDD(id,2)/=0)  GDD(id,2)=0  !If SDD is not zero by mid May, this is forced
          ! Set SDD to zero in summer time
          IF (GDD(id,1)> critDays.AND.id<170) GDD(id,2)=0
          ! Set GDD zero in winter time
          IF (GDD(id,2)<-critDays.AND.id>170) GDD(id,1)=0

          IF (LAItype(iv) < 0.5) THEN   !Original LAI type
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN       !Leaves can still grow
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN   !Start senescence
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(3,iv)*GDD(id,2)*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ELSEIF (LAItype(iv)>=0.5) THEN
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN        !Leaves can still grow
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
                !! Use day length to start senescence at high latitudes (N hemisphere)
             ELSEIF (GDD(id,5)<=12.AND.GDD(id,2)>SDDFull(iv)) THEN !Start senescence
                LAI(id,iv)=(LAI(id-1,iv)*LAIPower(3,iv)*(1-GDD(id,2))*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ENDIF

       ELSEIF (lat<0) THEN   !Southern hemisphere !! N.B. not identical to N hemisphere - return to later
          IF (id==300.AND.GDD(id,2)/=0)  GDD(id,2)=0   !If SDD is not zero by late Oct, this is forced
          ! Set SDD to zero in summer time
          IF (GDD(id,1)> critDays.AND.id>250) GDD(id,2)=0
          ! Set GDD zero in winter time
          IF (GDD(id,2)<-critDays.AND.id<250) GDD(id,1)=0

          IF (LAItype(iv) < 0.5) THEN   !Original LAI type
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(3,iv)*GDD(id,2)*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ELSE
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
                !! Day length not used to start senescence in S hemisphere (not much land)
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)*LAIPower(3,iv)*(1-GDD(id,2))*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ENDIF
       ENDIF   !N or S hemisphere

       ! Check LAI within limits; if not set to limiting value
       IF(LAI(id,iv).GT.LAImax(iv))THEN
          LAI(id,iv)=LAImax(iv)
       ELSEIF(LAI(id,iv).LT.LAImin(iv))THEN
          LAI(id,iv)=LAImin(iv)
       ENDIF

    ENDDO   !End of loop over veg surfaces

    IF(LAICalcYes==0)THEN ! moved to SUEWS_cal_DailyState, TS 18 Sep 2017
       LAI(id-1,:)=LAI_obs ! check -- this is going to be a problem as it is not for each vegetation class
    ENDIF
    !------------------------------------------------------------------------------

  END SUBROUTINE update_GDDLAI


  SUBROUTINE update_WaterUse(&
       id,WaterUseMethod,DayofWeek_id,lat,Faut,HDD,&!input
       Ie_a,Ie_m,Ie_start,Ie_end,DayWatPer,DayWat,&
       WU_Day) !inout

    IMPLICIT NONE
    ! INTEGER,PARAMETER :: ndays=366

    INTEGER,INTENT(IN) :: id
    INTEGER,INTENT(IN) :: WaterUseMethod
    INTEGER,INTENT(IN)::Ie_start   !Starting time of water use (DOY)
    INTEGER,INTENT(IN)::Ie_end       !Ending time of water use (DOY)
    INTEGER,DIMENSION(3),INTENT(IN)::DayofWeek_id

    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::Faut          !Fraction of irrigated area using automatic irrigation

    REAL(KIND(1d0)),DIMENSION(-4:366,6),INTENT(IN)::HDD
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN)::Ie_a
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN)::Ie_m   !Coefficients for automatic and manual irrigation models
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::DayWatPer  !% of houses following daily water
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::DayWat       !Days of watering allowed

    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(INOUT):: WU_Day       !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

    INTEGER::wd        !Water use calculation is done when calc = 1
    INTEGER::&
         calc        !Water use calculation is done when calc = 1

    IF (WaterUseMethod==0) THEN   !If water use is to be modelled (rather than observed)

       wd=DayofWeek_id(1)

       IF (DayWat(wd)==1.0) THEN      !1 indicates watering permitted on this day
          calc=0
          IF (lat>=0) THEN            !Northern Hemisphere
             IF (id>=Ie_start-1.AND.id<=Ie_end+1) calc=1   !Day between irrigation period
          ELSE                        !Southern Hemisphere
             calc=1
             IF (id>=Ie_end.AND.id<=Ie_start) calc=0       !Day between irrigation period
          ENDIF

          IF(calc==1) THEN
             ! Model daily water use based on HDD(id,6)(days since rain) and HDD(id,3)(average temp)
             ! WU_Day is the amount of water [mm] per day, applied to each of the irrigated areas
             ! N.B. These are the same for each vegetation type at the moment

             ! ---- Automatic irrigation (evergreen trees) ----
             WU_day(id,2) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,2)<0) WU_Day(id,2)=0   !If modelled WU is negative -> 0

             ! ---- Manual irrigation (evergreen trees) ----
             WU_day(id,3) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,3)<0) WU_Day(id,3)=0   !If modelled WU is negative -> 0

             ! ---- Total evergreen trees water use (automatic + manual) ----
             WU_Day(id,1)=(WU_day(id,2)+WU_day(id,3))

             ! ---- Automatic irrigation (deciduous trees) ----
             WU_day(id,5) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,5)<0) WU_Day(id,5)=0   !If modelled WU is negative -> 0

             ! ---- Manual irrigation (deciduous trees) ----
             WU_day(id,6) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,6)<0) WU_Day(id,6)=0   !If modelled WU is negative -> 0

             ! ---- Total deciduous trees water use (automatic + manual) ----
             WU_Day(id,4)=(WU_day(id,5)+WU_day(id,6))

             ! ---- Automatic irrigation (grass) ----
             WU_day(id,8) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,8)<0) WU_Day(id,8)=0   !If modelled WU is negative -> 0

             ! ---- Manual irrigation (grass) ----
             WU_day(id,9) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,9)<0) WU_Day(id,9)=0   !If modelled WU is negative -> 0

             ! ---- Total grass water use (automatic + manual) ----
             WU_Day(id,7)=(WU_day(id,8)+WU_day(id,9))

          ELSE   !If no irrigation on this day
             WU_Day(id,1)=0
             WU_Day(id,2)=0
             WU_Day(id,3)=0
             WU_Day(id,4)=0
             WU_Day(id,5)=0
             WU_Day(id,6)=0
             WU_Day(id,7)=0
             WU_Day(id,8)=0
             WU_Day(id,9)=0
          ENDIF
       ENDIF
    ENDIF

  END SUBROUTINE update_WaterUse


  SUBROUTINE update_HDD(&
       id,it,imin,tstep,& !input
       HDD) !inout
    IMPLICIT NONE
    INTEGER,INTENT(IN)::id,it,imin,tstep

    REAL(KIND(1d0)),DIMENSION(-4:366,6),INTENT(INOUT):: HDD

    INTEGER:: jj
    REAL(KIND(1d0))::tstepcount

    ! count of timesteps performed during day `id`
    tstepcount=(it*60+imin)*60/tstep*1.
    ! Heating degree days (HDD) -------------
    HDD(id,1)=HDD(id,1)/tstepcount   !Heating
    HDD(id,2)=HDD(id,2)/tstepcount   !Cooling
    HDD(id,3)=HDD(id,3)/tstepcount   !Average temp

    ! Calculate 5-day running mean temp     !!Need to deal with the previous year - CHECK!!
    DO jj=1,5
       HDD(id,4)=HDD(id,4) + HDD(id-(jj-1),3)
    ENDDO
    HDD(id,4) = HDD(id,4)/5

    ! Calculate number of days since rain
    IF(HDD(id,5)>0) THEN        !Rain occurred
       HDD(id,6)=0
    ELSE
       HDD(id,6)=HDD(id-1,6)+1  !Days since rain
    ENDIF

  END SUBROUTINE update_HDD


  SUBROUTINE Cal_DailyStateStart(&
       id,iy,lat,&!input
       dayofWeek_id)!output
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: id
    INTEGER,INTENT(IN) ::iy
    REAL(KIND(1d0)),INTENT(IN) ::lat

    INTEGER,DIMENSION(3),INTENT(OUT) ::dayofWeek_id

    INTEGER::wd
    INTEGER::mb
    INTEGER::date
    INTEGER::seas

    CALL day2month(id,mb,date,seas,iy,lat) !Calculate real date from doy
    CALL Day_of_Week(date,mb,iy,wd)        !Calculate weekday (1=Sun, ..., 7=Sat)

    dayofWeek_id(1)=wd      !Day of week
    dayofWeek_id(2)=mb      !Month
    dayofweek_id(3)=seas    !Season

  END SUBROUTINE Cal_DailyStateStart

  SUBROUTINE SUEWS_update_DailyState(&
       iy,id,it,imin,dectime,&!input
       Gridiv,NumberOfGrids,nsh_real,&
       DailyStateLine,&
       dataOutDailyState)!inout

    IMPLICIT NONE

    INTEGER,INTENT(IN) ::iy
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::it
    INTEGER,INTENT(IN) ::imin
    REAL(KIND(1d0)),INTENT(IN)::dectime

  !
    REAL(KIND(1d0)),INTENT(IN) ::nsh_real

    INTEGER,INTENT(IN)::Gridiv
    INTEGER,INTENT(IN)::NumberOfGrids
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5),INTENT(IN) :: DailyStateLine
    ! INTEGER,DIMENSION(MaxNumberOfGrids),INTENT(IN):: GridIDmatrix         !Array containing GridIDs in SiteSelect after sorting

    ! CHARACTER (LEN = 20),INTENT(IN) :: FileCode       !Set in RunControl
    ! CHARACTER (LEN = 150),INTENT(IN):: FileOutputPath !Filepath for output files (set in RunControl)


    REAL(KIND(1d0)),DIMENSION(ndays,ncolumnsDataOutDailyState,NumberOfGrids),INTENT(INOUT):: dataOutDailyState


    ! write out to dataOutDailyState
    dataOutDailyState(id,1:4,Gridiv)=[iy,id,it,imin]
    dataOutDailyState(id,5,Gridiv)=dectime
    ! DailyStateLine will be -999 unless realistic values are calculated at the last timestep of each day
    dataOutDailyState(id,6:ncolumnsDataOutDailyState,Gridiv)=DailyStateLine

  END SUBROUTINE SUEWS_update_DailyState


  ! transfer results to a one-line output for SUEWS_cal_DailyState
  SUBROUTINE update_DailyState(&
       iy,id,it,imin,nsh_real,&!input
       GDD,HDD,LAI,&
       DecidCap,albDecTr,albEveTr,albGrass,porosity,&
       WU_Day,&
       deltaLAI,VegPhenLumps,&
       SnowAlb,SnowDens,&
       a1,a2,a3,&
       DailyStateLine)!out

    IMPLICIT NONE
    ! INTEGER,PARAMETER::ndays=366
    ! INTEGER,PARAMETER::nvegsurf=3

    INTEGER,INTENT(IN) ::iy
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::it
    INTEGER,INTENT(IN) ::imin
    REAL(KIND(1d0)),INTENT(IN) ::nsh_real

    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(IN):: GDD          !Growing Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(IN):: HDD          !Heating Degree Days (see SUEWS_DailyState.f95)
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(IN):: LAI   !LAI for each veg surface [m2 m-2]

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::porosity
    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(IN):: WU_Day !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

    REAL(KIND(1d0)),INTENT(IN) ::deltaLAI
    REAL(KIND(1d0)),INTENT(IN) ::VegPhenLumps
    REAL(KIND(1d0)),INTENT(IN) ::SnowAlb
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::SnowDens
    REAL(KIND(1d0)),INTENT(IN) ::a1
    REAL(KIND(1d0)),INTENT(IN) ::a2
    REAL(KIND(1d0)),INTENT(IN) ::a3

    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5),INTENT(OUT) :: DailyStateLine

    ! initialise DailyStateLine
    DailyStateLine=-999
    IF (it==23 .AND. imin==(nsh_real-1)/nsh_real*60) THEN
       ! Write actual data only at the last timesstep of each day
       ! DailyStateLine(1:2)   = [iy,id]
       DailyStateLine(1:6)   = HDD(id,1:6)
       DailyStateLine(6+1:6+5)  = GDD(id,1:5)
       DailyStateLine(11+1:11+3) = LAI(id,1:nvegsurf)
       DailyStateLine(14+1:14+5) = [DecidCap(id),Porosity(id),AlbEveTr(id),AlbDecTr(id),AlbGrass(id)]
       DailyStateLine(19+1:19+9) = WU_day(id-1,1:9)
       DailyStateLine(28+1)    = deltaLAI
       DailyStateLine(29+1)    = VegPhenLumps
       DailyStateLine(30+1:30+8) = [SnowAlb,SnowDens(1:7)]
       DailyStateLine(38+1:38+3) = [a1,a2,a3]

    END IF


  END SUBROUTINE update_DailyState


END MODULE DailyState_module
