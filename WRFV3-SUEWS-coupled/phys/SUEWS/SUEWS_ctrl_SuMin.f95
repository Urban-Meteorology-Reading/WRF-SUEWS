!========================================================================================
! a mini version of SUEWS to be coupled with WRF
! TS 22 Apr 2018: initial
! TS 11 Jun 2018: modified according to recent SUEWS development


MODULE SuMin_Module
  USE SUEWS_Driver,ONLY:SUEWS_cal_Main,ndays,nsurf,nvegsurf,&
  PavSurf,BldgSurf,ConifSurf,DecidSurf,GrassSurf,BSoilSurf,WaterSurf,&
  ivConif,ivDecid,ivGrass,&
  ncolumnsDataOutSUEWS,ncolumnsDataOutSnow,&
  ncolumnsDataOutESTM,ncolumnsDataOutDailyState

  IMPLICIT NONE

CONTAINS

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
       qh,qe,qsfc,tsk)!output

    INTEGER::AerodynamicResistanceMethod
    INTEGER::Diagnose
    INTEGER::DiagQN
    INTEGER::DiagQS
    INTEGER,INTENT(IN)::startDLS
    INTEGER,INTENT(IN)::endDLS
    INTEGER::EmissionsMethod
    INTEGER::Gridiv
    INTEGER,PARAMETER::gsModel=2
    INTEGER,INTENT(IN)::id
    INTEGER::id_prev_t
    INTEGER::Ie_end
    INTEGER::Ie_start
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
    INTEGER,PARAMETER::veg_type=1
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
    REAL(KIND(1D0))::tau_a
    REAL(KIND(1D0))::tau_f
    REAL(KIND(1D0))::tau_r
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
    REAL(KIND(1D0)),DIMENSION(3) ::Ie_a
    REAL(KIND(1D0)),DIMENSION(3) ::Ie_m
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
    REAL(KIND(1D0)),DIMENSION(NSURF),PARAMETER           ::SoilDepth=0.2
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
    REAL(KIND(1D0)) ::SnowAlb
    REAL(KIND(1D0)) ::z0m_in
    REAL(KIND(1D0)) ::zdm_in
    ! INTEGER,DIMENSION(0:NDAYS,3),INTENT(INOUT)                ::DayofWeek
    REAL(KIND(1d0)),DIMENSION(24*3600/tstep)  ::Tair24HR
    ! REAL(KIND(1D0)),DIMENSION(2*3600/tstep+1),INTENT(INOUT)   ::qn1_av_store
    REAL(KIND(1D0)),DIMENSION(2*360+1),INTENT(INOUT)   ::qn1_av_store !NB:reduced size
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
    ! REAL(KIND(1D0)),DIMENSION(3600/tstep),INTENT(INOUT)       ::qn1_store
    REAL(KIND(1D0)),DIMENSION(360),INTENT(INOUT)       ::qn1_store ! NB: reduced size

    REAL(KIND(1D0)),DIMENSION(5)                           ::datetimeLine
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSUEWS-5)      ::dataOutLineSUEWS
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSnow-5)       ::dataOutLineSnow
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5)       ::dataOutLineESTM
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5) ::DailyStateLine

    REAL(KIND(1D0)),INTENT(out)::qh


    INTEGER,DIMENSION(NSURF)::snowCalcSwitch
    REAL(KIND(1D0)),INTENT(out)::qe
    REAL(KIND(1D0)),INTENT(out)::qsfc
    REAL(KIND(1D0)),INTENT(out)::tsk


    Diagnose=0
    snowCalcSwitch=0
    snowUse=0
    DiagQN=0
    DiagQS=0
    WaterUseMethod=1 ! use observed, don't model it
    ity=2
    LAICalcYes=1
    RoughLenHeatMethod=2
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
    z0m_in=0.05
    zdm_in=0.1

    CALL SUEWS_cal_Main(&
         AerodynamicResistanceMethod,AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,& ! input&inout in alphabetical order
         AH_SLOPE_Heating,alb,albDecTr,albEveTr,albGrass,AlbMax_DecTr,&
         AlbMax_EveTr,AlbMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
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
         WUProfM_tstep,xsmd,Z,z0m_in,zdm_in,&
         datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&!output
         DailyStateLine)!output

    qh=dataOutLineSUEWS(9)
    qe=dataOutLineSUEWS(10)
    qsfc=dataOutLineSUEWS(16)
    tsk=dataOutLineSUEWS(5)

    print*,''
    print*, 'avkdn,kup,ldown,lup,tsurf'
    print*, dataOutLineSUEWS(1:5)
    print*, 'qn1,qf,qs,qh,qe'
    print*, dataOutLineSUEWS(6:10)
    print*,''
    if ( abs(qe)>1000 ) then
      zdm_in=0.
      print*, 10./zdm_in
    end if

  END SUBROUTINE SuMin


END MODULE SuMin_Module