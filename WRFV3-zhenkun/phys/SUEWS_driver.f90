





MODULE SUEWS_Driver
  USE AtmMoist_module,ONLY:LUMPS_cal_AtmMoist,qsatf
  USE NARP_MODULE,ONLY:NARP_cal_SunPosition,RadMethod,NARP
  USE AnOHM_module,ONLY:AnOHM
  USE ESTM_module,ONLY:ESTM
  USE Snow_module,ONLY:SnowCalc,Snow_cal_MeltHeat
  USE DailyState_module,ONLY:SUEWS_cal_DailyState,update_DailyState
  USE WaterDist_module,ONLY:&
       drainage,soilstore,SUEWS_cal_SoilMoist,&
       SUEWS_update_SoilMoist,ReDistributeWater


  IMPLICIT NONE
  INTEGER, PARAMETER:: ndays = 366   
  
  INTEGER, PARAMETER:: nsurf=7                
  INTEGER, PARAMETER:: NVegSurf=3             
  INTEGER, PARAMETER:: nsurfIncSnow=nsurf+1   

  INTEGER,PARAMETER:: PavSurf   = 1,&   
       BldgSurf  = 2,&
       ConifSurf = 3,&
       DecidSurf = 4,&
       GrassSurf = 5,&   
       BSoilSurf = 6,&   
       WaterSurf = 7,&
       ExcessSurf= 8,&   
       NSurfDoNotReceiveDrainage=0,&   
       ivConif = 1,&     
       ivDecid = 2,&
       ivGrass = 3

  
  INTEGER, PARAMETER:: ncolumnsDataOutSUEWS=84,&    
       ncolumnsDataOutSnow=102,&
       ncolumnsdataOutSOL=31,&
       ncolumnsdataOutBL=22,&
       ncolumnsDataOutESTM=32,&
       ncolumnsDataOutDailyState=46



CONTAINS
  
  SUBROUTINE SUEWS_cal_Main(&
       AerodynamicResistanceMethod,AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,& 
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
       datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&
       DailyStateLine)

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
    
    REAL(KIND(1d0)),DIMENSION(24*3600/tstep),INTENT(inout)    ::Tair24HR
    
    REAL(KIND(1D0)),DIMENSION(2*360+1),INTENT(INOUT)   ::qn1_av_store
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
    
    REAL(KIND(1D0)),DIMENSION(360),INTENT(INOUT)       ::qn1_store

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

    
    
    
    REAL(KIND(1D0))::addImpervious=0
    REAL(KIND(1D0))::addPipes=0
    REAL(KIND(1D0))::addVeg=0
    REAL(KIND(1D0))::addWaterBody=0
    REAL(KIND(1D0)),DIMENSION(NSURF)::AddWater=0
    REAL(KIND(1D0)),DIMENSION(NSURF)::AddWaterRunoff=0

    
    INTEGER::nsh 
    REAL(KIND(1D0))::nsh_real 
    REAL(KIND(1D0))::tstep_real 

    
    REAL(KIND(1D0))::VegFraction
    REAL(KIND(1D0))::ImpervFraction
    REAL(KIND(1D0))::PervFraction
    REAL(KIND(1D0))::NonWaterFraction

    
    CALL SUEWS_cal_tstep(&
         tstep,& 
         nsh, nsh_real, tstep_real) 

    
    CALL SUEWS_cal_surf(&
         sfr,& 
         VegFraction,ImpervFraction,PervFraction,NonWaterFraction) 

    
    CALL SUEWS_cal_weekday(&
         iy,id,lat,& 
         dayofWeek_id) 

    
    CALL SUEWS_cal_DLS(&
         id,startDLS,endDLS,& 
         DLS) 


    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_RoughnessParameters...'
    
    CALL SUEWS_cal_RoughnessParameters(&
         RoughLenMomMethod,sfr,&
         bldgH,EveTreeH,DecTreeH,&
         porosity(id),FAIBldg,FAIEveTree,FAIDecTree,Z,&
         planF,&
         Zh,Z0m,Zdm,ZZD)

    
    IF(Diagnose==1) WRITE(*,*) 'Calling NARP_cal_SunPosition...'
    CALL NARP_cal_SunPosition(&
         REAL(iy,KIND(1d0)),&
         dectime-tstep/2,&
         timezone,lat,lng,alt,&
         azimuth,zenith_deg)


    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_DailyState...'
    CALL SUEWS_cal_DailyState(&
         iy,id,it,imin,tstep,DayofWeek_id,&
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
         SnowAlb,DecidCap,albDecTr,albEveTr,albGrass,&
         porosity,GDD,HDD,SnowDens,LAI,WU_Day,&
         deltaLAI)


    
    IF(Diagnose==1) WRITE(*,*) 'Calling LUMPS_cal_AtmMoist...'
    CALL LUMPS_cal_AtmMoist(&
         Temp_C,Press_hPa,avRh,dectime,&
         lv_J_kg,lvS_J_kg,&
         es_hPa,Ea_hPa,VPd_hpa,VPD_Pa,dq,dens_dry,avcp,avdens)


    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_update_SoilMoist...'
    CALL SUEWS_update_SoilMoist(&
         NonWaterFraction,&
         soilstoreCap,sfr,soilmoist,&
         SoilMoistCap,SoilState,&
         vsmd,smd)


    
    CALL SUEWS_cal_Qn(&
         NetRadiationMethod,snowUse,id,&
         Diagnose,snow_obs,ldown_obs,fcld_obs,&
         dectime,ZENITH_deg,avKdn,Temp_C,avRH,Ea_hPa,qn1_obs,&
         SnowAlb,DiagQN,&
         NARP_TRANS_SITE,NARP_EMIS_SNOW,IceFrac,sfr,emis,&
         alb,albDecTr,DecidCap,albEveTr,albGrass,surf,&
         snowFrac,ldown,fcld,&
         qn1,qn1_SF,qn1_S,kclear,kup,lup,tsurf,&
         qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)


    
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


    
    CALL SUEWS_cal_Qs(&
         StorageHeatMethod,OHMIncQF,Gridiv,&
         id,tstep,Diagnose,sfr,&
         OHM_coef,OHM_threshSW,OHM_threshWD,&
         soilmoist,soilstoreCap,state,nsh,SnowUse,DiagQS,&
         HDD,MetForcingData_grid,Ts5mindata_ir,qf,qn1,&
         avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
         bldgh,alb,emis,cpAnOHM,kkAnOHM,chAnOHM,EmissionsMethod,&
         Tair24HR,qn1_store,qn1_S_store,&
         qn1_av_store,qn1_S_av_store,surf,&
         qn1_S,snowFrac,dataOutLineESTM,qs,&
         deltaQi,a1,a2,a3)


    
    IF(Diagnose==1) WRITE(*,*) 'Calling MeltHeat'
    CALL Snow_cal_MeltHeat(&
         snowUse,&
         lvS_J_kg,lv_J_kg,tstep_real,RadMeltFact,TempMeltFact,SnowAlbMax,&
         SnowDensMin,Temp_C,Precip,PrecipLimit,PrecipLimitAlb,&
         nsh_real,sfr,Tsurf_ind,Tsurf_ind_snow,state,qn1_ind_snow,&
         kup_ind_snow,Meltwaterstore,deltaQi,&
         SnowPack,snowFrac,SnowAlb,SnowDens,SnowfallCum,&
         mwh,fwh,Qm,QmFreez,QmRain,&
         veg_fr,snowCalcSwitch,Qm_melt,Qm_freezState,Qm_rain,FreezMelt,&
         FreezState,FreezStateVol,rainOnSnow,SnowDepth,mw_ind,&
         dataOutLineSnow)



    
    IF(Diagnose==1) WRITE(*,*) 'Calling LUMPS_cal_QHQE...'
    
    CALL LUMPS_cal_QHQE(&
         veg_type,& 
         snowUse,id,qn1,qf,qs,Qm,Temp_C,Veg_Fr,avcp,Press_hPa,lv_J_kg,&
         tstep_real,DRAINRT,nsh_real,&
         Precip,RainMaxRes,RAINCOVER,sfr,LAI,LAImax,LAImin,&
         H_mod,E_mod,psyc_hPa,s_hPa,sIce_hpa,TempVeg,VegPhenLumps)


    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_WaterUse...'
    
    CALL SUEWS_cal_WaterUse(&
         nsh_real,& 
         SurfaceArea,sfr,&
         IrrFracConif,IrrFracDecid,IrrFracGrass,&
         dayofWeek_id,WUProfA_tstep,WUProfM_tstep,&
         InternalWaterUse_h,HDD(id-1,:),WU_Day(id-1,:),&
         WaterUseMethod,NSH,it,imin,DLS,&
         WUAreaEveTr_m2,WUAreaDecTr_m2,& 
         WUAreaGrass_m2,WUAreaTotal_m2,&
         wu_EveTr,wu_DecTr,wu_Grass,wu_m3,int_wu,ext_wu)


    
    CALL SUEWS_cal_Resistance(&
         StabilityMethod,&
         Diagnose,AerodynamicResistanceMethod,RoughLenHeatMethod,snowUse,&
         id,it,gsModel,SMDMethod,&
         qh_obs,avdens,avcp,h_mod,qn1,dectime,zzd,z0M,zdm,&
         avU1,Temp_C,UStar,VegFraction,avkdn,&
         Kmax,&
         g1,g2,g3,g4,&
         g5,g6,s1,s2,&
         th,tl,&
         dq,xsmd,vsmd,MaxConductance,LAIMax,LAI(id-1,:),snowFrac,sfr,&
         Tstar,L_mod,&
         psim,gsc,ResistSurf,RA,RAsnow,rb)


    
    CALL SUEWS_cal_Water(&
         Diagnose,&
         snowUse,NonWaterFraction,addPipes,addImpervious,addVeg,addWaterBody,&
         state,soilmoist,sfr,surf,WaterDist,nsh_real,&
         drain_per_tstep,&  
         drain,AddWaterRunoff,&
         AdditionalWater,runoffPipes,runoff_per_interval,&
         AddWater,stateOld,soilmoistOld)
    

    
    CALL SUEWS_cal_QE(&
         Diagnose,&
         id,tstep,imin,it,ity,snowCalcSwitch,DayofWeek_id,CRWmin,CRWmax,&
         nsh_real,dectime,lvS_J_kg,lv_j_kg,avdens,avRh,Press_hPa,Temp_C,&
         RAsnow,psyc_hPa,avcp,sIce_hPa,&
         PervFraction,vegfraction,addimpervious,qn1_SF,qf,qs,vpd_hPa,s_hPa,&
         ResistSurf,ra,rb,tstep_real,snowdensmin,precip,PipeCapacity,RunoffToWater,&
         NonWaterFraction,wu_EveTr,wu_DecTr,wu_Grass,addVeg,addWaterBody,SnowLimPaved,SnowLimBuild,&
         SurfaceArea,FlowChange,drain,WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
         freezmelt,freezstate,freezstatevol,Qm_Melt,Qm_rain,Tsurf_ind,sfr,&
         StateLimit,AddWater,addwaterrunoff,surf,snowD,&
         runoff_per_interval,state,soilmoist,SnowPack,snowFrac,MeltWaterStore,&
         iceFrac,SnowDens,&
         snowProf,& 
         runoffSnow,runoff,runoffSoil,chang,changSnow,&
         snowDepth,SnowToSurf,ev_snow,SnowRemoval,&
         evap,rss_nsurf,p_mm,rss,qe,state_per_tstep,NWstate_per_tstep,qeOut,&
         swe,ev,chSnow_per_interval,ev_per_tstep,qe_per_tstep,runoff_per_tstep,&
         surf_chang_per_tstep,runoffPipes,mwstore,runoffwaterbody,&
         runoffAGveg,runoffAGimpervious,runoffWaterBody_m3,runoffPipes_m3)
    

    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_QH...'
    CALL SUEWS_cal_QH(&
         1,&
         qn1,qf,QmRain,qeOut,qs,QmFreez,qm,avdens,avcp,tsurf,Temp_C,ra,&
         qh,qh_r)
    

    
    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_HorizontalSoilWater...'
    CALL SUEWS_cal_HorizontalSoilWater(&
         sfr,&
         SoilStoreCap,&
         SoilDepth,&
         SatHydraulicConduct,&
         SurfaceArea,&
         NonWaterFraction,&
         tstep_real,& 
         SoilMoist,&
         runoffSoil,&
         runoffSoil_per_tstep&
         )

    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_SoilMoist...'
    CALL SUEWS_cal_SoilMoist(&
         SMDMethod,xsmd,NonWaterFraction,SoilMoistCap,&
         SoilStoreCap,surf_chang_per_tstep,&
         soilmoist,soilmoistOld,sfr,&
         smd,smd_nsurf,tot_chang_per_tstep,SoilState)


    
    IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_cal_Diagnostics...'
    CALL SUEWS_cal_Diagnostics(&
         tsurf,qh,&
         Press_hPa,qe,&
         UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real,&
         RoughLenHeatMethod,StabilityMethod,&
         avU10_ms,t2_C,q2_gkg)
    


    

    
    CALL SUEWS_update_outputLine(&
         AdditionalWater,alb,avkdn,avU10_ms,azimuth,&
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
         datetimeLine,dataOutLineSUEWS)

    

    
    CALL update_DailyState(&
         iy,id,it,imin,nsh_real,&
         GDD,HDD,LAI,&
         DecidCap,albDecTr,albEveTr,albGrass,porosity,&
         WU_Day,&
         deltaLAI,VegPhenLumps,&
         SnowAlb,SnowDens,&
         a1,a2,a3,&
         DailyStateLine)

    

  END SUBROUTINE SUEWS_cal_Main
  

  
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

    QF=0
    QF_SAHP=0
    Fc_anthro=0
    Fc_metab=0
    Fc_traff=0
    Fc_build=0


    
    

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

       
       
    ELSE
       CALL ErrorHint(73,'RunControl.nml:EmissionsMethod unusable',notUsed,notUsed,EmissionsMethod)
    ENDIF
    
    
    IF(EmissionsMethod>=1) qf = QF_SAHP

    IF(EmissionsMethod>=11) THEN
       
       IF(Diagnose==1) WRITE(*,*) 'Calling CO2_biogen...'
       CALL CO2_biogen(EmissionsMethod,id,ndays,ivConif,ivDecid,ivGrass,ConifSurf,DecidSurf,GrassSurf,BSoilSurf,&
            snowFrac,nsurf,NVegSurf,avkdn,Temp_C,sfr,LAI,LaiMin,LaiMax,&
            alpha_bioCO2,beta_bioCO2,theta_bioCO2,alpha_enh_bioCO2,beta_enh_bioCO2,&
            resp_a,resp_b,min_res_bioCO2,Fc_biogen,Fc_respi,Fc_photo,&
            notUsed,notUsedI)
    ENDIF
    
    Fc = Fc_anthro + Fc_biogen

    

  END SUBROUTINE SUEWS_cal_AnthropogenicEmission
  

  
  SUBROUTINE SUEWS_cal_Qn(&
       NetRadiationMethod,snowUse,id,&
       Diagnose,snow_obs,ldown_obs,fcld_obs,&
       dectime,ZENITH_deg,avKdn,Temp_C,avRH,ea_hPa,qn1_obs,&
       SnowAlb,DiagQN,&
       NARP_TRANS_SITE,NARP_EMIS_SNOW,IceFrac,sfr,emis,&
       alb,albDecTr,DecidCap,albEveTr,albGrass,surf,&
       snowFrac,ldown,fcld,&
       qn1,qn1_SF,qn1_S,kclear,kup,lup,tsurf,&
       qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)


    IMPLICIT NONE
    
    
    
    

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
         NetRadiationMethod,&
         snowUse,&
         NetRadiationMethodX,AlbedoChoice,ldown_option)

    IF(NetRadiationMethodX>0)THEN

       
       IF (snowUse==0) snowFrac=0

       IF(ldown_option==1) THEN 
          ldown=ldown_obs
       ELSE
          ldown=-9              
       ENDIF

       IF(ldown_option==2) THEN 
          fcld=fcld_obs
       ENDIF



       
       alb(DecidSurf)    = albDecTr(id) 
       surf(6,DecidSurf) = DecidCap(id) 
       
       alb(ConifSurf) = albEveTr(id)
       alb(GrassSurf) = albGrass(id)

       IF(Diagnose==1) WRITE(*,*) 'Calling NARP...'


       CALL NARP(&
            nsurf,sfr,snowFrac,alb,emis,IceFrac,&
            NARP_TRANS_SITE,NARP_EMIS_SNOW,&
            dectime,ZENITH_deg,avKdn,Temp_C,avRH,ea_hPa,qn1_obs,&
            SnowAlb,&
            AlbedoChoice,ldown_option,NetRadiationMethodX,DiagQN,&
            qn1,qn1_SF,qn1_S,kclear,kup,LDown,lup,fcld,tsurf,&
            qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)

    ELSE 
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
  

  
  SUBROUTINE SUEWS_cal_Qs(&
       StorageHeatMethod,OHMIncQF,Gridiv,&
       id,tstep,Diagnose,sfr,&
       OHM_coef,OHM_threshSW,OHM_threshWD,&
       soilmoist,soilstoreCap,state,nsh,SnowUse,DiagQS,&
       HDD,MetForcingData_grid,Ts5mindata_ir,qf,qn1,&
       avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
       bldgh,alb,emis,cpAnOHM,kkAnOHM,chAnOHM,EmissionsMethod,&
       Tair24HR,qn1_store,qn1_S_store,&
       qn1_av_store,qn1_S_av_store,surf,&
       qn1_S,snowFrac,dataOutLineESTM,qs,&
       deltaQi,a1,a2,a3)

    IMPLICIT NONE
    INTEGER,INTENT(in)::StorageHeatMethod
    INTEGER,INTENT(in)::OHMIncQF
    INTEGER,INTENT(in)::Gridiv
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::tstep
    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::nsh                
    INTEGER,INTENT(in)::SnowUse            
    INTEGER,INTENT(in)::DiagQS             
    INTEGER,INTENT(in):: EmissionsMethod 


    REAL(KIND(1d0)),INTENT(in)::OHM_coef(nsurf+1,4,3)                 
    REAL(KIND(1d0)),INTENT(in)::OHM_threshSW(nsurf+1) 
    REAL(KIND(1d0)),INTENT(in)::OHM_threshWD(nsurf+1) 
    REAL(KIND(1d0)),INTENT(in)::soilmoist(nsurf)                
    REAL(KIND(1d0)),INTENT(in)::soilstoreCap(nsurf)             
    REAL(KIND(1d0)),INTENT(in)::state(nsurf) 


    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(in)::HDD
    REAL(KIND(1d0)),INTENT(in)::qf
    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown
    REAL(KIND(1d0)),INTENT(in)::bldgh

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::alb  
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::emis 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::cpAnOHM   
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::kkAnOHM   
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::chAnOHM   

    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::MetForcingData_grid 

    REAL(KIND(1d0)),DIMENSION(:),INTENT(in)::Ts5mindata_ir

    REAL(KIND(1d0)),DIMENSION(24*nsh),INTENT(inout):: Tair24HR
    
    REAL(KIND(1d0)),DIMENSION(360),INTENT(inout) ::qn1_store
    REAL(KIND(1d0)),DIMENSION(nsh),INTENT(inout) ::qn1_S_store 

    
    REAL(KIND(1d0)),DIMENSION(2*360+1),INTENT(inout)::qn1_av_store
    REAL(KIND(1d0)),DIMENSION(2*nsh+1),INTENT(inout)::qn1_S_av_store 
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(inout)::surf
    

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::deltaQi 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::snowFrac

    REAL(KIND(1d0)),DIMENSION(27),INTENT(out):: dataOutLineESTM
    REAL(KIND(1d0)),INTENT(out)::qn1_S
    REAL(KIND(1d0)),INTENT(out):: qs 
    REAL(KIND(1d0)),INTENT(out):: a1 
    REAL(KIND(1d0)),INTENT(out):: a2 
    REAL(KIND(1d0)),INTENT(out):: a3 


    REAL(KIND(1d0))::HDDday 
    REAL(KIND(1d0))::qn1_use 

    
    deltaQi=0
    snowFrac=0
    qn1_S=0
    dataOutLineESTM=-999
    qs=-999
    a1=-999
    a2=-999
    a3=-999


    
    IF(OHMIncQF == 1) THEN
       qn1_use= qf+qn1
    ELSEIF(OHMIncQF == 0) THEN
       qn1_use= qn1
    ENDIF

    IF(StorageHeatMethod==1) THEN           
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

    
    IF (StorageHeatMethod==3) THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AnOHM...'
       CALL AnOHM(qn1_use,qn1_store,qn1_av_store,qf,&
            MetForcingData_grid,state/surf(6,:),&
            alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
            sfr,nsurf,nsh,EmissionsMethod,id,Gridiv,&
            a1,a2,a3,qs,deltaQi)

    END IF


    
    IF(StorageHeatMethod==4 .OR. StorageHeatMethod==14) THEN
       
       IF(Diagnose==1) WRITE(*,*) 'Calling ESTM...'
       CALL ESTM(&
            Gridiv,&
            nsh,tstep,&
            avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
            bldgh,Ts5mindata_ir,&
            Tair24HR,&
            dataOutLineESTM,QS)
       
       
    ENDIF

  END SUBROUTINE SUEWS_cal_Qs
  

  
  SUBROUTINE SUEWS_cal_Water(&
       Diagnose,&
       snowUse,NonWaterFraction,addPipes,addImpervious,addVeg,addWaterBody,&
       state,soilmoist,sfr,surf,WaterDist,nsh_real,&
       drain_per_tstep,&  
       drain,AddWaterRunoff,&
       AdditionalWater,runoffPipes,runoff_per_interval,&
       AddWater,stateOld,soilmoistOld)

    IMPLICIT NONE
    
    
    INTEGER,INTENT(in) ::Diagnose
    INTEGER,INTENT(in) ::snowUse

    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in)::addPipes
    REAL(KIND(1d0)),INTENT(in)::addImpervious
    REAL(KIND(1d0)),INTENT(in)::addVeg
    REAL(KIND(1d0)),INTENT(in)::addWaterBody
    REAL(KIND(1d0)),INTENT(in)::nsh_real 

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)          ::sfr
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(in)        ::surf
    REAL(KIND(1d0)),DIMENSION(nsurf+1,nsurf-1),INTENT(in)::WaterDist

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: drain         
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: AddWaterRunoff
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: AddWater      
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: stateOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: soilmoistOld

    REAL(KIND(1d0)),INTENT(out)::drain_per_tstep
    REAL(KIND(1d0)),INTENT(out)::AdditionalWater
    REAL(KIND(1d0)),INTENT(out)::runoffPipes
    REAL(KIND(1d0)),INTENT(out)::runoff_per_interval
    INTEGER:: is

    
    stateOld     = state     
    soilmoistOld = soilmoist 


    
    
    
    

    
    AdditionalWater = addPipes+addImpervious+addVeg+addWaterBody  

    
    runoffPipes         = addPipes 
    
    runoff_per_interval = addPipes 


    
    
    IF(Diagnose==1) WRITE(*,*) 'Calling Drainage...'

    IF (NonWaterFraction/=0) THEN 
       DO is=1,nsurf-1

          CALL drainage(&
               is,&
               state(is),&
               surf(6,is),&
               surf(2,is),&
               surf(3,is),&
               surf(4,is),&
               nsh_real,&
               drain(is))

          
          
       ENDDO
       drain_per_tstep=DOT_PRODUCT(drain(1:nsurf-1),sfr(1:nsurf-1))/NonWaterFraction 
    ELSE
       drain(1:nsurf-1)=0
       drain_per_tstep=0
    ENDIF

    drain(WaterSurf) = 0  

    
    IF(Diagnose==1) WRITE(*,*) 'Calling ReDistributeWater...'
    
    
    CALL ReDistributeWater(&
         nsurf,& 
         WaterSurf, snowUse, WaterDist,  sfr,   Drain,&
         AddWaterRunoff,&  
         AddWater)

  END SUBROUTINE SUEWS_cal_Water
  

  
  SUBROUTINE SUEWS_init_QH(&
       qh_obs,avdens,avcp,h_mod,qn1,dectime,&
       H_init)

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


    IF(qh_obs/=NAN) THEN   
       H_init=qh_obs/(avdens*avcp)  
    ELSE
       IF(h_mod/=NAN) THEN
          H_init = h_mod/(avdens*avcp)   
       ELSE
          H_init=(qn1*0.2)/(avdens*avcp)   
          CALL ErrorHint(38,'LUMPS unable to calculate realistic value for H_mod.',h_mod, dectime, notUsedI)
       ENDIF
    ENDIF

  END SUBROUTINE SUEWS_init_QH
  

  
  
  SUBROUTINE SUEWS_cal_QE(&
       Diagnose,&
       id,tstep,imin,it,ity,snowCalcSwitch,dayofWeek_id,CRWmin,CRWmax,&
       nsh_real,dectime,lvS_J_kg,lv_j_kg,avdens,avRh,Press_hPa,Temp_C,&
       RAsnow,psyc_hPa,avcp,sIce_hPa,&
       PervFraction,vegfraction,addimpervious,qn1_SF,qf,qs,vpd_hPa,s_hPa,&
       ResistSurf,ra,rb,tstep_real,snowdensmin,precip,PipeCapacity,RunoffToWater,&
       NonWaterFraction,wu_EveTr,wu_DecTr,wu_Grass,addVeg,addWaterBody,SnowLimPaved,SnowLimBuild,&
       SurfaceArea,FlowChange,drain,WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
       freezmelt,freezstate,freezstatevol,Qm_Melt,Qm_rain,Tsurf_ind,sfr,&
       StateLimit,AddWater,addwaterrunoff,surf,snowD,&
       runoff_per_interval,state,soilmoist,SnowPack,snowFrac,MeltWaterStore,&
       iceFrac,SnowDens,&
       snowProf,& 
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
    INTEGER,INTENT(in) ::ity 
    

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
    REAL(KIND(1d0)),INTENT(in)::wu_EveTr
    REAL(KIND(1d0)),INTENT(in)::wu_DecTr
    REAL(KIND(1d0)),INTENT(in)::wu_Grass
    REAL(KIND(1d0)),INTENT(in)::addVeg
    REAL(KIND(1d0)),INTENT(in)::addWaterBody
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
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::StateLimit 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWater
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::addwaterrunoff
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(in)::surf
    REAL(KIND(1d0)), DIMENSION(0:23,2),INTENT(in):: snowProf

    
    REAL(KIND(1d0)),INTENT(inout)::runoff_per_interval

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowPack
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::snowFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::MeltWaterStore

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::iceFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens
    REAL(KIND(1d0)),DIMENSION(2)    ::SurplusEvap        


    
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoffSnow 
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

    REAL(KIND(1d0)),INTENT(out)::p_mm
    REAL(KIND(1d0)),INTENT(out)::rss
    REAL(KIND(1d0)),INTENT(out)::qe 
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


    
    INTEGER:: is

    REAL(KIND(1d0))::surplusWaterBody
    REAL(KIND(1d0))::pin
    REAL(KIND(1d0))::sae
    REAL(KIND(1d0))::vdrc
    REAL(KIND(1d0))::sp
    REAL(KIND(1d0))::numPM
    REAL(KIND(1d0))::tlv
    REAL(KIND(1d0))::runoffAGimpervious_m3
    REAL(KIND(1d0))::runoffAGveg_m3


    tlv=lv_J_kg/tstep_real 

    pin=MAX(0.,Precip)


    
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

    
    sae   = s_hPa*(qn1_SF+qf-qs)    
    vdrc  = vpd_hPa*avdens*avcp
    sp    = s_hPa/psyc_hPa
    numPM = sae+vdrc/ra
    
    

    IF(Diagnose==1) WRITE(*,*) 'Calling evap_SUEWS and SoilStore...'
    DO is=1,nsurf   
       IF (snowCalcSwitch(is)==1) THEN
          IF (sfr(is)/=0) THEN
             IF(Diagnose==1) WRITE(*,*) 'Calling SnowCalc...'
             CALL SnowCalc(&
                  id,& 
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
                  SnowPack,SurplusEvap,&
                  snowFrac,MeltWaterStore,iceFrac,SnowDens,&
                  runoffSnow,& 
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

          
          CALL Evap_SUEWS(&
               ity,&
               state(is),& 
               WetThresh(is),&
               surf(6,is),& 
               numPM,&
               s_hPa,&
               psyc_hPa,&
               ResistSurf,&
               sp,&
               ra,&
               rb,&
               tlv,&
               rss,&
               ev,&
               qe) 


          rss_nsurf(is) = rss 
          
          
          CALL soilstore(&
               is,& 
               sfr,&
               PipeCapacity,&
               RunoffToWater,&
               pin,&
               wu_EveTr,&
               wu_DecTr,&
               wu_Grass,&
               drain,&
               AddWater,&
               addImpervious,&
               nsh_real,&
               stateOld,&
               AddWaterRunoff,&
               PervFraction,&
               addVeg,&
               soilstoreCap,&
               addWaterBody,&
               FlowChange,&
               StateLimit,&
               runoffAGimpervious,&
               surplusWaterBody,&
               runoffAGveg,&
               runoffPipes,&
               ev,&
               soilmoist,&
               SurplusEvap,&
               runoffWaterBody,&
               runoff_per_interval,&
               p_mm,&
               chang,&
               runoff,&
               state&
               )

          evap(is)     = ev 

          
          ev_per_tstep = ev_per_tstep+evap(is)*sfr(is)

          
          surf_chang_per_tstep = surf_chang_per_tstep+(state(is)-stateOld(is))*sfr(is)
          
          runoff_per_tstep     = runoff_per_tstep+runoff(is)*sfr(is)
          
          state_per_tstep      = state_per_tstep+(state(is)*sfr(is))
          

          IF (NonWaterFraction/=0 .AND. is.NE.WaterSurf) THEN
             NWstate_per_tstep=NWstate_per_tstep+(state(is)*sfr(is)/NonWaterFraction)
          ENDIF

          ChangSnow(is)  = 0
          runoffSnow(is) = 0

       ENDIF
    ENDDO  


    
    qe_per_tstep = ev_per_tstep*tlv
    qeOut        = qe_per_tstep

    
    
    
    runoffAGimpervious_m3 = runoffAGimpervious/1000 *SurfaceArea
    runoffAGveg_m3        = runoffAGveg/1000 *SurfaceArea
    runoffWaterBody_m3    = runoffWaterBody/1000 *SurfaceArea
    runoffPipes_m3        = runoffPipes/1000 *SurfaceArea

  END SUBROUTINE SUEWS_cal_QE
  

  
  SUBROUTINE SUEWS_cal_QH(&
       QHMethod,&
       qn1,qf,QmRain,qeOut,qs,QmFreez,qm,avdens,avcp,tsurf,Temp_C,ra,&
       qh,qh_r)
    IMPLICIT NONE

    INTEGER,INTENT(in) :: QHMethod 

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

    

    SELECT CASE (QHMethod)
    CASE (1)
       
       qh=(qn1+qf+QmRain)-(qeOut+qs+Qm+QmFreez)     

    CASE (2)
       
       IF(ra/=0) THEN
          qh = avdens*avcp*(tsurf-Temp_C)/ra
       ELSE
          qh=NAN
       ENDIF

    END SELECT
    QH_R=qh

  END SUBROUTINE SUEWS_cal_QH
  

  
  SUBROUTINE SUEWS_cal_Resistance(&
       StabilityMethod,&
       Diagnose,AerodynamicResistanceMethod,RoughLenHeatMethod,snowUse,&
       id,it,gsModel,SMDMethod,&
       qh_obs,avdens,avcp,h_mod,qn1,dectime,zzd,z0M,zdm,&
       avU1,Temp_C,UStar,VegFraction,&
       avkdn,Kmax,G1,G2,G3,G4,G5,G6,S1,S2,TH,TL,dq,&
       xsmd,vsmd,MaxConductance,LAIMax,LAI_id,snowFrac,sfr,&
       Tstar,L_mod,&
       psim,gsc,ResistSurf,RA,RAsnow,rb)

    IMPLICIT NONE

    INTEGER,INTENT(in)::StabilityMethod
    INTEGER,INTENT(in)::Diagnose
    INTEGER,INTENT(in)::AerodynamicResistanceMethod
    INTEGER,INTENT(in)::RoughLenHeatMethod
    INTEGER,INTENT(in)::snowUse
    INTEGER,INTENT(in)::id
    INTEGER,INTENT(in)::it       
    INTEGER,INTENT(in)::gsModel  
    INTEGER,INTENT(in)::SMDMethod

    REAL(KIND(1d0)),INTENT(in)::qh_obs
    REAL(KIND(1d0)),INTENT(in)::avdens
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::h_mod
    REAL(KIND(1d0)),INTENT(in)::qn1
    REAL(KIND(1d0)),INTENT(in)::dectime    
    REAL(KIND(1d0)),INTENT(in)::zzd        
    REAL(KIND(1d0)),INTENT(in)::z0M        
    REAL(KIND(1d0)),INTENT(in)::zdm        
    REAL(KIND(1d0)),INTENT(in)::avU1       
    REAL(KIND(1d0)),INTENT(in)::Temp_C     
    REAL(KIND(1d0)),INTENT(in)::VegFraction
    REAL(KIND(1d0)),INTENT(in)::avkdn      
    REAL(KIND(1d0)),INTENT(in)::Kmax       
    REAL(KIND(1d0)),INTENT(in)::G1         
    REAL(KIND(1d0)),INTENT(in)::G2         
    REAL(KIND(1d0)),INTENT(in)::G3         
    REAL(KIND(1d0)),INTENT(in)::G4         
    REAL(KIND(1d0)),INTENT(in)::G5         
    REAL(KIND(1d0)),INTENT(in)::G6         
    REAL(KIND(1d0)),INTENT(in)::S1         
    REAL(KIND(1d0)),INTENT(in)::S2         
    REAL(KIND(1d0)),INTENT(in)::TH         
    REAL(KIND(1d0)),INTENT(in)::TL         
    REAL(KIND(1d0)),INTENT(in)::dq         
    REAL(KIND(1d0)),INTENT(in)::xsmd       
    REAL(KIND(1d0)),INTENT(in)::vsmd       

    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::MaxConductance
    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::LAIMax        
    REAL(KIND(1d0)),DIMENSION(3),INTENT(in) ::LAI_id        

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowFrac      
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr           

    REAL(KIND(1d0)),INTENT(out)::Tstar     
    REAL(KIND(1d0)),INTENT(out)::UStar     
    REAL(KIND(1d0)),INTENT(out)::psim      
    REAL(KIND(1d0)),INTENT(out)::gsc       
    REAL(KIND(1d0)),INTENT(out)::ResistSurf
    REAL(KIND(1d0)),INTENT(out)::RA        
    REAL(KIND(1d0)),INTENT(out)::RAsnow    
    REAL(KIND(1d0)),INTENT(out)::rb        
    REAL(KIND(1d0)),INTENT(out)::L_mod  
    REAL(KIND(1d0))::H_init 


    
    CALL SUEWS_init_QH(&
         qh_obs,avdens,avcp,h_mod,qn1,dectime,&
         H_init)

    IF(Diagnose==1) WRITE(*,*) 'Calling STAB_lumps...'
    
    CALL STAB_lumps(&
         StabilityMethod,&  
         dectime,& 
         zzd,&     
         z0M,&     
         zdm,&     
         avU1,&    
         Temp_C,&  
         H_init,& 
         L_mod,&
         Tstar,& 
         UStar,& 
         psim)

    IF(Diagnose==1) WRITE(*,*) 'Calling AerodynamicResistance...'
    CALL AerodynamicResistance(&
         ZZD,&
         z0m,&
         AVU1,&
         L_mod,&
         UStar,&
         VegFraction,&
         AerodynamicResistanceMethod,&
         StabilityMethod,&
         RoughLenHeatMethod,&
         RA) 

    IF (snowUse==1) THEN
       IF(Diagnose==1) WRITE(*,*) 'Calling AerodynamicResistance for snow...'
       CALL AerodynamicResistance(&
            ZZD,&
            z0m,&
            AVU1,&
            L_mod,&
            UStar,&
            VegFraction,&
            AerodynamicResistanceMethod,&
            StabilityMethod,&
            3,&
            RAsnow)     
    ENDIF

    IF(Diagnose==1) WRITE(*,*) 'Calling SurfaceResistance...'
    
    CALL SurfaceResistance(&
         id,it,&
         SMDMethod,snowFrac,sfr,avkdn,Temp_C,dq,xsmd,vsmd,MaxConductance,&
         LAIMax,LAI_id,gsModel,Kmax,&
         G1,G2,G3,G4,G5,G6,TH,TL,S1,S2,&
         gsc,ResistSurf)

    IF(Diagnose==1) WRITE(*,*) 'Calling BoundaryLayerResistance...'
    CALL BoundaryLayerResistance(&
         zzd,&
         z0M,&     
         avU1,&    
         UStar,&  
         rb)  

  END SUBROUTINE SUEWS_cal_Resistance
  

  
  SUBROUTINE SUEWS_update_outputLine(&
       AdditionalWater,alb,avkdn,avU10_ms,azimuth,&
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
       datetimeLine,dataOutLineSUEWS)
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

    
    REAL(KIND(1d0)):: LAI_wt


    REAL(KIND(1d0))::ResistSurf_x
    REAL(KIND(1d0))::l_mod_x
    REAL(KIND(1d0))::bulkalbedo
    REAL(KIND(1d0))::smd_nsurf_x(nsurf)
    REAL(KIND(1d0))::state_x(nsurf)

    

    
    state_x=UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr)), mask=(sfr<0.00001), field=state)
    smd_nsurf_x=UNPACK(SPREAD(NAN, dim=1, ncopies=SIZE(sfr)), mask=(sfr<0.00001), field=smd_nsurf)

    ResistSurf_x=MIN(9999.,ResistSurf)
    l_mod_x=MAX(MIN(9999.,l_mod), -9999.)

    
    IF(iy == (iy_prev_t+1) .AND. (id-1) == 0) THEN   
       
       
       
       
       LAI_wt=DOT_PRODUCT(LAI(id_prev_t,:),sfr(1+2:nvegsurf+2))
    ELSE
       
       
       
       
       LAI_wt=DOT_PRODUCT(LAI(id-1,:),sfr(1+2:nvegsurf+2))
    ENDIF

    
    bulkalbedo=DOT_PRODUCT(alb,sfr)

    
    
    datetimeLine=[&
         REAL(iy,KIND(1D0)),REAL(id,KIND(1D0)),&
         REAL(it,KIND(1D0)),REAL(imin,KIND(1D0)),dectime]
    
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
         t2_C,q2_gkg,avU10_ms& 
         ]
    
    


    

  END SUBROUTINE SUEWS_update_outputLine
  

  
  
  SUBROUTINE SUEWS_update_output(&
       SnowUse,storageheatmethod,&
       ReadLinesMetdata,NumberOfGrids,&
       ir,gridiv,datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&
       dataOutSUEWS,dataOutSnow,dataOutESTM)
    IMPLICIT NONE

    
    
    
    

    INTEGER,INTENT(in) ::ReadLinesMetdata
    
    
    
    INTEGER,INTENT(in) ::NumberOfGrids
    INTEGER,INTENT(in) ::Gridiv
    INTEGER,INTENT(in) ::SnowUse
    INTEGER,INTENT(in) ::storageheatmethod
    INTEGER,INTENT(in) ::ir

    REAL(KIND(1d0)),DIMENSION(5),INTENT(in) :: datetimeLine
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSUEWS-5),INTENT(in) :: dataOutLineSUEWS
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5),INTENT(in) :: dataOutLineESTM
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow-5),INTENT(in) :: dataOutLineSnow


    REAL(KIND(1d0)),INTENT(inout) :: dataOutSUEWS(ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids)
    REAL(KIND(1d0)),INTENT(inout) :: dataOutSnow(ReadLinesMetdata,ncolumnsDataOutSnow,NumberOfGrids)
    REAL(KIND(1d0)),INTENT(inout) :: dataOutESTM(ReadLinesMetdata,ncolumnsDataOutESTM,NumberOfGrids)


    
    
    dataOutSUEWS(ir,1:ncolumnsDataOutSUEWS,Gridiv)=[datetimeLine,set_nan(dataOutLineSUEWS)]
    
    

    IF (snowUse==1) THEN
       dataOutSnow(ir,1:ncolumnsDataOutSnow,Gridiv)=[datetimeLine,set_nan(dataOutLineSnow)]
    END IF

    IF (storageheatmethod==4) THEN
       dataOutESTM(ir,1:ncolumnsDataOutESTM,Gridiv)=[datetimeLine,set_nan(dataOutLineESTM)]
    END IF

    

  END SUBROUTINE SUEWS_update_output
  

  
  SUBROUTINE SUEWS_cal_Diagnostics(&
       tsurf,qh,&
       Press_hPa,qe,&
       UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real,&
       RoughLenHeatMethod,StabilityMethod,&
       avU10_ms,t2_C,q2_gkg)
    IMPLICIT NONE
    
    REAL(KIND(1d0)),INTENT(in) ::tsurf,qh
    REAL(KIND(1d0)),INTENT(in) ::Press_hPa,qe
    REAL(KIND(1d0)),INTENT(in) :: UStar,veg_fr,z0m,L_mod,avdens,avcp,lv_J_kg,tstep_real

    
    INTEGER,INTENT(in)         :: RoughLenHeatMethod,StabilityMethod

    REAL(KIND(1d0)),INTENT(out):: avU10_ms,t2_C,q2_gkg
    REAL(KIND(1d0))::tlv
    REAL(KIND(1d0)),PARAMETER::k=0.4

    tlv=lv_J_kg/tstep_real 
    
    CALL diagSfc(0d0,0d0,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,avU10_ms,0,RoughLenHeatMethod,StabilityMethod)
    
    CALL diagSfc(tsurf,qh,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,t2_C,1,RoughLenHeatMethod,StabilityMethod)
    
    CALL diagSfc(qsatf(tsurf,Press_hPa)*1000,& 
         qe,UStar,veg_fr,z0m,L_mod,k,avdens,avcp,tlv,q2_gkg,2,RoughLenHeatMethod,StabilityMethod)

  END SUBROUTINE SUEWS_cal_Diagnostics


  
  SUBROUTINE SUEWS_cal_tstep(&
       tstep,& 
       nsh, nsh_real, tstep_real) 
    IMPLICIT NONE
    INTEGER,INTENT(in)::tstep 
    
    INTEGER,INTENT(out)::nsh 
    REAL(KIND(1D0)),INTENT(out)::nsh_real 
    REAL(KIND(1D0)),INTENT(out)::tstep_real 
    nsh=3600/tstep
    nsh_real=nsh*1.0
    tstep_real=tstep*1.0

  END SUBROUTINE SUEWS_cal_tstep

  SUBROUTINE SUEWS_cal_surf(&
       sfr,& 
       vegfraction,ImpervFraction,PervFraction,NonWaterFraction) 
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
       iy,id,lat,& 
       dayofWeek_id) 
    IMPLICIT NONE

    INTEGER,INTENT(in) :: iy  
    INTEGER,INTENT(in) :: id  
    REAL(KIND(1d0)),INTENT(in):: lat

    INTEGER,DIMENSION(3),INTENT(OUT) ::dayofWeek_id

    INTEGER::wd
    INTEGER::mb
    INTEGER::date
    INTEGER::seas



    CALL day2month(id,mb,date,seas,iy,lat) 
    CALL Day_of_Week(date,mb,iy,wd)        

    dayofWeek_id(1)=wd      
    dayofWeek_id(2)=mb      
    dayofweek_id(3)=seas    

  END SUBROUTINE SUEWS_cal_weekday


  SUBROUTINE SUEWS_cal_DLS(&
       id,startDLS,endDLS,& 
       DLS) 
    IMPLICIT NONE

    INTEGER, INTENT(in) :: id,startDLS,endDLS
    INTEGER, INTENT(out) :: DLS

    DLS=0
    IF ( id>startDLS .AND. id<endDLS ) dls=1

  END SUBROUTINE SUEWS_cal_DLS

  SUBROUTINE diagSfc(&
       xSurf,xFlux,us,VegFraction,z0m,L_mod,k,avdens,avcp,tlv,&
       xDiag,opt,RoughLenHeatMethod,StabilityMethod)
    
    


    IMPLICIT NONE

    REAL(KIND(1d0)),INTENT(in) :: xSurf,xFlux,us,VegFraction,z0m,L_mod,k,avdens,avcp,tlv
    REAL(KIND(1d0)),INTENT(out):: xDiag
    INTEGER,INTENT(in)         :: opt 
    INTEGER,INTENT(in)         :: RoughLenHeatMethod,StabilityMethod

    REAL(KIND(1d0))            :: &
         psymz2,psymz10,psymz0,psyhz2,psyhz0,& 
         z0h,& 
         z2zd,z10zd,&
         stab_fn_mom,stab_fn_heat 
    REAL(KIND(1d0)),PARAMETER :: muu=1.46e-5 
    REAL(KIND(1d0)),PARAMETER :: nan=-999



    
    
    
    IF (RoughLenHeatMethod==1) THEN 
       z0h=z0m/10
    ELSEIF (RoughLenHeatMethod==2) THEN 
       
       
       z0h=z0m*EXP(2-(1.2-0.9*VegFraction**0.29)*(us*z0m/muu)**0.25)
    ELSEIF (RoughLenHeatMethod==3) THEN
       z0h=z0m*EXP(-20.) 
    ELSEIF (RoughLenHeatMethod==4) THEN
       z0h=z0m*EXP(2-1.29*(us*z0m/muu)**0.25) 
    ENDIF

    


    
    z2zd=2+z0h   
    z10zd=10+z0m 

    
    
    psymz10=stab_fn_mom(StabilityMethod,z10zd/L_mod,z10zd/L_mod)
    psymz2=stab_fn_mom(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
    psymz0=stab_fn_mom(StabilityMethod,z0m/L_mod,z0m/L_mod)

    

    psyhz2=stab_fn_heat(StabilityMethod,z2zd/L_mod,z2zd/L_mod)
    psyhz0=stab_fn_heat(StabilityMethod,z0h/L_mod,z0h/L_mod)
    
    IF ( xSurf==nan ) THEN
       
       
       xDiag=nan
    ELSE
       SELECT CASE (opt)
       CASE (0) 
          xDiag=us/k*(LOG(z10zd/z0m)-psymz10+psymz0)

       CASE (1) 
          xDiag=xSurf-xFlux/(k*us*avdens*avcp)*(LOG(z2zd/z0h)-psyhz2+psyhz0)

       CASE (2) 
          xDiag=xSurf-xFlux/(k*us*avdens*tlv)*(LOG(z2zd/z0h)-psyhz2+psyhz0)

       END SELECT


    END IF

  END SUBROUTINE diagSfc



  
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
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  SUBROUTINE SuMin(&
       alb,albDecTr,albEveTr,albGrass,alBMax_DecTr,&
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
       qh,qe,qsfc)

    INTEGER::AerodynamicResistanceMethod
    INTEGER::Diagnose
    INTEGER::DiagQN
    INTEGER::DiagQS
    INTEGER,INTENT(IN)::startDLS
    INTEGER,INTENT(IN)::endDLS
    INTEGER::EmissionsMethod
    INTEGER::Gridiv
    INTEGER,parameter::gsModel=2
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
    REAL(KIND(1D0)),DIMENSION(NSURF),parameter           ::SoilDepth=0.2
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
    
    REAL(KIND(1d0)),DIMENSION(24*3600/tstep)  ::Tair24HR
    
    REAL(KIND(1D0)),DIMENSION(2*360+1),INTENT(INOUT)   ::qn1_av_store 
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
    
    REAL(KIND(1D0)),DIMENSION(360),INTENT(INOUT)       ::qn1_store 

    REAL(KIND(1D0)),DIMENSION(5)                           ::datetimeLine
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSUEWS-5)      ::dataOutLineSUEWS
    REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSnow-5)       ::dataOutLineSnow
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutESTM-5)       ::dataOutLineESTM
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5) ::DailyStateLine

    REAL(KIND(1D0)),INTENT(out)::qh


    INTEGER,DIMENSION(NSURF)::snowCalcSwitch
    REAL(KIND(1D0)),INTENT(out)::qe
    REAL(KIND(1D0)),INTENT(out)::qsfc


    Diagnose=1
    snowCalcSwitch=0
    snowUse=0
    DiagQN=0
    DiagQS=0
    WaterUseMethod=1 
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

    CALL SUEWS_cal_Main(&
         AerodynamicResistanceMethod,AH_MIN,AHProf_tstep,AH_SLOPE_Cooling,& 
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
         datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&
         DailyStateLine)

    qh=dataOutLineSUEWS(9)
    qe=dataOutLineSUEWS(10)
    qsfc=dataOutLineSUEWS(16)

  END SUBROUTINE SuMin

END MODULE SUEWS_Driver
