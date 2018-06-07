

































SUBROUTINE SUEWS_Calculations(Gridiv,ir,iMB,irMax)

  USE data_in
  USE time
  USE NARP_MODULE
  USE defaultNotUsed
  USE allocateArray
  USE sues_data
  USE snowMod
  USE gis_data
  USE initial
  USE moist
  USE mod_z
  USE mod_k
  USE solweig_module
  USE WhereWhen
  USE SUEWS_Driver
  USE VegPhenogy
  USE resist
  USE DailyState_module,ONLY:SUEWS_update_DailyState


  IMPLICIT NONE

  INTEGER:: Gridiv
  INTEGER::ir
  INTEGER::iMB
  INTEGER:: irMax

  


  
  IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_Translate...'
  CALL SUEWS_Translate(Gridiv,ir,iMB)


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


  
  
  CALL SUEWS_update_DailyState(&
       iy,id,it,imin,dectime,&
       Gridiv,NumberOfGrids,nsh_real,&
       DailyStateLine,&
       dataOutDailyState)

  
  
  CALL SUEWS_update_output(&
       SnowUse,storageheatmethod,&
       ReadLinesMetdata,NumberOfGrids,&
       ir,gridiv,datetimeLine,dataOutLineSUEWS,dataOutLineSnow,dataOutLineESTM,&
       dataOutSUEWS,dataOutSnow,dataOutESTM)


  
  
  

  
  


  
  
  
  
  

  
  
  
  

  
  
  
  
  
  
  
  
  


  IF(Diagnose==1) WRITE(*,*) 'Calling SUEWS_TranslateBack...'
  CALL SUEWS_TranslateBack(Gridiv,ir,irMax)



END SUBROUTINE SUEWS_Calculations
