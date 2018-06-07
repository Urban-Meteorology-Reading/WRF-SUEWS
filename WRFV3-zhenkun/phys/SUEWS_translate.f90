




































SUBROUTINE SUEWS_Translate(Gridiv,ir,iMB)

  USE allocateArray
  USE ColNamesInputFiles
  USE ColNamesModelDailyState
  USE data_in        
  USE defaultnotUsed
  USE gis_data       
  USE initial
  USE mod_z          
  USE resist         
  USE snowMod        
  USE sues_data      
  USE time
  USE ESTM_data
  USE PhysConstants
  USE WhereWhen
  USE ESTM_module,ONLY:ESTM_translate


  IMPLICIT NONE

  INTEGER::Gridiv,&   
       ir,&       
       iMB,&      
       id_prev

  INTEGER::iv,j,i
  
  REAL (KIND(1d0)):: FCskip = -999  

  REAL(KIND(1d0)):: z0m_in, zdm_in  

  CHARACTER(len=20):: grid_txt
  CHARACTER(len=4):: year_txt
  CHARACTER(len=12)::SsG_YYYY 

  CHARACTER(len=4):: iy_text
  CHARACTER(len=3):: id_text
  CHARACTER(len=2):: it_text, imin_text









  
  
  
  GridID = GridIDmatrix(Gridiv) 
  
  lat = SurfaceChar(Gridiv,c_lat)
  lng = SurfaceChar(Gridiv,c_lng)
  
  TIMEZONE = SurfaceChar(Gridiv,c_tz)
  
  Alt = SurfaceChar(Gridiv,c_Alt)
  
  z = SurfaceChar(Gridiv,c_z)
  
  SurfaceArea_ha = SurfaceChar(Gridiv,c_Area)
  
  SurfaceArea = SurfaceArea_ha*10000 

  
  sfr(PavSurf)   = SurfaceChar(Gridiv,c_FrPaved)  
  sfr(BldgSurf)  = SurfaceChar(Gridiv,c_FrBldgs)  
  sfr(ConifSurf) = SurfaceChar(Gridiv,c_FrEveTr)  
  sfr(DecidSurf) = SurfaceChar(Gridiv,c_FrDecTr)  
  sfr(GrassSurf) = SurfaceChar(Gridiv,c_FrGrass)  
  sfr(BSoilSurf) = SurfaceChar(Gridiv,c_FrBSoil)  
  sfr(WaterSurf) = SurfaceChar(Gridiv,c_FrWater)  

  
  IF(SUM(sfr)>1.001.OR.SUM(sfr)<0.999) CALL ErrorHint(10,'Surface fractions (Fr_) should add up to 1.',SUM(sfr),notUsed,notUsedI)

  
  IrrFracConif = SurfaceChar(Gridiv,c_IrrEveTrFrac)  
  IrrFracDecid = SurfaceChar(Gridiv,c_IrrDecTrFrac)  
  IrrFracGrass = SurfaceChar(Gridiv,c_IrrGrassFrac)  

  
  

  
  areaZh = (sfr(BldgSurf) + sfr(ConifSurf) + sfr(DecidSurf))

  
  VegFraction = (sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) + sfr(BSoilSurf))
  

  
  
  IF(veg_type==1) THEN          
     veg_fr = (sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) + sfr(BSoilSurf) + sfr(WaterSurf))
  ELSEIF(veg_type==2) THEN      
     veg_fr = (IrrFracConif*sfr(ConifSurf) + IrrFracDecid*sfr(DecidSurf) + IrrFracGrass*sfr(GrassSurf))
  ENDIF

  ImpervFraction =  (sfr(PavSurf) + sfr(BldgSurf))
  PervFraction =  (sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) + sfr(BSoilSurf) + sfr(WaterSurf))
  NonWaterFraction = (sfr(PavSurf) + sfr(BldgSurf) + sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) + sfr(BSoilSurf))
  

  
  BldgH      = SurfaceChar(Gridiv,c_HBldgs)                                                            
  EveTreeH   = SurfaceChar(Gridiv,c_HEveTr)                                                            
  DecTreeH   = SurfaceChar(Gridiv,c_HDecTr)                                                            
  IF ( sfr(ConifSurf)+sfr(DecidSurf)>0. ) THEN 
     TreeH = (EveTreeH*sfr(ConifSurf) + DecTreeH*sfr(DecidSurf))/(sfr(ConifSurf)+sfr(DecidSurf))     
  ELSE
     TreeH = 1.
  END IF

  FAIBldg    = SurfaceChar(Gridiv,c_FAIBldgs)                                                          
  FAIEveTree = SurfaceChar(Gridiv,c_FAIEveTr)                                                          
  FAIDecTree = SurfaceChar(Gridiv,c_FAIDecTr)                                                          
  IF ( sfr(ConifSurf)+sfr(DecidSurf)>0. ) THEN 
     FAITree    = (FAIEveTree*sfr(ConifSurf) + FAIDecTree*sfr(DecidSurf))/(sfr(ConifSurf)+sfr(DecidSurf)) 
  ELSE
     FAITree = 1.
  END IF

  z0m        = SurfaceChar(Gridiv,c_z0m)                                                               
  zdm        = SurfaceChar(Gridiv,c_zdm)                                                               
  
  z0m_in = z0m
  zdm_in = zdm

  
  PopDensDaytime   = SurfaceChar(Gridiv,c_PopDensDay)   
  PopDensNighttime = SurfaceChar(Gridiv,c_PopDensNight) 
  
  IF(PopDensDaytime >= 0 .AND. PopDensNighttime <  0) PopDensNighttime = PopDensDaytime  
  IF(PopDensDaytime <  0 .AND. PopDensNighttime >= 0) PopDensDaytime = PopDensNighttime  
  IF(PopDensDaytime >= 0 .AND. PopDensNighttime >= 0) NumCapita = (PopDensDaytime+PopDensNighttime)/2  

  
  TrafficRate = SurfaceChar(Gridiv,(/c_TrafficRate_WD,c_TrafficRate_WE/)) 
  
  QF0_BEU          = SurfaceChar(Gridiv,(/c_QF0_BEU_WD,c_QF0_BEU_WE/))   

  
  alb(1:nsurf) = SurfaceChar(Gridiv,c_AlbMax)   

  
  AlbMin_EveTr = SurfaceChar(Gridiv,c_AlbMin(ConifSurf))
  AlbMax_EveTr = SurfaceChar(Gridiv,c_AlbMax(ConifSurf))
  AlbMin_DecTr = SurfaceChar(Gridiv,c_AlbMin(DecidSurf))
  AlbMax_DecTr = SurfaceChar(Gridiv,c_AlbMax(DecidSurf))
  AlbMin_Grass = SurfaceChar(Gridiv,c_AlbMin(GrassSurf))
  AlbMax_Grass = SurfaceChar(Gridiv,c_AlbMax(GrassSurf))

  
  emis(1:nsurf) = SurfaceChar(Gridiv,c_Emis)
  emis_snow     = SurfaceChar(Gridiv,c_SnowEmis)

  
  surf(1,1:nsurf) = SurfaceChar(Gridiv,c_StorMin)   
  surf(5,1:nsurf) = SurfaceChar(Gridiv,c_StorMax)   
  surf(6,1:nsurf) = surf(1,1:nsurf)  

  
  CapMin_dec = surf(1,DecidSurf)
  CapMax_dec = surf(5,DecidSurf)
  
  PorMin_dec = SurfaceChar(Gridiv,c_PorosityMin(ivDecid))   
  PorMax_dec = SurfaceChar(Gridiv,c_PorosityMax(ivDecid))   

  
  WetThresh(1:nsurf) = SurfaceChar(Gridiv,c_WetThresh)

  
  StateLimit(1:nsurf) = SurfaceChar(Gridiv,c_StateLimit)

  
  WaterDepth = SurfaceChar(Gridiv,c_WaterDepth)

  
  surf(2,1:nsurf) = SurfaceChar(Gridiv,c_DrEq)      
  surf(3,1:nsurf) = SurfaceChar(Gridiv,c_DrCoef1)   
  surf(4,1:nsurf) = SurfaceChar(Gridiv,c_DrCoef2)   

  
  snowD(1:(nsurf-1)) = SurfaceChar(Gridiv,c_SnowLimPat(1:(nsurf-1)))

  
  SnowLimPaved = SurfaceChar(Gridiv,c_SnowLimRem(PavSurf))
  SnowLimBuild = SurfaceChar(Gridiv,c_SnowLimRem(BldgSurf))
  

  
  SoilDepth(1:(nsurf-1))           = SurfaceChar(Gridiv,c_SoilDepth(1:(nsurf-1))) 
  SoilStoreCap(1:(nsurf-1))        = SurfaceChar(Gridiv,c_SoilStCap(1:(nsurf-1))) 
  SatHydraulicConduct(1:(nsurf-1)) = SurfaceChar(Gridiv,c_KSat(1:(nsurf-1)))      
  
  
  

  
  
  
  
  
  
  
  SoilDensity   = SurfaceChar(Gridiv,c_SoilDens(1)) 
  SoilDepthMeas = SurfaceChar(Gridiv,c_ObsSMDepth(1))
  SmCap         = SurfaceChar(Gridiv,c_ObsSMMax(1))
  SoilRocks     = SurfaceChar(Gridiv,c_ObsSNRFrac(1))

  
  BaseT  (1:nvegsurf) = SurfaceChar(Gridiv,c_BaseT)
  BaseTe (1:nvegsurf) = SurfaceChar(Gridiv,c_BaseTe)
  GDDFull(1:nvegsurf) = SurfaceChar(Gridiv,c_GDDFull)
  SDDFull(1:nvegsurf) = SurfaceChar(Gridiv,c_SDDFull)
  LAIMin (1:nvegsurf) = SurfaceChar(Gridiv,c_LAIMin)
  LAIMax (1:nvegsurf) = SurfaceChar(Gridiv,c_LAIMax)
  MaxConductance(1:nvegsurf) = SurfaceChar(Gridiv,c_GsMax)

  alpha_bioCO2(1:nvegsurf)     = SurfaceChar(Gridiv,c_alpha_bioCO2)
  beta_bioCO2(1:nvegsurf)      = SurfaceChar(Gridiv,c_beta_bioCO2)
  theta_bioCO2(1:nvegsurf)     = SurfaceChar(Gridiv,c_theta_bioCO2)
  alpha_enh_bioCO2(1:nvegsurf) = SurfaceChar(Gridiv,c_alpha_enh_bioCO2)
  beta_enh_bioCO2(1:nvegsurf)  = SurfaceChar(Gridiv,c_beta_enh_bioCO2)
  resp_a(1:nvegsurf)           = SurfaceChar(Gridiv,c_resp_a)
  resp_b(1:nvegsurf)           = SurfaceChar(Gridiv,c_resp_b)
  min_res_bioCO2(1:nvegsurf)   = SurfaceChar(Gridiv,c_min_res_bioCO2)

  
  LAItype(1:nvegsurf) = INT(SurfaceChar(Gridiv,c_LAIEq(1:nvegsurf)))
  LAIPower(1,1:nvegsurf) = SurfaceChar(Gridiv,c_LeafGP1(1:nvegsurf))
  LAIPower(2,1:nvegsurf) = SurfaceChar(Gridiv,c_LeafGP2(1:nvegsurf))
  LAIPower(3,1:nvegsurf) = SurfaceChar(Gridiv,c_LeafOP1(1:nvegsurf))
  LAIPower(4,1:nvegsurf) = SurfaceChar(Gridiv,c_LeafOP2(1:nvegsurf))

  
  DRAINRT    = SurfaceChar(Gridiv,c_LUMPSDr)      
  RAINCOVER  = SurfaceChar(Gridiv,c_LUMPSCover)   
  RAINMAXRES = SurfaceChar(Gridiv,c_LUMPSMaxRes)  

  
  TRANS_SITE = SurfaceChar(Gridiv,c_NARPTrans)    

  
  RadMeltFact    = SurfaceChar(Gridiv,c_SnowRMFactor)
  TempMeltFact   = SurfaceChar(Gridiv,c_SnowTMFactor)
  SnowAlbMin     = SurfaceChar(Gridiv,c_SnowAlbMin)
  SnowAlbMax     = SurfaceChar(Gridiv,c_SnowAlbMax)
  tau_a          = SurfaceChar(Gridiv,c_Snowtau_a)
  tau_f          = SurfaceChar(Gridiv,c_Snowtau_f)
  PrecipLimitAlb = SurfaceChar(Gridiv,c_SnowPlimAlb)
  SnowDensMin    = SurfaceChar(Gridiv,c_SnowSDMin)
  SnowDensMax    = SurfaceChar(Gridiv,c_SnowSDMax)
  tau_r          = SurfaceChar(Gridiv,c_Snowtau_r)
  CRWMin         = SurfaceChar(Gridiv,c_SnowCRWMin)
  CRWMax         = SurfaceChar(Gridiv,c_SnowCRWMax)
  PrecipLimit    = SurfaceChar(Gridiv,c_SnowPLimSnow)

  
  G1   = SurfaceChar(Gridiv,c_GsG1)
  G2   = SurfaceChar(Gridiv,c_GsG2)
  G3   = SurfaceChar(Gridiv,c_GsG3)
  G4   = SurfaceChar(Gridiv,c_GsG4)
  G5   = SurfaceChar(Gridiv,c_GsG5)
  G6   = SurfaceChar(Gridiv,c_GsG6)
  TH   = SurfaceChar(Gridiv,c_GsTH)
  TL   = SurfaceChar(Gridiv,c_GsTL)
  S1   = SurfaceChar(Gridiv,c_GsS1)
  S2   = SurfaceChar(Gridiv,c_GsS2)
  Kmax = SurfaceChar(Gridiv,c_GsKmax)
  gsModel = INT(SurfaceChar(Gridiv,c_gsModel))

  
  PipeCapacity = SurfaceChar(Gridiv,c_PipeCapacity)

  
  FlowChange = SurfaceChar(Gridiv,c_FlowChange)
  RunoffToWater = SurfaceChar(Gridiv,c_RunoffToWater)

  
  startDLS = INT(SurfaceChar(Gridiv,c_StartDLS))
  endDLS = INT(SurfaceChar(Gridiv,c_EndDLS))

  
  OHM_coef = 0 
  
  
  
  
  
  
  OHM_coef(1:nsurf,1,1) = SurfaceChar(Gridiv,c_a1_SWet(1:nsurf)) 
  OHM_coef(nsurf+1,1,1) = SurfaceChar(Gridiv,c_a1_SWet(nsurf+1)) 
  OHM_coef(1:nsurf,1,2) = SurfaceChar(Gridiv,c_a2_SWet(1:nsurf)) 
  OHM_coef(nsurf+1,1,2) = SurfaceChar(Gridiv,c_a2_SWet(nsurf+1)) 
  OHM_coef(1:nsurf,1,3) = SurfaceChar(Gridiv,c_a3_SWet(1:nsurf)) 
  OHM_coef(nsurf+1,1,3) = SurfaceChar(Gridiv,c_a3_SWet(nsurf+1)) 
  
  OHM_coef(1:nsurf,2,1) = SurfaceChar(Gridiv,c_a1_SDry(1:nsurf)) 
  OHM_coef(nsurf+1,2,1) = SurfaceChar(Gridiv,c_a1_SDry(nsurf+1)) 
  OHM_coef(1:nsurf,2,2) = SurfaceChar(Gridiv,c_a2_SDry(1:nsurf)) 
  OHM_coef(nsurf+1,2,2) = SurfaceChar(Gridiv,c_a2_SDry(nsurf+1)) 
  OHM_coef(1:nsurf,2,3) = SurfaceChar(Gridiv,c_a3_SDry(1:nsurf)) 
  OHM_coef(nsurf+1,2,3) = SurfaceChar(Gridiv,c_a3_SDry(nsurf+1)) 
  
  OHM_coef(1:nsurf,3,1) = SurfaceChar(Gridiv,c_a1_WWet(1:nsurf)) 
  OHM_coef(nsurf+1,3,1) = SurfaceChar(Gridiv,c_a1_WWet(nsurf+1)) 
  OHM_coef(1:nsurf,3,2) = SurfaceChar(Gridiv,c_a2_WWet(1:nsurf)) 
  OHM_coef(nsurf+1,3,2) = SurfaceChar(Gridiv,c_a2_WWet(nsurf+1)) 
  OHM_coef(1:nsurf,3,3) = SurfaceChar(Gridiv,c_a3_WWet(1:nsurf)) 
  OHM_coef(nsurf+1,3,3) = SurfaceChar(Gridiv,c_a3_WWet(nsurf+1)) 
  
  OHM_coef(1:nsurf,4,1) = SurfaceChar(Gridiv,c_a1_WDry(1:nsurf)) 
  OHM_coef(nsurf+1,4,1) = SurfaceChar(Gridiv,c_a1_WDry(nsurf+1)) 
  OHM_coef(1:nsurf,4,2) = SurfaceChar(Gridiv,c_a2_WDry(1:nsurf)) 
  OHM_coef(nsurf+1,4,2) = SurfaceChar(Gridiv,c_a2_WDry(nsurf+1)) 
  OHM_coef(1:nsurf,4,3) = SurfaceChar(Gridiv,c_a3_WDry(1:nsurf)) 
  OHM_coef(nsurf+1,4,3) = SurfaceChar(Gridiv,c_a3_WDry(nsurf+1)) 
  
  OHM_threshSW(1:nsurf) = SurfaceChar(Gridiv,c_OHMThresh_SW(1:nsurf)) 
  OHM_threshSW(nsurf+1) = SurfaceChar(Gridiv,c_OHMThresh_SW(nsurf+1)) 
  OHM_threshWD(1:nsurf) = SurfaceChar(Gridiv,c_OHMThresh_WD(1:nsurf)) 
  OHM_threshWD(nsurf+1) = SurfaceChar(Gridiv,c_OHMThresh_WD(nsurf+1)) 

  
  
  
  IF(StorageHeatMethod==4 .OR. StorageHeatMethod==14) THEN
     AreaWall = SurfaceChar(Gridiv,c_AreaWall)
     fwall=AreaWall/SurfaceArea

     
     ESTMsfr_Paved = SurfaceChar(Gridiv,c_Fr_ESTMClass_Paved)   
     ESTMsfr_Bldgs = SurfaceChar(Gridiv,c_Fr_ESTMClass_Bldgs)   
     
     IF(sfr(PavSurf) > 0) THEN  
        IF(SUM(ESTMsfr_Paved)>1.001.OR.SUM(ESTMsfr_Paved)<0.999) THEN
           CALL ErrorHint(10,'Surface fractions (Fr_ESTMClass_Paved) should sum to 1.',SUM(ESTMsfr_Paved),notUsed,notUsedI)
        ENDIF
     ELSEIF(sfr(PavSurf) == 0) THEN 
        IF(SUM(ESTMsfr_Paved)>1.001.OR.SUM(ESTMsfr_Paved)<0.999) THEN   
           ESTMsfr_Paved(1) = 1.000
           ESTMsfr_Paved(2:3) = 0.000
           CALL ErrorHint(67,'ESTM Paved classes do not sum to 1 (but no Paved surface present).',&
                SUM(ESTMsfr_Paved),notUsed,notUsedI)
        ENDIF
     ENDIF
     IF(sfr(BldgSurf) > 0) THEN
        IF(SUM(ESTMsfr_Bldgs)>1.001.OR.SUM(ESTMsfr_Bldgs)<0.999) THEN
           CALL ErrorHint(10,'Surface fractions (Fr_ESTMClass_Bldgs) should sum to 1.',SUM(ESTMsfr_Bldgs),notUsed,notUsedI)
        ENDIF
     ELSEIF(sfr(BldgSurf) == 0) THEN 
        IF(SUM(ESTMsfr_Bldgs)>1.001.OR.SUM(ESTMsfr_Bldgs)<0.999) THEN   
           ESTMsfr_Bldgs(1) = 1.000
           ESTMsfr_Bldgs(2:5) = 0.000
           CALL ErrorHint(67,'ESTM Bldgs classes do not sum to 1 (but no Bldgs surface present).',&
                SUM(ESTMsfr_Bldgs),notUsed,notUsedI)
        ENDIF
     ENDIF

     
     
     IF(SurfaceChar(Gridiv,c_ESTMCode(PavSurf)) == 0) THEN   
        
        DO i=1,3
           zSurf_Paved(:,i) = SurfaceChar(Gridiv,(/c_Surf_thick1_Paved(i),c_Surf_thick2_Paved(i),c_Surf_thick3_Paved(i), &
                c_Surf_thick4_Paved(i),c_Surf_thick5_Paved(i)/))
           kSurf_Paved(:,i) = SurfaceChar(Gridiv,(/c_Surf_k1_Paved(i),c_Surf_k2_Paved(i),c_Surf_k3_Paved(i), &
                c_Surf_k4_Paved(i),c_Surf_k5_Paved(i)/))
           rSurf_Paved(:,i) = SurfaceChar(Gridiv,(/c_Surf_rhoCp1_Paved(i),c_Surf_rhoCp2_Paved(i),c_Surf_rhoCp3_Paved(i), &
                c_Surf_rhoCp4_Paved(i),c_Surf_rhoCp5_Paved(i)/))
        ENDDO
        
        zSurf_SUEWSsurfs(:,PavSurf) = zSurf_Paved(:,1)*ESTMsfr_Paved(1) &
             + zSurf_Paved(:,2)*ESTMsfr_Paved(2) &
             + zSurf_Paved(:,3)*ESTMsfr_Paved(3)
        kSurf_SUEWSsurfs(:,PavSurf) = kSurf_Paved(:,1)*ESTMsfr_Paved(1) &
             + kSurf_Paved(:,2)*ESTMsfr_Paved(2) &
             + kSurf_Paved(:,3)*ESTMsfr_Paved(3)
        rSurf_SUEWSsurfs(:,PavSurf) = rSurf_Paved(:,1)*ESTMsfr_Paved(1) &
             + rSurf_Paved(:,2)*ESTMsfr_Paved(2) &
             + rSurf_Paved(:,3)*ESTMsfr_Paved(3)
     ELSEIF(SurfaceChar(Gridiv,c_ESTMCode(PavSurf)) /= 0) THEN   
        zSurf_SUEWSsurfs(:,PavSurf) = SurfaceChar(Gridiv,(/c_Surf_thick1(PavSurf),c_Surf_thick2(PavSurf),c_Surf_thick3(PavSurf),&
             c_Surf_thick4(PavSurf),c_Surf_thick5(PavSurf)/))
        kSurf_SUEWSsurfs(:,PavSurf) = SurfaceChar(Gridiv,(/c_Surf_k1(PavSurf),c_Surf_k2(PavSurf),c_Surf_k3(PavSurf),&
             c_Surf_k4(PavSurf),c_Surf_k5(PavSurf)/))
        rSurf_SUEWSsurfs(:,PavSurf) = SurfaceChar(Gridiv,(/c_Surf_rhoCp1(PavSurf),c_Surf_rhoCp2(PavSurf),c_Surf_rhoCp3(PavSurf),&
             c_Surf_rhoCp4(PavSurf),c_Surf_rhoCp5(PavSurf)/))
     ENDIF

     
     
     IF(SurfaceChar(Gridiv,c_ESTMCode(BldgSurf)) == 0) THEN   
        
        DO i=1,5
           zSurf_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Surf_thick1_Bldgs(i),c_Surf_thick2_Bldgs(i),c_Surf_thick3_Bldgs(i), &
                c_Surf_thick4_Bldgs(i),c_Surf_thick5_Bldgs(i)/))
           kSurf_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Surf_k1_Bldgs(i),c_Surf_k2_Bldgs(i),c_Surf_k3_Bldgs(i), &
                c_Surf_k4_Bldgs(i),c_Surf_k5_Bldgs(i)/))
           rSurf_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Surf_rhoCp1_Bldgs(i),c_Surf_rhoCp2_Bldgs(i),c_Surf_rhoCp3_Bldgs(i), &
                c_Surf_rhoCp4_Bldgs(i),c_Surf_rhoCp5_Bldgs(i)/))
           zwall_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Wall_thick1_Bldgs(i),c_Wall_thick2_Bldgs(i),c_Wall_thick3_Bldgs(i), &
                c_Wall_thick4_Bldgs(i),c_Wall_thick5_Bldgs(i)/))
           kwall_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Wall_k1_Bldgs(i),c_Wall_k2_Bldgs(i),c_Wall_k3_Bldgs(i), &
                c_Wall_k4_Bldgs(i),c_Wall_k5_Bldgs(i)/))
           rwall_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Wall_rhoCp1_Bldgs(i),c_Wall_rhoCp2_Bldgs(i),c_Wall_rhoCp3_Bldgs(i), &
                c_Wall_rhoCp4_Bldgs(i),c_Wall_rhoCp5_Bldgs(i)/))
           zibld_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Internal_thick1_Bldgs(i),c_Internal_thick2_Bldgs(i),&
                c_Internal_thick3_Bldgs(i), &
                c_Internal_thick4_Bldgs(i),c_Internal_thick5_Bldgs(i)/))
           kibld_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Internal_k1_Bldgs(i),c_Internal_k2_Bldgs(i),c_Internal_k3_Bldgs(i), &
                c_Internal_k4_Bldgs(i),c_Internal_k5_Bldgs(i)/))
           ribld_Bldgs(:,i) = SurfaceChar(Gridiv,(/c_Internal_rhoCp1_Bldgs(i),c_Internal_rhoCp2_Bldgs(i),&
                c_Internal_rhoCp3_Bldgs(i), &
                c_Internal_rhoCp4_Bldgs(i),c_Internal_rhoCp5_Bldgs(i)/))
           nroom_Bldgs(i)    = SurfaceChar(Gridiv,c_nroom_Bldgs(i))
           alb_ibld_Bldgs(i) = SurfaceChar(Gridiv,c_alb_ibld_Bldgs(i))
           em_ibld_Bldgs(i)  = SurfaceChar(Gridiv,c_em_ibld_Bldgs(i))
           CH_iwall_Bldgs(i) = SurfaceChar(Gridiv,c_CH_iwall_Bldgs(i))
           CH_iroof_Bldgs(i) = SurfaceChar(Gridiv,c_CH_iroof_Bldgs(i))
           CH_ibld_Bldgs(i)  = SurfaceChar(Gridiv,c_CH_ibld_Bldgs(i))
        ENDDO
        
        zSurf_SUEWSsurfs(:,BldgSurf) = zSurf_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + zSurf_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + zSurf_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + zSurf_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + zSurf_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        kSurf_SUEWSsurfs(:,BldgSurf) = kSurf_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + kSurf_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + kSurf_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + kSurf_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + kSurf_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        rSurf_SUEWSsurfs(:,BldgSurf) = rSurf_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + rSurf_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + rSurf_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + rSurf_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + rSurf_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        
        zwall = zwall_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + zwall_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + zwall_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + zwall_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + zwall_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        kwall = kwall_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + kwall_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + kwall_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + kwall_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + kwall_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        rwall = rwall_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + rwall_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + rwall_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + rwall_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + rwall_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        
        zibld = zibld_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + zibld_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + zibld_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + zibld_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + zibld_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        kibld = kibld_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + kibld_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + kibld_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + kibld_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + kibld_Bldgs(:,5)*ESTMsfr_Bldgs(5)
        ribld = ribld_Bldgs(:,1)*ESTMsfr_Bldgs(1) &
             + ribld_Bldgs(:,2)*ESTMsfr_Bldgs(2) &
             + ribld_Bldgs(:,3)*ESTMsfr_Bldgs(3) &
             + ribld_Bldgs(:,4)*ESTMsfr_Bldgs(4) &
             + ribld_Bldgs(:,5)*ESTMsfr_Bldgs(5)

        nroom = nroom_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + nroom_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + nroom_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + nroom_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + nroom_Bldgs(5)*ESTMsfr_Bldgs(5)
        alb_ibld = alb_ibld_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + alb_ibld_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + alb_ibld_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + alb_ibld_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + alb_ibld_Bldgs(5)*ESTMsfr_Bldgs(5)
        em_ibld = em_ibld_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + em_ibld_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + em_ibld_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + em_ibld_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + em_ibld_Bldgs(5)*ESTMsfr_Bldgs(5)
        CH_iwall = CH_iwall_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + CH_iwall_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + CH_iwall_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + CH_iwall_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + CH_iwall_Bldgs(5)*ESTMsfr_Bldgs(5)
        CH_iroof = CH_iroof_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + CH_iroof_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + CH_iroof_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + CH_iroof_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + CH_iroof_Bldgs(5)*ESTMsfr_Bldgs(5)
        CH_ibld = CH_ibld_Bldgs(1)*ESTMsfr_Bldgs(1) &
             + CH_ibld_Bldgs(2)*ESTMsfr_Bldgs(2) &
             + CH_ibld_Bldgs(3)*ESTMsfr_Bldgs(3) &
             + CH_ibld_Bldgs(4)*ESTMsfr_Bldgs(4) &
             + CH_ibld_Bldgs(5)*ESTMsfr_Bldgs(5)

     ELSEIF(SurfaceChar(Gridiv,c_ESTMCode(BldgSurf)) /= 0) THEN   
        zSurf_SUEWSsurfs(:,BldgSurf) = SurfaceChar(Gridiv,(/c_Surf_thick1(BldgSurf),c_Surf_thick2(BldgSurf),&
             c_Surf_thick3(BldgSurf),&
             c_Surf_thick4(BldgSurf),c_Surf_thick5(BldgSurf)/))
        kSurf_SUEWSsurfs(:,BldgSurf) = SurfaceChar(Gridiv,(/c_Surf_k1(BldgSurf),c_Surf_k2(BldgSurf),c_Surf_k3(BldgSurf),&
             c_Surf_k4(BldgSurf),c_Surf_k5(BldgSurf)/))
        rSurf_SUEWSsurfs(:,BldgSurf) = SurfaceChar(Gridiv,(/c_Surf_rhoCp1(BldgSurf),c_Surf_rhoCp2(BldgSurf),&
             c_Surf_rhoCp3(BldgSurf),&
             c_Surf_rhoCp4(BldgSurf),c_Surf_rhoCp5(BldgSurf)/))
        zwall = SurfaceChar(Gridiv,(/c_Wall_thick1,c_Wall_thick2,c_Wall_thick3,c_Wall_thick4,c_Wall_thick5/))
        kwall = SurfaceChar(Gridiv,(/c_Wall_k1,c_Wall_k2,c_Wall_k3,c_Wall_k4,c_Wall_k5/))
        rwall = SurfaceChar(Gridiv,(/c_Wall_rhoCp1,c_Wall_rhoCp2,c_Wall_rhoCp3,c_Wall_rhoCp4,c_Wall_rhoCp5/))
        zibld = SurfaceChar(Gridiv,(/c_Internal_thick1,c_Internal_thick2,c_Internal_thick3,c_Internal_thick4,c_Internal_thick5/))
        kibld = SurfaceChar(Gridiv,(/c_Internal_k1,c_Internal_k2,c_Internal_k3,c_Internal_k4,c_Internal_k5/))
        ribld = SurfaceChar(Gridiv,(/c_Internal_rhoCp1,c_Internal_rhoCp2,c_Internal_rhoCp3,c_Internal_rhoCp4,c_Internal_rhoCp5/))

        nroom = SurfaceChar(Gridiv,c_nroom)
        alb_ibld = SurfaceChar(Gridiv,c_alb_ibld)
        em_ibld = SurfaceChar(Gridiv,c_em_ibld)
        CH_iwall = SurfaceChar(Gridiv,c_CH_iwall)
        CH_iroof = SurfaceChar(Gridiv,c_CH_iroof)
        CH_ibld = SurfaceChar(Gridiv,c_CH_ibld)
     ENDIF

     
     DO iv=ConifSurf, nsurfIncSnow
        zSurf_SUEWSsurfs(:,iv) = SurfaceChar(Gridiv,(/c_Surf_thick1(iv),c_Surf_thick2(iv),c_Surf_thick3(iv),&
             c_Surf_thick4(iv),c_Surf_thick5(iv)/))
        kSurf_SUEWSsurfs(:,iv) = SurfaceChar(Gridiv,(/c_Surf_k1(iv),c_Surf_k2(iv),c_Surf_k3(iv),&
             c_Surf_k4(iv),c_Surf_k5(iv)/))
        rSurf_SUEWSsurfs(:,iv) = SurfaceChar(Gridiv,(/c_Surf_rhoCp1(iv),c_Surf_rhoCp2(iv),c_Surf_rhoCp3(iv),&
             c_Surf_rhoCp4(iv),c_Surf_rhoCp5(iv)/))
     ENDDO

     
     
     
     froof=sfr(BldgSurf)
     
     fground=sfr(PavSurf)+sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BsoilSurf)+sfr(WaterSurf)
     
     fveg=sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)

     
     zground = 0
     kground = 0
     rground = 0
     DO iv=1,nsurf
        IF(iv/=BldgSurf .AND. fground /= 0) THEN   
           zground = zground + zSurf_SUEWSsurfs(:,iv)*sfr(iv) /fground   
           kground = kground + kSurf_SUEWSsurfs(:,iv)*sfr(iv) /fground   
           rground = rground + rSurf_SUEWSsurfs(:,iv)*sfr(iv) /fground   
        ELSEIF ( fground==0. ) THEN 
           zground=zground+0.01
           kground=kground+0.01
           rground=rground+0.01
           
           
           
        ENDIF
     ENDDO
     
     zroof = zSurf_SUEWSsurfs(:,BldgSurf)
     kroof = kSurf_SUEWSsurfs(:,BldgSurf)
     rroof = rSurf_SUEWSsurfs(:,BldgSurf)

     DO i=1,5
        IF (zground(i)<=0)THEN
           Nground=i-1
           EXIT
        ENDIF
     ENDDO
     DO i=1,5
        IF (zroof(i)<=0) THEN
           Nroof=i-1
           EXIT
        ENDIF
     ENDDO
     DO i=1,5
        IF (zwall(i)<=0) THEN
           Nwall=i-1
           EXIT
        ENDIF
     ENDDO
     DO i=1,5
        IF (zibld(i)<=0) THEN
           Nibld=i-1
           EXIT
        ENDIF
     ENDDO
  ENDIF 

  
  IF ( StorageHeatMethod==3 ) THEN
     cpAnOHM(1:nsurf) = SurfaceChar(Gridiv,c_cpAnOHM) 
     kkAnOHM(1:nsurf) = SurfaceChar(Gridiv,c_kkAnOHM) 
     chAnOHM(1:nsurf) = SurfaceChar(Gridiv,c_chAnOHM) 

     
     
     
     







     
     
     
     
     
     
     
     
     




     
     
     
     
     
     
     



     

     
     
     
     
     
     
     


     
     
     
  END IF



  
  BaseTHDD         = -999 
  QF_A             = 0
  QF_B             = 0
  QF_C             = 0
  AH_min           = 0
  T_CRITIC_Heating = 0
  T_CRITIC_Cooling = 0
  AH_slope_Heating = 0
  AH_slope_Cooling = 0

  BaseTHDD             = SurfaceChar(Gridiv,c_BaseTHDD)
  QF_A                 = SurfaceChar(Gridiv,(/c_QF_A1,c_QF_A2/))
  QF_B                 = SurfaceChar(Gridiv,(/c_QF_B1,c_QF_B2/))
  QF_C                 = SurfaceChar(Gridiv,(/c_QF_C1,c_QF_C2/))
  AH_min               = SurfaceChar(Gridiv,(/c_AHMin_WD,c_AHMin_WE/))
  AH_slope_Heating     = SurfaceChar(Gridiv,(/c_AHSlopeHeating_WD,c_AHSlopeHeating_WE/))
  AH_slope_Cooling     = SurfaceChar(Gridiv,(/c_AHSlopeCooling_WD,c_AHSlopeCooling_WE/))
  T_Critic_Heating     = SurfaceChar(Gridiv,(/c_TCriticHeating_WD,c_TCriticHeating_WE/))
  T_Critic_Cooling     = SurfaceChar(Gridiv,(/c_TCriticCooling_WD,c_TCriticCooling_WE/))
  EnProfWD             = SurfaceChar(Gridiv,c_EnProfWD)
  EnProfWE             = SurfaceChar(Gridiv,c_EnProfWE)
  CO2mWD               = SurfaceChar(Gridiv,c_CO2mWD)
  CO2mWE               = SurfaceChar(Gridiv,c_CO2mWE)
  TraffProfWD          = SurfaceChar(Gridiv,c_TraffProfWD)
  TraffProfWE          = SurfaceChar(Gridiv,c_TraffProfWE)
  PopProfWD            = SurfaceChar(Gridiv,c_PopProfWD)
  PopProfWE            = SurfaceChar(Gridiv,c_PopProfWE)
  MinQFMetab           = SurfaceChar(Gridiv,c_MinQFMetab)
  MaxQFMetab           = SurfaceChar(Gridiv,c_MaxQFMetab)
  FrFossilFuel_heat    = SurfaceChar(Gridiv,c_FrFossilFuel_heat)
  FrFossilFuel_NonHeat = SurfaceChar(Gridiv,c_FrFossilFuel_NonHeat)
  EF_umolCO2perJ       = SurfaceChar(Gridiv,c_EF_umolCO2perJ)
  EnEF_v_Jkm           = SurfaceChar(Gridiv,c_EnEF_v_Jkm)
  FcEF_v_kgkm          = SurfaceChar(Gridiv,c_FcEF_v_kgkm)
  TrafficUnits         = SurfaceChar(Gridiv,c_TrafficUnits)

  
  Ie_start           = INT(SurfaceChar(Gridiv,c_IeStart))
  Ie_end             = INT(SurfaceChar(Gridiv,c_IeEnd))
  InternalWaterUse_h = SurfaceChar(Gridiv,c_IntWU)
  Faut               = SurfaceChar(Gridiv,c_Faut)
  Ie_a = SurfaceChar(Gridiv,c_Ie_a)   
  Ie_m = SurfaceChar(Gridiv,c_Ie_m)   
  DayWat             = SurfaceChar(Gridiv,c_DayWat)
  DayWatPer          = SurfaceChar(Gridiv,c_DayWatPer)

  
  AHProf(0:23,1)   = SurfaceChar(Gridiv,c_HrProfEnUseWD)   
  AHProf(0:23,2)   = SurfaceChar(Gridiv,c_HrProfEnUseWE)   
  WUProfM(0:23,1)  = SurfaceChar(Gridiv,c_HrProfWUManuWD)  
  WUProfM(0:23,2)  = SurfaceChar(Gridiv,c_HrProfWUManuWE)  
  WUProfA(0:23,1)  = SurfaceChar(Gridiv,c_HrProfWUAutoWD)  
  WUProfA(0:23,2)  = SurfaceChar(Gridiv,c_HrProfWUAutoWE)  
  SnowProf(0:23,1) = SurfaceChar(Gridiv,c_HrProfSnowCWD)   
  SnowProf(0:23,2) = SurfaceChar(Gridiv,c_HrProfSnowCWE)   
  HumActivityProf(0:23,1) = SurfaceChar(Gridiv,c_HrProfHumActivityWD)    
  HumActivityProf(0:23,2) = SurfaceChar(Gridiv,c_HrProfHumActivityWE)    
  TraffProf(0:23,1) = SurfaceChar(Gridiv,c_HrProfTraffWD)  
  TraffProf(0:23,2) = SurfaceChar(Gridiv,c_HRProfTraffWE)  
  PopProf(0:23,1) = SurfaceChar(Gridiv,c_HRProfPopWD)      
  PopProf(0:23,2) = SurfaceChar(Gridiv,c_HRProfPopWE)      



  
  AHProf_tstep(:,1)      = TstepProfiles(Gridiv,cTP_EnUseWD,:)       
  AHProf_tstep(:,2)      = TstepProfiles(Gridiv,cTP_EnUseWE,:)       
  WUProfM_tstep(:,1)     = TstepProfiles(Gridiv,cTP_WUManuWD,:)      
  WUProfM_tstep(:,2)     = TstepProfiles(Gridiv,cTP_WUManuWE,:)      
  WUProfA_tstep(:,1)     = TstepProfiles(Gridiv,cTP_WUAutoWD,:)      
  WUProfA_tstep(:,2)     = TstepProfiles(Gridiv,cTP_WUAutoWE,:)      
  HumActivity_tstep(:,1) = TstepProfiles(Gridiv,cTP_HumActivityWD,:) 
  HumActivity_tstep(:,2) = TstepProfiles(Gridiv,cTP_HumActivityWE,:) 
  TraffProf_tstep(:,1)   = TstepProfiles(Gridiv,cTP_TraffProfWD,:)   
  TraffProf_tstep(:,2)   = TstepProfiles(Gridiv,cTP_TraffProfWE,:)   
  PopProf_tstep(:,1)     = TstepProfiles(Gridiv,cTP_PopProfWD,:)     
  PopProf_tstep(:,2)     = TstepProfiles(Gridiv,cTP_PopProfWE,:)     


  
  
  
  
  
  
  WaterDist(PavSurf,  1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToPaved(1:(nsurf-1)))
  WaterDist(BldgSurf, 1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToBldgs(1:(nsurf-1)))
  WaterDist(ConifSurf,1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToEveTr(1:(nsurf-1)))
  WaterDist(DecidSurf,1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToDecTr(1:(nsurf-1)))
  WaterDist(GrassSurf,1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToGrass(1:(nsurf-1)))
  WaterDist(BSoilSurf,1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToBSoil(1:(nsurf-1)))
  WaterDist(WaterSurf,1:(nsurf-1)) = SurfaceChar(Gridiv,c_WGToWater(1:(nsurf-1)))
  
  DO iv = 1,(nsurf-1)
     IF(SurfaceChar(Gridiv,c_WGToRunoff(iv)) /= 0) THEN
        WaterDist((nsurf+1),iv) = SurfaceChar(Gridiv,c_WGToRunoff(iv))
     ELSE
        WaterDist((nsurf+1),iv) = SurfaceChar(Gridiv,c_WGToSoilStore(iv))
     ENDIF
  ENDDO

  
  HDD(:,:)    = HDD_grids(:,:,Gridiv)
  GDD(:,:)    = GDD_grids(:,:,Gridiv)
  LAI(:,:)    = LAI_grids(:,:,Gridiv)
  WU_day(:,:) = WU_Day_grids(:,:,Gridiv)
  AlbDecTr(:) = AlbDecTr_grids(:,Gridiv)
  DecidCap(:) = DecidCap_grids(:,Gridiv)
  Porosity(:) = Porosity_grids(:,Gridiv)
  AlbEveTr(:) = AlbEveTr_grids(:,Gridiv)
  AlbGrass(:) = AlbGrass_grids(:,Gridiv)
  SnowAlb     = ModelDailyState(Gridiv,cMDS_SnowAlb)


  

  
  
  
  
  

  
  
  
  

  



  
  
  
  IF(NetRadiationMethod>0)THEN
     NARP_LAT = SurfaceChar(Gridiv,c_lat)
     NARP_LONG = SurfaceChar(Gridiv,c_lng)    
     NARP_YEAR = INT(SurfaceChar(Gridiv,c_Year))
     NARP_TZ = TIMEZONE                           
     NARP_EMIS_SNOW = SurfaceChar(Gridiv,c_SnowEmis)
     NARP_TRANS_SITE = TRANS_SITE

     
     
     

     
     
     
     
     

     
     
     
     
  ENDIF


  
  
  IF(ir == 0) THEN


     
     
     

     
     id_prev = INT(ModelDailyState(Gridiv,cMDS_id_prev))

     porosity    = ModelDailyState(Gridiv,cMDS_porosity)
     albDecTr    = ModelDailyState(Gridiv,cMDS_albDecTr)
     albEveTr    = ModelDailyState(Gridiv,cMDS_albEveTr)
     albGrass    = ModelDailyState(Gridiv,cMDS_albGrass)
     DecidCap    = ModelDailyState(Gridiv,cMDS_DecidCap)
     SnowfallCum = ModelDailyState(Gridiv,cMDS_SnowfallCum)
     SnowAlb     = ModelDailyState(Gridiv,cMDS_SnowAlb)

     
     LAI=0
     LAI(id_prev,ivConif)  = ModelDailyState(Gridiv,cMDS_LAIInitialEveTr)
     LAI(id_prev,ivDecid)  = ModelDailyState(Gridiv,cMDS_LAIInitialDecTr)
     LAI(id_prev,ivGrass) = ModelDailyState(Gridiv,cMDS_LAIInitialGrass)

     
     GDD = 0
     GDD(:,1)=0
     GDD(:,2)=0
     GDD(:,3) = ModelDailyState(Gridiv,cMDS_GDDMin)
     GDD(:,4) = ModelDailyState(Gridiv,cMDS_GDDMax)
     GDD(:,5)=0
     GDD(id_prev,1) = ModelDailyState(Gridiv,cMDS_GDD1_0)
     GDD(id_prev,2) = ModelDailyState(Gridiv,cMDS_GDD2_0)

     
     HDD = 0
     HDD(id_prev,1) = ModelDailyState(Gridiv,cMDS_HDD1)         
     HDD(id_prev,2) = ModelDailyState(Gridiv,cMDS_HDD2)         
     HDD(id_prev-3,3) = ModelDailyState(Gridiv,cMDS_TempCOld3)  
     HDD(id_prev-2,3) = ModelDailyState(Gridiv,cMDS_TempCOld2)
     HDD(id_prev-1,3) = ModelDailyState(Gridiv,cMDS_TempCOld1)
     HDD(id_prev,3)   = ModelDailyState(Gridiv,cMDS_TempC)
     
     
     HDD(id_prev,6) = ModelDailyState(Gridiv,cMDS_DaysSinceRain)  


     
     HDD_grids(:,:,Gridiv)    = HDD(:,:)
     GDD_grids(:,:,Gridiv)    = GDD(:,:)
     LAI_grids(:,:,Gridiv)    = LAI(:,:)
     WU_Day_grids(:,:,Gridiv) = WU_day(:,:)
     AlbDecTr_grids(:,Gridiv) = AlbDecTr(:)
     AlbEveTr_grids(:,Gridiv) = AlbEveTr(:)
     AlbGrass_grids(:,Gridiv) = AlbGrass(:)
     DecidCap_grids(:,Gridiv) = DecidCap(:)
     Porosity_grids(:,Gridiv) = Porosity(:)

     
     SnowDens(1:nsurf) = ModelDailyState(Gridiv,cMDS_SnowDens(1:nsurf))

     
     
     
     
     State(1:nsurf)             = ModelOutputData(0,cMOD_State(1:nsurf),Gridiv)
     
     
     SoilMoist(1:nsurf)             = ModelOutputData(0,cMOD_SoilState(1:nsurf),Gridiv)
     
     
     SnowFrac(1:nsurf)  = ModelOutputData(0,cMOD_SnowFrac(1:nsurf), Gridiv)
     
     SnowPack(1:nsurf)  = ModelOutputData(0,cMOD_SnowPack(1:nsurf), Gridiv)
     
     MeltWaterStore(1:nsurf)  = ModelOutputData(0,cMOD_SnowWaterState(1:nsurf), Gridiv)

  ENDIF  
  


  
  
  
  IF (ir==1.AND.iMB==1) THEN   

     FileChoices=TRIM(FileOutputPath)//TRIM(FileCode)//'_FileChoices.txt'
     OPEN(12,file=FileChoices,position='append')

     WRITE(grid_txt,'(I5)') INT(SurfaceChar(Gridiv,c_Grid))
     WRITE(year_txt,'(I4)') INT(SurfaceChar(Gridiv,c_Year))
     WRITE(SsG_YYYY,'(A12)') TRIM(FileCode)//TRIM(ADJUSTL(grid_txt))//'_'//TRIM(ADJUSTL(year_txt))


     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Surface characteristics'//' -----'
     
     WRITE(12,'(8a10,a16)') 'Paved','Bldgs','EveTr','DecTr','Grass','BSoil','Water','Snow', ' SurfType'
     WRITE(12,120) (sfr(iv),iv=1,nsurf),FCskip, ' SurfFr'
     WRITE(12,120) FCskip,FCskip,IrrFracConif,IrrFracDecid,IrrFracGrass,FCskip,FCskip,FCskip, ' IrrFr'
     WRITE(12,120) FCskip,FCskip,WUAreaEveTr_m2,WUAreaDecTr_m2,WUAreaGrass_m2,FCskip,FCskip,FCskip, ' WaterUseArea'
     WRITE(12,120) FCskip,BldgH,EveTreeH,DecTreeH,FCskip,FCskip,FCskip,FCskip, ' H'
     WRITE(12,120) FCskip,FAIBldg,FAIEveTree,FAIDecTree,FCskip,FCskip,FCskip,FCskip, ' FAI'
     WRITE(12,120) FCskip,FCskip,AlbMin_EveTr,AlbMin_DecTr,AlbMin_Grass,FCskip,FCskip,SnowAlbMin, ' AlbedoMin'
     WRITE(12,120) FCskip,FCskip,AlbMax_EveTr,AlbMax_DecTr,AlbMax_Grass,FCskip,FCskip,SnowAlbMax, ' AlbedoMax'

     WRITE(12,120) (emis(iv),iv=1,nsurf),emis_snow, ' Emissivity'
     WRITE(12,120) FCskip, FCskip,(baseT(iv),iv=1,nvegsurf), FCskip, FCskip, FCskip, ' BaseT'
     WRITE(12,120) FCskip, FCskip,(baseTe(iv),iv=1,nvegsurf),FCskip, FCskip, FCskip, ' BaseTe'
     WRITE(12,120) (Surf(1,iv),iv=1,nsurf), FCskip ,' StorageMin'
     WRITE(12,120) (Surf(5,iv),iv=1,nsurf), FCskip ,' StorageMax'
     WRITE(12,120) (WetThresh(iv),iv=1,nsurf), FCskip, ' WetThreshold'
     WRITE(12,120) (StateLimit(iv),iv=1,nsurf), FCskip,' StateLimit'
     WRITE(12,120) (Surf(2,iv),iv=1,nsurf), FCskip, ' DrainageEq'    
     WRITE(12,120) (Surf(3,iv),iv=1,nsurf), FCskip, ' DrainageCoef1'
     WRITE(12,120) (Surf(4,iv),iv=1,nsurf), FCskip, ' DrainageCoef2'
     WRITE(12,120) FCskip,FCskip,(GDDFull(iv),iv=1,nvegsurf),FCskip,FCskip,FCskip, ' GDDFull'
     WRITE(12,120) FCskip,FCskip,(SDDFull(iv),iv=1,nvegsurf),FCskip,FCskip,FCskip, ' SDDFull'
     WRITE(12,120) FCskip,FCskip,(LAImin(iv),iv=1,nvegsurf),FCskip,FCskip,FCskip, ' LAIMin'
     WRITE(12,120) FCskip,FCskip,(LAImax(iv),iv=1,nvegsurf),FCskip,FCskip,FCskip, ' LAIMax'
     WRITE(12,120) FCskip,FCskip,FCskip,PorMin_dec,FCSkip,FCskip,FCskip,FCskip, ' PorosityMin'
     WRITE(12,120) FCskip,FCskip,FCskip,PorMax_dec,FCSkip,FCskip,FCskip,FCskip, ' PorosityMax'
     WRITE(12,'(2f10.3,3i10,  3f10.3,a16)') FCskip,FCskip,LAItype(1:nvegsurf),FCskip,FCskip,FCskip, ' LAIEq'   
     WRITE(12,'(2f10.3,3f10.5,3f10.3,a16)') FCskip,FCskip,LAIPower(1,1:nvegsurf),FCskip,FCskip,FCskip, ' LAI_LeafGP1'
     WRITE(12,'(2f10.3,3f10.5,3f10.3,a16)') FCskip,FCskip,LAIPower(2,1:nvegsurf),FCskip,FCskip,FCskip, ' LAI_LeafGP2'
     WRITE(12,'(2f10.3,3f10.5,3f10.3,a16)') FCskip,FCskip,LAIPower(3,1:nvegsurf),FCskip,FCskip,FCskip, ' LAI_LeafOP1'
     WRITE(12,'(2f10.3,3f10.5,3f10.3,a16)') FCskip,FCskip,LAIPower(4,1:nvegsurf),FCskip,FCskip,FCskip, ' LAI_LeafOP2'
     WRITE(12,120) FCskip,FCskip,(MaxConductance(iv),iv=1,nvegsurf),FCskip,FCskip,FCskip, ' MaxCond'
     WRITE(12,120) (SoilDepth(iv),iv=1,(nsurf-1)),FCskip,FCskip, ' SoilDepth'
     WRITE(12,120) (soilstoreCap(iv),iv=1,(nsurf-1)), FCskip, FCskip, ' SoilStoreCap'
     WRITE(12,'(6f10.5,2f10.3,a16)') (SatHydraulicConduct(iv),iv=1,(nsurf-1)),FCskip,FCskip, ' SatHydraulicConduct'
     
     WRITE(12,120) (snowD(iv),iv=1,(nsurf-1)),FCskip,FCskip, ' SnowLimPatch'
     WRITE(12,120) SnowLimPaved,SnowLimBuild,FCskip,FCskip,FCskip,FCskip,FCskip,FCskip, ' SnowLimRemove'
     WRITE(12,120) (OHM_coef(1:nsurf,1,1)),OHM_coef(nsurf+1,1,1), ' OHM_a1_Sum_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,2,1)),OHM_coef(nsurf+1,2,1), ' OHM_a1_Sum_Dry'
     WRITE(12,120) (OHM_coef(1:nsurf,3,1)),OHM_coef(nsurf+1,3,1), ' OHM_a1_Win_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,4,1)),OHM_coef(nsurf+1,4,1), ' OHM_a1_Win_Dry'
     WRITE(12,120) (OHM_coef(1:nsurf,1,2)),OHM_coef(nsurf+1,1,2), ' OHM_a2_Sum_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,2,2)),OHM_coef(nsurf+1,2,2), ' OHM_a2_Sum_Dry'
     WRITE(12,120) (OHM_coef(1:nsurf,3,2)),OHM_coef(nsurf+1,3,2), ' OHM_a2_Win_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,4,2)),OHM_coef(nsurf+1,4,2), ' OHM_a2_Win_Dry'
     WRITE(12,120) (OHM_coef(1:nsurf,1,3)),OHM_coef(nsurf+1,1,3), ' OHM_a3_Sum_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,2,3)),OHM_coef(nsurf+1,2,3), ' OHM_a3_Sum_Dry'
     WRITE(12,120) (OHM_coef(1:nsurf,3,3)),OHM_coef(nsurf+1,3,3), ' OHM_a3_Win_Wet'
     WRITE(12,120) (OHM_coef(1:nsurf,4,3)),OHM_coef(nsurf+1,4,3), ' OHM_a3_Win_Dry'
     WRITE(12,120) (OHM_threshSW(1:nsurf)),OHM_threshSW(nsurf+1), ' OHMthreshold_SW'
     WRITE(12,120) (OHM_threshWD(1:nsurf)),OHM_threshWD(nsurf+1), ' OHMthreshold_WD'

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Snow parameters'//' -----'
     WRITE(12,'(a12,11a10)') 'Grid','RadMeltF','TempMeltF','tau_a','tau_f','PLimAlb','SDensMin','SDensMax', &
          'tau_r','CRWMin','CRWMax','PLimSnow'
     WRITE(12,'(a12,11f10.4)') SsG_YYYY,RadMeltFact,TempMeltFact,tau_a,tau_f,PrecipLimitAlb,SnowDensMin,SnowDensMax, &
          tau_r,CRWmin,CRWmax,PrecipLimit

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Conductance parameters'//' -----'
     WRITE(12,'(a12,12a10)') 'Grid','G1','G2','G3','G4','G5','G6','TH','TL','S1','S2','Kmax','gsModel'
     WRITE(12,'(a12,11f10.3,i3)') SsG_YYYY,G1,G2,G3,G4,G5,G6,TH,TL,S1,S2,Kmax,gsModel

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Energy-use parameters'//' -----'
     WRITE(12,'(a12,11a10)') 'Grid','NumCapita','BaseTHDD','QF_A_WD','QF_A_WE','QF_B_WD','QF_B_WE','QF_C_WD','QF_C_WE', &
          'AH_Min','AH_Slope','T_critic_Heating'
     WRITE(12,'(a12,11f10.3)') SsG_YYYY,NumCapita,BaseTHDD,QF_A(1:2),QF_B(1:2),QF_C(1:2), &
          AH_Min,AH_Slope_Heating,T_critic_Heating

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Water-use parameters'//' -----'
     WRITE(12,'(a12,10a10)') 'Grid','IeStart','IeEnd','IntWatUse','Faut', &
          'Ie_a1','Ie_a2','Ie_a3','Ie_m1','Ie_m2','Ie_m3'
     WRITE(12,'(a12,2i10,8f10.3)') SsG_YYYY,Ie_start,Ie_end,InternalWaterUse_h,Faut, &
          Ie_a(1:3),Ie_m(1:3)

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Weekly profiles'//' -----'
     WRITE(12,'(a12,7a10,  a16)') 'Grid','1_Sun','2_Mon','3_Tue','4_Wed','5_Thu','6_Fri','7_Sat', ' DayOfWeek'
     WRITE(12,'(a12,7f10.3,a16)') SsG_YYYY,DayWat(1:7), ' Irr allowed'
     WRITE(12,'(a12,7f10.3,a16)') SsG_YYYY,DayWatPer(1:7), ' Frac properties'

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Hourly profiles'//' -----'
     WRITE(12,'(a12,24i10,a20)') 'Grid',(iv,iv=0,23),'HourOfDay'
     WRITE(12,121) SsG_YYYY,AHProf(0:23,1), ' Anthrop heat WD'
     WRITE(12,121) SsG_YYYY,AHProf(0:23,2), ' Anthrop heat WE'
     WRITE(12,121) SsG_YYYY,WUProfM(0:23,1),' Manual water use WD'
     WRITE(12,121) SsG_YYYY,WUProfM(0:23,2),' Manual water use WE'
     WRITE(12,121) SsG_YYYY,WUProfA(0:23,1),' Auto. water use WD'
     WRITE(12,121) SsG_YYYY,WUProfA(0:23,2),' Auto. water use WE'
     WRITE(12,121) SsG_YYYY,SnowProf(0:23,1), ' Snow clearing WD'
     WRITE(12,121) SsG_YYYY,SnowProf(0:23,2), ' Snow clearing WE'

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Within-grid water distribution'//' -----'
     WRITE(12,'(9a10)') 'ToPaved','ToBldgs','ToEveTr','ToDecTr','ToGrass','ToBSoil','ToWater','ToROorSS'

     DO iv=1,(nsurf-1)
        WRITE(12,'(8f10.4)')(WaterDist(j,iv),j=1,nsurf+1)
     ENDDO

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Other parameters'//' -----'
     WRITE(12,'(a12,7a10)') 'Grid','FlowChange','ROToWater','PipeCap', &   
          'DrRate','Cover','MaxRes',&   
          'Trans'   
     WRITE(12,'(a12,7f10.3)') SsG_YYYY,FlowChange,RunoffToWater,PipeCapacity, &
          DRAINRT,RAINCOVER,RAINMAXRES, &
          Trans_Site

     WRITE(12,*) '----- '//TRIM(ADJUSTL(SsG_YYYY))//' Site parameters'//' -----'
     WRITE(12,'(a12,9a10)') 'Grid','lat','lon','tz','alt','SurfA_ha','z','NumCapita','z0_input','zd_input','StartDLS','EndDLS'
     WRITE(12,'(a12,4f10.4,f10.2,4f10.4,2i10)') SsG_YYYY,lat,lng*(-1.0),timezone,alt,SurfaceArea_ha,z,NumCapita,z0m_in,zdm_in, &
          startDLS,endDLS

     WRITE(12,*) ''



     CLOSE(12)


     
     

     
     IF(EmissionsMethod==1) THEN   
        IF(AH_min(1)==0.AND.Ah_slope_Heating(1)==0.AND.T_Critic_Heating(1)==0) THEN
           CALL ErrorHint(53,'Check QF calculation coefficients.',notUsed,notUsed,EmissionsMethod)
        ENDIF

     ELSEIF(EmissionsMethod==2) THEN   
        IF(SUM(QF_A)==0.AND.SUM(QF_B)==0.AND.SUM(QF_C)==0) THEN
           CALL ErrorHint(54,'Check QF calculation coefficients.',notUsed,notUsed,EmissionsMethod)
        ENDIF
     ENDIF

     
     IF(RoughLenMomMethod==1) THEN   
        
        IF(z0m<0.00001) CALL ErrorHint(1,'z0 value provided is very small (RoughLenMomMethod=1).',z0m,notUsed,GridID)
        IF(zdm<0.00001) CALL ErrorHint(1,'zd value provided is very small (RoughLenMomMethod=1).',zdm,notUsed,GridID)
        zzd=z-zdm
     ELSEIF(RoughLenMomMethod==3) THEN   
        
        IF(FAIBLdg<0) CALL ErrorHint(1,'FAI_Bldgs value provided is very small (RoughLenMomMethod=3)',FAIBldg,notUsed,GridID)
        IF(FAITree<0) CALL ErrorHint(1,'FAI_EveTr/DecTr value provided is very small (RoughLenMomMethod=3)',FAITree,notUsed,GridID)
     ENDIF

  ENDIF   


  
  
  
  IF (ir>0) THEN
     
     
     
     iy        = INT(MetForcingData(ir, 1,Gridiv)) 
     id        = INT(MetForcingData(ir, 2,Gridiv))
     it        = INT(MetForcingData(ir, 3,Gridiv))
     imin      = INT(MetForcingData(ir, 4,Gridiv))
     qn1_obs   = MetForcingData(ir, 5,Gridiv)      
     qh_obs    = MetForcingData(ir, 6,Gridiv)
     qe_obs    = MetForcingData(ir, 7,Gridiv)
     qs        = MetForcingData(ir, 8,Gridiv)
     qf        = MetForcingData(ir, 9,Gridiv)
     avu1      = MetForcingData(ir,10,Gridiv)
     avrh      = MetForcingData(ir,11,Gridiv)
     Temp_C    = MetForcingData(ir,12,Gridiv)
     Press_hPa = MetForcingData(ir,13,Gridiv)
     Precip    = MetForcingData(ir,14,Gridiv)
     avkdn     = MetForcingData(ir,15,Gridiv)
     snow_obs  = MetForcingData(ir,16,Gridiv)
     ldown_obs = MetForcingData(ir,17,Gridiv)
     fcld_obs  = MetForcingData(ir,18,Gridiv)
     wu_m3     = MetForcingData(ir,19,Gridiv)
     xsmd      = MetForcingData(ir,20,Gridiv)
     LAI_obs   = MetForcingData(ir,21,Gridiv)
     kdiff     = MetForcingData(ir,22,Gridiv)
     kdir      = MetForcingData(ir,23,Gridiv)
     wdir      = MetForcingData(ir,24,Gridiv)

     
     MetForcingData_grid=MetForcingData(:,:,Gridiv)

     
     dectime = REAL(id-1,KIND(1d0))+REAL(it,KIND(1d0))/24+REAL(imin,KIND(1d0))/(60*24)
     
     WRITE(iy_text,'(i4)') iy
     WRITE(id_text,'(i3)') id
     WRITE(it_text,'(i2)') it
     WRITE(imin_text,'(i2)') imin
     datetime = TRIM(ADJUSTL(iy_text))//' '//TRIM(ADJUSTL(id_text))//' '//TRIM(ADJUSTL(it_text))//' '//TRIM(ADJUSTL(imin_text))
     WRITE(GridID_text,'(i10)') GridID

     
     
     
     porosity(id) = ModelDailyState(Gridiv,cMDS_porosity)
     albDecTr(id) = ModelDailyState(Gridiv,cMDS_albDecTr)
     albEveTr(id) = ModelDailyState(Gridiv,cMDS_albEveTr)
     albGrass(id) = ModelDailyState(Gridiv,cMDS_albGrass)
     DecidCap(id) = ModelDailyState(Gridiv,cMDS_DecidCap)
     SnowfallCum  = ModelDailyState(Gridiv,cMDS_SnowfallCum)
     
     SnowDens(1:nsurf) = ModelDailyState(Gridiv,cMDS_SnowDens(1:nsurf))

     
     
     
     
     State(1:nsurf)          = ModelOutputData(ir-1,cMOD_State(1:nsurf),Gridiv)
     
     SoilMoist(1:nsurf)      = ModelOutputData(ir-1,cMOD_SoilState(1:nsurf),Gridiv)
     
     SnowFrac(1:nsurf)       = ModelOutputData(ir-1,cMOD_SnowFrac(1:nsurf), Gridiv)
     
     SnowPack(1:nsurf)       = ModelOutputData(ir-1,cMOD_SnowPack(1:nsurf), Gridiv)
     
     MeltWaterStore(1:nsurf) = ModelOutputData(ir-1,cMOD_SnowWaterState(1:nsurf), Gridiv)


     
     IF(StorageHeatMethod==4 .OR. StorageHeatMethod==14) THEN

        Ts5mindata(ir,1:ncolsESTMdata) = ESTMForcingData(ir,1:ncolsESTMdata,Gridiv)
        Ts5mindata_ir(1:ncolsESTMdata) = ESTMForcingData(ir,1:ncolsESTMdata,Gridiv)
        CALL ESTM_translate(Gridiv)
     ENDIF

  ENDIF 

  
  
  IF (ir==1.AND.iMB==1) THEN   
     CALL CheckInitial
  ENDIF
  

  RETURN

120 FORMAT (8f10.3, a16)  
121 FORMAT (a12,24f10.4, a20)

END SUBROUTINE SUEWS_Translate










SUBROUTINE SUEWS_TranslateBack(Gridiv,ir,irMax)

  USE allocateArray
  USE ColNamesInputFiles
  USE ColNamesModelDailyState
  USE data_in
  USE defaultnotUsed
  USE gis_data
  USE Initial
  USE mod_z
  USE resist
  USE snowMod
  USE sues_data
  USE time

  IMPLICIT NONE

  INTEGER::Gridiv,& 
       ir,        & 
       irMax        

  
  
  

  ModelDailyState(Gridiv,cMDS_porosity)    = porosity(id)
  ModelDailyState(Gridiv,cMDS_albDecTr)    = albDecTr(id)
  ModelDailyState(Gridiv,cMDS_albEveTr)    = albEveTr(id)
  ModelDailyState(Gridiv,cMDS_albGrass)    = albGrass(id)
  ModelDailyState(Gridiv,cMDS_DecidCap)    = DecidCap(id)
  ModelDailyState(Gridiv,cMDS_SnowfallCum) = SnowfallCum

  
  HDD_grids(:,:,Gridiv)    = HDD(:,:)
  GDD_grids(:,:,Gridiv)    = GDD(:,:)
  LAI_grids(:,:,Gridiv)    = LAI(:,:)
  WU_Day_grids(:,:,Gridiv) = WU_day(:,:)
  AlbDecTr_grids(:,Gridiv) = AlbDecTr(:)
  AlbEveTr_grids(:,Gridiv) = AlbEveTr(:)
  AlbGrass_grids(:,Gridiv) = AlbGrass(:)
  DecidCap_grids(:,Gridiv) = DecidCap(:)
  Porosity_grids(:,Gridiv) = Porosity(:)

  
  ModelDailyState(Gridiv,cMDS_SnowDens(1:nsurf)) = SnowDens(1:nsurf)
  ModelDailyState(Gridiv,cMDS_SnowAlb) = SnowAlb


  
  
  

  ModelOutputData(ir,cMOD_State(1:nsurf),Gridiv)           = State(1:nsurf)
  ModelOutputData(ir,cMOD_SoilState(1:nsurf),Gridiv)       = SoilMoist(1:nsurf)
  ModelOutputData(ir,cMOD_SnowFrac(1:nsurf), Gridiv)       = SnowFrac(1:nsurf)
  ModelOutputData(ir,cMOD_SnowPack(1:nsurf), Gridiv)       = SnowPack(1:nsurf)
  ModelOutputData(ir,cMOD_SnowWaterState(1:nsurf), Gridiv) = MeltWaterStore(1:nsurf)

  IF(ir==irMax) THEN   
     ModelOutputData(0,cMOD_State(1:nsurf),Gridiv)          = State(1:nsurf)
     ModelOutputData(0,cMOD_SoilState(1:nsurf),Gridiv)      = SoilMoist(1:nsurf)
     ModelOutputData(0,cMOD_SnowFrac(1:nsurf),Gridiv)       = SnowFrac(1:nsurf)
     ModelOutputData(0,cMOD_SnowPack(1:nsurf),Gridiv)       = SnowPack(1:nsurf)
     ModelOutputData(0,cMOD_SnowWaterState(1:nsurf),Gridiv) = MeltWaterStore(1:nsurf)
  ENDIF

  RETURN
endsubroutine SUEWS_TranslateBack

