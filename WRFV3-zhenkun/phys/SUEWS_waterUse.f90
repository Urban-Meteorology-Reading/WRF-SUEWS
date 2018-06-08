













SUBROUTINE SUEWS_cal_WaterUse(&
     nsh_real,& 
     SurfaceArea,sfr,&
     IrrFracConif,IrrFracDecid,IrrFracGrass,&
     DayofWeek_id,WUProfA_tstep,WUProfM_tstep,&
     InternalWaterUse_h,HDD_id,WU_Day_id,&
     WaterUseMethod,NSH,it,imin,DLS,&
     WUAreaEveTr_m2,WUAreaDecTr_m2,& 
     WUAreaGrass_m2,WUAreaTotal_m2,&
     wu_EveTr,wu_DecTr,wu_Grass,wu_m3,int_wu,ext_wu)

  
  
  
  
  

  IMPLICIT NONE
  INTEGER,PARAMETER:: nsurf   = 7
  
  
  INTEGER,PARAMETER::ConifSurf = 3
  INTEGER,PARAMETER::DecidSurf = 4
  INTEGER,PARAMETER::GrassSurf = 5 
  
  
  
  
  
  
  

  REAL(KIND(1d0)),INTENT(in):: &
       nsh_real,&
       SurfaceArea,& 
       sfr(nsurf),& 
       IrrFracConif,&
       IrrFracDecid,&
       IrrFracGrass,&
       WUProfA_tstep(24*NSH,2),& 
       WUProfM_tstep(24*NSH,2),& 
       InternalWaterUse_h,& 
       HDD_id(6),& 
       WU_Day_id(9) 

  INTEGER,INTENT(in):: &
       DayofWeek_id(3),& 
       WaterUseMethod,& 
                                
                                
                                
       NSH,&
       it,& 
       imin,& 
       DLS 
  


  REAL(KIND(1d0)),INTENT(out):: &
       WUAreaEveTr_m2,&
       WUAreaDecTr_m2,&
       WUAreaGrass_m2,&
       WUAreaTotal_m2,&
       wu_EveTr,&
       wu_DecTr,&
       wu_Grass,&
       wu_m3,&
       int_wu,&
       ext_wu


  REAL(KIND(1d0)):: &
       InternalWaterUse,&    
       WuFr=1,&
       wu
  INTEGER:: ih   
  INTEGER:: iu   
  REAL(KIND(1d0)),PARAMETER::NAN=-999.
  REAL(KIND(1d0)):: OverUse

  
  
  OverUse=0

  
  
  
  IF (WaterUseMethod==1) THEN   
     
     WUAreaEveTr_m2 = IrrFracConif*sfr(ConifSurf)*SurfaceArea
     WUAreaDecTr_m2 = IrrFracDecid*sfr(DecidSurf)*SurfaceArea
     WUAreaGrass_m2 = IrrFracGrass*sfr(GrassSurf)*SurfaceArea
     WUAreaTotal_m2 = WUAreaEveTr_m2 + WUAreaDecTr_m2 + WUAreaGrass_m2

     
     wu_EveTr=0
     wu_DecTr=0
     wu_Grass=0
     IF(wu_m3==NAN.OR.wu_m3==0) THEN 
        wu_m3=0
        wu=wu_m3
     ELSE                            
        IF (WUAreaTotal_m2>0) THEN
           wu = (wu_m3/WUAreaTotal_m2*1000)  
           IF (WUAreaEveTr_m2>0) THEN
              wu_EveTr=wu                    
              wu_EveTr=wu_EveTr*IrrFracConif 
           ENDIF
           IF (WUAreaDecTr_m2>0) THEN
              wu_DecTr=wu                        
              wu_DecTr=wu_DecTr*IrrFracDecid     
           ENDIF
           IF (WUAreaGrass_m2>0) THEN
              wu_Grass=wu                    
              wu_Grass=wu_Grass*IrrFracGrass 
           ENDIF
           wu = (wu_m3/SurfaceArea*1000)     
        ENDIF
     ENDIF

     
     
  ELSEIF (WaterUseMethod==0) THEN   

     
     ih=it-DLS
     IF (ih<0) ih=23

     
     iu=1     
     
     IF(DayofWeek_id(1)==1.OR.DayofWeek_id(1)==7) THEN
        iu=2  
     ENDIF

     

     
     wu_EveTr = WUProfA_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(2)   
     wu_DecTr = WUProfA_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(5)   
     wu_Grass = WUProfA_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(8)   

     
     WuFr=1 
     
     IF(HDD_id(5)>2) THEN    
        WuFr=0   
     ENDIF

     
     wu_EveTr = wu_EveTr + (WuFr*WUProfM_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(3)) 
     wu_DecTr = wu_DecTr + (WuFr*WUProfM_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(6)) 
     wu_Grass = wu_Grass + (WuFr*WUProfM_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)*WU_Day_id(9)) 

     
     
     
     
     wu_EveTr=wu_EveTr*IrrFracConif  
     wu_DecTr=wu_DecTr*IrrFracDecid  
     wu_Grass=wu_Grass*IrrFracGrass  

     
     wu = wu_EveTr*sfr(ConifSurf) + wu_DecTr*sfr(DecidSurf) + wu_Grass*sfr(GrassSurf)

  ENDIF   
  

  
  
  InternalWaterUse = InternalWaterUse_h/nsh_real

  
  ext_wu = wu-(InternalWaterUse+OverUse)
  
  IF (ext_wu<0) THEN
     overUse=ABS(ext_wu)
     ext_wu=0
  ELSE
     OverUse=0
  ENDIF

  int_wu = wu-ext_wu

  
  IF(ext_wu/=0.AND.wu/=0) THEN
     wu_EveTr = wu_EveTr*ext_wu/wu
     wu_DecTr = wu_DecTr*ext_wu/wu
     wu_Grass = wu_Grass*ext_wu/wu
  ENDIF

endsubroutine SUEWS_cal_WaterUse



