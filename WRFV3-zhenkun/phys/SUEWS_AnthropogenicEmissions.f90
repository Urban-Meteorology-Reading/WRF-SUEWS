



















 SUBROUTINE AnthropogenicEmissions(EmissionsMethod,&
       id,it,imin,DLS,nsh,DayofWeek,ndays,&
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
 
 

  IMPLICIT NONE

  INTEGER,INTENT(in):: EmissionsMethod
  INTEGER,INTENT(in)::&
       id,&                
       it,&                
       imin,&              
       DLS,&               
       nsh,&               
       ndays,&             
       notUsedI

  INTEGER,DIMENSION(0:ndays,3),INTENT(in)::DayofWeek   

  REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(in):: HDD 

  REAL(KIND(1d0)),DIMENSION(2),INTENT(in)::&
       Qf_A,Qf_B,Qf_C,&    
       AH_MIN,&            
       AH_SLOPE_Heating,&  
       AH_SLOPE_Cooling,&
       T_CRITIC_Heating,&  
       T_CRITIC_Cooling,&  
       TrafficRate,&       
       QF0_BEU

   REAL(KIND(1d0)),DIMENSION(24*nsh,2),INTENT(in)::&
       AHProf_tstep,&
       HumActivity_tstep,&
       TraffProf_tstep,&
       PopProf_tstep

  REAL(KIND(1D0)),INTENT(in):: &
       EF_umolCO2perJ,&
       FcEF_v_kgkm,&       
       EnEF_v_Jkm,&        
       TrafficUnits,&      
       FrFossilFuel_Heat,& 
       FrFossilFuel_NonHeat,&
       MinQFMetab,&        
       MaxQFMetab,&        
       NumCapita,&         
       PopDensDaytime,&    
       PopDensNighttime,&  
       Temp_C,&            
       notUsed

  REAL(KIND(1D0)),INTENT(out):: &
       QF_SAHP,&
       Fc_anthro,&
       Fc_metab,Fc_traff,Fc_build

  INTEGER::&
       iu,&               
       ih

  REAL(KIND(1D0)):: &
       DP_x_RhoPop, DP_x_RhoPop_traff,&
       MinFcMetab, MaxFcMetab,&
       QF_build,QF_metab,QF_traff,&
       QF_SAHP_base,&     
       QF_SAHP_heat,&     
       QF_SAHP_ac,&       
       PopDorNorT,&       
       ActDorNorT,&       
       TraffDorNorT,&     
       AHDorNorT          

QF_SAHP=0
Fc_anthro=0
Fc_metab=0
Fc_traff=0
Fc_build=0


  
  ih=it-DLS
  IF(ih<0) ih=2

  
  iu=1   
  IF(DayofWeek(id,1)==1 .OR. DayofWeek(id,1)==7) iu=2  
  if ( EmissionsMethod > 0 ) then
    
    
    PopDorNorT   = PopProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)      
    ActDorNorT   = HumActivity_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)  
    TraffDorNorT = TraffProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)    
    AHDorNorT    = AHProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)       

    
    DP_x_RhoPop = AHDorNorT * NumCapita

    MinFcMetab = 8/5. * MinQFMetab
    MaxFcMetab = 8/5. * MaxQFMetab 

    
    
    
    

    QF_metab = (PopDensNighttime*MinQFMetab*((2-ActDorNorT)+(2-PopDorNorT))/2 + &
               PopDensDaytime*MaxQFMetab*((ActDorNorT-1)+(PopDorNorT-1))/2)/10000 

    Fc_metab = (PopDensNighttime*MinFcMetab*((2-ActDorNorT)+(2-PopDorNorT))/2 + &
               PopDensDaytime*MaxFcMetab*((ActDorNorT-1)+(PopDorNorT-1))/2)/10000 

  end if



  
  
  
  

  IF(EmissionsMethod==1 .OR. EmissionsMethod==4 .OR. EmissionsMethod==11 .OR. EmissionsMethod==14 .OR. &
     EmissionsMethod==21 .OR. EmissionsMethod==24 .OR. EmissionsMethod==31 .OR. EmissionsMethod==34) THEN   
     
     
     

     IF(Temp_C .lt. T_CRITIC_Heating(iu)) THEN
        QF_SAHP = (AH_MIN(iu) + AH_SLOPE_Heating(iu)*(T_CRITIC_Heating(iu)-Temp_C)) * AHDorNorT
     ELSE
        QF_SAHP = AH_MIN(iu) * AHDorNorT
     ENDIF

     
     
     QF_SAHP_base = AH_MIN(iu) * AHDorNorT         
     QF_SAHP_heat = QF_SAHP - QF_SAHP_base         
     QF_SAHP_ac   = 0                              


  ELSEIF(EmissionsMethod==2 .OR. EmissionsMethod==5 .OR. EmissionsMethod==12 .OR. EmissionsMethod==15 .OR. &
     EmissionsMethod==22 .OR. EmissionsMethod==25 .OR. EmissionsMethod==32 .OR. EmissionsMethod==35) THEN   
     
     
     
     QF_SAHP      = (Qf_a(iu)+Qf_b(iu)*HDD(id-1,2)+Qf_c(iu)*HDD(id-1,1)) * DP_x_RhoPop  
     QF_SAHP_base = (Qf_a(iu)) * DP_x_RhoPop                
     QF_SAHP_heat = (Qf_c(iu)*HDD(id-1,1)) * DP_x_RhoPop    
     QF_SAHP_ac   = (Qf_b(iu)*HDD(id-1,2)) * DP_x_RhoPop    


  ELSEIF(EmissionsMethod==3 .OR. EmissionsMethod==6 .OR. EmissionsMethod==13 .OR. EmissionsMethod==16 .OR. &
     EmissionsMethod==23 .OR. EmissionsMethod==26 .OR. EmissionsMethod==33 .OR. EmissionsMethod==36) THEN
     
     
     
     

     
     
     QF_SAHP_base = AH_MIN(iu) * AHDorNorT           

     IF(HDD(id-1,3) < T_CRITIC_Heating(iu)) THEN     
        QF_SAHP = (AH_MIN(iu) + AH_SLOPE_Heating(iu)*(T_CRITIC_Heating(iu)-HDD(id-1,3)))* AHDorNorT
        QF_SAHP_heat = QF_SAHP - QF_SAHP_base        
        QF_SAHP_ac = 0

     ELSEIF(HDD(id-1,3) > T_CRITIC_Cooling(iu)) THEN 
        QF_SAHP = (AH_MIN(iu) + AH_SLOPE_Cooling(iu)*(HDD(id-1,3)-T_CRITIC_Cooling(iu))) * AHDorNorT
        QF_SAHP_heat = 0
        QF_SAHP_ac = QF_SAHP - QF_SAHP_base          

     ELSE
        QF_SAHP = AH_MIN(iu) * AHDorNorT
     ENDIF

  ENDIF

  IF(EmissionsMethod>=1 .AND. EmissionsMethod<=3 .OR. EmissionsMethod>=11 .AND. EmissionsMethod<=13 .OR. &
     EmissionsMethod>=21 .AND. EmissionsMethod<=23 .OR. EmissionsMethod>=31 .AND. EmissionsMethod<=33) THEN

     
     IF((QF_SAHP_base-QF_metab)>0)THEN
        QF_build = (QF_SAHP_base-QF_metab)*QF0_BEU(iu) + QF_SAHP_heat + QF_SAHP_ac 
                                                                                   
     ELSE
        CALL ErrorHint(69,'QF metab exceeds base QF.',QF_metab,QF_SAHP_base,notUsedI)
        QF_build =  QF_SAHP_heat + QF_SAHP_ac + (QF_SAHP_base-QF_metab) 
     ENDIF

     
     
     
     Fc_build = QF_SAHP_heat*FrFossilFuel_Heat * EF_umolCO2perJ

     
     IF((QF_SAHP_base-QF_metab)>0)THEN
        Fc_build = Fc_build + (QF_SAHP_base-QF_metab)*QF0_BEU(iu)*FrFossilFuel_NonHeat * EF_umolCO2perJ
     ENDIF

     
     
     QF_traff = QF_SAHP_base*(1.0-QF0_BEU(iu)) - QF_metab

     
     Fc_traff = QF_traff/EnEF_v_Jkm * FcEF_v_kgkm*1e3*1e6/44

     
     Fc_anthro = Fc_metab + Fc_traff + Fc_build


  ELSEIF(EmissionsMethod>=4 .AND. EmissionsMethod<=6 .OR. EmissionsMethod>=14 .AND. EmissionsMethod<=16 .OR. &
     EmissionsMethod>=24 .AND. EmissionsMethod<=26 .OR. EmissionsMethod>=34 .AND. EmissionsMethod<=36) THEN
     

     
     IF(TrafficUnits==1) THEN            
       
       QF_traff = TrafficRate(iu)/(60*60*24) * EnEF_v_Jkm * TraffDorNorT
       
       Fc_traff = TrafficRate(iu)/(60*60*24) * FcEF_v_kgkm*1e3*1e6 /44 * TraffDorNorT

     ELSEIF(TrafficUnits==2) THEN        
       DP_x_RhoPop_traff = TraffDorNorT * NumCapita/10000   
       
       QF_traff = TrafficRate(iu)/(60*60*24) * EnEF_v_Jkm * DP_x_RhoPop_traff
       
       Fc_traff = TrafficRate(iu)/(60*60*24) * FcEF_v_kgkm*1e3*1e6/44 * DP_x_RhoPop_traff

     ELSE 
       CALL ErrorHint(75,'Check SUEWS_AnthropogenicEmissions.txt',TrafficUnits, notUsed, notUsedI)
     ENDIF

     
     QF_build = QF_SAHP_base + QF_SAHP_heat + QF_SAHP_ac

     
     Fc_build = QF_SAHP_heat*FrFossilFuel_Heat * EF_umolCO2perJ
     
     Fc_build = Fc_build + QF_SAHP_base*QF0_BEU(iu)*FrFossilFuel_NonHeat * EF_umolCO2perJ

     
     QF_SAHP_base = QF_SAHP_base + QF_traff + QF_metab

     
     QF_SAHP = QF_metab + QF_traff + QF_build
     
     Fc_anthro = Fc_metab + Fc_traff + Fc_build

  ENDIF

  RETURN
 ENDSUBROUTINE AnthropogenicEmissions



