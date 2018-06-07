SUBROUTINE AerodynamicResistance(&

                                
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

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  


  

  IMPLICIT NONE


  REAL(KIND(1d0)),INTENT(in)::ZZD
  REAL(KIND(1d0)),INTENT(in)::z0m
  REAL(KIND(1d0)),INTENT(in)::AVU1
  REAL(KIND(1d0)),INTENT(in)::L_mod
  REAL(KIND(1d0)),INTENT(in)::UStar
  REAL(KIND(1d0)),INTENT(in)::VegFraction

  INTEGER,INTENT(in)::AerodynamicResistanceMethod
  INTEGER,INTENT(in)::StabilityMethod
  INTEGER,INTENT(in)::RoughLenHeatMethod

  REAL(KIND(1d0)),INTENT(out)::RA 

  INTEGER, PARAMETER :: notUsedI=-55

  REAL(KIND(1d0)), PARAMETER :: &
       notUsed=-55.5,&
       k2=0.16,& 
       muu=1.46e-5 
  REAL(KIND(1d0))::stab_fn_heat,stab_fn_mom,&
       psym,&
       psyh,z0V


  
  IF(AerodynamicResistanceMethod==1) THEN
     RA=(LOG(ZZD/z0m)**2)/(k2*AVU1)

     
     
     
     
  ELSEIF(AerodynamicResistanceMethod==2) THEN  

     psym=stab_fn_mom(StabilityMethod,ZZD/L_mod,zzd/L_mod)
     psyh=stab_fn_heat(StabilityMethod,ZZD/L_mod,zzd/L_mod)

     
     IF (RoughLenHeatMethod==1) THEN 
        z0V=Z0m/10
     ELSEIF (RoughLenHeatMethod==2) THEN 
       	
        
        z0V=Z0m*EXP(2-(1.2-0.9*VegFraction**0.29)*(UStar*Z0m/muu)**0.25)
     ELSEIF (RoughLenHeatMethod==3) THEN
        z0V=Z0m*EXP(-20.) 
     ELSEIF (RoughLenHeatMethod==4) THEN
        z0V=Z0m*EXP(2-1.29*(UStar*Z0m/muu)**0.25) 
     ENDIF

     IF(Zzd/L_mod==0.OR.UStar==0) THEN
        RA=(LOG(ZZD/z0m)*LOG(ZZD/z0V))/(k2*AVU1) 
     ELSE
        RA=((LOG(ZZD/z0m)-PSYM)*(LOG(ZZD/z0V)-PSYH))/(K2*AVU1)
     ENDIF

     
  ELSEIF(AerodynamicResistanceMethod==3) THEN
     RA=(4.72*LOG(ZZD/z0m)**2)/(1 + 0.54*AVU1)
  ENDIF

  
  IF(RA>200) THEN   
     CALL errorHint(7,'In AerodynamicResistance.f95, calculated ra > 200 s m-1; ra set to 200 s m-1',RA,notUsed,notUsedI)
     RA=200
  ELSEIF(RA<10) THEN   
     CALL errorHint(7,'In AerodynamicResistance.f95, calculated ra < 10 s m-1; ra set to 10 s m-1',RA,notUsed,notUsedI)
     RA=10
     
     IF(avu1<0) WRITE(*,*) avu1,ra
  ENDIF

  RETURN
END SUBROUTINE AerodynamicResistance
