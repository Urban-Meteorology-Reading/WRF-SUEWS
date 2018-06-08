SUBROUTINE SUEWS_cal_HorizontalSoilWater(&
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
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  IMPLICIT NONE
  INTEGER , PARAMETER :: nsurf=7

  REAL(KIND(1d0)), INTENT(in) ::sfr(nsurf)
  REAL(KIND(1d0)), INTENT(in) ::SoilStoreCap(nsurf)
  REAL(KIND(1d0)), INTENT(in) ::SoilDepth(nsurf)
  REAL(KIND(1d0)), INTENT(in) ::SatHydraulicConduct(nsurf)
  REAL(KIND(1d0)), INTENT(in) ::SurfaceArea
  REAL(KIND(1d0)), INTENT(in) ::NonWaterFraction
  REAL(KIND(1d0)), INTENT(in) ::tstep_real 

  REAL(KIND(1d0)),DIMENSION(nsurf), INTENT(inout) ::SoilMoist
  REAL(KIND(1d0)),DIMENSION(nsurf), INTENT(inout) ::runoffSoil

  REAL(KIND(1d0)), INTENT(out) :: runoffSoil_per_tstep



  INTEGER::jj,is
  REAL(KIND(1d0))::&
       DimenWaterCon1,DimenWaterCon2,&
       SoilMoistCap_Vol1,&
       SoilMoist_vol1,&
       SoilMoistCap_Vol2,&
       SoilMoist_vol2,&
       B_r1,MatPot1,Km1,&
       B_r2,MatPot2,Km2,&
       Distance,KmWeight,dI,&
       dI_dt

  REAL(KIND(1d0)), PARAMETER::&
       alphavG=0.0005,&  
       NUnits = 1   


  
  
  
  
  
  
  
  
  
  
  
  


  runoffSoil_per_tstep=0


  DO is=1,nsurf-1 

     IF (sfr(is)/=0.AND.SoilStoreCap(is)>0) THEN  
        
        DO jj=is+1,nsurf-1 

           IF (sfr(jj)/=0.AND.SoilStoreCap(jj)>0) THEN  
              

              
              
              SoilMoistCap_Vol1=SoilStoreCap(is)/SoilDepth(is) 
              SoilMoist_vol1=SoilMoist(is)/SoilDepth(is) 

              
              B_r1=0.1 
              
              

              
              
              IF(B_r1 >= SoilMoist_vol1) THEN
                 MatPot1 = 100000
                 Km1 = 0 
                 
              ELSE
                 DimenWaterCon1=(SoilMoist_vol1-B_r1)/(SoilMoistCap_Vol1-B_r1) 

                 
                 IF(DimenWaterCon1>0.99999) THEN
                    DimenWaterCon1=DimenWaterCon1-0.0001 
                 ENDIF

                 IF(DimenWaterCon1<0.00000005) THEN
                    DimenWaterCon1=DimenWaterCon1+0.0000001   
                 ENDIF

                 
                 
                 MatPot1=SQRT(1/DimenWaterCon1**2-1)/alphavG

                 
                 Km1=SatHydraulicConduct(is)*SQRT(DimenWaterCon1)*(1-(1-DimenWaterCon1**2)**0.5)**2

                 
                 IF(MatPot1>100000) THEN
                    MatPot1 = 100000  
                    Km1 = 0   
                 ENDIF

              ENDIF

              
              
              SoilMoistCap_Vol2=SoilStoreCap(jj)/SoilDepth(jj) 
              SoilMoist_vol2=SoilMoist(jj)/SoilDepth(jj) 

              
              B_r2=0.1 
              
              

              
              IF(B_r2>=SoilMoist_vol2) THEN
                 MatPot2=100000
                 Km2 = 0 
              ELSE
                 DimenWaterCon2=(SoilMoist_vol2-B_r2)/(SoilMoistCap_Vol2-B_r2) 

                 IF(DimenWaterCon2>0.99999) THEN
                    DimenWaterCon2=DimenWaterCon2-0.0001 
                 ENDIF

                 IF(DimenWaterCon2<0.00000005) THEN
                    DimenWaterCon2=DimenWaterCon2+0.0000001   
                 ENDIF

                 
                 
                 MatPot2=SQRT(1/DimenWaterCon2**2-1)/alphavG

                 
                 Km2=SatHydraulicConduct(jj)*SQRT(DimenWaterCon2)*(1-(1-DimenWaterCon2**2)**0.5)**2

                 IF((MatPot2)>100000) THEN
                    MatPot2=100000 
                    Km2 = 0   
                 ENDIF

              ENDIF

              

              
              
              Distance=(SQRT(sfr(is)*SurfaceArea/NUnits)+SQRT(sfr(jj)*SurfaceArea/NUnits))/2

              
              KmWeight=(sfr(is)*Km1+sfr(jj)*Km2)/(sfr(is)+sfr(jj))

              
              
              dI_dt=-(KmWeight)*(-MatPot1+MatPot2)/(Distance*1000)

              
              
              dI=dI_dt*tstep_real  

              
              

              
              IF ((SoilMoist(jj)>=dI*sfr(is)/sfr(jj)).AND.((SoilMoist(is)+dI)>=0)) THEN
                 SoilMoist(is)=SoilMoist(is)+dI
                 SoilMoist(jj)=SoilMoist(jj)-dI*sfr(is)/sfr(jj)  

                 
              ELSEIF ((SoilMoist(is)+dI)<0) THEN
                 SoilMoist(jj)=SoilMoist(jj)+SoilMoist(is)*sfr(is)/sfr(jj) 
                 SoilMoist(is)=0    

                 
              ELSE
                 SoilMoist(is)=SoilMoist(is)+SoilMoist(jj)*sfr(jj)/sfr(is)
                 SoilMoist(jj)=0
              ENDIF

              
              IF (SoilMoist(is)>SoilStoreCap(is)) THEN
                 runoffSoil(is)=runoffSoil(is)+(SoilMoist(is)-SoilStoreCap(is))
                 SoilMoist(is)=SoilStoreCap(is)
                 
                 
              ENDIF

              
              IF (SoilMoist(jj)>SoilStoreCap(jj)) THEN
                 runoffSoil(jj)=runoffSoil(jj)+(SoilMoist(jj)-SoilStoreCap(jj))
                 SoilMoist(jj)=SoilStoreCap(jj)
                 
                 
              ENDIF

           ENDIF  

        ENDDO  

        runoffSoil_per_tstep=runoffSoil_per_tstep+(runoffSoil(is)*sfr(is)/NonWaterFraction)  

     ENDIF  

     

  ENDDO 

END SUBROUTINE SUEWS_cal_HorizontalSoilWater


