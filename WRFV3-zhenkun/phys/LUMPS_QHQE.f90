SUBROUTINE LUMPS_cal_QHQE(&
     veg_type,& 
     snowUse,id,qn1,qf,qs,Qm,Temp_C,Veg_Fr,avcp,Press_hPa,lv_J_kg,&
     tstep_real,DRAINRT,nsh_real,&
     Precip,RainMaxRes,RAINCOVER,sfr,LAI,LAImax,LAImin,&
     H_mod,& 
     E_mod,psyc_hPa,s_hPa,sIce_hpa,TempVeg,VegPhenLumps)
  
  
  
  
  
  
  
  
  
  
  
  
  USE AtmMoist_module,ONLY:psyc_const,slope_svp,slopeice_svp

  IMPLICIT NONE
  INTEGER,PARAMETER::ndays=366
  INTEGER,PARAMETER::NSurf=7
  INTEGER,PARAMETER::NVegSurf=3
  INTEGER,PARAMETER::ivConif=1
  INTEGER,PARAMETER::ivGrass=3

  INTEGER,INTENT(in) :: veg_type  
  INTEGER,INTENT(in) :: snowUse 
  INTEGER,INTENT(in) :: id 

  REAL(KIND(1d0)),INTENT(in) :: qn1
  REAL(KIND(1d0)),INTENT(in) :: qf
  REAL(KIND(1d0)),INTENT(in) :: qs
  REAL(KIND(1d0)),INTENT(in) :: Qm
  REAL(KIND(1d0)),INTENT(in) :: Temp_C
  REAL(KIND(1d0)),INTENT(in) :: Veg_Fr
  REAL(KIND(1d0)),INTENT(in) :: avcp
  REAL(KIND(1d0)),INTENT(in) :: Press_hPa
  REAL(KIND(1d0)),INTENT(in) :: lv_J_kg
  REAL(KIND(1d0)),INTENT(in) :: tstep_real 
  REAL(KIND(1d0)),INTENT(in) :: DRAINRT
  REAL(KIND(1d0)),INTENT(in) :: nsh_real
  REAL(KIND(1d0)),INTENT(in) :: Precip
  REAL(KIND(1d0)),INTENT(in) :: RainMaxRes
  REAL(KIND(1d0)),INTENT(in) :: RAINCOVER

  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) :: sfr
  REAL(KIND(1D0)),DIMENSION(-4:NDAYS,NVEGSURF),INTENT(in) :: LAI
  REAL(KIND(1d0)),DIMENSION(3),INTENT(in) :: LAImax
  REAL(KIND(1d0)),DIMENSION(3),INTENT(in) :: LAImin    

  REAL(KIND(1d0)),INTENT(out) ::H_mod
  REAL(KIND(1d0)),INTENT(out) ::E_mod 
  REAL(KIND(1d0)),INTENT(out) ::psyc_hPa 
  REAL(KIND(1d0)),INTENT(out) ::s_hPa
  REAL(KIND(1d0)),INTENT(out) ::sIce_hpa
  REAL(KIND(1d0)),INTENT(out) ::TempVeg 
  REAL(KIND(1d0)),INTENT(out) ::VegPhenLumps
  
  
  REAL(KIND(1d0)),DIMENSION(3) :: sfrVeg
  REAL(KIND(1d0)),DIMENSION(3) :: LAIDay
  REAL(KIND(1d0))::VegPhen,VegMax,VegMin,&   
       psyc_s,&       
       alpha_sl,alpha_in,&    	  
       beta,&                      
       alpha_qhqe,RAINRES,RainBucket,tlv

  tlv=lv_J_kg/tstep_real 
  
  VegPhenLumps=0
  
  RainBucket=0.

  sfrVeg=sfr(ivConif+2:ivGrass+2)

  LAIDay= LAI(id-1,:)

  
  s_hPa=slope_svp(Temp_C)
  psyc_hPa=psyc_const(avcp,Press_hPa,lv_J_kg)
  psyc_s=psyc_hPa/s_hPa

  
  
  IF (snowUse==1) THEN
     IF (Temp_C<=0) THEN
        sIce_hpa=slopeIce_svp(Temp_C)
     ELSE
        sIce_hpa=slope_svp(Temp_C)
     ENDIF
     psyc_s=psyc_hPa/sIce_hPa   
  ENDIF

  
  

  
  
  

  
  
  
  
  
  
  
  

  
  
  
  
  VegPhen=DOT_PRODUCT(sfrVeg,LAIDay)
  VegMax=DOT_PRODUCT(sfrVeg,LAImax)
  VegMin=DOT_PRODUCT(sfrVeg,LAImin)

  
  
  
  
  

  IF(VegMax<=0.01000) THEN   
     TempVeg=0
  ELSE
     VegPhenLumps=(VegPhen)/(VegMax)
     TempVeg=Veg_Fr*VegPhenLumps   
  ENDIF

  IF (TempVeg>0.9000) THEN   
     beta = (20-3)*TempVeg+3
     alpha_qhqe=TempVeg*0.8+0.2
  ELSE
     beta=3
     IF(veg_type==1) THEN   
        alpha_sl=0.686
        alpha_in=0.189
     ELSEIF(veg_type==2) THEN   
        alpha_sl=0.610
        alpha_in=0.222
     ENDIF
     alpha_qhqe=TempVeg*alpha_sl+alpha_in
  ENDIF

  
  H_mod= (1-alpha_qhqe)
  H_mod= ((1-alpha_qhqe)+psyc_s)
  H_mod= ((1-alpha_qhqe)+psyc_s)/(1+psyc_s)
  H_mod= (qn1+qf-qs-Qm)
  H_mod= ((1-alpha_qhqe)+psyc_s)/(1+psyc_s)*(qn1+qf-qs-Qm)-beta   
  E_mod= (alpha_qhqe/(1+psyc_s)*(qn1+qf-qs-Qm))+beta              

  
  
  IF (E_MOD>0.) RainBucket=RainBucket-E_MOD/tlv   
  IF (Temp_C>0.) RainBucket=RainBucket - DRAINRT/nsh_real  
  IF (RainBucket<0.) RainBucket=0.
  IF (Precip>0) RainBucket=MIN(RainMaxRes,RainBucket+Precip)

  RAINRES = RainBucket
  IF (RAINRES>RAINCOVER) RAINRES=RAINCOVER


  RETURN

END SUBROUTINE LUMPS_cal_QHQE
