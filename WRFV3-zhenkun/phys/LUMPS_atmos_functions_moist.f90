MODULE AtmMoist_module
  IMPLICIT NONE

CONTAINS
  
  
  
  SUBROUTINE LUMPS_cal_AtmMoist(&
       Temp_C,Press_hPa,avRh,dectime,&
       lv_J_kg,lvS_J_kg,&
       es_hPa,Ea_hPa,VPd_hpa,VPD_Pa,dq,dens_dry,avcp,air_dens)
    
    
    
    
    
    

    IMPLICIT NONE
    REAL(KIND(1d0))::vap_dens

    REAL(KIND(1d0)),INTENT(in)::&
         Temp_C,&
         Press_hPa,&
         avRh,dectime
    REAL(KIND(1d0)),INTENT(out)::&
         lv_J_kg,&
         lvS_J_kg,&
         es_hPa,&
         Ea_hPa,&
         VPd_hpa,& 
         VPD_Pa,& 
         dq,&
         dens_dry,& 
         avcp,&
         air_dens

     


    REAL (KIND(1d0)),PARAMETER:: &
                                
                                
                                
                                
                                
                                
                                
         gas_ct_dry    = 8.31451/0.028965,&  
         gas_ct_wv     = 8.31451/0.0180153 
    
    INTEGER::from=1

    
    es_hPa = sat_vap_press(Temp_C,Press_hPa,from,dectime) 

    
    Ea_hPa=avRh/100*es_hPa



    VPd_hpa=es_hPa-ea_hpa           
    VPD_Pa=(es_hPa*100)-(Ea_hPa*100)

    dq=(spec_hum_def(vpd_hPa,Press_hPa)) 

    
    vap_dens=(Ea_hPa*100/((Temp_C+273.16)*gas_ct_wv))

    
    dens_dry=((Press_hPa-Ea_hPa)*100)/(gas_ct_dry*(273.16+Temp_C))

    
    air_dens=(Press_hPa*100)/(gas_ct_dry*(Temp_C+273.16))

    
    avcp=spec_heat_beer(Temp_C,avRh,vap_dens,dens_dry)

    
    lv_J_kg=lat_vap(Temp_C,Ea_hPa,Press_hPa,avcp,dectime)
    
    
    IF(Temp_C<0.000) THEN
       lvS_J_kg=lat_vapSublim(Temp_C,Ea_hPa,Press_hPa,avcp)
    ELSE 
       lvS_J_kg = 2834000
    ENDIF

    IF(press_hPa<900) THEN
       CALL ErrorHint(46, 'Function LUMPS_cal_AtmMoist',press_hPa,-55.55, -55)
    ENDIF
    RETURN
  END SUBROUTINE LUMPS_cal_AtmMoist

  
  
  
  
  
  
  
  

  FUNCTION sat_vap_press(Temp_c,PRESS_hPa,from,dectime) RESULT(es_hPa)
    
    
    IMPLICIT NONE

    REAL(KIND(1d0))::temp_C,press_hpa,dectime
    REAL(KIND(1d0))::e_mb,f,press_kpa,es_hPA
    INTEGER:: from,iv
    INTEGER,PARAMETER::notUsedI=-55

    
    IF(ABS(temp_C)<0.001000)THEN
       IF(from==1) THEN  
          iv=INT(press_Hpa)
          CALL errorHint(29,'Function sat_vap_press: temp_C, dectime,press_Hpa = ',temp_C, dectime,iv)

       ENDIF
       temp_C=0.001000
    ENDIF

    Press_kPa=Press_hPa/10

    IF(Temp_C<50.AND.Temp_C>-40)THEN
       
       

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
    
    
    IMPLICIT NONE

    REAL(KIND(1d0))::e_mb,f,temp_C,press_hpa,press_kpa,es_hPA,dectime
    INTEGER:: from,iv
    INTEGER,PARAMETER::notUsedI=-55

    
    IF(ABS(temp_C)<0.001000)THEN
       IF(from==1) THEN  
          iv=INT(press_Hpa)
          CALL errorHint(29,'Function sat_vap_press: temp_C, dectime,press_Hpa = ',temp_C, dectime,iv)

       ENDIF
       temp_C=0.001000
    ENDIF

    Press_kPa=Press_hPa/10

    IF(Temp_C<50.AND.Temp_C>-40)THEN
       e_mb=6.1115*EXP(((23.036-Temp_C/333.7)*Temp_C)/(Temp_C+279.82))
       f=1.00022+Press_kPa*(3.83E-6+6.4E-10*Temp_C**2) 
       es_hPa=e_mb*f

    ELSE
       CALL ErrorHint(28,'FUNCTION sat_vap_press: [Temperature is out of range], Temp_C,dectime',Temp_C,dectime,notUsedI)

    ENDIF

    RETURN
  END FUNCTION sat_vap_pressIce

  
  
  
  FUNCTION spec_hum_def(vpd_hPa,press_hPa) RESULT(dq)
    
    IMPLICIT NONE
    REAL(KIND(1d0))           :: press_hPa,vpd_hPa,dq
    REAL(KIND(1d0)),PARAMETER :: epsil_gkg = 621.97 
    dq=epsil_gkg*vpd_hPa/press_hPa 
  END FUNCTION spec_hum_def

  
  FUNCTION spec_heat_beer(Temp_C,rh,rho_v,rho_d) RESULT (cp)
    
    
    
    
    

    
    IMPLICIT NONE

    REAL(KIND(1d0))::cp,cpd,cpm,rho_v,rho_d,rh,temp_C

    
    CPd = 1005.0+((Temp_C+23.16)**2)/3364.0 

    
    cpm = 1859 + 0.13*rH+ (19.3+0.569*rH)*(Temp_C/100.) + &
         (10.+0.5*rH)*(Temp_C/100.)**2

    IF(ABS(rho_d)<0.000100.OR.ABS(rho_v)<0.000100.OR.ABS(rho_d+rho_v)<0.000100)THEN
       CALL ErrorHint(42,'spec-heat_beer',rho_v,rho_d,INT(Temp_C))
    ENDIF

    cp=cpd*(rho_d/(rho_d+rho_v))+cpm*(rho_v/(rho_d+rho_v))

    
  END FUNCTION spec_heat_beer

  
  
  
  

  FUNCTION Lat_vap(Temp_C,Ea_hPa,Press_hPa,cp,dectime) RESULT (lv_J_kg)
    
    

    
    
    

    IMPLICIT NONE
    REAL(KIND(1d0))::cp,lv_J_kg,ea_fix,tw,&
         incr,es_tw,psyc,ea_est,press_hPa,ea_HPa, temp_C,dectime
    

    LOGICAL:: switch1=.FALSE.,switch2=.FALSE.
    INTEGER:: ii,from=2
    REAL(KIND(1d0)),PARAMETER::notUsed=-55.55

    ea_fix=ea_hPa

    

    

    lv_J_kg=(2500.25-2.365*temp_C)*1000  


    tw=Temp_C/2.  
    incr=3.
    DO ii=1,100
       IF(Press_hPa<900) THEN
          CALL ErrorHint(45,'function Lat_vap',Press_hPA,notUsed,ii)
       ENDIF



       es_tw=sat_vap_press(Tw,Press_hPa,from,dectime)  



       IF(Press_hPa<900) THEN
          CALL ErrorHint(45,'function Lat_vap - 2',Press_hPA,notUsed,ii)
       ENDIF

       psyc=psyc_const(cp,Press_hPa,lv_J_kg) 

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
    
    

    

    IMPLICIT NONE
   
    REAL(KIND(1d0))::lvS_J_kg,temp_C,tw,incr,Ea_hPa,Press_hPa,cp
   
   
   
   

    
    

   

    lvS_J_kg=(2834.1-0.29*temp_C)*1e3 

    tw=Temp_C/2.  
    incr=3.
    Press_hPa=Press_hPa
    Ea_hPa=Ea_hPa
    cp=cp

    

   

     

  
  

  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  

   
  END FUNCTION Lat_vapSublim



  
  
  
  

  FUNCTION psyc_const(cp,Press_hPa,lv_J_kg) RESULT(psyc_hPa) 
    USE gas

    IMPLICIT NONE
    REAL (KIND(1d0))::cp,lv_J_kg,press_hPa,psyc_hpa

    
    IF(cp*press_hPa<900.OR.lv_J_kg<10000)THEN
       CALL errorHint(19,'in psychrometric constant calculation:  cp [J kg-1 K-1], p [hPa], Lv [J kg-1]',cp,Press_hPa,INT(lv_J_kg))
    ENDIF

    psyc_hPa=(cp*press_hPa)/(epsil*lv_J_kg)

    
    
    
    
    
  END FUNCTION psyc_const

  

  FUNCTION dewpoint(ea_hPa) RESULT(Temp_C_dew)
    
    
    
    
    

    REAL(KIND(1d0))::ea_hPa,temp_C_dew
    Temp_C_dew = (237.3 * LOG(ea_hPa/6.1078)) / (17.27 - (LOG(ea_hPa/6.1078)))
  END FUNCTION dewpoint
  
  FUNCTION slope_svp(temp_C) RESULT(s_hPa)
    
    

    IMPLICIT  NONE

    REAL (KIND(1d0)):: b1,b2,b3,b4,b5,b6,b7,S_hPa,temp_C
    B1=4.438099984D-1
    B2=2.857002636D-2
    B3=7.938054040D-4
    B4=1.215215065D-5
    B5=1.036561403D-7
    B6=3.532421810D-10
    B7=-7.090244804D-13

    
    
    S_hPa=B1+temp_C*(B2+temp_C*(B3+temp_C*(B4+temp_C*(B5+temp_C*(B6+B7*temp_C)))))

    
    
    
    
    RETURN
  END FUNCTION slope_svp

  
  FUNCTION slopeIce_svp(temp_C) RESULT(s_hPa)
    
    

    IMPLICIT  NONE

    REAL (KIND(1d0)):: b1,b2,b3,b4,b5,b6,b7,S_hPa,temp_C

    B1=5.030305237D-1
    B2=3.773255020D-2
    B3=1.267995369D-3
    B4=2.477563108D-5
    B5=3.005693132D-7
    B6=2.158542548D-9
    B7=7.131097725D-12

    
    
    S_hPa=B1+temp_C*(B2+temp_C*(B3+temp_C*(B4+temp_C*(B5+temp_C*(B6+B7*temp_C)))))

    RETURN
  END FUNCTION slopeIce_svp

  
  FUNCTION qsatf(T,PMB) RESULT(qsat)
    
    
    

    REAL (KIND(1D0))::T,es,qsat,PMB

    REAL (KIND(1D0)),PARAMETER::&

                                
         A=6.106,&
         B=17.27,&
         C=237.3,&
         molar=0.028965,& 
         molar_wat_vap=0.0180153 


    IF(t.GT.55)THEN
       CALL ErrorHint(34,'Function qsatf',T,0.00D0,-55)
    ENDIF

    ES = A*dEXP(B*T/(C+T))
    qsat = (molar_wat_vap/molar)*ES/PMB
  END FUNCTION qsatf

END MODULE AtmMoist_module


