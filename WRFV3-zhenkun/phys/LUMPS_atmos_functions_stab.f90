












SUBROUTINE STAB_lumps(&

                                
     StabilityMethod,&
     dectime,& 
     zzd,&     
     z0M,&     
     zdm,&     
     avU1,&    
     Temp_C,&  
     h_init,    & 
                                
     L_MOD,& 
     Tstar,& 
     UStar,& 
     psim)

  
  
  
  
  
  
  
  IMPLICIT NONE
  INTEGER,INTENT(in):: StabilityMethod


  REAL(KIND(1d0)),INTENT(in)::dectime 
  REAL(KIND(1d0)),INTENT(in)::zzd     
  REAL(KIND(1d0)),INTENT(in)::z0M     
  REAL(KIND(1d0)),INTENT(in)::zdm     
  REAL(KIND(1d0)),INTENT(in)::avU1    
  REAL(KIND(1d0)),INTENT(in)::Temp_C    
  REAL(KIND(1d0)),INTENT(in)::h_init    


  REAL(KIND(1d0)),INTENT(out)::L_MOD
  REAL(KIND(1d0)),INTENT(out)::Tstar
  REAL(KIND(1d0)),INTENT(out)::UStar
  REAL(KIND(1d0)),INTENT(out)::psim   

  REAL(KIND(1d0))::stab_fn_mom,&
       G_T_k,&
       KUZ,&
       LOLD,&
       zL,&
       z0l,&
       psimz0,&
       h
  REAL(KIND(1d0)),PARAMETER :: &
       k=0.4,&             
       grav=9.80665,&  
       notUsedI=-55

  INTEGER :: i

  LOGICAL :: debug=.FALSE.

  IF(debug) WRITE(*,*)StabilityMethod,z0M,avU1,h_init,UStar,L_MOD
  G_T_k=(Grav/(Temp_C+273.16))*k 
  KUZ=k*AvU1                     
  IF(zzd<0) CALL ErrorHint(32,'Windspeed Ht too low relative to zdm [Stability calc]- values [z-zdm, zdm]',Zzd,zdm,notUsedI)

  UStar=KUZ/LOG(Zzd/Z0M)      
  IF ( ABS(h_init)<0.001 ) THEN    
     h=0.001
  ELSE
     h=h_init
  END IF
  Tstar=(-H/UStar)
  L_MOD=(UStar**2)/(G_T_K*Tstar)


  IF(LOG(zzd/z0M)<0.001000) CALL ErrorHint(17,'In stability subroutine, (z-zd) < z0.',zzd,z0m,notUsedI)
  DO i=1,330 
     LOLD=L_MOD
     zL=zzd/L_MOD
     z0L=z0M/L_MOD  

     IF (zL>2)THEN
        CALL ErrorHint(73,'LUMPS_atmos_functions_stab.f95: stability parameter, UStar',zL,UStar,notUsedI)
        RETURN 
        
     END IF

     psim=stab_fn_mom(StabilityMethod,zL,zL)
     psimz0=stab_fn_mom(StabilityMethod,zL,z0L)


     UStar=KUZ/(LOG(Zzd/Z0M)-PSIM+psimz0) 

     IF(UStar<0.001000)THEN       
        UStar=KUZ/(LOG(Zzd/Z0M))
        CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] zl,dectime',zl,dectime,notUsedI)
        CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] z0l,UStar',z0l,UStar,notUsedI)
        CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] psim,psimz0',psim,psimz0,notUsedI)
        CALL ErrorHint(30,'SUBROUTINE STAB_lumps:[ u*< 0.001] AVU1,log(zzd/z0m)',AVU1,LOG(zzd/z0m),notUsedI)

        RETURN
     ENDIF

     tstar=(-H/UStar)
     L_MOD=(UStar**2)/(G_T_K*Tstar)

     IF(ABS(LOLD-L_MOD)<0.01)THEN
        IF (ABS(L_MOD)>1e6) L_MOD = L_MOD/ABS(L_MOD)*1e6
        RETURN
     ENDIF
  ENDDO

  RETURN
END SUBROUTINE STAB_lumps



FUNCTION stab_fn_mom(StabilityMethod,ZL,zl_f) RESULT(psym)
  
  
  
  

  
  

  IMPLICIT NONE
  REAL(KIND(1d0)),PARAMETER :: &


       neut_limit=0.001000 
  

  REAL (KIND(1d0)):: piover2,psym,zl,zl_f,x,x2
  INTEGER ::StabilityMethod

  PIOVER2=ACOS(-1.)/2.
  
  IF(ABS(zL)<neut_limit) THEN
     psym=0
  ELSEIF(zL<-neut_limit) THEN    

     IF(StabilityMethod==1)THEN     
        psym=((1.-16.*zl_f)**0.25)-1
     ELSEIF(StabilityMethod==2) THEN 
        X=(1.-(15.2*zl_f))**0.25
        X2=LOG((1+(X**2.))/2.)
        PSYM=(2.*LOG((1+X)/2.))+X2-(2.*ATAN(X))+PIOVER2
     ELSEIF(StabilityMethod==3)THEN 
        psym=0.6*(2)*LOG((1+(1-16*zl_f)**0.5)/2)
     ELSEIF(StabilityMethod==4) THEN 
        x=(1-19.3*zl_f)**(-0.25)
        X2=LOG((1+(X**2.))/2.)
        PSYM=(2.*LOG((1+X)/2.))+X2-(2.*ATAN(X))+PIOVER2
     ELSEIF(StabilityMethod==7) THEN 
        X=(1+(28.*zl_f))**0.25
        X2=LOG((1+X**2.)/2.)
        PSYM=(2.*LOG((1+X)/2.))+X2-(2.*ATAN(X))+PIOVER2
     ELSEIF(StabilityMethod==5)THEN 
        IF(zl_f>=-0.16)THEN
           x=1+1.38*zl_f
        ELSE
           x=0.42*(-1)*zl_f**0.333
        ENDIF
        X2=LOG((1+(X**2.))/2.)
        PSYM=(2.*LOG((1+X)/2.))+X2-(2.*ATAN(X))+PIOVER2

     ELSEIF(StabilityMethod==6)THEN 
        IF(zl_f>=0.06)THEN
           x=1
        ELSE
           x=((-1)*zl_f/0.06)**0.25
        ENDIF
        X2=LOG((1+(X**2.))/2.)
        PSYM=(2.*LOG((1+X)/2.))+X2-(2.*ATAN(X))+PIOVER2
     ENDIF

  ELSEIF(zL>neut_limit) THEN            

     IF(StabilityMethod==1)THEN         
        psym=(-4.8)*zl_f
     ELSEIF(StabilityMethod==2)THEN     
        IF ( zl_f >1000. ) THEN
           zl_f=1000.
        END IF
        PSYM=(-17.*(1.-EXP(-0.29*zl_f)))
     ELSEIF(StabilityMethod==4)THEN 
        
        psym=(-6)*zl_f   
     ELSEIF(StabilityMethod==3)THEN 
        psym=(-6)*LOG(1+zl_f)

     ENDIF
  ENDIF
  RETURN
END FUNCTION stab_fn_mom




FUNCTION stab_fn_heat(StabilityMethod,ZL,zl_f) RESULT (psyh)
  
  IMPLICIT NONE
  REAL(KIND(1d0)),PARAMETER :: &


       neut_limit=0.001000 
  

  REAL (KIND(1d0)):: zl,zl_f,psyh,x
  INTEGER :: StabilityMethod

  IF(ABS(zl)<neut_limit)THEN      
     psyh=0
  ELSEIF(zL<-neut_limit) THEN     
     IF(StabilityMethod==3)THEN
        
        psyh=0.6*(2)*LOG((1+(1-16*zl_f)**0.5)/2)
     ELSE

        IF(StabilityMethod==4)THEN 
           x=0.95*(1.-11.6*zl_f)**(-0.5)
        ELSEIF(StabilityMethod==7) THEN
           x=(1-(28.*ZL))**0.25
        ELSEIF(StabilityMethod==2)THEN 
           x=0.95*(1.-15.2*zl_f)**0.5
        ENDIF
        PSYH=2*LOG((1+x**2)/2)
     ENDIF

  ELSE IF (zL>neut_limit) THEN    
     IF ( zL<=1 ) THEN 
        IF(StabilityMethod==4)THEN 
           
           psyh=(-7.8)*zl_f   
        ELSE 
           PSYH=(-4.5)*Zl_f
        ENDIF
     ELSE 
        
        IF(StabilityMethod==4)THEN 
           psyh=(-7.8)*(1+LOG(zl_f))
        ELSE 
           PSYH=(-4.5)*(1+LOG(zl_f))
        ENDIF
     END IF

  ENDIF

  RETURN
END FUNCTION stab_fn_heat





FUNCTION stab_fn_rou(z,zstar) RESULT (psys)
  IMPLICIT NONE
  REAL(KIND(1d0))::alpha,zeta,z,psys,zstar,alpha1
  
  
  
  alpha=0.5
  alpha1=0.7
  zeta=z/zstar
  psys=(alpha-1)* LOG(zeta)-(alpha*alpha1)*(1-zeta)-(alpha*alpha1**2) &
       *(1-zeta**2)/6.-(alpha*alpha1**3)*(1-zeta**3)/24.
  RETURN
END FUNCTION stab_fn_rou


