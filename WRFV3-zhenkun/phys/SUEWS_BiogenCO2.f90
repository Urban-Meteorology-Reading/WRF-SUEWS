























 SUBROUTINE CO2_biogen(EmissionsMethod,id,ndays,ivConif,ivDecid,ivGrass,ConifSurf,DecidSurf,GrassSurf,BSoilSurf,&
            snowFrac,nsurf,NVegSurf,avkdn,Temp_C,sfr,LAI,LaiMin,LaiMax,&
            alpha_bioCO2,beta_bioCO2,theta_bioCO2,alpha_enh_bioCO2,beta_enh_bioCO2,&
            resp_a,resp_b,min_res_bioCO2,Fc_biogen,Fc_respi,Fc_photo,&
            notUsed,notUsedI)

  IMPLICIT NONE
  INTEGER,INTENT(in):: EmissionsMethod
  INTEGER,INTENT(in)::&
       id,&                
       ndays,&             
       ivConif,ivDecid,ivGrass,ConifSurf,DecidSurf,GrassSurf,BSoilSurf,&
       nsurf,nvegSurf,&
       notUsedI
  REAL(KIND(1d0)),INTENT(in)::&
       avkdn,&
       Temp_C,&
       notUsed

  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::&
       sfr,&   
       snowFrac
  REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(in):: LAI
  REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(in)::&
       LaiMin, LaiMax,&      
      
       alpha_bioCO2,&
       beta_bioCO2,&
       theta_bioCO2,&
       alpha_enh_bioCO2,&
       beta_enh_bioCO2,&
       resp_a,&
       resp_b,&
       min_res_bioCO2
  REAL(KIND(1D0)),INTENT(out):: &
       Fc_biogen,&
       Fc_respi,Fc_photo

  INTEGER:: iv 

  REAL(KIND(1d0)):: &
       PAR_umolm2s1,&
       Bellucco2017_Pho,&     
       Bellucco2017_Res,&     
       Bellucco2017_Res_surf,&
       VegFracSum             

  REAL(KIND(1d0)),DIMENSION(nvegsurf)::&
       active_veg_fr,&         
       Fc_photo_surf,&         
       Bellucco2017_Pho_surf 

  REAL(KIND(1d0)),DIMENSION(nvegsurf)::&
       alpha_bioCO2_v2,&
       beta_bioCO2_v2,&
       theta_bioCO2_v2

  REAL(KIND(1d0)),PARAMETER :: &
       JtoumolPAR = 4.6,&
       KdntoPAR = 0.46



  
  PAR_umolm2s1 = JtoumolPAR * KdntoPAR * avKdn

  VegFracSum = sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf)

  
  
  
  
  DO iv=ivConif,ivGrass   
     active_veg_fr(iv) = (sfr(iv+2)*(1-snowFrac(iv+2)))*(lai(id-1,iv)-LaiMin(iv))/(LaiMax(iv)-LaiMin(iv))
  ENDDO
  
  
  


  IF(EmissionsMethod>=11 .AND. EmissionsMethod<=16) THEN   

     
     Fc_photo = 0
     DO iv=ivConif,ivGrass
        Fc_photo_surf(iv) = -beta_bioCO2(iv)*alpha_bioCO2(iv)*PAR_umolm2s1/(alpha_bioCO2(iv)*PAR_umolm2s1 + beta_bioCO2(iv))
        
        Fc_photo = Fc_photo + Fc_photo_surf(iv)*active_veg_fr(iv)  
     ENDDO

  ELSEIF(EmissionsMethod>=21 .AND. EmissionsMethod<=26) THEN  

     
     Bellucco2017_Pho = 0
     DO iv=ivConif,ivGrass
       Bellucco2017_Pho_surf(iv) = -(1/(2*theta_bioCO2(iv))*(alpha_bioCO2(iv)*PAR_umolm2s1+beta_bioCO2(iv)- &
                              sqrt((alpha_bioCO2(iv)*PAR_umolm2s1+beta_bioCO2(iv))**2-4* &
                              alpha_bioCO2(iv)*beta_bioCO2(iv)*theta_bioCO2(iv)*PAR_umolm2s1)))
       
       Bellucco2017_Pho = Bellucco2017_Pho + Bellucco2017_Pho_surf(iv)*active_veg_fr(iv)
     ENDDO

     
     
     

     Fc_photo = Bellucco2017_Pho

  ELSEIF(EmissionsMethod>=31 .AND. EmissionsMethod<=36) THEN  
  	 
     

     
     IF(alpha_bioCO2(ivConif) == alpha_bioCO2(ivDecid) .AND. alpha_bioCO2(ivConif) == alpha_bioCO2(ivGrass) .AND. &
        beta_bioCO2(ivConif) == beta_bioCO2(ivDecid) .AND. beta_bioCO2(ivConif) == beta_bioCO2(ivGrass) .AND. &
        theta_bioCO2(ivConif) == theta_bioCO2(ivDecid) .AND. theta_bioCO2(ivConif) == theta_bioCO2(ivGrass)) THEN

       
        alpha_bioCO2_v2(ivConif) = alpha_bioCO2(ivConif) + alpha_enh_bioCO2(ivConif)* &
                               (sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BSoilSurf))      
        beta_bioCO2_v2(ivConif) = -beta_bioCO2(ivConif) + beta_enh_bioCO2(ivConif)* &
                                (sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BSoilSurf))     

        
        
        
        

        Bellucco2017_Pho = -(1/(2*theta_bioCO2(ivConif))*(alpha_bioCO2_v2(ivConif)*PAR_umolm2s1+beta_bioCO2_v2(ivConif)- &
              sqrt((alpha_bioCO2_v2(ivConif)*PAR_umolm2s1+beta_bioCO2_v2(ivConif))**2-4* &
              alpha_bioCO2_v2(ivConif)*beta_bioCO2_v2(ivConif)*theta_bioCO2(ivConif)*PAR_umolm2s1)))

     ELSE 
        CALL ErrorHint(74,'Check values in SUEWS_BiogenCO2.txt: ',notUsed,notUsed,notUsedI)

        
        alpha_bioCO2_v2(ivConif) = (alpha_bioCO2(ivConif)*sfr(ConifSurf)/VegFracSum + &
                                    alpha_bioCO2(ivDecid)*sfr(DecidSurf)/VegFracSum&
                                   + alpha_bioCO2(ivGrass)*sfr(GrassSurf)/VegFracSum) &
                                / (alpha_bioCO2(ivConif) + alpha_bioCO2(ivDecid) + alpha_bioCO2(ivGrass))
        beta_bioCO2_v2(ivConif)  = (beta_bioCO2(ivConif)*sfr(ConifSurf)/VegFracSum + &
        beta_bioCO2(ivDecid)*sfr(DecidSurf)/VegFracSum&
        + beta_bioCO2(ivGrass)*sfr(GrassSurf)/VegFracSum )/ (beta_bioCO2(ivConif) + &
        beta_bioCO2(ivDecid) + beta_bioCO2(ivGrass))
        theta_bioCO2_v2(ivConif) = (theta_bioCO2(ivConif)*sfr(ConifSurf)/VegFracSum  + &
        theta_bioCO2(ivDecid)*sfr(DecidSurf)/VegFracSum&
        + theta_bioCO2(ivGrass)*sfr(GrassSurf)/VegFracSum )/( theta_bioCO2(ivConif) + &
        theta_bioCO2(ivDecid) + theta_bioCO2(ivGrass))

        
        alpha_bioCO2_v2(ivConif) = alpha_bioCO2_v2(ivConif) + alpha_enh_bioCO2(ivConif)* &
                          (sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BSoilSurf))     
        beta_bioCO2_v2(ivConif) = -beta_bioCO2_v2(ivConif) + beta_enh_bioCO2(ivConif)* &
                          (sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BSoilSurf))     

        
        
        
        
        Bellucco2017_Pho = -(1/(2*theta_bioCO2_v2(ivConif))*(alpha_bioCO2_v2(ivConif)*PAR_umolm2s1+beta_bioCO2_v2(ivConif)- &
                           sqrt((alpha_bioCO2_v2(ivConif)*PAR_umolm2s1+beta_bioCO2_v2(ivConif))**2-4* &
                           alpha_bioCO2_v2(ivConif)*beta_bioCO2_v2(ivConif)*theta_bioCO2_v2(ivConif)*PAR_umolm2s1)))

     ENDIF

     
     Fc_photo = Bellucco2017_Pho*active_veg_fr(ConifSurf-2)+ &
               Bellucco2017_Pho*active_veg_fr(DecidSurf-2)+ &
               Bellucco2017_Pho*active_veg_fr(GrassSurf-2)

  ENDIF

  
  Bellucco2017_Res = 0.0
  Bellucco2017_Res_surf = 0.0
  IF (VegFracSum>0.01) THEN 
    DO iv=ivConif,ivGrass
      IF (sfr(2+iv)>0.005) THEN
        Bellucco2017_Res_surf = MAX(min_res_bioCO2(iv), resp_a(iv)*exp(resp_b(iv)*Temp_C))
        
        
        Bellucco2017_Res = Bellucco2017_Res + Bellucco2017_Res_surf * sfr(2+iv)/VegFracSum
      ENDIF
    ENDDO
  ENDIF
  
  
  Fc_respi = Bellucco2017_Res * (sfr(ConifSurf)+sfr(DecidSurf)+sfr(GrassSurf)+sfr(BSoilSurf))

  
  Fc_biogen = Fc_photo + Fc_respi
  

  RETURN

 ENDSUBROUTINE CO2_biogen



