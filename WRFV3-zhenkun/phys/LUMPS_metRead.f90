










  subroutine MetRead(lfn,MetArray,InputmetFormat,ldown_option,NetRadiationMethod,&
               snowUse,SMDMethod,SoilDepthMeas,SoilRocks,SoilDensity,SmCap)

  use defaultNotUsed

  IMPLICIT NONE

  
  real (kind(1d0)),dimension(24)::MetArray 
                                           
                                           
                                           

  real (kind(1d0))::SmCap,&
                    SoilDepthMeas,&        
                    SoilRocks,&            
                    SoilDensity            

  integer::InputmetFormat,&     
           ldown_option,&       
           NetRadiationMethod,& 
           SMDMethod,&         
           snowUse

  
  real (kind(1d0))::avkdn,&     
                    avrh,&      
                    avu1,&      
                    dectime,&   
                    fcld_obs,&  
                    iy,&        
                    id,&        
                    it,&        
                    imin,&      
                    kdiff,&     
                    kdir,&      
                    LAI_obs,&   
                    ldown_obs,& 
                    Precip,& 
                    Pres_hPa,&  
                    Pres_kPa,&  
                    snow_obs,&  
                    qe_obs,&    
                    qf_obs,&    
                    qh_obs,&    
                    qn1_obs,&   
                    qs_obs,&    
                    Temp_C,&    
                    wdir,&      
                    wu_m3,&     
                    xsmd        

  integer::iostat_var,lfn

  
  

  if (InputMetFormat==0) then   

    READ(lfn,*,iostat=iostat_var)iy,id,it,imin,qn1_obs,avu1,avrh,&
             Temp_C,wdir,Pres_kPa,Precip,avkdn,snow_obs,ldown_obs,fcld_obs

    
    qf_obs=NaN
    qs_obs=NaN
    qh_obs=NaN
    qe_obs=NaN
    xsmd=-99999
    kdiff=NaN
    kdir=NaN
    wdir=NaN

  elseif (InputMetFormat==10) then 
      READ(lfn,*,iostat=iostat_var) iy,id,it,imin,qn1_obs,qh_obs,qe_obs,qs_obs,qf_obs,avu1,avrh,&
                                    Temp_C,Pres_kPa,Precip,avkdn,snow_obs,ldown_obs,fcld_obs,&
                                    wu_m3,xsmd,LAI_obs,kdiff,kdir,wdir









      
      if (SMDMethod==1.and.xsmd/=-999) then 
         xsmd=(SmCap-xsmd)*SoilDepthMeas*SoilRocks
      elseif (SMDMethod==2.and.xsmd/=-999) then 
         xsmd=(SmCap-xsmd)*SoilDensity*SoilDepthMeas*SoilRocks
      else
         xsmd=-999
      endif

  else
     call ErrorHint(55,'RunControl.nml, InputMetFormat not usable.',notUsed,notUsed,InputmetFormat)
  endif

  
  Pres_hPa=Pres_kPa*10. 

  
  
  
  
  

  if(iostat_var<0)THEN
     iostat_var=0
     CLOSE(lfn)
     RETURN
  ENDIF

  if(AvKdn<0) then
    call ErrorHint(27,'Met Data: avKdn - needed for Surf. resistance, If present, check file not tab delimited',&
                      avkdn,dectime,notUsedI)
     
     
  endif

  if((ldown_option==1).and.(ldown_obs<0))then
     call ErrorHint(27,'Met Data: LWdn (ldown_obs) - impact Q* calc',ldown_obs,dectime,notUsedI)

  elseif(ldown_option==2) then
     if(fcld_obs==-999.0.or.fcld_obs<0.or.fcld_obs>1) then
        call ErrorHint(27,'Met Data: flcd_obs - impacts LW & Q* radiation',fcld_obs,dectime,notUsedI)
     endif
  endif

  if(qn1_obs==-999.and.NetRadiationMethod==0) then  
     call ErrorHint(27,'Met Data: Q* - will impact everything', qn1_obs,dectime, notUsedI)
  endif

  if(avu1<=0) then 
    call ErrorHint(27,'Met Data: avU1 - impacts aeroydnamic resistances', avU1,dectime, notUsedI)
  endif


  if(Temp_C<-50.or.Temp_C>60)then 
    call ErrorHint(27,'Met Data: Temp_C - beyond what is expected', Temp_C,dectime, notUsedI)
  endif

  if(avrh>100.or.avrh<1)then 
      call ErrorHint(27,'Met Data: avRH - beyond what is expected', avRH,dectime, notUsedI)
  endif

  if(Pres_kPa<90)then  
    call ErrorHint(27,'Met Data: Pres_kPa - too low - this could be fixed in model',Pres_kPa ,dectime, notUsedI)
  endif

  if (Precip<0) then  
     call ErrorHint(27,'Met Data: Precip - less than 0',Precip ,dectime, notUsedI)
  endif

  if (snow_obs==NAN) snow_obs=0

  if (snowUse==0.and.(snow_obs<0.or.snow_obs>1)) then
     call ErrorHint(27,'Met Data: snow not between [0  1]',snow_obs ,dectime, notUsedI)
  endif

  if (xsmd<0.and.SMDMethod==1) then  
    call ErrorHint(27,'Met Data: xsmd - less than 0',xsmd ,dectime, notUsedI)
  endif

  
  MetArray(1:24)=(/iy,id,it,imin,qn1_obs,qh_obs,qe_obs,qs_obs,qf_obs,avu1,&
                   avrh,Temp_C,Pres_hPa,Precip,avkdn,snow_obs,ldown_obs,&
                   fcld_obs,wu_m3,xsmd,LAI_obs,kdiff,kdir,wdir/)







  RETURN

 END SUBROUTINE MetRead


