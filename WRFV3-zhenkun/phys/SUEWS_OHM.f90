










SUBROUTINE OHM(qn1,qn1_store,qn1_av_store,&
     qn1_S,qn1_S_store,qn1_S_av_store,&
     nsh,&
     sfr,nsurf,&
     HDDday,&
     OHM_coef,&
     OHM_threshSW,OHM_threshWD,&
     soilmoist,soilstoreCap,state,&
     BldgSurf,WaterSurf,&
     SnowUse,SnowFrac,&
     DiagQS,&
     a1,a2,a3,qs,deltaQi)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  IMPLICIT NONE


  REAL(KIND(1d0)),INTENT(in)::qn1                             
  REAL(KIND(1d0)),INTENT(in)::qn1_S                           
  REAL(KIND(1d0)),INTENT(in)::sfr(nsurf)                      
  REAL(KIND(1d0)),INTENT(in)::SnowFrac(nsurf)                 
  REAL(KIND(1d0)),INTENT(in)::HDDday                          
  REAL(KIND(1d0)),INTENT(in)::OHM_coef(nsurf+1,4,3)                 
  REAL(KIND(1d0)),INTENT(in)::OHM_threshSW(nsurf+1),OHM_threshWD(nsurf+1) 
  REAL(KIND(1d0)),INTENT(in)::soilmoist(nsurf)                
  REAL(KIND(1d0)),INTENT(in)::soilstoreCap(nsurf)             
  REAL(KIND(1d0)),INTENT(in)::state(nsurf) 

  INTEGER,INTENT(in)::nsurf     
  INTEGER,INTENT(in)::nsh       
  INTEGER,INTENT(in)::BldgSurf  
  INTEGER,INTENT(in)::WaterSurf 
  INTEGER,INTENT(in)::SnowUse   
  INTEGER,INTENT(in)::DiagQS    

  
  
  REAL(KIND(1d0)),INTENT(inout)::qn1_store(360)
  REAL(KIND(1d0)),INTENT(inout)::qn1_av_store(2*360+1)
  REAL(KIND(1d0)),INTENT(inout)::qn1_S_store(nsh)
  REAL(KIND(1d0)),INTENT(inout)::qn1_S_av_store(2*nsh+1)


  REAL(KIND(1d0)),INTENT(out):: qs 
  
  REAL(KIND(1d0)),INTENT(out)::deltaQi(nsurf) 

  REAL(KIND(1d0)),INTENT(out):: a1,a2,a3 

  

  REAL(KIND(1d0)):: dqndt    
  

  
  REAL(KIND(1d0)):: deltaQi0 

  

  
  
  
  

  CALL OHM_coef_cal(sfr,nsurf,&
       HDDday,OHM_coef,OHM_threshSW,OHM_threshWD,&
       soilmoist,soilstoreCap,state,&
       BldgSurf,WaterSurf,&
       SnowUse,SnowFrac,&
       a1,a2,a3)

  



  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


  
  
  qs=-999              
  IF(qn1>-999) THEN   
     
     
     CALL OHM_dqndt_cal(360,qn1,qn1_store,qn1_av_store,dqndt)

     
     CALL OHM_QS_cal(qn1,dqndt,a1,a2,a3,qs)
     IF(DiagQS==1) WRITE(*,*) 'qs: ',qs,'qn1:',qn1,'dqndt: ',dqndt

  ELSE

  ENDIF

  


  
  
  IF(snowUse==1) THEN
     deltaQi=-999
     IF(qn1_S>-999) THEN
        
        
        
        
        
        
        
        
        
        
        
        
        CALL OHM_dqndt_cal(nsh,qn1_S,qn1_S_store,qn1_S_av_store,dqndt)

        
        CALL OHM_QS_cal(qn1_S,dqndt,&
             OHM_coef(nsurf+1,3,1),OHM_coef(nsurf+1,3,2),OHM_coef(nsurf+1,3,3),&
             deltaQi0)
        deltaQi=deltaQi0


     ELSE

     ENDIF

  ENDIF

  RETURN
ENDSUBROUTINE OHM


SUBROUTINE OHM_coef_cal(sfr,nsurf,&
     HDDday,OHM_coef,OHM_threshSW,OHM_threshWD,&
     soilmoist,soilstoreCap,state,&
     BldgSurf,WaterSurf,&
     SnowUse,SnowFrac,&
     a1,a2,a3)
  IMPLICIT NONE
  INTEGER , INTENT(in) :: &
       nsurf,& 
       SnowUse,& 
       BldgSurf,WaterSurf 
  REAL(KIND(1d0)), INTENT(in) :: &
       sfr(nsurf),& 
       SnowFrac(nsurf),& 
       HDDday,& 
       OHM_coef(nsurf+1,4,3),&
       OHM_threshSW(nsurf+1),OHM_threshWD(nsurf+1),& 
       soilmoist(nsurf),& 
       soilstoreCap(nsurf),&
       state(nsurf) 
  REAL(KIND(1d0)), INTENT(out):: a1,a2,a3

  REAL(KIND(1d0)) :: surfrac
  INTEGER :: i,ii,is

  
  
  a1=0   
  a2=0   
  a3=0   
  

  
  DO is=1,nsurf
     surfrac=sfr(is)

     
     IF(HDDday >= OHM_threshSW(is)) THEN 
        ii=0
     ELSE          
        ii=2
     ENDIF

     IF(state(is) > 0) THEN     
        i=ii+1
     ELSE                    
        i=ii+2
        
        IF(is>BldgSurf.AND.is/=WaterSurf)THEN    
           IF(soilmoist(is)/soilstoreCap(is) > OHM_threshWD(is) ) THEN
              i=ii+1
           ENDIF
        ENDIF
     ENDIF

     
     IF(SnowUse==1.AND.is/=BldgSurf.AND.is/=WaterSurf) THEN   
        surfrac=surfrac*(1-SnowFrac(is))
     ENDIF

     
     a1 = a1+surfrac*OHM_coef(is,i,1)
     a2 = a2+surfrac*OHM_coef(is,i,2)
     a3 = a3+surfrac*OHM_coef(is,i,3)

  ENDDO  
END SUBROUTINE OHM_coef_cal

SUBROUTINE OHM_dqndt_cal(nsh,qn1,qn1_store,qn1_av_store,dqndt)
  IMPLICIT NONE
  INTEGER, INTENT(in)            :: nsh                   
  REAL(KIND(1d0)), INTENT(in)    :: qn1
  REAL(KIND(1d0)), INTENT(inout) :: qn1_store(nsh)        
  REAL(KIND(1d0)), INTENT(inout) :: qn1_av_store(2*nsh+1) 
  REAL(KIND(1d0)), INTENT(out)   :: dqndt                 

  REAL(KIND(1d0)) :: qn1_av
  INTEGER :: nsh_nna

  dqndt=-999 

  
  IF(nsh > 1) THEN
     qn1_store=CSHIFT(qn1_store,1) 
     qn1_store(nsh)=qn1
     nsh_nna = COUNT(qn1_store/=-999, dim=1) 
     qn1_av = SUM(qn1_store, mask=qn1_store/= -999)/nsh_nna
  ELSEIF(nsh==1) THEN
     qn1_store(:) = qn1
     qn1_av = qn1
  ENDIF
  
  IF(nsh > 1) THEN
     qn1_av_store=CSHIFT(qn1_av_store,1)
     qn1_av_store(2*nsh+1) = qn1_av
  ELSEIF(nsh==1) THEN
     qn1_av_store(:) = qn1_av
  ENDIF
  
  IF(ANY(qn1_av_store == -999)) THEN
     dqndt=0  
  ELSE
     dqndt=0.5*(qn1_av_store((2*nsh+1))-qn1_av_store(1))
  ENDIF

END SUBROUTINE OHM_dqndt_cal

SUBROUTINE OHM_QS_cal(qn1,dqndt,a1,a2,a3,qs)
  IMPLICIT NONE
  REAL(KIND(1d0)), INTENT(in) :: qn1,dqndt,a1,a2,a3
  REAL(KIND(1d0)), INTENT(out):: qs
  qs=-999 
  qs = qn1*a1 + dqndt*a2 + a3   

END SUBROUTINE OHM_QS_cal





