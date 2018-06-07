SUBROUTINE Evap_SUEWS(&

                                
     ity,&
     state_is,& 
     WetThresh_is,&
     capStore,& 
     numPM,&
     s_hPa,&
     psyc_hPa,&
     ResistSurf,&
     sp,&
     ra,&
     rb,&
     tlv,&

                                
     rss,&
     ev,&
     qe& 
     )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  

  IMPLICIT NONE
  INTEGER,INTENT(in) :: ity

  REAL(KIND(1d0)),INTENT(in)::state_is 
  REAL(KIND(1d0)),INTENT(in)::WetThresh_is
  REAL(KIND(1d0)),INTENT(in)::capStore 
  REAL(KIND(1d0)),INTENT(in)::numPM
  REAL(KIND(1d0)),INTENT(in)::s_hPa
  REAL(KIND(1d0)),INTENT(in)::psyc_hPa
  REAL(KIND(1d0)),INTENT(in)::ResistSurf
  REAL(KIND(1d0)),INTENT(in)::sp
  REAL(KIND(1d0)),INTENT(in)::ra
  REAL(KIND(1d0)),INTENT(in)::rb
  REAL(KIND(1d0)),INTENT(in)::tlv

  REAL(KIND(1d0)),INTENT(out)::rss
  REAL(KIND(1d0)),INTENT(out)::ev
  REAL(KIND(1d0)),INTENT(out)::qe 

  REAL(KIND(1d0)):: &
       rbsg,&  

       rsrbsg,&  
       rst,&
       W,&  
       x,&
       r
  REAL(KIND(1d0)),PARAMETER:: &
       NAN=-999
  
  
  





  
  IF(state_is<=0.001) THEN
     qe  = numPM/(s_hPa+psyc_hPa*(1+ResistSurf/ra))  
     ev  = qe/tlv              
     W   = NAN                  
     rst = 1                  

     
  ELSE
     rst=0   

     
     
     IF(ity==2) THEN   
        rbsg   = rb*(sp+1)           
        rsrbsg = ResistSurf+rbsg   

        
        
        IF(state_is>=WetThresh_is.OR.ResistSurf<25) THEN   
           W=1                                            
           
        ELSE   
           r = (ResistSurf/ra)*(ra-rb)/rsrbsg
           W = (r-1)/(r-(WetThresh_is/state_is))
        ENDIF




        rss=(1/((W/rbsg)+((1-W)/rsrbsg)))-rbsg 

        
        qe = numPM/(s_hPa+psyc_hPa*(1+rss/ra))   
        ev = qe/tlv                              



     ELSEIF(ity==1) THEN   
        qe = numPM/(s_hPa+psyc_hPa)
        ev = qe/tlv

        IF(state_is >= capStore) THEN
           x = 1.0
        ELSE
           x = state_is/capStore
        ENDIF
        ev = ev*x     
        qe = ev*tlv   
     ENDIF   
  ENDIF   



  
  
  
  
END SUBROUTINE Evap_SUEWS
