









SUBROUTINE SnowUpdate(&
     nsurf,tstep,&
     Temp_C_hr,&
     tau_a,&
     tau_f,&
     tau_r,&
     SnowDensMax,&
     SnowDensMin,&
     SnowAlbMin,&
     SnowPack,&
     SnowAlb,&
     SnowDens)


  IMPLICIT NONE

  INTEGER,INTENT(in)::nsurf
  INTEGER,INTENT(in)::tstep

  REAL(KIND(1D0)),INTENT(in)::Temp_C_hr        
  REAL(KIND(1D0)),INTENT(in)::tau_a
  REAL(KIND(1D0)),INTENT(in)::tau_f
  REAL(KIND(1D0)),INTENT(in)::tau_r
  REAL(KIND(1D0)),INTENT(in)::SnowDensMax
  REAL(KIND(1D0)),INTENT(in)::SnowDensMin
  REAL(KIND(1D0)),INTENT(in)::SnowAlbMin

  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::SnowPack

  REAL(KIND(1d0)),INTENT(inout)::SnowAlb

  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens


  INTEGER::is
  REAL(KIND(1D0))::alb_change,&     
       dens_change,&    
       tau_1         

  
  alb_change=0
  dens_change=0
  tau_1=24*60*60

  
  
  
  IF (SUM(SnowPack)>0) THEN 
     IF (Temp_C_hr<0) THEN
        
        alb_change = tau_a*(tstep)/tau_1
        SnowAlb = SnowAlb-alb_change
     ELSE
        
        alb_change = EXP(-tau_f*(tstep)/tau_1)
        SnowAlb = (SnowAlb-SnowAlbMin)*alb_change+SnowAlbMin
     ENDIF
     IF (SnowAlb<SnowAlbMin) SnowAlb=SnowAlbMin 
  ELSE
     SnowAlb = 0
  ENDIF

  
  DO is=1,nsurf

     
     IF (SnowPack(is)>0) THEN
        dens_change = EXP(-tau_r*(tstep)/tau_1)
        IF (SnowPack(is)>0) SnowDens(is) = (SnowDens(is)-SnowDensMax)*dens_change+SnowDensMax
        IF (SnowDens(is)>SnowDensMax) SnowDens(is)=SnowDensMax
     ELSE
        SnowDens(is) = SnowDensMin
     ENDIF
  ENDDO

  

END SUBROUTINE SnowUpdate



