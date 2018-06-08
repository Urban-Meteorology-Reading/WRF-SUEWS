SUBROUTINE SurfaceResistance(&
     id,it,&
     SMDMethod,snowFrac,sfr,avkdn,Temp_C,dq,xsmd,vsmd,MaxConductance,&
     LAIMax,LAI_id,gsModel,Kmax,&
     G1,G2,G3,G4,G5,G6,TH,TL,S1,S2,&
     gsc,ResistSurf)
  
  
  
  

  
  
  
  
  

  
  
  
  
  
  
  

  IMPLICIT NONE
  
  INTEGER,PARAMETER::ConifSurf=3
  INTEGER,PARAMETER::DecidSurf=4
  INTEGER,PARAMETER::GrassSurf=5
  
  
  
  
  INTEGER,PARAMETER::nsurf=7
  
  
  INTEGER,PARAMETER::WaterSurf=7


  INTEGER,INTENT(in)::id
  INTEGER,INTENT(in)::it 
  INTEGER,INTENT(in)::gsModel
  INTEGER,INTENT(in)::SMDMethod
  
  
  
  
  

  REAL(KIND(1d0)),INTENT(in)::avkdn
  REAL(KIND(1d0)),INTENT(in)::Temp_C
  REAL(KIND(1d0)),INTENT(in)::Kmax
  REAL(KIND(1d0)),INTENT(in)::G1
  REAL(KIND(1d0)),INTENT(in)::G2
  REAL(KIND(1d0)),INTENT(in)::G3
  REAL(KIND(1d0)),INTENT(in)::G4
  REAL(KIND(1d0)),INTENT(in)::G5
  REAL(KIND(1d0)),INTENT(in)::G6
  REAL(KIND(1d0)),INTENT(in)::S1
  REAL(KIND(1d0)),INTENT(in)::S2
  REAL(KIND(1d0)),INTENT(in)::TH
  REAL(KIND(1d0)),INTENT(in)::TL
  REAL(KIND(1d0)),INTENT(in)::dq
  REAL(KIND(1d0)),INTENT(in)::xsmd
  REAL(KIND(1d0)),INTENT(in)::vsmd

  REAL(KIND(1d0)),DIMENSION(3),INTENT(in)    ::MaxConductance
  REAL(KIND(1d0)),DIMENSION(3),INTENT(in)    ::LAIMax        
  REAL(KIND(1d0)),DIMENSION(3),INTENT(in)    ::LAI_id        
  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowFrac      
  REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr           

  REAL(KIND(1d0)),INTENT(out)::gsc
  REAL(KIND(1d0)),INTENT(out)::ResistSurf

  REAL(KIND(1d0)):: &
       gl,&
       QNM,&
       gq,&
       gdq,&
       TC,&
       TC2,&
       gtemp,&
       sdp,&
       gs


  INTEGER:: iv
  REAL(KIND(1d0)):: id_real

  INTEGER,PARAMETER :: notUsed=-55
  

  id_real = REAL(id) 

  
  

  IF(gsModel == 1) THEN
     IF(avkdn<=0) THEN   
        gsc=0.1   
     ELSE
        
        QNM=Kmax/(Kmax+G2)
        
        gq=(avkdn/(G2+avkdn))/QNM 

        
        IF(dq<G4) THEN
           gdq=1-G3*dq
        ELSE
           gdq=1-G3*G4
        ENDIF

        
        TC=(TH-G5)/(G5-TL)
        TC2=(G5-TL)*(TH-G5)**TC
        
        IF (Temp_C<=tl) THEN
           gtemp=(tl+0.1-tl)*(th-(tl+0.1))**tc/tc2

           
           
           IF (MINVAL(snowFrac(1:6))/=1) THEN
              CALL errorHint(29,'subroutine SurfaceResistance.f95: T changed to fit limits TL=0.1,Temp_c,id,it',&
                   REAL(Temp_c,KIND(1d0)),id_real,it)
           ENDIF

        ELSEIF (Temp_C>=th) THEN
           gtemp=((th-0.1)-tl)*(th-(th-0.1))**tc/tc2
           CALL errorHint(29,'subroutine SurfaceResistance.f95: T changed to fit limits TH=39.9,Temp_c,id,it',&
                REAL(Temp_c,KIND(1d0)),id_real,it)
        ELSE
           gtemp=(Temp_C-tl)*(th-Temp_C)**tc/tc2
        ENDIF

        
        sdp=S1/G6+S2
        IF(SMDMethod>0)THEN         
           gs=1-EXP(g6*(xsmd-sdp))  
        ELSE
           gs=1-EXP(g6*(vsmd-sdp))   
           IF(sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) == 0 .OR. sfr(WaterSurf)==1 ) THEN
              gs=0   
           ENDIF
        ENDIF

        gs = gs*(1-SUM(snowFrac(1:6))/6)

        IF(gs<0)THEN
           CALL errorHint(65,'subroutine SurfaceResistance.f95 (gsModel=1): g(smd) < 0 calculated, setting to 0.0001',gs,id_real,it)
           gs=0.0001
        ENDIF

        
        
        
        
        gl=0    
        
        
        
        
        DO iv=1,3
           
           gl=gl+(sfr(iv+2)*(1-snowFrac(iv+2)))*LAI_id(iv)/LAIMax(iv)*MaxConductance(iv)
        ENDDO

        
        gsc=(G1*gq*gdq*gtemp*gs*gl)

        IF(gsc<=0) THEN
           CALL errorHint(65,'subroutine SurfaceResistance.f95 (gsModel=1): gs <= 0, setting to 0.1 mm s-1',gsc,id_real,it)
           gsc=0.1
        ENDIF
     ENDIF

  ELSEIF(gsModel == 2) THEN
     IF(avkdn<=0) THEN      
        gsc=0.1
     ELSE
        
        QNM = Kmax/(Kmax+G2)
        gq = (avkdn/(avkdn+G2))/QNM
        IF(avkdn >= Kmax) THEN   
           WRITE(*,*) 'Kmax exceeds Kdn setting to g(Kdn) to 1'
           gq = 1
        ENDIF
        
        gdq = G3 + (1-G3)*(G4**dq)   
        
        Tc=(TH-G5)/(G5-TL)
        Tc2=(G5-TL)*(TH-G5)**Tc
        
        IF (Temp_C <= TL) THEN
           gtemp=(TL+0.1-TL)*(TH-(TL+0.1))**Tc/Tc2
           
           IF (MIN(snowFrac(1),snowFrac(2),snowFrac(3),snowFrac(4),snowFrac(5),snowFrac(6))/=1) THEN
              CALL errorHint(29,'subroutine SurfaceResistance.f95: T changed to fit limits TL+0.1,Temp_C,id,it',&
                   REAL(Temp_c,KIND(1d0)),id_real,it)
           ENDIF
        ELSEIF (Temp_C >= TH) THEN
           gtemp=((TH-0.1)-TL)*(TH-(TH-0.1))**Tc/Tc2
           CALL errorHint(29,'subroutine SurfaceResistance.f95: T changed to fit limits TH-0.1,Temp_C,id,it',&
                REAL(Temp_c,KIND(1d0)),id_real,it)
        ELSE
           gtemp=(Temp_C-TL)*(TH-Temp_C)**Tc/Tc2
        ENDIF
        
        sdp=S1/G6+S2
        IF(SMDMethod>0) THEN   
           gs=(1-EXP(g6*(xsmd-sdp)))/(1-EXP(g6*(-sdp)))   
        ELSE
           
           gs=(1-EXP(g6*(vsmd-sdp)))/(1-EXP(g6*(-sdp)))
           IF(sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) == 0 .OR. sfr(WaterSurf)==1 ) THEN
              gs=0   
           ENDIF
        ENDIF

        gs = gs*(1-SUM(snowFrac(1:6))/6)

        IF(gs<0)THEN
           CALL errorHint(65,'subroutine SurfaceResistance.f95 (gsModel=2): gs < 0 calculated, setting to 0.0001',gs,id_real,it)
           gs=0.0001
        ENDIF

        
        gl=0    
        
        DO iv=1,3   
           
           gl=gl+(sfr(iv+2)*(1-snowFrac(iv+2)))*LAI_id(iv)/LAIMax(iv)*MaxConductance(iv)
        ENDDO

        
        gsc=(G1*gq*gdq*gtemp*gs*gl)

        IF(gsc<=0) THEN
           CALL errorHint(65,'subroutine SurfaceResistance.f95 (gsModel=2): gsc <= 0, setting to 0.1 mm s-1',gsc,id_real,it)
           gsc=0.1
        ENDIF

     ENDIF

  ELSEIF(gsModel < 1 .OR. gsModel > 2) THEN
     CALL errorHint(71,'Value of gsModel not recognised.',notUsed,NotUsed,gsModel)
  ENDIF

  ResistSurf=1/(gsc/1000)  

  RETURN
END SUBROUTINE SurfaceResistance


