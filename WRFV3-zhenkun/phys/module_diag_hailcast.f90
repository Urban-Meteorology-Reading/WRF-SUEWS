
MODULE module_diag_hailcast

CONTAINS

  SUBROUTINE hailcast_diagnostic_driver (   grid , config_flags     &
                             , moist                             &
                             , rho  &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end               )

    USE module_domain, ONLY : domain , domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
    USE module_utility
    USE module_streams, ONLY: history_alarm, auxhist2_alarm

    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER             :: k_start , k_end, its, ite, jts, jte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist),    &
         INTENT(IN   ) ::                                moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                 rho

    
    
    CHARACTER*512 :: message
    CHARACTER*256 :: timestr 
    INTEGER :: i,j,k,nz
    INTEGER :: i_start, i_end, j_start, j_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qr  &
                                              ,          qs  &
                                              ,          qg  &
                                              ,          qv  &
                                              ,          qc  &
                                              ,          qi  &
                                              ,        ptot 
    REAL, DIMENSION( ims:ime, jms:jme ) ::       wup_mask_prev  &
                                              ,      wdur_prev  
    REAL :: dhail1,dhail2,dhail3,dhail4,dhail5
    
    
    
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime, StartTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep, is_first_timestep

    
    write ( message, * ) 'inside hailcast_diagnostics_driver'
    CALL wrf_debug( 100 , message )

    
    
    
    
    
    CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time, &
         ringinterval=histint)
    CALL WRFU_ALARMGET( grid%alarms( AUXHIST2_ALARM ), prevringtime=aux2_time, &
         ringinterval=aux2int)

    
   
    CALL domain_clock_get ( grid, current_time=CurrTime, &
         simulationStartTime=StartTime, &            
         current_timestr=timestr, time_step=dtint )

    
    
  
    is_after_history_dump = ( Currtime .lt. hist_time + dtint )

    
 
    is_output_timestep = (Currtime .ge. hist_time + histint - dtint .or. &
                         Currtime .ge. aux2_time + aux2int - dtint )
    write ( message, * ) 'is output timestep? ', is_output_timestep
    CALL wrf_debug( 100 , message )

    

    is_first_timestep = ( Currtime .eq. StartTime + dtint )
        
    

    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          qv(i,k,j) = moist(i,k,j,P_QV)
          qr(i,k,j) = moist(i,k,j,P_QR)
          qs(i,k,j) = moist(i,k,j,P_QS)
          qg(i,k,j) = moist(i,k,j,P_QG)
          qc(i,k,j) = moist(i,k,j,P_QC)
          qi(i,k,j) = moist(i,k,j,P_QI)
        ENDDO
      ENDDO
    ENDDO

    

    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO

    

    IF ( is_after_history_dump ) THEN
      DO j = jms, jme
        DO i = ims, ime
           grid%hailcast_dhail1(i,j) = 0.
           grid%hailcast_dhail2(i,j) = 0.
           grid%hailcast_dhail3(i,j) = 0.
           grid%hailcast_dhail4(i,j) = 0.
           grid%hailcast_dhail5(i,j) = 0.
        ENDDO
      ENDDO
    ENDIF  


    

    

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = ite
    

    

    wdur_prev(:,:) = grid%hailcast_wdur(:,:)
    wup_mask_prev(:,:) = grid%hailcast_wup_mask(:,:)

    

    DO j = jts, jte
      DO i = its, ite
        grid%hailcast_wup_mask(i,j) = 0
        grid%hailcast_wdur(i,j) = 0

        DO k = k_start, k_end
          IF ( grid%w_2(i,k,j) .ge. 10. ) THEN
              grid%hailcast_wup_mask(i,j) = 1
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    

    DO j = j_start, j_end
      DO i = i_start, i_end

        

        IF ( (grid%hailcast_wup_mask(i,j).eq.1) .OR.                 &
           (MAXVAL(wup_mask_prev(i-1:i+1,j-1:j+1)).eq.1) ) THEN
             grid%hailcast_wdur(i,j) =                                 &
                  MAXVAL(wdur_prev(i-1:i+1,j-1:j+1)) + grid%dt
        ENDIF
      ENDDO
    ENDDO


    

    nz = k_end - k_start
    DO j = jts, jte
      DO i = its, ite

        
        

        IF (grid%hailcast_wdur(i,j) .gt. 900) THEN
          CALL hailstone_driver ( grid%t_phy(i,kms:kme,j), &
                                  grid%z(i,kms:kme,j),     &
                                  grid%ht(i,       j),     &
                                  ptot(i,kms:kme,j),     &
                                  rho(i,kms:kme,j),   &
                                  qv(i,kms:kme,j),    &
                                  qi(i,kms:kme,j),    &
                                  qc(i,kms:kme,j),    &
                                  qr(i,kms:kme,j),    &
                                  qs(i,kms:kme,j),    &
                                  qg(i,kms:kme,j),    &
                                  grid%w_2(i,kms:kme,j),   &
                                  grid%hailcast_wdur(i,j),          &
                                  nz,                 &
                                  dhail1, dhail2,     &
                                  dhail3, dhail4,     &
                                  dhail5              )
          IF (dhail1 .gt. grid%hailcast_dhail1(i,j)) THEN
              grid%hailcast_dhail1(i,j) = dhail1
          ENDIF
          IF (dhail2 .gt. grid%hailcast_dhail2(i,j)) THEN
              grid%hailcast_dhail2(i,j) = dhail2
          ENDIF
          IF (dhail3 .gt. grid%hailcast_dhail3(i,j)) THEN
              grid%hailcast_dhail3(i,j) = dhail3
          ENDIF
          IF (dhail4 .gt. grid%hailcast_dhail4(i,j)) THEN
              grid%hailcast_dhail4(i,j) = dhail4
          ENDIF
          IF (dhail5 .gt. grid%hailcast_dhail5(i,j)) THEN
              grid%hailcast_dhail5(i,j) = dhail5
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    
    

    DO j = jms, jme
      DO i = ims, ime
        
        grid%hailcast_diam_mean(i,j)=(grid%hailcast_dhail1(i,j)+&
             grid%hailcast_dhail2(i,j) +grid%hailcast_dhail3(i,j)+&
             grid%hailcast_dhail4(i,j) +grid%hailcast_dhail5(i,j))/5.
        
        grid%hailcast_diam_std(i,j) = SQRT( ( &
          (grid%hailcast_dhail1(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
          (grid%hailcast_dhail2(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
          (grid%hailcast_dhail3(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
          (grid%hailcast_dhail4(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
          (grid%hailcast_dhail5(i,j)-grid%hailcast_diam_mean(i,j))**2.)&
          / 4.0)
      ENDDO
    ENDDO

  END SUBROUTINE hailcast_diagnostic_driver























































  SUBROUTINE hailstone_driver ( TCA, h1d, ht, PA, rho1d,&
                                RA, qi1d,qc1d,qr1d,qs1d,qg1d,   &
                                VUU, wdur,                          &
                                nz,dhail1,dhail2,dhail3,dhail4,     &
                                dhail5                             )
    IMPLICIT NONE
    INTEGER, INTENT(IN ) :: nz

    REAL, DIMENSION( nz ),             &
         INTENT(IN   ) ::                                  TCA  & 
                                              ,          rho1d  &
                                              ,            h1d  &
                                              ,             PA  & 
                                              ,             RA  & 
                                              ,            VUU  & 
                                              , qi1d,qc1d,qr1d  &
                                              , qs1d,qg1d

    REAL, INTENT(IN   ) ::                                  ht  &
                                              ,           wdur
    
    
    REAL, INTENT(INOUT) ::                              dhail1 & 
                                              ,         dhail2 &
                                              ,         dhail3 &
                                              ,         dhail4 &
                                              ,         dhail5
    
    REAL ZBAS, TBAS, WBASP     
    REAL RBAS                  
    REAL cwitot                
    INTEGER KBAS               
    REAL tk_embryo             
    REAL ZFZL, TFZL, WFZLP     
    REAL RFZL                  
    REAL VUFZL, DENSAFZL       
    INTEGER KFZL               
    INTEGER nofroze            
    INTEGER CLOUDON            
    REAL RTIME                 
    REAL TAU, TAU_1, TAU_2     
    REAL delTAU                
    REAL g                     
    REAL r_d                   
    
    REAL*8 DD, D, D_ICE        
    REAL VT                    
    REAL V                     
    REAL TS                    
    
    REAL TSm1, TSm2            
    REAL FW                    
    REAL WATER                 
    REAL CRIT                  
    REAL DENSE                 
    INTEGER ITYPE              
    
    REAL, DIMENSION( nz ) ::  &
      RIA, &                   
      RWA, &                   
      VUU_pert                 
    
    REAL P                     
    REAL RS                    
    REAL RI, RW                
    REAL XI, XW                
    REAL PC                    
    REAL TC                    
    REAL VU                    
    REAL VUMAX                 
    REAL VUCORE                
    REAL DENSA                 
    REAL Z                     
    REAL DELRW                 
    
    REAL, DIMENSION(600) :: gd       
    REAL, DIMENSION(600) :: ng_d     
    REAL, DIMENSION(600) :: sum_ng_d 
    REAL lambdag                     
    REAL n0g                         
    REAL deng                        
    REAL xslw1,ygra1,zans1           
    REAL pile                        
    REAL d02,d05,d10,d15,d20 
    REAL n02,n05,n10,n15,n20 
    REAL, DIMENSION(5) :: dhails     
                                     
    
    REAL TLAYER,RLAYER,PLAYER  
    REAL TSUM,RSUM,PSUM        
    REAL LDEPTH                
    
    REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE
    REAL dum
      
    REAL sec, secdel           
    INTEGER i, j, k, IFOUT, ind(1)
    CHARACTER*256 :: message


    
    secdel = 5.0
    g=9.81
    r_d = 287.
            

    TAU = 7200.    

    
    
    DO i=1,nz
       VUU_pert(i) = VUU(i) * 1.
    ENDDO

      

    DO i=1,5
       dhails(i) = 0.
    ENDDO


    IF (wdur .GT. 2000) THEN
        RTIME  = 2000.
    ELSE
        RTIME = wdur
    ENDIF

 
    
    
    KBAS=nz
    
    DO k=1,nz
         cwitot = qi1d(k) + qc1d(k)
         
         
         RIA(k) = qi1d(k) + qs1d(k)
         
         RWA(k) = qc1d(k)
         
         
         
         
         IF ((cwitot .ge. 1.E-12) .and. (k .lt. KBAS)) THEN
            KBAS = k
         ENDIF
    ENDDO

    
    
    

    
    
    
    
    
    ZBAS = h1d(KBAS)
    TBAS = TCA(KBAS)
    WBASP = PA(KBAS)
    RBAS = RA(KBAS)
    
    
    tk_embryo = 260.155
    TFZL = tk_embryo
    CALL INTERPP(PA, WFZLP, TCA, tk_embryo, IFOUT, nz)
    CALL INTERP(h1d, ZFZL, WFZLP, IFOUT, PA, nz)
    CALL INTERP(RA,  RFZL, WFZLP, IFOUT, PA, nz)
    CALL INTERP(VUU_pert, VUFZL, WFZLP, IFOUT, PA, nz)
    CALL INTERP(rho1d, DENSAFZL, WFZLP, IFOUT, PA, nz)

    
    
    
    
    
    d02 = 9.E-4
    d05 = 2.E-3
    d10 = 5.E-3
    d15 = 7.5E-3
    d20 = 1.E-2    

    
    DO i=1,5
      SELECT CASE (i)   
        CASE (1)
        
        DD = d02
        CASE (2)
        DD = d05
        CASE (3)  
        DD = d10
        CASE (4)
        DD = d15
        CASE (5)
        DD = d20
      END SELECT

      
      sec = 0.
 
      
      P = WFZLP
      RS = RFZL
      TC = TFZL
      VU = VUFZL  
      Z = ZFZL - ht
      LDEPTH = Z
      DENSA = DENSAFZL

      
      nofroze=1 
      TS = TC
      TSm1 = TS
      TSm2 = TS      
      D = DD   
      FW = 0.0
      DENSE = 500.  
      ITYPE=1.  
      CLOUDON=1  

      
      DO WHILE (sec .lt. TAU)
         sec = sec + secdel
         
         
         
         
         
         CALL INTERP(VUU_pert,VUMAX,P,IFOUT,PA,nz)

         
         
         IF (IFOUT.EQ.1) GOTO 100
                  
         
         IF (SEC .GT. 0.0 .AND. SEC .LT. RTIME) THEN
            VUCORE = VUMAX * SIN( (3.14159 * SEC)/(RTIME) )
            VU = VUCORE
      
         ELSEIF (SEC .GE. RTIME) THEN
            VU = 0.0
            CLOUDON = 0
         ENDIF
         
         
         
         CALL TERMINL(DENSA,DENSE,D,VT,TC)
         
         
         V = VU - VT
         

         P = P - DENSA*g*V*secdel
         Z = Z + V*secdel

         
         CALL INTERP(TCA,TC,P,IFOUT,PA,nz)
         CALL INTERP(RA,RS,P,IFOUT,PA,nz)
         
         
         DENSA=P/(r_d*(1.+0.609*RS/(1.+RS))*TC)
         
         
         CALL INTERP(RIA,RI,P,IFOUT,PA,nz)
         CALL INTERP(RWA,RW,P,IFOUT,PA,nz)
         XI = RI * DENSA * CLOUDON
         XW = RW * DENSA * CLOUDON
         IF( (XW+XI).GT.0) THEN
           PC = XI / (XW+XI)
         ELSE
           PC = 1.
         ENDIF
         
         
        

        
        

        
        
        
 
        
        CALL VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
      
        

        CALL MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE,&
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,secdel,ITYPE,DELRW) 


        
        CALL HEATBUD(TS,TSm1,TSm2,FW,TC,VT,DELRW,D,DENSA,GM1,GM,DGM,DGMW,  & 
                     DGMV,DGMI,GMW,GMI,DI,ANU,RE,AE,secdel,ITYPE,P)

 
        
        
        
        
        
        
        WATER=FW*GM  

        CRIT = 1.0E-10
        IF (WATER.GT.CRIT)THEN
           CALL BREAKUP(DENSE,D,GM,FW)
        ENDIF
        
        
        
        IF (Z .LE. ZBAS) GOTO 200

        
        D_ICE = ( (6*GM*(1.-FW)) / (3.141592654*DENSE) )**0.33333333 


        IF ((D_ICE .LT. 1.E-8) .AND. (TC.GT.273.155)) GOTO 300

        
        TSm1 = TS
        TSm2 = TSm1
        
      ENDDO  

100   CONTINUE 
200   CONTINUE 
300   CONTINUE 

      
      

      IF (P.lt.PA(nz)) THEN
         D=0.0
      
      ELSE IF(ABS(FW - 1.0).LT.0.001) THEN
         D=0.0
      ELSE IF (Z.GT.0) THEN
         
         
        
         
         TSUM = 0.
         RSUM = 0.
         PSUM = 0.
         DO k=1,KBAS
            TSUM = TSUM + TCA(k)
            PSUM = PSUM + PA(k)
            RSUM = RSUM + RA(k)
         ENDDO
         TLAYER = TSUM / KBAS
         PLAYER = PSUM / KBAS
         RLAYER = RSUM / KBAS
         
         


         D_ICE = ( (6*GM*(1.-FW)) / (3.141592654*DENSE) )**0.33333333 
         D = D_ICE  
         CALL MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)

      ENDIF 
      
      
      dhails(i) = D * 1000

    ENDDO  
  
    
    DO j=1,4
     DO k=j+1,5
         IF (dhails(j).lt.dhails(k)) THEN
            dum = dhails(j)
            dhails(j) = dhails(k)
            dhails(k) = dum
         ENDIF
      ENDDO
    ENDDO
    
    dhail1 = dhails(1)
    dhail2 = dhails(2)
    dhail3 = dhails(3)
    dhail4 = dhails(4)
    dhail5 = dhails(5)
 
  END SUBROUTINE hailstone_driver


  SUBROUTINE INTERPP(PA,PVAL,TA,TVAL,IFOUT,ITEL)
  
  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      
      REAL PVAL, TVAL
      REAL, DIMENSION( ITEL) :: TA, PA
      INTEGER ITEL, IFOUT
      
      INTEGER I
      REAL FRACT
      
      IFOUT=1
      
      DO I=1,ITEL-1
         IF ( (TVAL .LT. TA(I) .AND. TVAL .GE. TA(I+1)) .or.  &   
              (TVAL .GT. TA(I) .AND. TVAL .LE. TA(I+1)) ) THEN    

            FRACT = (TA(I) - TVAL) / (TA(I) - TA(I+1))
            
            PVAL = ((1.0 - FRACT) * PA(I)) + (FRACT * PA(I+1))
          
            
            IFOUT=0
            EXIT
         ENDIF
      ENDDO
      
  END SUBROUTINE INTERPP



  SUBROUTINE INTERP(AA,A,P,IFOUT,PA,ITEL)
  
  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      
      REAL A, P
      REAL, DIMENSION( ITEL) :: AA, PA
      INTEGER ITEL, IFOUT
      
      INTEGER I
      REAL PDIFF, VDIFF, RDIFF, VERH, ADIFF
      
      IFOUT=1
      
      DO I=1,ITEL-1
        IF (P.LE.PA(I) .AND. P.GT.PA(I+1)) THEN
          
          PDIFF = PA(I)-PA(I+1)
          VDIFF = PA(I)-P
          VERH = VDIFF/PDIFF     
          
          
          RDIFF = AA(I+1) - AA(I)
          
          
          A = AA(I) + RDIFF*VERH
          
          
          IFOUT=0
          EXIT
        ENDIF
      ENDDO
      
  END SUBROUTINE INTERP
      

  SUBROUTINE TERMINL(DENSA,DENSE,D,VT,TC)
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      
      REAL*8 D
      REAL DENSA, DENSE, TC, VT
      REAL GMASS, GX, RE, W, Y
      REAL, PARAMETER :: PI = 3.141592654, G = 9.78956
      REAL ANU
      
      
      GMASS = (DENSE * PI * (D**3.)) / 6.
      
      
      ANU = (0.00001718)*(273.155+120.)/(TC+120.)*(TC/273.155)**(1.5)
      
      
      GX=(8.0*GMASS*G*DENSA)/(PI*(ANU*ANU))
      RE=(GX/0.6)**0.5

      
      
      IF (GX.LT.550) THEN
        W=LOG10(GX)
        Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0)      
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.550.AND.GX.LT.1800) THEN
        W=LOG10(GX)
        Y= -1.81391 + 1.34671*W - 0.12427*(W**2.0) + 0.0063*(W**3.0)
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.1800.AND.GX.LT.3.45E08) THEN
        RE=0.4487*(GX**0.5536)
        VT=ANU*RE/(D*DENSA)
      ELSE 
        RE=(GX/0.6)**0.5
        VT=ANU*RE/(D*DENSA)
      ENDIF
      
  END SUBROUTINE TERMINL   

   
   
  SUBROUTINE VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
  
  

  
  
  
  
  
  
  
  
  
  
  

      IMPLICIT NONE
      REAL DELRW, PC, TS, TC
      INTEGER ITYPE
      
      REAL RV, ALV, ALS, RATIO
      DATA RV/461.48/,ALV/2500000./,ALS/2836050./ 
      REAL ESAT, RHOKOR, ESATW, RHOOMGW, ESATI, RHOOMGI, RHOOMG

      
      RATIO = 1./273.155
      IF(ITYPE.EQ.2) THEN 
        ESAT=611.*EXP(ALV/RV*(RATIO-1./TS))
      ELSE  
        ESAT=611.*EXP(ALS/RV*(RATIO-1./TS))
      ENDIF
      RHOKOR=ESAT/(RV*TS)
      
      
      ESATW=611.*EXP(ALV/RV*(RATIO-1./TC))
      RHOOMGW=ESATW/(RV*TC)
      ESATI=611.*EXP(ALS/RV*(RATIO-1./TC))
      RHOOMGI=ESATI/(RV*TC)
      
      RHOOMG = RHOOMGI  

      
      
      DELRW=(RHOKOR-RHOOMG) 

  END SUBROUTINE VAPORCLOSE
     
      

  SUBROUTINE MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE,& 
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,SEKDEL,ITYPE,DELRW) 
  

  
            
      IMPLICIT NONE
      REAL*8 D
      REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,ANU,RE,AE,  &
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,SEKDEL,DELRW
      INTEGER ITYPE 
      
      REAL PI, D0, GMW2, GMI2, EW, EI,DGMV
      REAL DENSEL, DENSELI, DENSELW 
      REAL DC 
      REAL VOLL, VOLT 
      REAL VOL1, DGMW_NOSOAK, SOAK, SOAKM
      REAL DENSAC, E
      PI=3.141592654

      
      D0=0.226*1.E-4  
      DI=D0*(TC/273.155)**1.81*(100000./P)
  
      
      
      
      
      
      
      
      
      

      
      EW=1.0      
      
      IF (TC .GE. 273.155) THEN
         EI=1.00
      ELSE IF (TC.GE.233.155) THEN
         EI= 1.0 - ( (273.155 - TS) / 40. )
      ELSE  
         EI = 0.0
      ENDIF

      
      
      
      DENSAC = DENSA * (1.E3) * (1.E-6)
      
      ANU=1.717E-4*(393.0/(TC+120.0))*(TC/273.155)**1.5
      
      RE=D*VT*DENSAC/ANU   
      E=(0.60)**(0.333333333)*(RE**0.50) 
      
      IF(RE.LT.6000.0)THEN
         AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AE=(0.57+9.0E-6*RE)*E
      ENDIF



      
      GM=PI/6.*(D**3.)*DENSE
      GMW=FW*GM
      GMI=GM-GMW
  
      
      GM1=GM
      
      
      
      
      
      
      GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      DGMW=GMW2-GMW 
      GMW=GMW2

      
      GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      DGMI=GMI2-GMI 
      GMI=GMI2
  
      
      
      DGMV = SEKDEL*2*PI*D*AE*DI*DELRW
      IF (DGMV .LT. 0) DGMV=0

      
      DGM=DGMW+DGMI+DGMV
      
      IF (ITYPE.EQ.1) THEN 
          
          
          IF ((DGMW.GT.0).OR.(DGMV.GT.0)) THEN
             
             DC = (0.74*XW / (3.14159*1000.*3.E8))**0.33333333 * 1.E6 
             
             DENSELW = 0.11*(DC*VT / (273.16-TS))**0.76 
             DENSELW = DENSELW * 1000. 
             
             IF (DENSELW.LT.100) DENSELW=100
             IF (DENSELW.GT.900) DENSELW=900
          ENDIF
          IF (DGMI.GT.0) THEN
             
             
             
             
             
             DI = (0.74*XI / (3.14159*1000.*1.E3))**0.33333333 
             DENSELI = 0.13 / DI  
             IF (DENSELI .LT. 100) THEN
                 DENSELI = 100.
             ENDIF
             IF (DENSELI .GT. 900) THEN
                 DENSELI = 900.
             ENDIF
          ENDIF
          
          
          DGMW_NOSOAK = DGMW  
                              

      ELSE 
          
          
          
          VOL1 = GM/DENSE
          
          
          SOAK = 900*VOL1 - GM
          
          SOAKM = DGMW
          
          IF (SOAKM.GT.SOAK) SOAKM=SOAK
          GM = GM+SOAKM  
          
          DENSE = GM/VOL1 

          
          DGMW_NOSOAK = DGMW - SOAKM
          
          
          DENSELW = 900.  
          DENSELI = 900.
         
      ENDIF

      
      
      
      
      IF (DGMI.LE.0) THEN
         VOLL = (DGMW_NOSOAK+DGMV) / DENSELW
      ELSE IF (DGMW.LE.0) THEN
         VOLL = (DGMI) / DENSELI
      ELSE
         VOLL = (DGMI) / DENSELI + (DGMW_NOSOAK+DGMV) / DENSELW
      ENDIF

      
      VOLT = VOLL + GM/DENSE
      
      DENSE = (GM+DGMI+DGMV+DGMW_NOSOAK) / VOLT
      
      GM = GM+DGMI+DGMW_NOSOAK+DGMV
      D = ( (6*GM) / (PI*DENSE) )**0.33333333 

  END SUBROUTINE MASSAGR



  SUBROUTINE HEATBUD(TS,TSm1,TSm2,FW,TC,VT,DELRW,D,DENSA,GM1,GM,DGM,DGMW,     &
                     DGMV,DGMI,GMW,GMI,DI,ANU,RE,AE,SEKDEL,ITYPE,P)
  

  

  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE
      REAL*8 D
      REAL TS,TSm1,TSm2,FW,TC,VT,DELRW,DENSA,GM1,GM,DGM,DGMW,DGMV,  &
                    DGMI,GMW,GMI,DI,ANU,RE,AE,SEKDEL,P
      INTEGER ITYPE
      
      REAL RV, RD, G, PI, ALF, ALV, ALS, CI, CW, AK
      REAL H, AH, TCC, TSC, DELRWC, DENSAC, TDIFF
      REAL DMLT
      REAL TSCm1, TSCm2
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/
      DATA ALS/677.0/,CI/0.5/,CW/1./
      
      
      TSC = TS - 273.155
      TSCm1 = TSm1 - 273.155
      TSCm2 = TSm2 - 273.155
      TCC = TC - 273.155
      DELRWC = DELRW * (1.E3) * (1.E-6)
      DENSAC = DENSA * (1.E3) * (1.E-6)
      


      
      AK=(5.8+0.0184*TCC)*1.E-5  
      
      

      
      
      
      H=(0.71)**(0.333333333)*(RE**0.50) 
      

      
      IF(RE.LT.6000.0)THEN
         AH=0.78+0.308*H
         
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AH=0.76*H
         
      ELSEIF(RE.GE.20000.0) THEN
         AH=(0.57+9.0E-6*RE)*H
         
      ENDIF

      
      


      IF(ITYPE.EQ.1) THEN
      
         
         
         
         
         TSC=0.6*(TSC-TSC*DGM/GM1+SEKDEL/(GM1*CI)*                &
            (2.*PI*D*(AH*AK*(TCC-TSC)-AE*ALS*DI*DELRWC)+     &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)) + &
            0.2*TSCm1 + 0.2*TSCm2
         
         TS = TSC+273.155
         IF (TS.GE.273.155) THEN 
            TS=273.155
            TDIFF = ABS(TS-273.155)
         ENDIF
         TDIFF = ABS(TS-273.155)         
         IF (TDIFF.LE.1.E-6) ITYPE=2  
     
      ELSE IF (ITYPE.EQ.2) THEN
      
         
         IF (TCC.LT.0.) THEN
            
            FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*               &
                (2.*PI*D*(AH*AK*TCC-AE*ALV*DI*DELRWC)+          &
                DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
         ELSE
            
            DMLT = (2.*PI*D*AH*AK*TCC + 2.*PI*D*AE*ALV*DI*DELRWC + &
                    DGMW/SEKDEL*CW*TCC) / ALF
            FW = (FW*GM + DMLT) / GM
         ENDIF
         
         IF(FW.GT.1.)FW=1.
         IF(FW.LT.0.)FW=0.

         
         IF(FW.LE.1.E-6) THEN
            ITYPE=1  
         ENDIF
         
      ENDIF

  END SUBROUTINE HEATBUD


  
  SUBROUTINE BREAKUP(DENSE,D,GM,FW)
  
  
  

      IMPLICIT NONE
      REAL*8 D
      REAL DENSE, GM, FW
      
      REAL WATER, GMI, CRIT, WAT, PI
      DATA PI/3.141592654/

      WATER=FW*GM  
      


      
      
      
      CRIT = 1.0E-10

      
         WAT=WATER-CRIT
         GM=GM-WAT
         FW=(CRIT)/GM
       
         IF(FW.GT.1.0) FW=1.0
         IF(FW.LT.0.0) FW=0.0

         
         
         
         D=(6.*GM/(PI*DENSE))**(0.333333333)
      
  END SUBROUTINE BREAKUP
  
  
  SUBROUTINE MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
  
  
  
  
  
  
  
  
  
  
  
  
  
      IMPLICIT NONE

      REAL*8 D
      REAL TLAYER, PLAYER, RLAYER, LDEPTH, VT
      REAL eenv, delta, ewet, de, der, wetold, wetbulb, wetbulbk
      REAL tdclayer, tclayer, eps, b, hplayer
      REAL*8 a
      REAL sd, lt, ka, lf, lv, t0, dv, pi, rv, rhoice, &
           tres, re, delt, esenv, rhosenv, essfc, rhosfc, dsig, &
           dmdt, mass, massorg, newmass, gamma, r, rho
      INTEGER wcnt
      
      
      tclayer = TLAYER - 273.155
      a = 2.53E11
      b = 5.42E3
      tdclayer = b / LOG(a*eps / (rlayer*player))
      hplayer = player / 100.
      
      
      eps = 0.622
      eenv = (player*rlayer) / (rlayer+eps)
      eenv = eenv / 100.  
      
      
      gamma = 6.6E-4*player
      delta = (4098.0*eenv)/((tdclayer+237.7)*(tdclayer+237.7))
      wetbulb = ((gamma*tclayer)+(delta*tdclayer))/(gamma+delta)
      
      
      wcnt = 0
      DO WHILE (wcnt .lt. 11)
        ewet = 6.108*(exp((17.27*wetbulb)/(237.3 + wetbulb))) 
        de = (0.0006355*hplayer*(tclayer-wetbulb))-(ewet-eenv)
        der= (ewet*(.0091379024 - (6106.396/(273.155+wetbulb)**2))) &
             - (0.0006355*hplayer)
        wetold = wetbulb
        wetbulb = wetbulb - de/der
        wcnt = wcnt + 1
        IF ((abs(wetbulb-wetold)/wetbulb.gt.0.0001)) THEN
           EXIT
        ENDIF
      ENDDO
      
      wetbulbk = wetbulb + 273.155  
      ka = .02 
      lf = 3.34e5 
      lv = 2.5e6  
      t0 = 273.155 
      dv = 0.25e-4 
      pi = 3.1415927
      rv = 1004. - 287. 
      rhoice = 917.0 
      r = D/2. 
      
      
      tres = LDEPTH / VT
        
      
      
      
      rho = 85000./(287.*TLAYER)
      re = rho*r*VT*.01/1.7e-5
      
      
      delt = wetbulb 
                            

      
      
      esenv = 610.8*(exp((17.27*wetbulb)/  &
               (237.3 + wetbulb))) 
      rhosenv = esenv/(rv*wetbulbk)
      essfc = 610.8*(exp((17.27*(t0-273.155))/  &
               (237.3 + (t0-273.155)))) 
      rhosfc = essfc/(rv*t0)
      dsig = rhosenv - rhosfc

      
      dmdt = (-1.7*pi*r*(re**0.5)/lf)*((ka*delt)+((lv-lf)*dv*dsig))
      IF (dmdt.gt.0.) dmdt = 0
      mass = dmdt*tres
      
      
      massorg = 1.33333333*pi*r*r*r*rhoice
      newmass = massorg + mass
      if (newmass.lt.0.0) newmass = 0.0
      D = 2.*(0.75*newmass/(pi*rhoice))**0.333333333
  END SUBROUTINE MELT

  
END MODULE module_diag_hailcast

