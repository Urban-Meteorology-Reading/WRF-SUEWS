

MODULE module_bl_acm



  REAL, PARAMETER      :: RIC    = 0.25                
  REAL, PARAMETER      :: CRANKP = 0.5                 

CONTAINS



   SUBROUTINE ACMPBL(XTIME,    DTPBL,    ZNW,   SIGMAH,               &
                     U3D,      V3D,      PP3D,  DZ8W, TH3D, T3D,      &
                     QV3D,     QC3D,     QI3D,  RR3D,                 &
                     UST,      HFX,      QFX,   TSK,                  &
                     PSFC,     EP1,      G,                           &
                     ROVCP,    RD,       CPD,                         &
                     PBLH,     KPBL2D,   EXCH_H, REGIME,              &
                     GZ1OZ0,   WSPD,     PSIM, MUT, RMOL,             &
                     RUBLTEN,  RVBLTEN,  RTHBLTEN,                    &
                     RQVBLTEN, RQCBLTEN, RQIBLTEN,                    &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte)































































 





























     IMPLICIT NONE



    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte, XTIME


    REAL,                                INTENT(IN)  ::  DTPBL, EP1,   &
                                                        G, ROVCP, RD, CPD

    REAL,    DIMENSION( kms:kme ),       INTENT(IN)  :: ZNW, SIGMAH

    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),                         &
             INTENT(IN) ::                              U3D, V3D,            &
                                                        PP3D, DZ8W, T3D,     &
                                                        QV3D, QC3D, QI3D,    &
                                                        RR3D, TH3D

    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: PSIM, GZ1OZ0,     &
                                                          HFX, QFX, TSK,    &
                                                          PSFC, WSPD, MUT

    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::  PBLH, REGIME,  &
                                                              UST, RMOL

    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),                         &
             INTENT(INOUT)   ::                         RUBLTEN, RVBLTEN,    &
                                                        RTHBLTEN, RQVBLTEN,  &
                                                        RQCBLTEN, RQIBLTEN

   real,     dimension( ims:ime, kms:kme, jms:jme ),                         &
             intent(inout)   ::                         exch_h

    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(OUT  ) ::  KPBL2D

 



      INTEGER :: I, J, K, L

      REAL, DIMENSION( kts:kte ) :: DSIGH, DSIGHI, DSIGFI
      REAL, DIMENSION( 0:kte )   :: SIGMAF
      REAL  RDT
      REAL, PARAMETER :: KARMAN = 0.4



   RDT = 1.0 / DTPBL

   DO K = 1, kte
     SIGMAF(K-1) = ZNW(K)
   ENDDO
   SIGMAF(kte) = 0.0

   DO K = 1, kte
     DSIGH(K)  = SIGMAF(K) - SIGMAF(K-1)
     DSIGHI(K) = 1.0 / DSIGH(K)
   ENDDO

   DO K = kts,kte-1
     DSIGFI(K) = 1.0 / (SIGMAH(K+1) - SIGMAH(K))
   ENDDO

   DSIGFI(kte) = DSIGFI(kte-1)
   
   DO j = jts,jte   
      CALL ACM2D(j=J,xtime=XTIME, dtpbl=DTPBL, sigmaf=SIGMAF, sigmah=SIGMAH    &
              ,dsigfi=DSIGFI,dsighi=DSIGHI,dsigh=DSIGH             &
              ,us=u3d(ims,kms,j),vs=v3d(ims,kms,j)                 &
              ,theta=th3d(ims,kms,j),tt=t3d(ims,kms,j)             &
              ,qvs=qv3d(ims,kms,j),qcs=qc3d(ims,kms,j)             &
              ,qis=qi3d(ims,kms,j)                                 &
              ,dzf=DZ8W(ims,kms,j)                                 &
              ,densx=RR3D(ims,kms,j)                               &
              ,utnp=rublten(ims,kms,j),vtnp=rvblten(ims,kms,j)     &
              ,ttnp=rthblten(ims,kms,j),qvtnp=rqvblten(ims,kms,j)  &
              ,qctnp=rqcblten(ims,kms,j),qitnp=rqiblten(ims,kms,j) &
              ,cpd=cpd,g=g,rovcp=rovcp,rd=rd,rdt=rdt               &
              ,psfcpa=psfc(ims,j),ust=ust(ims,j)                   &
              ,pbl=pblh(ims,j)                                     &
              ,exch_hx=exch_h(ims,kms,j)                           &
              ,regime=regime(ims,j),psim=psim(ims,j)               &
              ,hfx=hfx(ims,j),qfx=qfx(ims,j)                       &
              ,tg=tsk(ims,j),gz1oz0=gz1oz0(ims,j)                  &
              ,wspd=wspd(ims,j) ,klpbl=kpbl2d(ims,j)               &
              ,mut=mut(ims,j), rmol=rmol(ims,j)                    &
              ,ep1=ep1,karman=karman                               &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde   &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme   &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
   ENDDO

   END SUBROUTINE ACMPBL






   SUBROUTINE ACM2D(j,XTIME, DTPBL, sigmaf, sigmah          &
              ,dsigfi,dsighi,dsigh                          &
              ,us,vs,theta,tt,qvs,qcs,qis                   &
              ,dzf,densx,utnp,vtnp,ttnp,qvtnp,qctnp,qitnp   &
              ,cpd,g,rovcp,rd,rdt,psfcpa,ust                &
              ,pbl,exch_hx,regime,psim                      &
              ,hfx,qfx,tg,gz1oz0,wspd ,klpbl                &
              ,mut, rmol                                    &
              ,ep1,karman                                   &
              ,ids,ide, jds,jde, kds,kde   &
              ,ims,ime, jms,jme, kms,kme   &
              ,its,ite, jts,jte, kts,kte   )



      IMPLICIT NONE




      REAL, DIMENSION( 0:kte ),             INTENT(IN)  :: SIGMAF
      REAL, DIMENSION( kms:kme ),           INTENT(IN)  :: SIGMAH
      REAL, DIMENSION( kts:kte ),           INTENT(IN)  :: DSIGH, DSIGHI, DSIGFI
      REAL ,                                INTENT(IN)  :: DTPBL, G, RD,ep1,karman,CPD,ROVCP,RDT
      REAL , DIMENSION( ims:ime ),          INTENT(INOUT)  :: PBL, UST
      
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS, THETA, TT,   &
                                                           QVS, QCS, QIS, DENSX
      REAL,  DIMENSION( ims:ime, kms:kme ), intent(in)  :: DZF
      REAL,  DIMENSION( ims:ime, kms:kme ), intent(inout)   ::  utnp, &
							        vtnp, &
							        ttnp, &
							        qvtnp, &
							        qctnp, &
							        qitnp
      real,     dimension( ims:ime ), intent(in   )   ::   psfcpa
      real,     dimension( ims:ime ), intent(in   )   ::   tg
      real,     dimension( ims:ime ), intent(inout)   ::   regime, rmol
      real,     dimension( ims:ime ), intent(in)      ::   wspd, psim, gz1oz0
      real,     dimension( ims:ime ), intent(in)      ::   hfx, qfx
      real,     dimension( ims:ime ), intent(in)      ::   mut
      real,     dimension( ims:ime, kms:kme ),                    &
                intent(inout)                         ::   exch_hx

      INTEGER, DIMENSION( ims:ime ),       INTENT(OUT):: KLPBL
      INTEGER,  INTENT(IN)      ::      XTIME
      integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, j


      INTEGER I, K     
      INTEGER :: KPBLHT
      INTEGER, DIMENSION( its:ite ) :: KPBLH, NOCONV


      REAL    ::  TVCON, WSS, TCONV, TH1, TOG, DTMP, WSSQ
      REAL    ::  ZH1,UH1,VH1                                   
      REAL    ::  psix, THV1
      REAL, DIMENSION( its:ite )          :: FINT, PSTAR, CPAIR
      REAL, DIMENSION( its:ite, kts:kte ) :: THETAV, RIB,             &
                                             EDDYZ, EDDYZM, UX, VX, THETAX,   &
                                             QVX, QCX, QIX, ZA
      REAL, DIMENSION( its:ite, 0:kte )   :: ZF
      REAL,    DIMENSION( its:ite)           :: WST, TST, QST, USTM, TSTV
      REAL,    DIMENSION( its:ite )          :: PBLSIG, MOL
      REAL    ::  FINTT, ZMIX, UMIX, VMIX
      REAL    ::  TMPFX, TMPVTCON, TMPP, TMPTHS, TMPTH1, TMPVCONV, WS1, DTH
	REAL    ::  A,TST12,RL,ZFUNC,DENSF



      INTEGER :: KL, jtf, ktf, itf, KMIX, KSRC

        character*512 :: message


      DO i = its,ite
        DO k = kts,kte
          utnp(i,k) = 0.
          vtnp(i,k) = 0.
          ttnp(i,k) = 0.
        ENDDO
      ENDDO

      DO k = kts,kte
        DO i = its,ite
          qvtnp(i,k) = 0.
        ENDDO
      ENDDO

      DO k = kts,kte
        DO i = its,ite
          qctnp(i,k) = 0.
          qitnp(i,k) = 0.
        ENDDO
      ENDDO



     DO I = its,ite
           CPAIR(I)  = CPD * (1.0 + 0.84 * QVS(I,1))                    
           TMPFX     = HFX(I)  / (cpair(i) * DENSX(I,1))
           TMPVTCON  = 1.0 + EP1 * QVS(I,1)                             
           WS1       = SQRT(US(I,1)**2 + VS(I,1)**2)                    
           TST(I)    = -TMPFX / UST(I)
           QST(I)    = -QFX(I) / (UST(I)*DENSX(I,1))
           USTM(I)   = UST(I) * WS1 / wspd(i)
           THV1      = TMPVTCON * THETA(I,1) 
           TSTV(I)   = TST(I)*TMPVTCON + THV1*EP1*QST(I)
           IF(ABS(TSTV(I)).LT.1.0E-6) THEN
             TSTV(I) = SIGN(1.0E-6,TSTV(I))
           ENDIF
           MOL(I)    = THV1 * UST(i)**2/(KARMAN*G*TSTV(I))
           RMOL(I)   = 1./MOL(I)
           WST(I)    = UST(I) * (PBL(I)/(KARMAN*ABS(MOL(I)))) ** 0.333333       
           PSTAR(I)  =  MUT(I)/1000.                                     
     ENDDO





     DO I = its,ite
       ZF(I,0)    = 0.0
       KLPBL(I)   = 1
     ENDDO

     DO K = kts, kte
       DO I = its,ite
         ZF(I,K) = DZF(I,K) + ZF(I,K-1)
         ZA(I,K) = 0.5 * (ZF(I,K) + ZF(I,K-1))
       ENDDO
     ENDDO

     DO K = kts, kte
       DO I = its,ite
         TVCON       = 1.0 + EP1 * QVS(I,K)
         THETAV(I,K) = THETA(I,K) * TVCON
       ENDDO
     ENDDO



     DO 100 I = its,ite
       DO K = 1,kte
         KSRC = K
         IF (SIGMAF(K).lT.0.9955) GO TO 69
       ENDDO
69     CONTINUE
       TH1 = 0.0
       ZH1 = 0.0
       UH1 = 0.0
       VH1 = 0.0
       DO K = 1,KSRC
         TH1 = TH1 + THETAV(I,K)  
         ZH1 = ZH1 + ZA(I,K)
         UH1 = UH1 + US(I,K)
         VH1 = VH1 + VS(I,K)
       ENDDO  
       TH1 = TH1/KSRC
       ZH1 = ZH1/KSRC
       UH1 = UH1/KSRC
       VH1 = VH1/KSRC
       IF(MOL(I).LT.0.0 .AND. XTIME.GT.1) then
         WSS   = (UST(I) ** 3 + 0.6 * WST(I) ** 3) ** 0.33333
         TCONV = -8.5 * UST(I) * TSTV(I) / WSS
         TH1   = TH1 + TCONV
       ENDIF

99     KMIX = KSRC
       DO K = KSRC,kte
         DTMP   = THETAV(I,K) - TH1
         IF (DTMP.LT.0.0) KMIX = K
       ENDDO
       IF(KMIX.GT.KSRC) THEN
         FINTT = (TH1 - THETAV(I,KMIX)) / (THETAV(I,KMIX+1)               &
               - THETAV(I,KMIX))
         ZMIX = FINTT * (ZA(I,KMIX+1)-ZA(I,KMIX)) + ZA(I,KMIX)
         UMIX = FINTT * (US(I,KMIX+1)-US(I,KMIX)) + US(I,KMIX)
         VMIX = FINTT * (VS(I,KMIX+1)-VS(I,KMIX)) + VS(I,KMIX)
       ELSE
         ZMIX = ZH1
         UMIX = UH1
         VMIX = VH1
       ENDIF
       DO K = KMIX,kte
         DTMP   = THETAV(I,K) - TH1
         TOG = 0.5 * (THETAV(I,K) + TH1) / G
         WSSQ = (US(I,K)-UMIX)**2                                     &
              + (VS(I,K)-VMIX)**2
         IF (KMIX == KSRC) WSSQ = WSSQ + 100.*UST(I)*UST(I) 
         WSSQ = MAX( WSSQ, 0.1 )
         RIB(I,K) = ABS(ZA(I,K)-ZMIX) * DTMP / (TOG * WSSQ)
         IF (RIB(I,K) .GE. RIC) GO TO 201
       ENDDO

       write (message, *)' RIBX never exceeds RIC, RIB(i,kte) = ',rib(i,5),        &
               ' THETAV(i,1) = ',thetav(i,1),' MOL=',mol(i),            &
               ' TCONV = ',TCONV,' WST = ',WST(I),                      &
               ' KMIX = ',kmix,' UST = ',UST(I),                       &
               ' TST = ',TST(I),' U,V = ',US(I,1),VS(I,1),              &
               ' I,J=',I,J
       CALL wrf_error_fatal3("<stdin>",425,&
message )
201    CONTINUE

       KPBLH(I) = K

100  CONTINUE

     DO I = its,ite
       IF (KPBLH(I) .GT. KSRC) THEN

         FINT(I) = (RIC - RIB(I,KPBLH(I)-1)) / (RIB(I,KPBLH(I)) -       &
                    RIB(I,KPBLH(I)-1))
         IF (FINT(I) .GT. 0.5) THEN
           KPBLHT  = KPBLH(I)
           FINT(I) = FINT(I) - 0.5
         ELSE
           KPBLHT  = KPBLH(I) - 1
           FINT(I) = FINT(I) + 0.5
         ENDIF
         PBL(I)  = FINT(I) * (ZF(I,KPBLHT) - ZF(I,KPBLHT-1)) +          &
                     ZF(I,KPBLHT-1)
         KLPBL(I) = KPBLHT
         PBLSIG(I)   = FINT(I) * DSIGH(KPBLHT) + SIGMAF(KPBLHT-1)    
       ELSE
         KLPBL(I) = KSRC
         PBL(I)    = ZA(I,KSRC)                                                  
         PBLSIG(I)   = SIGMAH(KSRC)                                             
       ENDIF

     ENDDO

     DO I = its,ite       
       NOCONV(I) = 0
       

       IF (PBL(I) / MOL(I) .LT. -0.02 .AND. KLPBL(I) .GT. 3        &
           .AND. THETAV(I,1) .GT. THETAV(I,2) .AND. XTIME .GT. 1) THEN
          NOCONV(I)   = 1
          REGIME(I) = 4.0                     
       ENDIF
     ENDDO


     CALL EDDYX(DTPBL, ZF,  ZA,     MOL, PBL,  UST,                &
                US,    VS,  TT,  THETAV, DENSX, PSTAR,              &
                QVS,   QCS, QIS, DSIGFI, G, RD, CPAIR,              &
                EDDYZ, EDDYZM, its,ite, kts,kte,ims,ime, kms,kme)

     CALL ACM(DTPBL, PSTAR,  NOCONV, SIGMAF, DSIGH, DSIGHI, J,      &
                 KLPBL, PBL,   PBLSIG, MOL,  UST,                  &
                 TST, QST,  USTM,   EDDYZ,  DENSX,                  &
                 THETA,  QVS,    QCS,    QIS,        &
                 THETAX, QVX,    QCX,    QIX,        &
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte)
     CALL ACMM(DTPBL, PSTAR,  NOCONV, SIGMAF, DSIGH, DSIGHI, J,      &
                 KLPBL, PBL,   PBLSIG, MOL,  UST,                  &
                 TST, QST,  USTM,  EDDYZM, DENSX,                  &
                 US,    VS,         &
                 UX,    VX,         &
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte)



     DO K = kts, kte-1
       DO I = its, ite
         DENSF     = 0.5 * (DENSX(I,K+1) + DENSX(I,K))
         exch_hx(I,K) = EDDYZ(I,K) /( (DENSF * G / PSTAR(I)) ** 2 *  &
                       DTPBL * DSIGFI(K)*1E-6 )
       ENDDO
     ENDDO



     DO K = kts, kte
       DO I = its, ite
         UTNP(I,K)  = UTNP(I,K) + (UX(I,K) - US(I,K)) * RDT
         VTNP(I,K)  = VTNP(I,K) + (VX(I,K) - VS(I,K)) * RDT
         TTNP(I,K)  = TTNP(I,K) + (THETAX(I,K) - THETA(I,K)) * RDT
         QVTNP(I,K) = QVTNP(I,K) + (QVX(I,K) - QVS(I,K)) * RDT
         QCTNP(I,K) = QCTNP(I,K) + (QCX(I,K) - QCS(I,K)) * RDT
         QITNP(I,K) = QITNP(I,K) + (QIX(I,K) - QIS(I,K)) * RDT
       ENDDO
     ENDDO

   END SUBROUTINE ACM2D





   SUBROUTINE ACMINIT(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,           &
                      RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
                      restart, allowed_to_read ,                   &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )












     IMPLICIT NONE

   LOGICAL , INTENT(IN)          :: restart , allowed_to_read

   INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR


   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
                                                         RUBLTEN, &
                                                         RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN, &
                                                         RQCBLTEN, & 
                                                         RQIBLTEN


   INTEGER :: i, j, k, itf, jtf, ktf


   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RUBLTEN(i,k,j)=0.
        RVBLTEN(i,k,j)=0.
        RTHBLTEN(i,k,j)=0.
        RQVBLTEN(i,k,j)=0.
        RQCBLTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RQIBLTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO
   ENDIF


   END SUBROUTINE acminit





   SUBROUTINE EDDYX(DTPBL, ZF,  ZA,     MOL, PBL,  UST,               &
                    US,    VS,  TT,  THETAV, DENSX, PSTAR,            &
                    QVS,   QCS, QIS, DSIGFI, G, RD, CPAIR,            &
                    EDDYZ, EDDYZM, its,ite, kts,kte,ims,ime,kms,kme )


































      IMPLICIT NONE


  

      INTEGER,  INTENT(IN)   ::    its,ite, kts,kte,ims,ime,kms,kme

      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL, G, RD
      REAL , DIMENSION( kts:kte ),          INTENT(IN)  :: DSIGFI
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: MOL, PSTAR, CPAIR

      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS, TT,   &
                                                           QVS, QCS, QIS, DENSX
      REAL, DIMENSION( its:ite, kts:kte ), INTENT(IN) :: ZA, THETAV
      REAL, DIMENSION( its:ite, 0:kte )  , INTENT(IN) :: ZF
      
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: EDDYZ,EDDYZM




      INTEGER  :: ILX, KL, KLM, K, I


      REAL     :: ZOVL, PHIH, WT, ZSOL, ZFUNC, DZF, SS, GOTH, EDYZ
      REAL     :: RI, QMEAN, TMEAN, XLV, ALPH, CHI, ZK, SQL, DENSF, KZO
      REAL     :: FH, FM
      REAL     :: WM, EDYZM, PHIM

      REAL, PARAMETER :: RV     = 461.5
      REAL, PARAMETER :: RC     = 0.25
      REAL, PARAMETER :: RLAM   = 80.0
      REAL, PARAMETER :: GAMH   = 16.0 
      REAL, PARAMETER :: GAMM   = 16.0 
      REAL, PARAMETER :: BETAH  = 5.0   
      REAL, PARAMETER :: KARMAN = 0.4
      REAL, PARAMETER :: P      = 2.0   
      REAL, PARAMETER :: EDYZ0  = 0.01  
      REAL, PARAMETER :: PR     = 0.8   


      INTEGER, PARAMETER :: imvdif = 1

      ILX = ite 
      KL  = kte
      KLM = kte - 1
      
      DO K = kts,KLM
        DO I = its,ILX
          EDYZ = 0.0
          ZOVL = 0.0
          DZF  = ZA(I,K+1) - ZA(I,K)
          KZO = EDYZ0

          IF (ZF(I,K) .LT. PBL(I)) THEN
            ZOVL = ZF(I,K) / MOL(I)
            IF (ZOVL .LT. 0.0) THEN
              IF (ZF(I,K) .LT. 0.1 * PBL(I)) THEN
                PHIH = 1.0 / SQRT(1.0 - GAMH * ZOVL)
                PHIM = (1.0 - GAMM * ZOVL)**(-0.25)
                WT   = UST(I) / PHIH
                WM   = UST(I) / PHIM
              ELSE
                ZSOL = 0.1 * PBL(I) / MOL(I)
                PHIH = 1.0 / SQRT(1.0 - GAMH * ZSOL)
                PHIM = (1.0 - GAMM * ZSOL)**(-0.25)
                WT   = UST(I) / PHIH
                WM   = UST(I) / PHIM
              ENDIF
            ELSE IF (ZOVL .LT. 1.0) THEN
              PHIH = 1.0 + BETAH * ZOVL
              WT   = UST(I) / PHIH
              WM   = WT
            ELSE
              PHIH = BETAH + ZOVL
              WT   = UST(I) / PHIH
              WM   = WT
            ENDIF
            ZFUNC      = ZF(I,K) * (1.0 - ZF(I,K) / PBL(I)) ** P
            EDYZ = KARMAN * WT * ZFUNC
            EDYZM = KARMAN * WM * ZFUNC
          ENDIF

          SS   = ((US(I,K+1) - US(I,K)) ** 2 + (VS(I,K+1) - VS(I,K)) ** 2)   &
                  / (DZF * DZF) + 1.0E-9
          GOTH = 2.0 * G / (THETAV(I,K+1) + THETAV(I,K))
          RI   = GOTH * (THETAV(I,K+1) - THETAV(I,K)) / (DZF * SS)


          IF(imvdif.eq.1)then
            IF ((QCS(I,K)+QIS(I,K)) .GT. 0.01E-3 .OR. (QCS(I,K+1)+             &
                 QIS(I,K+1)) .GT. 0.01E-3) THEN
              QMEAN = 0.5 * (QVS(I,K) + QVS(I,K+1))
              TMEAN = 0.5 * (TT(I,K) + TT(I,K+1))
              XLV   = (2.501 - 0.00237 * (TMEAN - 273.15)) * 1.E6
              ALPH  =  XLV * QMEAN / RD / TMEAN
              CHI   =  XLV * XLV * QMEAN / CPAIR(I) / RV / TMEAN / TMEAN
              RI    = (1.0 + ALPH) * (RI -G * G / SS / TMEAN / CPAIR(I) *       &
                      ((CHI - ALPH) / (1.0 + CHI)))
            ENDIF
          ENDIF

            
	        ZK  = 0.4 * ZF(I,K)
          SQL = (ZK * RLAM / (RLAM + ZK)) ** 2
            
          IF (RI .GE. 0.0) THEN






            FH=1./(1.+10.*RI+50.*RI**2+5000.*RI**4)+0.0012  
            FM= PR*FH + 0.00104

            EDDYZ(I,K) = KZO + SQRT(SS) * FH * SQL
            EDDYZM(I,K) = KZO + SQRT(SS) * FM * SQL
          ELSE
            EDDYZ(I,K) = KZO + SQRT(SS * (1.0 - 25.0 * RI)) * SQL
            EDDYZM(I,K) = EDDYZ(I,K) * PR
          ENDIF
	  
          IF(EDYZ.GT.EDDYZ(I,K)) THEN
            EDDYZ(I,K) = EDYZ
            EDDYZM(I,K) = MIN(EDYZM,EDYZ*0.8)  
          ENDIF

          EDDYZ(I,K) = MIN(1000.0,EDDYZ(I,K))
          EDDYZ(I,K) = MAX(KZO,EDDYZ(I,K))
          EDDYZM(I,K) = MIN(1000.0,EDDYZM(I,K))
          EDDYZM(I,K) = MAX(KZO,EDDYZM(I,K))

          DENSF     = 0.5 * (DENSX(I,K+1) + DENSX(I,K))

          EDDYZ(I,K) = EDDYZ(I,K) * (DENSF * G / PSTAR(I)) ** 2 *       &
                       DTPBL * DSIGFI(K)*1E-6
          EDDYZM(I,K) = EDDYZM(I,K) * (DENSF * G / PSTAR(I)) ** 2 *       &
                       DTPBL * DSIGFI(K)*1E-6


        ENDDO             
      ENDDO               

      DO I = its,ILX
        EDDYZ(I,KL) = 0.0 
        EDDYZM(I,KL) = 0.0
      ENDDO

   END SUBROUTINE EDDYX





   SUBROUTINE ACM (DTPBL, PSTAR,  NOCONV, SIGMAF, DSIGH, DSIGHI, JX, &
                   KLPBL, PBL,   PBLSIG, MOL,  UST,                  &
                   TST, QST,  USTM,   EDDYZ,  DENSX,               &
                   THETA,  QVS,    QCS,    QIS,     &
                   THETAX, QVX,    QCX,    QIX,     &
                   ids,ide, jds,jde, kds,kde,                      &
                   ims,ime, jms,jme, kms,kme,                      &
                   its,ite, jts,jte, kts,kte)














































      IMPLICIT NONE




      INTEGER,  INTENT(IN)      ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, JX
      INTEGER,  DIMENSION( its:ite ), INTENT(IN)  :: NOCONV
      INTEGER,  DIMENSION( ims:ime ), INTENT(IN)  :: KLPBL


      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: PSTAR, PBLSIG,  &
                                                           MOL, TST, &
                                                           QST, USTM
      REAL , DIMENSION( kts:kte ),          INTENT(IN)  :: DSIGHI, DSIGH
      REAL , DIMENSION( 0:kte ),            INTENT(IN)  :: SIGMAF
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(INOUT)  :: EDDYZ
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: THETA,   &
                                                           QVS, QCS, QIS, DENSX
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: THETAX,      &
                                                           QVX, QCX, QIX



      INTEGER, PARAMETER :: NSP   = 4 




      REAL,    PARAMETER :: G1000 = 9.8 * 1.0E-3
      REAL,    PARAMETER :: XX    = 0.5          
      REAL,    PARAMETER :: KARMAN = 0.4


      INTEGER :: ILX, KL, KLM, I, K, NSPX, NLP, NL, JJ, L,LL
      INTEGER :: KCBLMX
      INTEGER, DIMENSION( its:ite ) :: KCBL


      REAL                               :: G1000I, MBMAX, HOVL, MEDDY, MBAR
      REAL                               :: EKZ, RZ, FM, WSPD, DTS, DTRAT, F1
      REAL, DIMENSION( its:ite )         :: PSTARI, FSACM, DTLIM
      REAL, DIMENSION( kts:kte, its:ite) :: MBARKS, MDWN
      REAL, DIMENSION( kts:kte )         :: XPLUS, XMINUS
      REAL  DELC
      REAL, DIMENSION( kts:kte )                :: AI, BI, CI, EI 
      REAL, ALLOCATABLE, DIMENSION( : , : )     :: DI, UI    
      REAL, ALLOCATABLE, DIMENSION( : , : )     :: FS
      REAL, ALLOCATABLE, DIMENSION( : , : , : ) :: VCI

      CHARACTER*80 :: message




      ILX = ite
      KL  = kte
      KLM = kte - 1
      NSPX = NSP

      G1000I = 1.0 / G1000
      KCBLMX = 0
      MBMAX  = 0.0

      ALLOCATE (DI( 1:NSPX,kts:kte ))       
      ALLOCATE (UI( 1:NSPX,kts:kte ))  
      ALLOCATE (FS( 1:NSPX, its:ite )) 
      ALLOCATE (VCI( 1:NSPX,its:ite,kts:kte  ))


      DO I = its, ILX
        DTLIM(I)  = DTPBL
        PSTARI(I) = 1.0 / PSTAR(I)
        KCBL(I)   = 1
        FSACM(I)  = 0.0

        IF (NOCONV(I) .EQ. 1) THEN
          KCBL(I) = KLPBL(I)



          HOVL     = -PBL(I) / MOL(I)
          FSACM(I) = 1./(1.+((KARMAN/(HOVL))**0.3333)/(0.72*KARMAN))
          MEDDY    = EDDYZ(I,1) / (DTPBL * (PBLSIG(I) - SIGMAF(1)))
          MBAR     = MEDDY * FSACM(I)
          DO K = kts,KCBL(I)-1
            EDDYZ(I,K) = EDDYZ(I,K) * (1.0 - FSACM(I))
          ENDDO

          MBMAX = AMAX1(MBMAX,MBAR)
          DO K = kts+1,KCBL(I)
            MBARKS(K,I) = MBAR
            MDWN(K,I)   = MBAR * (PBLSIG(I) - SIGMAF(K-1)) * DSIGHI(K)
          ENDDO
          MBARKS(1,I) = MBAR
          MBARKS(KCBL(I),I) = MDWN(KCBL(I),I)
          MDWN(KCBL(I)+1,I) = 0.0
        ENDIF
      ENDDO                              

      DO K = kts,KLM
        DO I = its,ILX
          EKZ   = EDDYZ(I,K) / DTPBL * DSIGHI(K)
          DTLIM(I) = AMIN1(0.75 / EKZ,DTLIM(I))
        ENDDO
      ENDDO
       
      DO I = its,ILX 
        IF (NOCONV(I) .EQ. 1) THEN
          KCBLMX = AMAX0(KLPBL(I),KCBLMX)
          RZ     = (SIGMAF(KCBL(I)) - SIGMAF(1)) * DSIGHI(1)
          DTLIM(I)  = AMIN1(XX / (MBARKS(1,I) * RZ),DTLIM(I))
        ENDIF
      ENDDO

      DO K = kts,KL
        DO I = its,ILX
          VCI(1,I,K) = THETA(I,K)
          VCI(2,I,K) = QVS(I,K)
          
          
          VCI(3,I,K) = QCS(I,K)
          VCI(4,I,K) = QIS(I,K)
        ENDDO
      ENDDO

      DO I = its,ILX
        FS(1,I) = -UST(I) * TST(I) * DENSX(I,1) * PSTARI(I)
        FS(2,I) = -UST(I) * QST(I) * DENSX(I,1) * PSTARI(I)
        FS(3,I) = 0.0
        FS(4,I) = 0.0                      
      ENDDO


      DO I = its,ILX      

        NLP   = INT(DTPBL / DTLIM(I) + 1.0)
        DTS   = (DTPBL / NLP)
        DTRAT = DTS / DTPBL
        DO NL = 1,NLP           



          DO K = kts,kte
            AI(K) = 0.0
            BI(K) = 0.0
            CI(K) = 0.0
            EI(K) = 0.0
          ENDDO

          DO K = 2, KCBL(I)
            EI(K-1) = -CRANKP * MDWN(K,I) * DTS * DSIGH(K) * DSIGHI(K-1)
            BI(K)   = 1.0 + CRANKP * MDWN(K,I) * DTS
            AI(K)   = -CRANKP * MBARKS(K,I) * DTS
          ENDDO

          EI(1) = EI(1) -EDDYZ(I,1) * CRANKP * DSIGHI(1 )* DTRAT
          AI(2) = AI(2) -EDDYZ(I,1) * CRANKP * DSIGHI(2) * DTRAT

          DO K =  KCBL(I)+1, KL
            BI(K) = 1.0
          ENDDO

          DO K = 2,KL
            XPLUS(K)  = EDDYZ(I,K) * DSIGHI(K) * DTRAT
            XMINUS(K) = EDDYZ(I,K-1) * DSIGHI(K) * DTRAT
            CI(K)     = - XMINUS(K) * CRANKP
            EI(K)     = EI(K) - XPLUS(K) * CRANKP
            BI(K)     = BI(K) + XPLUS(K) * CRANKP + XMINUS(K) * CRANKP
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            BI(1) = 1.0 + CRANKP * MBARKS(1,I) * (PBLSIG(I) - SIGMAF(1)) *    &
                    DTS * DSIGHI(1) + EDDYZ(I,1) * DSIGHI(1) * CRANKP * DTRAT
          ELSE
            BI(1) = 1.0  + EDDYZ(I,1) * DSIGHI(1) * CRANKP * DTRAT
          ENDIF


          DO K = 1,KL
            DO L = 1,NSPX                    
              DI(L,K) = 0.0
            ENDDO
          ENDDO


          DO K = 2,KCBL(I)
            DO L = 1,NSPX                    
              DELC = DTS * (MBARKS(K,I) * VCI(L,I,1) - MDWN(K,I) *          &
                 VCI(L,I,K) + DSIGH(K+1) * DSIGHI(K) *                  &
                        MDWN(K+1,I) * VCI(L,I,K+1))
              DI(L,K)   = VCI(L,I,K) + (1.0 - CRANKP) * DELC
            ENDDO
          ENDDO

          DO K = KCBL(I)+1, KL
            DO L = 1,NSPX                    
              DI(L,K) = VCI(L,I,K)
            ENDDO
          ENDDO

          DO K = 2,KL
            IF (K .EQ. KL) THEN
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K)  - (1.0 - CRANKP) * XMINUS(K) *                  &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ELSE
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K) + (1.0 - CRANKP) * XPLUS(K) *                   &
                          (VCI(L,I,K+1) - VCI(L,I,K))  -                         &
                          (1.0 - CRANKP) * XMINUS(K) *                           &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ENDIF
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            DO L = 1,NSPX                    
              F1    = -G1000I * (MBARKS(1,I) *                                &
                      (PBLSIG(I) - SIGMAF(1)) * VCI(L,I,1) -                  &
                      MDWN(2,I) * VCI(L,I,2) * DSIGH(2))

              DI(L,1) = VCI(L,I,1) - G1000 * (FS(L,I) - (1.0 - CRANKP)        &
                        * F1) * DSIGHI(1) * DTS
            ENDDO
          ELSE
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) - G1000 * FS(L,I) * DSIGHI(1) * DTS
            ENDDO
          ENDIF
          DO L = 1,NSPX                    
            DI(L,1) = DI(L,1) + (1.0 - CRANKP) * EDDYZ(I,1) * DSIGHI(1)      &
                     * DTRAT * (VCI(L,I,2) - VCI(L,I,1))
          ENDDO
          IF ( NOCONV(I) .EQ. 1 ) THEN
            CALL MATRIX (AI, BI, CI, DI, EI, UI, KL, NSPX)
          ELSE
            CALL TRI (CI, BI, EI, DI, UI, KL, NSPX)
          END IF


          DO K = 1,KL
            DO L = 1,NSPX                    
              VCI(L,I,K) = UI(L,K)
            ENDDO
          ENDDO

        ENDDO                   
      ENDDO                     



      DO K = kts,KL
        DO I = its,ILX
          THETAX(I,K) = VCI(1,I,K)
          QVX(I,K)    = VCI(2,I,K)
          QCX(I,K)    = VCI(3,I,K)
          QIX(I,K)    = VCI(4,I,K)
      ENDDO
      ENDDO

      DEALLOCATE (DI)       
      DEALLOCATE (UI)  
      DEALLOCATE (FS)
      DEALLOCATE (VCI)

   END SUBROUTINE ACM



   SUBROUTINE ACMM (DTPBL, PSTAR,  NOCONV, SIGMAF, DSIGH, DSIGHI, JX, &
                   KLPBL, PBL,   PBLSIG, MOL,  UST,                  &
                   TST, QST,  USTM,   EDDYZM, DENSX,        &
                   US,    VS,     &
                   UX,    VX,     &
                   ids,ide, jds,jde, kds,kde,                      &
                   ims,ime, jms,jme, kms,kme,                      &
                   its,ite, jts,jte, kts,kte)










































      IMPLICIT NONE




      INTEGER,  INTENT(IN)      ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, JX
      INTEGER,  DIMENSION( its:ite ), INTENT(IN)  :: NOCONV
      INTEGER,  DIMENSION( ims:ime ), INTENT(IN)  :: KLPBL


      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: PSTAR, PBLSIG,  &
                                                           MOL, TST, &
                                                           QST, USTM
      REAL , DIMENSION( kts:kte ),          INTENT(IN)  :: DSIGHI, DSIGH
      REAL , DIMENSION( 0:kte ),            INTENT(IN)  :: SIGMAF
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(INOUT)  :: EDDYZM
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS,    &
                                                           DENSX
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: UX, VX



      INTEGER, PARAMETER :: NSP   = 2




      REAL,    PARAMETER :: G1000 = 9.8 * 1.0E-3
      REAL,    PARAMETER :: XX    = 0.5          
      REAL,    PARAMETER :: KARMAN = 0.4


      INTEGER :: ILX, KL, KLM, I, K, NSPX, NLP, NL, JJ, L
      INTEGER :: KCBLMX
      INTEGER, DIMENSION( its:ite ) :: KCBL


      REAL                               :: G1000I, MBMAX, HOVL, MEDDY, MBAR
      REAL                               :: EKZ, RZ, FM, WSPD, DTS, DTRAT, F1
      REAL, DIMENSION( its:ite )         :: PSTARI, FSACM, DTLIM
      REAL, DIMENSION( kts:kte, its:ite) :: MBARKS, MDWN
      REAL, DIMENSION( 1:NSP, its:ite )  :: FS
      REAL, DIMENSION( kts:kte )         :: XPLUS, XMINUS
      REAL  DELC
      REAL, DIMENSION( 1:NSP,its:ite,kts:kte  ) :: VCI

      REAL, DIMENSION( kts:kte )               :: AI, BI, CI, EI 
      REAL, DIMENSION( 1:NSP,kts:kte )         :: DI, UI    



      ILX = ite
      KL  = kte
      KLM = kte - 1

      G1000I = 1.0 / G1000
      KCBLMX = 0
      MBMAX  = 0.0


      DO I = its, ILX
        DTLIM(I)  = DTPBL
        PSTARI(I) = 1.0 / PSTAR(I)
        KCBL(I)   = 1
        FSACM(I)  = 0.0

        IF (NOCONV(I) .EQ. 1) THEN
          KCBL(I) = KLPBL(I)



          HOVL     = -PBL(I) / MOL(I)
          FSACM(I) = 1./(1.+((KARMAN/(HOVL))**0.3333)/(0.72*KARMAN))
          MEDDY    = EDDYZM(I,1) / (DTPBL * (PBLSIG(I) - SIGMAF(1)))
          MBAR     = MEDDY * FSACM(I)
          DO K = kts,KCBL(I)-1
            EDDYZM(I,K) = EDDYZM(I,K) * (1.0 - FSACM(I))
          ENDDO

          MBMAX = AMAX1(MBMAX,MBAR)
          DO K = kts+1,KCBL(I)
            MBARKS(K,I) = MBAR
            MDWN(K,I)   = MBAR * (PBLSIG(I) - SIGMAF(K-1)) * DSIGHI(K)
          ENDDO
          MBARKS(1,I) = MBAR
          MBARKS(KCBL(I),I) = MDWN(KCBL(I),I)
          MDWN(KCBL(I)+1,I) = 0.0
        ENDIF
      ENDDO                              

      DO K = kts,KLM
        DO I = its,ILX
          EKZ   = EDDYZM(I,K) / DTPBL * DSIGHI(K)
          DTLIM(I) = AMIN1(0.75 / EKZ,DTLIM(I))
        ENDDO
      ENDDO
       
      DO I = its,ILX 
        IF (NOCONV(I) .EQ. 1) THEN
          KCBLMX = AMAX0(KLPBL(I),KCBLMX)
          RZ     = (SIGMAF(KCBL(I)) - SIGMAF(1)) * DSIGHI(1)
          DTLIM(I)  = AMIN1(XX / (MBARKS(1,I) * RZ),DTLIM(I))
        ENDIF
      ENDDO

      DO K = kts,KL
        DO I = its,ILX
          VCI(1,I,K) = US(I,K)
          VCI(2,I,K) = VS(I,K)
        ENDDO
      ENDDO

      NSPX=2

      DO I = its,ILX
        FM      = -USTM(I) * USTM(I) * DENSX(I,1) * PSTARI(I)
        WSPD    = SQRT(US(I,1) * US(I,1) + VS(I,1) * VS(I,1)) + 1.E-9
        FS(1,I) = FM * US(I,1) / WSPD
        FS(2,I) = FM * VS(I,1) / WSPD
      ENDDO


      DO I = its,ILX      

        NLP   = INT(DTPBL / DTLIM(I) + 1.0)
        DTS   = (DTPBL / NLP)
        DTRAT = DTS / DTPBL
        DO NL = 1,NLP           



          DO K = kts,KL
            AI(K) = 0.0
            BI(K) = 0.0
            CI(K) = 0.0
            EI(K) = 0.0
          ENDDO

          DO K = 2, KCBL(I)
            EI(K-1) = -CRANKP * MDWN(K,I) * DTS * DSIGH(K) * DSIGHI(K-1)
            BI(K)   = 1.0 + CRANKP * MDWN(K,I) * DTS
            AI(K)   = -CRANKP * MBARKS(K,I) * DTS
          ENDDO

          EI(1) = EI(1) -EDDYZM(I,1) * CRANKP * DSIGHI(1 )* DTRAT
          AI(2) = AI(2) -EDDYZM(I,1) * CRANKP * DSIGHI(2) * DTRAT

          DO K =  KCBL(I)+1, KL
            BI(K) = 1.0
          ENDDO

          DO K = 2,KL
            XPLUS(K)  = EDDYZM(I,K) * DSIGHI(K) * DTRAT
            XMINUS(K) = EDDYZM(I,K-1) * DSIGHI(K) * DTRAT
            CI(K)     = - XMINUS(K) * CRANKP
            EI(K)     = EI(K) - XPLUS(K) * CRANKP
            BI(K)     = BI(K) + XPLUS(K) * CRANKP + XMINUS(K) * CRANKP
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            BI(1) = 1.0 + CRANKP * MBARKS(1,I) * (PBLSIG(I) - SIGMAF(1)) *    &
                    DTS * DSIGHI(1) + EDDYZM(I,1) * DSIGHI(1) * CRANKP * DTRAT
          ELSE
            BI(1) = 1.0  + EDDYZM(I,1) * DSIGHI(1) * CRANKP * DTRAT
          ENDIF


          DO K = 1,KL
            DO L = 1,NSPX                    
              DI(L,K) = 0.0
            ENDDO
          ENDDO


          DO K = 2,KCBL(I)
            DO L = 1,NSPX                    
              DELC = DTS * (MBARKS(K,I) * VCI(L,I,1) - MDWN(K,I) *          &
                 VCI(L,I,K) + DSIGH(K+1) * DSIGHI(K) *                  &
                        MDWN(K+1,I) * VCI(L,I,K+1))
              DI(L,K)   = VCI(L,I,K) + (1.0 - CRANKP) * DELC
            ENDDO
          ENDDO

          DO K = KCBL(I)+1, KL
            DO L = 1,NSPX                    
              DI(L,K) = VCI(L,I,K)
            ENDDO
          ENDDO

          DO K = 2,KL
            IF (K .EQ. KL) THEN
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K)  - (1.0 - CRANKP) * XMINUS(K) *                  &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ELSE
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K) + (1.0 - CRANKP) * XPLUS(K) *                   &
                          (VCI(L,I,K+1) - VCI(L,I,K))  -                         &
                          (1.0 - CRANKP) * XMINUS(K) *                           &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ENDIF
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            DO L = 1,NSPX                    
              F1    = -G1000I * (MBARKS(1,I) *                                &
                      (PBLSIG(I) - SIGMAF(1)) * VCI(L,I,1) -                  &
                      MDWN(2,I) * VCI(L,I,2) * DSIGH(2))

              DI(L,1) = VCI(L,I,1) - G1000 * (FS(L,I) - (1.0 - CRANKP)        &
                        * F1) * DSIGHI(1) * DTS
            ENDDO
          ELSE
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) - G1000 * FS(L,I) * DSIGHI(1) * DTS
            ENDDO
          ENDIF
          DO L = 1,NSPX                    
            DI(L,1) = DI(L,1) + (1.0 - CRANKP) * EDDYZM(I,1) * DSIGHI(1)      &
                     * DTRAT * (VCI(L,I,2) - VCI(L,I,1))
          ENDDO
          IF ( NOCONV(I) .EQ. 1 ) THEN
            CALL MATRIX (AI, BI, CI, DI, EI, UI, KL, NSPX)
          ELSE
            CALL TRI (CI, BI, EI, DI, UI, KL, NSPX)
          END IF


          DO K = 1,KL
            DO L = 1,NSPX                    
              VCI(L,I,K) = UI(L,K)
            ENDDO
          ENDDO

        ENDDO                   
      ENDDO                     



      DO K = kts,KL
        DO I = its,ILX
          UX(I,K)     = VCI(1,I,K)
          VX(I,K)     = VCI(2,I,K)
        ENDDO
      ENDDO

   END SUBROUTINE ACMM




   SUBROUTINE MATRIX(A,B,C,D,E,X,KL,NSP)
   


   IMPLICIT NONE




























      INTEGER, INTENT(IN)   :: KL
      INTEGER, INTENT(IN)   :: NSP
      REAL A(KL),B(KL),E(KL)
      REAL C(KL),D(NSP,KL),X(NSP,KL)


      REAL Y(NSP,KL),AIJ,SUM
      REAL L(KL,KL),UII(KL),UIIP1(KL),RUII(KL)
      INTEGER I,J,V


      L(1,1) = 1.
      UII(1) = B(1)
      RUII(1) = 1./UII(1)
      DO I = 2, KL
	      L(I,I) = 1.
	      L(I,1) = A(I)/B(1)
        UIIP1(I-1)=E(I-1)
	      IF(I.GE.3) THEN
	        DO J = 2,I-1
	          IF(I.EQ.J+1) THEN
	            AIJ = C(I)
	          ELSE
	            AIJ = 0.
	          ENDIF
	          L(I,J) = (AIJ-L(I,J-1)*E(J-1))/      &
                      (B(J)-L(J,J-1)*E(J-1))
	        ENDDO
	      ENDIF
      ENDDO
	  
      DO I = 2,KL
        UII(I) = B(I)-L(I,I-1)*E(I-1)
        RUII(I) = 1./UII(I)
      ENDDO
  

      DO V= 1, NSP
        Y(V,1) = D(V,1)
        DO I=2,KL
	        SUM = D(V,I)
	        DO J=1,I-1
	          SUM = SUM - L(I,J)*Y(V,J)
	        ENDDO
	        Y(V,I) = SUM
        ENDDO
      ENDDO



      DO V= 1, NSP
        X(V,KL) = Y(V,KL)*RUII(KL)
      ENDDO
      DO I = KL-1,1,-1
        DO V= 1, NSP
         X(V,I) = (Y(V,I)-UIIP1(I)*X(V,I+1))*RUII(I)
        ENDDO
      ENDDO

   END SUBROUTINE MATRIX




      SUBROUTINE TRI ( L, D, U, B, X,KL,NSP)






















      IMPLICIT NONE



      INTEGER, INTENT(IN)   :: KL
      INTEGER, INTENT(IN)   :: NSP

      REAL        L( KL )               
      REAL        D(KL)   
      REAL        U( KL )               
      REAL        B(NSP,KL )   
      REAL        X( NSP,KL )   



      REAL        GAM( KL )
      REAL        BET
      INTEGER     V, K


      BET = 1.0 / D( 1 )
      DO V = 1, NSP
         X( V,1 ) = BET * B(V,1 )
      ENDDO

      DO K = 2, KL
        GAM(K ) = BET * U( K-1 )
        BET = 1.0 / ( D( K ) - L( K ) * GAM( K ) )
	      DO V = 1, NSP
           X( V, K ) = BET * ( B( V,K ) - L( K ) * X( V,K-1 ) )
	      ENDDO
      ENDDO



      DO K = KL - 1, 1, -1
        DO V = 1, NSP
          X( V,K ) = X( V,K ) - GAM( K+1 ) * X( V,K+1 )
        ENDDO
      ENDDO
     
  END SUBROUTINE TRI



END MODULE module_bl_acm
                        
