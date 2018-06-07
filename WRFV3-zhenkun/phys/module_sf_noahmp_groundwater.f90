MODULE module_sf_noahmp_groundwater








CONTAINS

  SUBROUTINE WTABLE_mmf_noahmp (NSOIL     ,XLAND    ,XICE    ,XICE_THRESHOLD  ,ISICE ,& 
                                ISLTYP    ,SMOISEQ  ,DZS     ,WTDDT                  ,& 
                                FDEPTH    ,AREA     ,TOPO    ,ISURBAN ,IVGTYP        ,& 
                                RIVERCOND ,RIVERBED ,EQWTD   ,PEXP                   ,& 
                                SMOIS     ,SH2OXY   ,SMCWTD  ,WTD  ,QRF              ,& 
                                DEEPRECH  ,QSPRING  ,QSLAT   ,QRFS ,QSPRINGS  ,RECH  ,& 
                                ids,ide, jds,jde, kds,kde,                    &
                                ims,ime, jms,jme, kms,kme,                    &
                                its,ite, jts,jte, kts,kte                     )


  USE NOAHMP_TABLES, ONLY: BEXP_TABLE, DKSAT_TABLE, SMCMAX_TABLE,PSISAT_TABLE, SMCWLT_TABLE

  IMPLICIT NONE



  INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
       &                           ims,ime, jms,jme, kms,kme,  &
       &                           its,ite, jts,jte, kts,kte
    REAL,   INTENT(IN)        ::     WTDDT
    REAL,   INTENT(IN)        ::     XICE_THRESHOLD
    INTEGER,  INTENT(IN   )   ::     ISICE
    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(IN   )    ::                          XLAND, &
                                                           XICE
    INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
             INTENT(IN   )    ::                         ISLTYP, &
                                                         IVGTYP
    INTEGER, INTENT(IN)       ::     nsoil
    INTEGER, INTENT(IN)       ::     ISURBAN
    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(IN)      ::                        SMOISEQ
    REAL,     DIMENSION(1:nsoil), INTENT(IN)     ::         DZS
    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(IN)       ::                         FDEPTH, &
                                                           AREA, &
                                                           TOPO, &
                                                          EQWTD, &
                                                           PEXP, &
                                                       RIVERBED, &
                                                      RIVERCOND



    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                SH2OXY 


    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(INOUT)    ::                            WTD, &
                                                         SMCWTD, &
                                                       DEEPRECH, &
                                                          QSLAT, &
                                                           QRFS, &
                                                       QSPRINGS, &
                                                           RECH



    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(OUT)      ::                            QRF, &  
                                                        QSPRING     


  
  INTEGER                          :: I,J,K  
  REAL, DIMENSION(       0:NSOIL)  :: ZSOIL 
  REAL,  DIMENSION(      1:NSOIL)  :: SMCEQ  
  REAL,  DIMENSION(      1:NSOIL)  :: SMC,SH2O
  REAL                                        :: DELTAT,RCOND,TOTWATER,PSI &
                                                ,WFLUXDEEP,WCNDDEEP,DDZ,SMCWTDMID &
                                                ,WPLUS,WMINUS
  REAL,      DIMENSION( ims:ime, jms:jme )    :: QLAT
  INTEGER,   DIMENSION( ims:ime, jms:jme )    :: LANDMASK 
  
  REAL :: BEXP,DKSAT,PSISAT,SMCMAX,SMCWLT

    DELTAT = WTDDT * 60. 

    ZSOIL(0) = 0.
    ZSOIL(1) = -DZS(1)
    DO K = 2, NSOIL
       ZSOIL(K)         = -DZS(K) + ZSOIL(K-1)
    END DO

    WHERE(XLAND-1.5.LT.0..AND.XICE.LT. XICE_THRESHOLD.AND.IVGTYP.NE.ISICE)
         LANDMASK=1
    ELSEWHERE
         LANDMASK=-1
    ENDWHERE



    QLAT = 0.
CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      &
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )




    DO J=jts,jte
       DO I=its,ite
          IF(LANDMASK(I,J).GT.0)THEN
             IF(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) THEN
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             ELSE    
               RCOND = RIVERCOND(I,J)       
             ENDIF
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)

             QRF(I,J) = MAX(QRF(I,J),0.)
          ELSE
             QRF(I,J) = 0.
          ENDIF
       ENDDO
    ENDDO


    DO J=jts,jte
       DO I=its,ite
          IF(LANDMASK(I,J).GT.0)THEN

            BEXP   = BEXP_TABLE   (ISLTYP(I,J))
            DKSAT  = DKSAT_TABLE  (ISLTYP(I,J))
            PSISAT = -1.0*PSISAT_TABLE (ISLTYP(I,J))
            SMCMAX = SMCMAX_TABLE (ISLTYP(I,J))
            SMCWLT = SMCWLT_TABLE (ISLTYP(I,J))

             IF(IVGTYP(I,J)==ISURBAN)THEN
                 SMCMAX = 0.45
                 SMCWLT = 0.40
             ENDIF


             IF(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL))THEN

                DDZ = ZSOIL(NSOIL)-WTD(I,J)
                SMCWTDMID = 0.5 * (SMCWTD(I,J) + SMCMAX )
                PSI = PSISAT * ( SMCMAX / SMCWTD(I,J) ) ** BEXP
                WCNDDEEP = DKSAT * ( SMCWTDMID / SMCMAX ) ** (2.0*BEXP + 3.0)
                WFLUXDEEP =  - DELTAT * WCNDDEEP * ( (PSISAT-PSI) / DDZ - 1.)

                SMCWTD(I,J) = SMCWTD(I,J)  + (DEEPRECH(I,J) -  WFLUXDEEP)  / DDZ
                WPLUS       = MAX((SMCWTD(I,J)-SMCMAX), 0.0) * DDZ
                WMINUS       = MAX((1.E-4-SMCWTD(I,J)), 0.0) * DDZ
                SMCWTD(I,J) = MAX( MIN(SMCWTD(I,J),SMCMAX) , 1.E-4)
                WFLUXDEEP = WFLUXDEEP + WPLUS - WMINUS
                DEEPRECH(I,J) = WFLUXDEEP
              ENDIF



             TOTWATER = QLAT(I,J) - QRF(I,J) + DEEPRECH(I,J)

             SMC(1:NSOIL) = SMOIS(I,1:NSOIL,J)
             SH2O(1:NSOIL) = SH2OXY(I,1:NSOIL,J)
             SMCEQ(1:NSOIL) = SMOISEQ(I,1:NSOIL,J)


             CALL UPDATEWTD ( NSOIL, DZS , ZSOIL, SMCEQ, SMCMAX, SMCWLT, PSISAT, BEXP ,I , J , &
                              TOTWATER, WTD(I,J), SMC, SH2O, SMCWTD(I,J)      , &
                              QSPRING(I,J) ) 


             SMOIS(I,1:NSOIL,J) = SMC(1:NSOIL)
             SH2OXY(I,1:NSOIL,J) = SH2O(1:NSOIL)

           ENDIF
       ENDDO
    ENDDO



    DO J=jts,jte
       DO I=its,ite
           QSLAT(I,J) = QSLAT(I,J) + QLAT(I,J)*1.E3
           QRFS(I,J) = QRFS(I,J) + QRF(I,J)*1.E3
           QSPRINGS(I,J) = QSPRINGS(I,J) + QSPRING(I,J)*1.E3
           RECH(I,J) = RECH(I,J) + DEEPRECH(I,J)*1.E3

           DEEPRECH(I,J) =0.
       ENDDO
    ENDDO


END  SUBROUTINE WTABLE_mmf_noahmp


  SUBROUTINE LATERALFLOW  (ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                           ,ids,ide,jds,jde,kds,kde                      &
                           ,ims,ime,jms,jme,kms,kme                      &
                           ,its,ite,jts,jte,kts,kte                      )

  USE NOAHMP_TABLES, ONLY : DKSAT_TABLE

  IMPLICIT NONE


  INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
       &                           ims,ime, jms,jme, kms,kme,  &
       &                           its,ite, jts,jte, kts,kte
  REAL                                  , INTENT(IN) :: DELTAT                                 
  INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: ISLTYP, LANDMASK
  REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: FDEPTH,WTD,TOPO,AREA


  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: QLAT


  INTEGER                              :: I, J, itsh,iteh,jtsh,jteh
  REAL                                 :: Q,KLAT
  REAL, DIMENSION( ims:ime , jms:jme ) :: KCELL, HEAD

  REAL, DIMENSION(19)      :: KLATFACTOR
  DATA KLATFACTOR /2.,3.,4.,10.,10.,12.,14.,20.,24.,28.,40.,48.,2.,0.,10.,0.,20.,2.,2./

  REAL,    PARAMETER :: PI = 3.14159265 
  REAL,    PARAMETER :: FANGLE = 0.22754493   

itsh=max(its-1,ids)
iteh=min(ite+1,ide-1)
jtsh=max(jts-1,jds)
jteh=min(jte+1,jde-1)


    DO J=jtsh,jteh
       DO I=itsh,iteh
           IF(FDEPTH(I,J).GT.0.)THEN
                 KLAT = DKSAT_TABLE(ISLTYP(I,J)) * KLATFACTOR(ISLTYP(I,J))
                 IF(WTD(I,J) < -1.5)THEN
                     KCELL(I,J) = FDEPTH(I,J) * KLAT * EXP( (WTD(I,J) + 1.5) / FDEPTH(I,J) )
                 ELSE
                     KCELL(I,J) = KLAT * ( WTD(I,J) + 1.5 + FDEPTH(I,J) )  
                 ENDIF
           ELSE
                 KCELL(i,J) = 0.
           ENDIF

           HEAD(I,J) = TOPO(I,J) + WTD(I,J)
       ENDDO
    ENDDO

itsh=max(its,ids+1)
iteh=min(ite,ide-2)
jtsh=max(jts,jds+1)
jteh=min(jte,jde-2)

    DO J=jtsh,jteh
       DO I=itsh,iteh
          IF(LANDMASK(I,J).GT.0)THEN
                 Q=0.
                             
                 Q  = Q + (KCELL(I-1,J+1)+KCELL(I,J)) &
                        * (HEAD(I-1,J+1)-HEAD(I,J))/SQRT(2.)
                             
                 Q  = Q +  (KCELL(I-1,J)+KCELL(I,J)) &
                        *  (HEAD(I-1,J)-HEAD(I,J))

                 Q  = Q +  (KCELL(I-1,J-1)+KCELL(I,J)) &
                        * (HEAD(I-1,J-1)-HEAD(I,J))/SQRT(2.)

                 Q  = Q +  (KCELL(I,J+1)+KCELL(I,J)) &
                        * (HEAD(I,J+1)-HEAD(I,J))

                 Q  = Q +  (KCELL(I,J-1)+KCELL(I,J)) &
                        * (HEAD(I,J-1)-HEAD(I,J))

                 Q  = Q +  (KCELL(I+1,J+1)+KCELL(I,J)) &
                        * (HEAD(I+1,J+1)-HEAD(I,J))/SQRT(2.)
  
                 Q  = Q +  (KCELL(I+1,J)+KCELL(I,J)) &
                        * (HEAD(I+1,J)-HEAD(I,J))

                 Q  = Q +  (KCELL(I+1,J-1)+KCELL(I,J)) &
                        * (HEAD(I+1,J-1)-HEAD(I,J))/SQRT(2.)


                 QLAT(I,J) = FANGLE* Q * DELTAT / AREA(I,J)
          ENDIF
       ENDDO
    ENDDO


END  SUBROUTINE LATERALFLOW


  SUBROUTINE UPDATEWTD  (NSOIL,  DZS,  ZSOIL ,SMCEQ                ,& 
                         SMCMAX, SMCWLT, PSISAT, BEXP ,ILOC ,JLOC  ,& 
                         TOTWATER, WTD ,SMC, SH2O ,SMCWTD          ,& 
                         QSPRING                                 )  

  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: NSOIL 
  INTEGER,                         INTENT(IN) :: ILOC, JLOC
  REAL,                         INTENT(IN)    :: SMCMAX
  REAL,                         INTENT(IN)    :: SMCWLT
  REAL,                         INTENT(IN)    :: PSISAT
  REAL,                         INTENT(IN)    :: BEXP
  REAL,  DIMENSION(       0:NSOIL), INTENT(IN) :: ZSOIL 
  REAL,  DIMENSION(       1:NSOIL), INTENT(IN) :: SMCEQ  
  REAL,  DIMENSION(       1:NSOIL), INTENT(IN) :: DZS 

  REAL                           , INTENT(INOUT) :: TOTWATER
  REAL                           , INTENT(INOUT) :: WTD
  REAL                           , INTENT(INOUT) :: SMCWTD
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O

  REAL                           , INTENT(OUT) :: QSPRING

  INTEGER                                     :: K
  INTEGER                                     :: K1
  INTEGER                                     :: IWTD
  INTEGER                                     :: KWTD
  REAL                                        :: MAXWATUP, MAXWATDW ,WTDOLD
  REAL                                        :: WGPMID
  REAL                                        :: SYIELDDW
  REAL                                        :: DZUP
  REAL                                        :: SMCEQDEEP
  REAL, DIMENSION(       1:NSOIL)             :: SICE




  QSPRING=0.

  SICE = SMC - SH2O

iwtd=1


IF(totwater.gt.0.)then


         if(wtd.ge.zsoil(nsoil))then

            do k=nsoil-1,1,-1
              if(wtd.lt.zsoil(k))exit
            enddo
            iwtd=k
            kwtd=iwtd+1


            maxwatup=dzs(kwtd)*(smcmax-smc(kwtd))

            if(totwater.le.maxwatup)then
               smc(kwtd) = smc(kwtd) + totwater / dzs(kwtd)
               smc(kwtd) = min(smc(kwtd),smcmax)
               if(smc(kwtd).gt.smceq(kwtd))wtd = min ( ( smc(kwtd)*dzs(kwtd) &
                 - smceq(kwtd)*zsoil(iwtd) + smcmax*zsoil(kwtd) ) / &
                     ( smcmax-smceq(kwtd) ) , zsoil(iwtd) )
               totwater=0.
            else   
              smc(kwtd) = smcmax
              totwater=totwater-maxwatup
              k1=iwtd
              do k=k1,0,-1
                 wtd = zsoil(k)
                 iwtd=k-1
                 if(k.eq.0)exit
                 maxwatup=dzs(k)*(smcmax-smc(k))
                 if(totwater.le.maxwatup)then
                   smc(k) = smc(k) + totwater / dzs(k)
                   smc(k) = min(smc(k),smcmax)
                   if(smc(k).gt.smceq(k))wtd = min ( ( smc(k)*dzs(k) &
                     - smceq(k)*zsoil(iwtd) + smcmax*zsoil(k) ) / &
                     ( smcmax-smceq(k) ) , zsoil(iwtd) )
                   totwater=0.
                   exit
                 else
                    smc(k) = smcmax
                    totwater=totwater-maxwatup
                 endif

              enddo

            endif

         elseif(wtd.ge.zsoil(nsoil)-dzs(nsoil))then 

            
               smceqdeep = smcmax * ( psisat / &
                           (psisat - dzs(nsoil)) ) ** (1./bexp)

               smceqdeep = max(smceqdeep,1.E-4)

            maxwatup=(smcmax-smcwtd)*dzs(nsoil)

            if(totwater.le.maxwatup)then
                smcwtd = smcwtd + totwater / dzs(nsoil)
                smcwtd = min(smcwtd,smcmax)
                if(smcwtd.gt.smceqdeep)wtd = min( ( smcwtd*dzs(nsoil) &
                 - smceqdeep*zsoil(nsoil) + smcmax*(zsoil(nsoil)-dzs(nsoil)) ) / &
                     ( smcmax-smceqdeep ) , zsoil(nsoil) )
                totwater=0.
            else
                smcwtd=smcmax
                totwater=totwater-maxwatup
                do k=nsoil,0,-1
                    wtd=zsoil(k)
                    iwtd=k-1
                    if(k.eq.0)exit
                    maxwatup=dzs(k)*(smcmax-smc(k))
                    if(totwater.le.maxwatup)then
                     smc(k) = min(smc(k) + totwater / dzs(k),smcmax)
                     if(smc(k).gt.smceq(k))wtd = min ( ( smc(k)*dzs(k) &
                        - smceq(k)*zsoil(iwtd) + smcmax*zsoil(k) ) / &
                           ( smcmax-smceq(k) ) , zsoil(iwtd) )
                     totwater=0.
                     exit
                    else
                     smc(k) = smcmax
                     totwater=totwater-maxwatup
                    endif
                enddo
             endif


       else

            maxwatup=(smcmax-smcwtd)*(zsoil(nsoil)-dzs(nsoil)-wtd)
            if(totwater.le.maxwatup)then
               wtd = wtd + totwater/(smcmax-smcwtd)
               totwater=0.
            else
               totwater=totwater-maxwatup
               wtd=zsoil(nsoil)-dzs(nsoil)
               maxwatup=(smcmax-smcwtd)*dzs(nsoil)
              if(totwater.le.maxwatup)then

            
               smceqdeep = smcmax * ( psisat / &
                           (psisat - dzs(nsoil)) ) ** (1./bexp)

               smceqdeep = max(smceqdeep,1.E-4)

                smcwtd = smcwtd + totwater / dzs(nsoil)
                smcwtd = min(smcwtd,smcmax)
                wtd = ( smcwtd*dzs(nsoil) &
                 - smceqdeep*zsoil(nsoil) + smcmax*(zsoil(nsoil)-dzs(nsoil)) ) / &
                     ( smcmax-smceqdeep )
                totwater=0.
              else
                smcwtd=smcmax
                totwater=totwater-maxwatup
                do k=nsoil,0,-1
                    wtd=zsoil(k)
                    iwtd=k-1
                    if(k.eq.0)exit
                    maxwatup=dzs(k)*(smcmax-smc(k))

                    if(totwater.le.maxwatup)then
                     smc(k) = smc(k) + totwater / dzs(k)
                     smc(k) = min(smc(k),smcmax)
                     if(smc(k).gt.smceq(k))wtd = ( smc(k)*dzs(k) &
                        - smceq(k)*zsoil(iwtd) + smcmax*zsoil(k) ) / &
                           ( smcmax-smceq(k) )
                     totwater=0.
                     exit
                    else
                     smc(k) = smcmax
                     totwater=totwater-maxwatup
                    endif
                   enddo
               endif
             endif
         endif


        qspring=totwater


ELSEIF(totwater.lt.0.)then


         if(wtd.ge.zsoil(nsoil))then 

            do k=nsoil-1,1,-1
               if(wtd.lt.zsoil(k))exit
            enddo
            iwtd=k

               k1=iwtd+1
               do kwtd=k1,nsoil


                  maxwatdw=dzs(kwtd)*(smc(kwtd)-max(smceq(kwtd),sice(kwtd)))

                  if(-totwater.le.maxwatdw)then
                        smc(kwtd) = smc(kwtd) + totwater / dzs(kwtd)
                        if(smc(kwtd).gt.smceq(kwtd))then
                              wtd = ( smc(kwtd)*dzs(kwtd) &
                                 - smceq(kwtd)*zsoil(iwtd) + smcmax*zsoil(kwtd) ) / &
                                 ( smcmax-smceq(kwtd) )
                         else
                              wtd=zsoil(kwtd)
                              iwtd=iwtd+1
                         endif
                         totwater=0.
                         exit
                   else
                         wtd = zsoil(kwtd)
                         iwtd=iwtd+1
                         if(maxwatdw.ge.0.)then
                            smc(kwtd) = smc(kwtd) + maxwatdw / dzs(kwtd)
                            totwater = totwater + maxwatdw
                         endif
                   endif

                enddo

               if(iwtd.eq.nsoil.and.totwater.lt.0.)then
            
               smceqdeep = smcmax * ( psisat / &
                           (psisat - dzs(nsoil)) ) ** (1./bexp)

               smceqdeep = max(smceqdeep,1.E-4)

                  maxwatdw=dzs(nsoil)*(smcwtd-smceqdeep)

                  if(-totwater.le.maxwatdw)then

                       smcwtd = smcwtd + totwater / dzs(nsoil)
                       wtd = max( ( smcwtd*dzs(nsoil) &
                           - smceqdeep*zsoil(nsoil) + smcmax*(zsoil(nsoil)-dzs(nsoil)) ) / &
                            ( smcmax-smceqdeep ) , zsoil(nsoil)-dzs(nsoil) )

                  else

                       wtd=zsoil(nsoil)-dzs(nsoil)
                       smcwtd = smcwtd + totwater / dzs(nsoil)

                       dzup=(smceqdeep-smcwtd)*dzs(nsoil)/(smcmax-smceqdeep)
                       wtd=wtd-dzup
                       smcwtd=smceqdeep

                  endif

                endif



        elseif(wtd.ge.zsoil(nsoil)-dzs(nsoil))then


            
               smceqdeep = smcmax * ( psisat / &
                           (psisat - dzs(nsoil)) ) ** (1./bexp)

               smceqdeep = max(smceqdeep,1.E-4)

            maxwatdw=dzs(nsoil)*(smcwtd-smceqdeep)

            if(-totwater.le.maxwatdw)then

               smcwtd = smcwtd + totwater / dzs(nsoil)
               wtd = max( ( smcwtd*dzs(nsoil) &
                    - smceqdeep*zsoil(nsoil) + smcmax*(zsoil(nsoil)-dzs(nsoil)) ) / &
                    ( smcmax-smceqdeep ) , zsoil(nsoil)-dzs(nsoil) )

            else

               wtd=zsoil(nsoil)-dzs(nsoil)
               smcwtd = smcwtd + totwater / dzs(nsoil)

               dzup=(smceqdeep-smcwtd)*dzs(nsoil)/(smcmax-smceqdeep)
               wtd=wtd-dzup
               smcwtd=smceqdeep

             endif

         else

               wgpmid = smcmax * ( psisat / &
                    (psisat - (zsoil(nsoil)-wtd)) ) ** (1./bexp)

               wgpmid=max(wgpmid,1.E-4)
               syielddw=smcmax-wgpmid
               wtdold=wtd
               wtd = wtdold + totwater/syielddw

               smcwtd = (smcwtd*(zsoil(nsoil)-wtdold)+wgpmid*(wtdold-wtd) ) / (zsoil(nsoil)-wtd)

          endif

          qspring=0.

ENDIF

         SH2O = SMC - SICE


END  SUBROUTINE UPDATEWTD



END MODULE module_sf_noahmp_groundwater
