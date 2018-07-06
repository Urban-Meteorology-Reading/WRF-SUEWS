

      MODULE MODULE_SF_QNSESFC



      USE MODULE_MODEL_CONSTANTS
      USE MODULE_DM, ONLY : WRF_DM_MAXVAL












      INTEGER :: ITRMX=5 

      REAL,PARAMETER :: EPSQ2L=0.01
      REAL,PARAMETER :: VKARMAN=0.4
      REAL,PARAMETER :: CAPA=R_D/CP,ELOCP=2.72E6/CP,RCAP=1./CAPA
      REAL,PARAMETER :: GOCP02=G/CP*2.,GOCP10=G/CP*10.


      REAL,PARAMETER :: EPSU2=1.E-6,EPSUST=1.E-9,EPSZT=1.E-28
      REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC
      REAL,PARAMETER :: BETA=1./273.,CZIL=0.1,EXCML=0.0001,EXCMS=0.0001  &

     &                 ,GLKBR=10.,GLKBS=30.,PI=3.1415926               &
     &                 ,QVISC=2.1E-5,RIC=0.505,SMALL=0.35              &
     &                 ,SQPR=0.84,SQSC=0.84,SQVISC=258.2,TVISC=2.1E-5  &
     &                 ,USTC=0.7,USTR=0.225,VISC=1.5E-5,FH=1.01        &
     &                 ,WWST=1.2,ZTFC=0.1,TOPOFAC=9.0E-6

      REAL,PARAMETER :: BTG=BETA*G,CZIV=SMALL*GLKBS                    &
     &                 ,GRRS=GLKBR/GLKBS                               &
     &                 ,RTVISC=1./TVISC,RVISC=1./VISC                  &
     &                 ,ZQRZT=SQSC/SQPR                                &
     &                 ,FZQ1=RTVISC*QVISC*ZQRZT                        &
     &                 ,FZQ2=RTVISC*QVISC*ZQRZT                        &
     &                 ,FZT1=RVISC *TVISC*SQPR                         &
     &                 ,FZT2=CZIV*GRRS*TVISC*SQPR                      &
     &                 ,FZU1=CZIV*VISC                                 &
     &                 ,PIHF=0.5*PI                                    &
     &                 ,PRT0=0.72                                      &
     &                 ,RQVISC=1./QVISC                                &
     &                 ,USTFC=0.018/G                                  &
     &                 ,WWST2=WWST*WWST                                &
     &                 ,ZILFC=-CZIL*VKARMAN*SQVISC


      INTEGER, PARAMETER :: KZTM=10001,KZTM2=KZTM-2

      REAL :: DZETA1,DZETA2,FH01,FH02,ZTMAX1,ZTMAX2,ZTMIN1,ZTMIN2

      REAL,DIMENSION(KZTM) :: PSIH1,PSIH2,PSIM1,PSIM2



CONTAINS


      SUBROUTINE QNSESFC(ITIMESTEP,HT,DZ                                & 
     &                 ,PMID,PINT,TH,T,QV,QC,U,V,Q2                    &
     &                 ,TSK,QSFC,THZ0,QZ0,UZ0,VZ0                      &
     &                 ,LOWLYR,XLAND                                   &
     &                 ,USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL              &
     &                 ,AKHS,AKMS                                      &
     &                 ,RIB                                            &
     &                 ,CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC         &
     &                 ,QGH,CPM,CT                                     &
     &                 ,U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR          &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                 ,IMS,IME,JMS,JME,KMS,KME                        &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE,SCM_FORCE_FLUX  )


      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,INTENT(IN) :: ITIMESTEP

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LOWLYR

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT,MAVAIL,TSK      &
     &                                             ,XLAND,Z0BASE

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ         &
     &                                                     ,PMID,PINT  &
     &                                                     ,Q2,QC,QV   &
     &                                                     ,T,TH       &
     &                                                     ,U,V   

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PSHLTR,Q10,QSHLTR &
     &                                              ,TH10,TSHLTR,T02   &
     &                                              ,U10,V10,TH02,Q02
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: FLX_LH,HFX,QFX 

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: AKHS,AKMS       &
     &                                                ,PBLH,QSFC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT)   :: RIB

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: QZ0,RMOL,THZ0   &
     &                                                ,USTAR,UZ0,VZ0   &
     &                                                ,ZNT

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CHS,CHS2,CQS2     &
     &                                              ,CPM,CT,FLHC,FLQC  &
     &                                              ,QGH 

     INTEGER,  OPTIONAL,  INTENT(IN )   ::     SCM_FORCE_FLUX  





      INTEGER :: I,J,K,KFLIP,LMH,LPBL,NTSD

      REAL :: A,APESFC,B,BTGX,CWMLOW                                   &
     &       ,DQDT,DTDIF,DTDT,DUDT,DVDT                                &
     &       ,FIS                                                      &
     &       ,P02P,P10P,PLOW,PSFC,PTOP,QLOW,QS02,QS10                  &
     &       ,RAPA,RAPA02,RAPA10,RATIOMX,RDZ,SEAMASK,SM                &
     &       ,T02P,T10P,TEM,TH02P,TH10P,THLOW,THELOW,THM               &
     &       ,TLOW,TZ0,ULOW,VLOW,ZSL

      REAL,DIMENSION(KTS:KTE) :: CWMK,PK,Q2K,QK,THEK,THK,TK,UK,VK

      REAL,DIMENSION(KTS:KTE-1) :: EL,ELM

      REAL,DIMENSION(KTS:KTE+1) :: ZHK

      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: THSK

      REAL,DIMENSION(ITS:ITE,KTS:KTE+1,JTS:JTE) :: ZINT








      DO J=JTS,JTE
      DO K=KTS,KTE+1
      DO I=ITS,ITE
        ZINT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO

      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,KTE+1,J)=HT(I,J)     
        PBLH(I,J)=-1.







      ENDDO
      ENDDO

      DO J=JTS,JTE
      DO K=KTE,KTS,-1
        KFLIP=KTE+1-K
        DO I=ITS,ITE
          ZINT(I,K,J)=ZINT(I,K+1,J)+DZ(I,KFLIP,J)
        ENDDO
      ENDDO
      ENDDO

      NTSD=ITIMESTEP

      IF(NTSD==1)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          USTAR(I,J)=0.1
          FIS=HT(I,J)*G
          SM=XLAND(I,J)-1.


        ENDDO
        ENDDO
      ENDIF


        DO J=JTS,JTE
        DO I=ITS,ITE
          CT(I,J)=0.
        ENDDO
        ENDDO



        setup_integration:  DO J=JTS,JTE


        DO I=ITS,ITE



          LMH=KTE-LOWLYR(I,J)+1

          PTOP=PINT(I,KTE+1,J)      
          PSFC=PINT(I,LOWLYR(I,J),J)

          THSK(I,J)=TSK(I,J)/(PSFC*1.E-5)**CAPA



          SEAMASK=XLAND(I,J)-1.





          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            THK(K)=TH(I,KFLIP,J)
            TK(K)=T(I,KFLIP,J)
            RATIOMX=QV(I,KFLIP,J)
            QK(K)=RATIOMX/(1.+RATIOMX)
            PK(K)=PMID(I,KFLIP,J)
            CWMK(K)=QC(I,KFLIP,J)
            THEK(K)=(CWMK(K)*(-ELOCP/TK(K))+1.)*THK(K)
            Q2K(K)=2.*Q2(I,KFLIP,J)




            ZHK(K)=ZINT(I,K,J)

          ENDDO
          ZHK(KTE+1)=HT(I,J)          

          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            UK(K)=U(I,KFLIP,J)
            VK(K)=V(I,KFLIP,J)
          ENDDO



          LPBL=LMH
          DO K=LMH-1,1,-1
            IF(Q2K(K)<=EPSQ2L*FH) THEN
              LPBL=K
              GO TO 110
            ENDIF
          ENDDO

          LPBL=1





 110      PBLH(I,J)=ZHK(LPBL)-ZHK(LMH+1)






          PLOW=PK(LMH)
          TLOW=TK(LMH)
          THLOW=THK(LMH)
          THELOW=THEK(LMH)
          QLOW=QK(LMH)
          CWMLOW=CWMK(LMH)
          ULOW=UK(LMH)
          VLOW=VK(LMH)
          ZSL=(ZHK(LMH)-ZHK(LMH+1))*0.5
          APESFC=(PSFC*1.E-5)**CAPA
          TZ0=THZ0(I,J)*APESFC

          CALL SFCDIF(NTSD,SEAMASK,THSK(I,J),QSFC(I,J),PSFC            &
     &               ,UZ0(I,J),VZ0(I,J),TZ0,THZ0(I,J),QZ0(I,J)         &
     &               ,USTAR(I,J),ZNT(I,J),Z0BASE(I,J),CT(I,J),RMOL(I,J) &
     &               ,AKMS(I,J),AKHS(I,J),RIB(I,J),PBLH(I,J),MAVAIL(I,J) &
     &               ,CHS(I,J),CHS2(I,J),CQS2(I,J)                     &
     &               ,HFX(I,J),QFX(I,J),FLX_LH(I,J)                    &
     &               ,FLHC(I,J),FLQC(I,J),QGH(I,J),CPM(I,J)            &
     &               ,ULOW,VLOW,TLOW,THLOW,THELOW,QLOW,CWMLOW          &
     &               ,ZSL,PLOW                                         &
     &               ,U10(I,J),V10(I,J),TSHLTR(I,J),TH10(I,J)          &
     &               ,QSHLTR(I,J),Q10(I,J),PSHLTR(I,J)                 &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE,i,j,ZHK(LMH+1)           &
     &               ,SCM_FORCE_FLUX) 




          RAPA=APESFC
          TH02P=TSHLTR(I,J)
          TH10P=TH10(I,J)
          TH02(I,J)=TSHLTR(I,J)    
         IF (SEAMASK.EQ.1.AND.I.EQ.170.AND.J.EQ.20) THEN
          print*,'HFX_SEA_point',HFX(I,J)
         END IF

          RAPA02=RAPA-GOCP02/TH02P
          RAPA10=RAPA-GOCP10/TH10P

          T02P=TH02P*RAPA02
          T10P=TH10P*RAPA10
           T02(I,J) = TH02(I,J)*APESFC  


          P02P=(RAPA02**RCAP)*1.E5
          P10P=(RAPA10**RCAP)*1.E5

          QS02=PQ0/P02P*EXP(A2*(T02P-A3)/(T02P-A4))
          QS10=PQ0/P10P*EXP(A2*(T10P-A3)/(T10P-A4))

          IF(QSHLTR(I,J)>QS02)QSHLTR(I,J)=QS02
          IF(Q10   (I,J)>QS10)Q10   (I,J)=QS10
           Q02(I,J)=QSHLTR(I,J)/(1.-QSHLTR(I,J))  


        ENDDO


      ENDDO setup_integration

 
      END SUBROUTINE QNSESFC


      SUBROUTINE SFCDIF(NTSD,SEAMASK,THS,QS,PSFC                       &
     &                 ,UZ0,VZ0,TZ0,THZ0,QZ0                           &
     &                 ,USTAR,Z0,Z0BASE,CT,RLMO,AKMS,AKHS,RIB,PBLH,WETM &
     &                 ,CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,QGH,CPM &
     &                 ,ULOW,VLOW,TLOW,THLOW,THELOW,QLOW,CWMLOW        &
     &                 ,ZSL,PLOW                                       &
     &                 ,U10,V10,TH02,TH10,Q02,Q10,PSHLTR               &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                 ,IMS,IME,JMS,JME,KMS,KME                        &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE,i,j,ZSFC,SCM_FORCE_FLUX)







      IMPLICIT NONE


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,i,j

      INTEGER,INTENT(IN) :: NTSD

      REAL,INTENT(IN) :: CWMLOW,PBLH,PLOW,QLOW,PSFC,SEAMASK,ZSFC       &
     &                  ,THELOW,THLOW,THS,TLOW,ULOW,VLOW,WETM,ZSL  &
     &                  ,Z0BASE

      REAL,INTENT(OUT) :: CHS,CHS2,CPM,CQS2,CT,FLHC,FLQC,    &
     &                   PSHLTR,Q02,Q10,QGH,RIB,RLMO,TH02,TH10,U10,V10

      REAL,INTENT(INOUT) :: FLX_LH,HFX,QFX  

      REAL,INTENT(INOUT) :: AKHS,AKMS,QZ0,THZ0,USTAR,UZ0,VZ0,Z0,QS,TZ0

      INTEGER,  OPTIONAL,  INTENT(IN )   ::     SCM_FORCE_FLUX





      INTEGER :: ITR,K

      REAL :: A,B,BTGH,BTGX,CXCHL,CXCHS,DTHV,DU2,ELFC,FCT              &
     &       ,HLFLX,HSFLX,HV,PSH02,PSH10,PSHZ,PSHZL,PSM10,PSMZ,PSMZL   &
     &       ,RDZ,RDZT,RLMA,RLMN,RLMP                                  &
     &       ,RLOGT,RLOGU,RWGH,RZ,RZST,RZSU,SIMH,SIMM,TEM,THM          &
     &       ,UMFLX,USTARK,VMFLX,WGHT,WGHTT,WGHTQ,WSTAR2               &
     &       ,X,XLT,XLT4,XLU,XLU4,XT,XT4,XU,XU4,ZETALT,ZETALU          &
     &       ,ZETAT,ZETAU,ZQ,ZSLT,ZSLU,ZT,ZU,TOPOTERM,ZZIL



      REAL :: AKHS02,AKHS10,AKMS02,AKMS10,EKMS10,QSAT10,QSAT2          &
     &       ,RLNT02,RLNT10,RLNU10,SIMH02,SIMH10,SIMM10,T02,T10        &
     &       ,TERM1,RLOW,U10E,V10E,WSTAR,XLT02,XLT024,XLT10            &
     &       ,XLT104,XLU10,XLU104,XU10,XU104,ZT02,ZT10,ZTAT02,ZTAT10   &
     &       ,ZTAU,ZTAU10,ZU10,ZUUZ



      RDZ=1./ZSL
      CXCHL=EXCML*RDZ
      CXCHS=EXCMS*RDZ

      BTGX=G/THLOW
      ELFC=VKARMAN*BTGX

      IF(PBLH>1000.)THEN
        BTGH=BTGX*PBLH
      ELSE
        BTGH=BTGX*1000.
      ENDIF

      RLOW=PLOW/(R_D*TLOW)







      IF(SEAMASK>0.5)THEN 


        DO ITR=1,ITRMX

          Z0=MAX(USTFC*USTAR*USTAR,1.59E-5)




          IF(USTAR<USTC)THEN


            IF(USTAR<USTR)THEN

              IF(NTSD==1)THEN
                AKMS=CXCHS
                AKHS=CXCHS
                QS=QLOW
              ENDIF

              ZU=FZU1*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
              WGHT=AKMS*ZU*RVISC
              RWGH=WGHT/(WGHT+1.)
              UZ0=(ULOW*RWGH+UZ0)*0.5
              VZ0=(VLOW*RWGH+VZ0)*0.5

              ZT=FZT1*ZU
              ZQ=FZQ1*ZT
              WGHTT=AKHS*ZT*RTVISC
              WGHTQ=AKHS*ZQ*RQVISC

              IF(NTSD>1)THEN
                THZ0=((WGHTT*THLOW+THS)/(WGHTT+1.)+THZ0)*0.5
                QZ0=((WGHTQ*QLOW+QS)/(WGHTQ+1.)+QZ0)*0.5
              ELSE
                THZ0=(WGHTT*THLOW+THS)/(WGHTT+1.)            
                QZ0=(WGHTQ*QLOW+QS)/(WGHTQ+1.)
              ENDIF

            ENDIF

            IF(USTAR>=USTR.AND.USTAR<USTC)THEN
              ZU=Z0
              UZ0=0.
              VZ0=0.

              ZT=FZT2*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
              ZQ=FZQ2*ZT
              WGHTT=AKHS*ZT*RTVISC
              WGHTQ=AKHS*ZQ*RQVISC

              IF(NTSD>1)THEN
                THZ0=((WGHTT*THLOW+THS)/(WGHTT+1.)+THZ0)*0.5
                QZ0=((WGHTQ*QLOW+QS)/(WGHTQ+1.)+QZ0)*0.5
              ELSE
                THZ0=(WGHTT*THLOW+THS)/(WGHTT+1.)
                QZ0=(WGHTQ*QLOW+QS)/(WGHTQ+1.)
              ENDIF

            ENDIF

          ELSE

            ZU=Z0
            UZ0=0.
            VZ0=0.

            ZT=Z0
            THZ0=THS

            ZQ=Z0
            QZ0=QS

          ENDIF

          TEM=(TLOW+TZ0)*0.5
          THM=(THELOW+THZ0)*0.5

          A=THM*P608
          B=(ELOCP/TEM-1.-P608)*THM

          DTHV=((THELOW-THZ0)*((QLOW+QZ0+CWMLOW)*(0.5*P608)+1.)        &
     &        +(QLOW-QZ0+CWMLOW)*A+CWMLOW*B) 

          DU2=MAX((ULOW-UZ0)**2+(VLOW-VZ0)**2,EPSU2)
          RIB=BTGX*DTHV*ZSL/DU2








            ZSLU=ZSL+ZU
            ZSLT=ZSL+ZT

            RZSU=ZSLU/ZU
            RZST=ZSLT/ZT

            RLOGU=LOG(RZSU)
            RLOGT=LOG(RZST)





            RLMO=ELFC*AKHS*DTHV/USTAR**3

            ZETALU=ZSLU*RLMO
            ZETALT=ZSLT*RLMO
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO

            ZETALU=MIN(MAX(ZETALU,ZTMIN1),ZTMAX1)
            ZETALT=MIN(MAX(ZETALT,ZTMIN1),ZTMAX1)
            ZETAU=MIN(MAX(ZETAU,ZTMIN1/RZSU),ZTMAX1/RZSU)
            ZETAT=MIN(MAX(ZETAT,ZTMIN1/RZST),ZTMAX1/RZST)





            RZ=(ZETAU-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZ=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)    

            RZ=(ZETALU-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZL=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)

            SIMM=PSMZL-PSMZ+RLOGU

            RZ=(ZETAT-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZ=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)

            RZ=(ZETALT-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZL=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)

            SIMH=(PSHZL-PSHZ+RLOGT)*PRT0


            USTARK=USTAR*VKARMAN
            AKMS=MAX(USTARK/SIMM,CXCHS)
            AKHS=MAX(USTARK/SIMH,CXCHS)





            IF(DTHV<=0.)THEN                                           
              WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                
            ELSE                                                       
              WSTAR2=0.                                                
            ENDIF                                                      
            USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)





        ENDDO  







      ELSE  


        IF(NTSD==1)THEN
          QS=QLOW
        ENDIF

        ZU=Z0
        UZ0=0.
        VZ0=0.

        ZT=ZU*ZTFC
        THZ0=THS


        ZQ=ZT
        QZ0=QS

        TEM=(TLOW+TZ0)*0.5
        THM=(THELOW+THZ0)*0.5

        A=THM*P608
        B=(ELOCP/TEM-1.-P608)*THM

        DTHV=((THELOW-THZ0)*((QLOW+QZ0+CWMLOW)*(0.5*P608)+1.)          &
     &        +(QLOW-QZ0+CWMLOW)*A+CWMLOW*B) 

        DU2=MAX((ULOW-UZ0)**2+(VLOW-VZ0)**2,EPSU2)
        RIB=BTGX*DTHV*ZSL/DU2







          ZSLU=ZSL+ZU

          RZSU=ZSLU/ZU

          RLOGU=LOG(RZSU)

          ZSLT=ZSL+ZU 




          TOPOTERM=TOPOFAC*ZSFC**2.
          TOPOTERM=MAX(TOPOTERM,3.0)

          IF(DTHV>0.)THEN
            ZZIL=ZILFC*TOPOTERM
          ELSE
            ZZIL=ZILFC
          ENDIF



          land_point_iteration: DO ITR=1,ITRMX






            ZT=MAX(EXP(ZZIL*SQRT(USTAR*Z0BASE))*Z0BASE,EPSZT)
            ZT=MAX(ZT,ZU*ZTFC)               

            RZST=ZSLT/ZT
            RLOGT=LOG(RZST)





            RLMO=ELFC*AKHS*DTHV/USTAR**3
            ZETALU=ZSLU*RLMO
            ZETALT=ZSLT*RLMO
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO

            ZETALU=MIN(MAX(ZETALU,ZTMIN2),ZTMAX2)
            ZETALT=MIN(MAX(ZETALT,ZTMIN2),ZTMAX2)
            ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)
            ZETAT=MIN(MAX(ZETAT,ZTMIN2/RZST),ZTMAX2/RZST)





            RZ=(ZETAU-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZ=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

            RZ=(ZETALU-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZL=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

            SIMM=PSMZL-PSMZ+RLOGU

            RZ=(ZETAT-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZ=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

            RZ=(ZETALT-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZL=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

            SIMH=(PSHZL-PSHZ+RLOGT)*PRT0


            USTARK=USTAR*VKARMAN
            AKMS=MAX(USTARK/SIMM,CXCHL)
            AKHS=MAX(USTARK/SIMH,CXCHL)
            THZ0=HFX/(AKHS*RLOW*CP)+THLOW
            TZ0=(PSFC*1.E-5)**CAPA*THZ0





            IF(DTHV<=0.)THEN                                           
              WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                
            ELSE                                                       
              WSTAR2=0.                                                
            ENDIF                                                      

            USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)


          ENDDO land_point_iteration






      ENDIF  










       CT=0.











      WSTAR=SQRT(WSTAR2)/WWST

      UMFLX=AKMS*(ULOW -UZ0 )
      VMFLX=AKMS*(VLOW -VZ0 )
      HSFLX=AKHS*(THLOW-THZ0)
      HLFLX=AKHS*(QLOW -QZ0 )















        ZU10=ZU+10.
        ZT02=ZT+02.
        ZT10=ZT+10.

        RLNU10=LOG(ZU10/ZU)
        RLNT02=LOG(ZT02/ZT)
        RLNT10=LOG(ZT10/ZT)

        ZTAU10=ZU10*RLMO
        ZTAT02=ZT02*RLMO
        ZTAT10=ZT10*RLMO





        IF(SEAMASK>0.5)THEN


          ZTAU10=MIN(MAX(ZTAU10,ZTMIN1),ZTMAX1)
          ZTAT02=MIN(MAX(ZTAT02,ZTMIN1),ZTMAX1)
          ZTAT10=MIN(MAX(ZTAT10,ZTMIN1),ZTMAX1)

          RZ=(ZTAU10-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSM10=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)

          SIMM10=PSM10-PSMZ+RLNU10

          RZ=(ZTAT02-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH02=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)

          SIMH02=(PSH02-PSHZ+RLNT02)*PRT0


          RZ=(ZTAT10-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH10=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)

          SIMH10=(PSH10-PSHZ+RLNT10)*PRT0


          AKMS10=MAX(USTARK/SIMM10,CXCHS)
          AKHS02=MAX(USTARK/SIMH02,CXCHS)
          AKHS10=MAX(USTARK/SIMH10,CXCHS)





        ELSE


          ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
          ZTAT02=MIN(MAX(ZTAT02,ZTMIN2),ZTMAX2)
          ZTAT10=MIN(MAX(ZTAT10,ZTMIN2),ZTMAX2)

          RZ=(ZTAU10-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)

          SIMM10=PSM10-PSMZ+RLNU10

          RZ=(ZTAT02-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH02=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

          SIMH02=(PSH02-PSHZ+RLNT02)*PRT0


          RZ=(ZTAT10-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH10=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)

          SIMH10=(PSH10-PSHZ+RLNT10)*PRT0


          AKMS10=MAX(USTARK/SIMM10,CXCHL)
          AKHS02=MAX(USTARK/SIMH02,CXCHL)
          AKHS10=MAX(USTARK/SIMH10,CXCHL)

        ENDIF



      U10 =UMFLX/AKMS10+UZ0
      V10 =VMFLX/AKMS10+VZ0
      TH02=HSFLX/AKHS02+THZ0
      TH10=HSFLX/AKHS10+THZ0
      Q02 =HLFLX/AKHS02+QZ0
      Q10 =HLFLX/AKHS10+QZ0
      TERM1=-0.068283/TLOW
      PSHLTR=PSFC*EXP(TERM1)





      U10E=U10
      V10E=V10

      IF(SEAMASK<0.5)THEN






        ZUUZ=AMIN1(ZU*0.50,0.18)
        ZU=AMAX1(ZU*0.35,ZUUZ)

        ZU10=ZU+10.
        RZSU=ZU10/ZU
        RLNU10=LOG(RZSU)

        ZETAU=ZU*RLMO
        ZTAU10=ZU10*RLMO

        ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
        ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)

        RZ=(ZTAU10-ZTMIN2)/DZETA2
        K=INT(RZ)
        RDZT=RZ-REAL(K)
        K=MIN(K,KZTM2)
        K=MAX(K,0)
        PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
        SIMM10=PSM10-PSMZ+RLNU10
        EKMS10=MAX(USTARK/SIMM10,CXCHL)

        U10E=UMFLX/EKMS10+UZ0
        V10E=VMFLX/EKMS10+VZ0

      ENDIF

      U10=U10E
      V10=V10E





      CHS=AKHS
      CHS2=AKHS02
      CQS2=AKHS02
     



     
      IF ( PRESENT(SCM_FORCE_FLUX) ) THEN              
         IF (SCM_FORCE_FLUX.EQ.0) THEN              
           HFX=-RLOW*CP*HSFLX
           QFX=-RLOW*HLFLX*WETM
         ENDIF
      ENDIF
      FLX_LH=XLV*QFX
      FLHC=RLOW*CP*AKHS
      FLQC=RLOW*AKHS*WETM

      QGH=((1.-SEAMASK)*PQ0+SEAMASK*PQ0SEA)                            &
     &     /PLOW*EXP(A2S*(TLOW-A3S)/(TLOW-A4S))
      QGH=QGH/(1.-QGH)    
      CPM=CP*(1.+0.8*QLOW)






      IF(SEAMASK>0.5)THEN
        QS=QLOW+QFX/(RLOW*AKHS)
        QS=QS/(1.-QS)
      ENDIF


      END SUBROUTINE SFCDIF


      SUBROUTINE QNSESFCINIT(LOWLYR,USTAR,Z0                            &
     &                     ,SEAMASK,XICE,IVGTYP,RESTART                &
     &                     ,ALLOWED_TO_READ                            &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)

      IMPLICIT NONE

      LOGICAL,INTENT(IN) :: RESTART,ALLOWED_TO_READ

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IVGTYP

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: LOWLYR

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SEAMASK,XICE

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: USTAR,Z0

      REAL,DIMENSION(0:30) :: VZ0TBL
      REAL,DIMENSION(0:30) :: VZ0TBL_24

      INTEGER :: I,IDUM,IRECV,J,JDUM,K,ITF,JTF,KTF,MAXGBL_IVGTYP       &
     &,          MAXLOC_IVGTYP,MPI_COMM_COMP



      REAL :: SM,X,ZETA1,ZETA2,ZRNG1,ZRNG2,PSIMQ,PSIHQ

      REAL :: PIHF=3.1415926/2.,EPS=1.E-6

      VZ0TBL=                                                          &
     &  (/0.,                                                          &
     &    2.653,0.826,0.563,1.089,0.854,0.856,0.035,0.238,0.065,0.076  &
     &   ,0.011,0.035,0.011,0.000,0.000,0.000,0.000,0.000,0.000,0.000  &
     &   ,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000/)

      VZ0TBL_24= (/0.,                                                 &
     &            1.00,  0.07,  0.07,  0.07,  0.07,  0.15,             &
     &            0.08,  0.03,  0.05,  0.86,  0.80,  0.85,             &
     &            2.65,  1.09,  0.80,  0.001, 0.04,  0.05,             &
     &            0.01,  0.04,  0.06,  0.05,  0.03,  0.001,            &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)



      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)




      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1

      ENDDO
      ENDDO


      IF(.NOT.RESTART)THEN
        DO J=JTS,JTE
        DO I=ITS,ITF
          USTAR(I,J)=0.1
        ENDDO
        ENDDO
      ENDIF





      FH01=1.
      FH02=1.





      ZTMIN1=-5.0

      ZTMAX1=2.3   
      ZTMIN2=-5.0

      ZTMAX2=2.3   

      ZRNG1=ZTMAX1-ZTMIN1
      ZRNG2=ZTMAX2-ZTMIN2

      DZETA1=ZRNG1/(KZTM-1)
      DZETA2=ZRNG2/(KZTM-1)





      ZETA1=ZTMIN1
      ZETA2=ZTMIN2

      DO K=1,KZTM





        IF(ZETA1<0.)THEN




          X=SQRT(SQRT(1.-16.*ZETA1))
 
          PSIM1(K)=-2.*LOG((X+1.)/2.)-LOG((X*X+1.)/2.)+2.*ATAN(X)-PIHF
          PSIH1(K)=-2.*LOG((X*X+1.)/2.)





          PSIMQ=2.25*ZETA1+ZETA1**2+45.9*ZETA1**3
          PSIHQ=2.0*ZETA1+10.3*ZETA1**2+130*ZETA1**3

          PSIM1(K)=MAX(PSIMQ,PSIM1(K))
          PSIH1(K)=MAX(PSIHQ,PSIH1(K))





        ELSE


















          PSIM1(K)=2.25*ZETA1-0.2*ZETA1*ZETA1
          PSIH1(K)=2.0*ZETA1+0.14*((ZETA1-0.5)**5+0.03125)



        ENDIF





        IF(ZETA2<0.)THEN





          X=SQRT(SQRT(1.-16.*ZETA2))
 
          PSIM2(K)=-2.*LOG((X+1.)/2.)-LOG((X*X+1.)/2.)+2.*ATAN(X)-PIHF
          PSIH2(K)=-2.*LOG((X*X+1.)/2.)





          PSIMQ=2.25*ZETA2+ZETA2**2+45.9*ZETA2**3
          PSIHQ=2.0*ZETA2+10.3*ZETA2**2+130*ZETA2**3

          PSIM2(K)=MAX(PSIMQ,PSIM2(K))
          PSIH2(K)=MAX(PSIHQ,PSIH2(K))





        ELSE



















          PSIM2(K)=2.25*ZETA2-0.2*ZETA2*ZETA2
          PSIH2(K)=2.0*ZETA2+0.14*((ZETA2-0.5)**5+0.03125)



        ENDIF


        IF(K==KZTM)THEN
          ZTMAX1=ZETA1
          ZTMAX2=ZETA2
        ENDIF

        ZETA1=ZETA1+DZETA1
        ZETA2=ZETA2+DZETA2

      ENDDO

      ZTMAX1=ZTMAX1-EPS
      ZTMAX2=ZTMAX2-EPS


      END SUBROUTINE QNSESFCINIT



      END MODULE MODULE_SF_QNSESFC


