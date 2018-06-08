MODULE BLUEWS_module
  USE AtmMoist_module,ONLY:qsatf

  IMPLICIT NONE
CONTAINS
  
  
  
  
  


  SUBROUTINE CBL(ifirst,Gridiv)

    USE mod_z
    USE mod_k
    USE gas
    USE time
    USE data_in
    USE sues_data
    USE moist
    USE allocateArray
    USE defaultNotUsed
    USE cbl_module
    USE gis_data
    USE WhereWhen
    USE AtmMoist_module,ONLY:sat_vap_press

    IMPLICIT NONE

    
    REAL(KIND(1d0))::qh_use,qe_use,tm_K_zm,qm_gkg_zm
    REAL(KIND(1d0))::Temp_C1,avrh1,es_hPa1
    REAL(KIND(1d0))::secs0,secs1,Lv
    INTEGER::idoy,ifirst,Gridiv,startflag,iNBL

    
    startflag=0

    
    IF(ifirst == 1) THEN
       iCBLcount = 0
    ENDIF

    
    
    
    IF(ifirst==1 .OR. IniCBLdata(id,2)==-999) THEN   
       iCBLcount=iCBLcount+1

       dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime, &
            (NAN,is=6,ncolumnsdataOutBL)/)
       RETURN
    ELSEIF(avkdn<5)THEN
       iNBL=1
       IF (iNBL==-9) THEN
          CALL CBL_initial(qh_use,qe_use,tm_K_zm,qm_gkg_zm,startflag, Gridiv)
          RETURN
       ELSE
          
          CALL NBL(qh_use,qe_use,tm_K_zm,qm_gkg_zm,startflag, Gridiv)
          RETURN
       ENDIF
    ENDIF

    IF(startflag==0)THEN 

       dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime,blh_m,tm_K, &
            qm_kgkg*1000,tp_K,qp_kgkg*1000,(NAN,is=11,20),gamt_Km,gamq_kgkgm/)
       startflag=1
    ENDIF

    qh_use=qhforCBL(Gridiv)   
    qe_use=qeforCBL(Gridiv)
    IF(qh_use<-900.OR.qe_use<-900)THEN  
       CALL ErrorHint(22,'Unrealistic qh or qe_value for CBL.',qh_use,qe_use,qh_choice)
    ENDIF
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    

    
    



    cbldata(1)=float(it)+float(imin)/60.
    cbldata(2)=qh_use
    cbldata(3)=qe_use
    cbldata(4)=avdens
    cbldata(5)=lv_J_kg
    cbldata(6)=avcp
    cbldata(7)=avu1
    cbldata(8)=UStar
    cbldata(9)=Press_hPa
    cbldata(10)=psyh

    secs0=cbldata(1)*3600.
    secs1=secs0+float(tstep) 
    
    fhbl_Kms    = cbldata(2)/ (cbldata(4)*cbldata(6))  
    febl_kgkgms = cbldata(3)/ (cbldata(4)*cbldata(5))  
    IF(CO2_included==1)THEN
       fcbl = 0
    ELSE
       cm=NAN
    ENDIF

    
    

    IF(sondeflag.EQ.1) THEN
       CALL gamma_sonde
    ENDIF
    
    blh1_m=blh_m
    y(1)=blh_m 
    y(2)=tm_K  
    y(3)=qm_kgkg   
    y(4)=cm
    y(5)=tp_K
    y(6)=qp_kgkg

    
    CALL rkutta(neqn,secs0,secs1,y,1)
    
    blh_m   =y(1)
    tm_K    =y(2)  
    qm_kgkg =y(3)  
    cm      =y(4)  
    tp_K    =y(5)  
    qp_kgkg =y(6)  
    

    
    
    

    
    
    

    
    

    tp_C=tp_K-C2K
    tm_C=tm_K-C2K

    
    
    

    qm_gkg=qm_kgkg*1000 

    
    idoy=id
    
    IF(it==0 .AND. imin==(nsh_real-1)/nsh_real*60) idoy=id-1  


    
    IF((qh_choice==1).OR.(qh_choice==2))THEN 
       
       
       Temp_C=tm_K/((1000/cbldata(9))**(gas_ct_dry/cbldata(6)))-C2K
       es_hPa=sat_vap_press(Temp_C,cbldata(9),1,dectime)
       lv=(2500.25-2.365*Temp_C)*1000
       
       avrh=100*((qm_gkg*cbldata(9)/(622+qm_gkg))/es_hPa) 
       IF(avrh>100)THEN
          CALL errorHint(34,'subroutine CBL dectime, relative humidity',idoy+cbldata(1)/24.0,avrh,100)
          avrh=100
       ENDIF
       iCBLcount=iCBLcount+1

       dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime,blh_m,tm_K, &
            qm_kgkg*1000, tp_K,qp_kgkg*1000,&
            Temp_C,avrh,cbldata(2),cbldata(3),cbldata(9),cbldata(7),cbldata(8),cbldata(4),cbldata(5),cbldata(6),&
            gamt_Km,gamq_kgkgm/)
    ELSEIF(qh_choice==3)THEN 
       
       Temp_C1=tm_K/((1000/cbldata(9))**(gas_ct_dry/cbldata(6)))-C2K
       es_hPa1=sat_vap_press(Temp_C1,cbldata(9),1,dectime)
       lv=(2500.25-2.365*Temp_C1)*1000
       
       
       avrh1=100*((qm_gkg*cbldata(9)/(622+qm_gkg))/es_hPa1) 
       IF(avrh1>100)THEN
          CALL errorHint(34,'subroutine CBL dectime, relative humidity',idoy+cbldata(1)/24.0,avrh,100)
          avrh1=100
       ENDIF
       iCBLcount=iCBLcount+1

       dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime,blh_m,tm_K, &
            qm_kgkg*1000,tp_K,qp_kgkg*1000,&
            Temp_C1,avrh1,cbldata(2),cbldata(3),cbldata(9),cbldata(7),cbldata(8),cbldata(4),cbldata(5),cbldata(6),&
            gamt_Km,gamq_kgkgm/)
    ENDIF

    RETURN

  END SUBROUTINE CBL

  
  
  SUBROUTINE CBL_ReadInputData
    USE allocateArray
    USE data_in
    USE sues_data
    USE cbl_module
    USE initial
    USE WhereWhen

    IMPLICIT NONE

    INTEGER::i, ios
    REAL(KIND(1d0))::l

    NAMELIST/CBLInput/EntrainmentType,&
         QH_choice,&
         isubs,&
         CO2_included,&
         cblday,&
         wsb,&
         InitialData_use,&
         InitialDataFileName,&
         sondeflag,&
         FileSonde


    OPEN(51,file=TRIM(FileInputPath)//'CBLInput.nml',status='old', err=24)
    READ(51,nml=CBLInput,err=24)
    CLOSE(51)


    IF(InitialData_use==1 .OR. InitialData_use==2)THEN
       OPEN(52,file=TRIM(FileInputPath)//TRIM(InitialDataFileName),status='old', err=25)
       READ(52,*)
       nlineInData = 0   
       DO
          READ(52,*, iostat=ios) l
          IF(ios<0 .OR. l == -9) EXIT   
          nlineInData = nlineInData + 1
       ENDDO
       CLOSE(52)

       ALLOCATE(IniCBLdata(1:nlineInData,1:8))
       OPEN(52,file=TRIM(FileInputPath)//TRIM(InitialDataFileName),status='old', err=25)
       READ(52,*)
       DO i=1,nlineInData
          READ(52,*)IniCBLdata(i,1:8)
       ENDDO
       CLOSE(52)
    ENDIF

    IF(CO2_included==0)THEN
       fcbl=0       
    ENDIF

    iCBLcount=0

    RETURN

24  CALL ErrorHint(24,'CBLInput.nml',0.00D0,0.000D0,0)
25  CALL ErrorHint(24,TRIM(FileInputPath)//TRIM(InitialDataFileName),0.00D0,0.00D0,0)

  END SUBROUTINE CBL_ReadInputData

  
  
  SUBROUTINE CBL_initial(qh_use,qe_use,tm_K_zm,qm_gkg_zm,startflag,Gridiv)

    USE mod_z
    USE mod_k
    USE gas
    USE time
    USE data_in
    USE sues_data
    USE moist
    USE allocateArray
    USE defaultNotUsed
    USE cbl_module
    USE gis_data
    USE WhereWhen
    USE AtmMoist_module,ONLY:sat_vap_press

    IMPLICIT NONE

    REAL(KIND(1d0))::qh_use,qe_use,tm_K_zm,qm_gkg_zm
    REAL(KIND(1d0))::lv
    INTEGER::i,nLineDay,Gridiv,startflag


    qh_use=qhforCBL(Gridiv)   
    qe_use=qeforCBL(Gridiv)
    IF(qh_use<-900.OR.qe_use<-900)THEN  
       CALL ErrorHint(22,'Unrealistic qh or qe_value for CBL.',qh_use,qe_use,qh_choice)
    ENDIF
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    


    blh_m=NAN
    iCBLcount=iCBLcount+1

    dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=(/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime, &
         (NAN,is=6,ncolumnsdataOutBL)/)

    nLineDay=0
    DO i=1,nlineInData
       IF (INT(IniCBLdata(i,1))<=id)THEN
          nLineDay=nLineDay+1
       ENDIF
    ENDDO


    IF(InitialData_use==2) THEN
       blh_m=IniCBLdata(nLineDay,2)
       gamt_Km=IniCBLdata(nLineDay,3)
       gamq_gkgm=IniCBLdata(nLineDay,4)
       tp_K=IniCBLdata(nLineDay,5)
       qp_gkg=IniCBLdata(nLineDay,6)
       tm_K=IniCBLdata(nLineDay,7)
       qm_gkg=IniCBLdata(nLineDay,8)
    ELSEIF(InitialData_use==1 .AND. IniCBLdata(nlineDay,1)==id)THEN   
       blh_m=IniCBLdata(nLineDay,2)
       gamt_Km=IniCBLdata(nLineDay,3)
       gamq_gkgm=IniCBLdata(nLineDay,4)
       tm_K_zm=(Temp_C+C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
       tm_K=tm_K_zm-psyh*qh_use/(k*UStar*avcp*avdens)
       es_hPa=sat_vap_press(Temp_C,Press_hPa,1,dectime)
       qm_gkg_zm=622*avrh/(100*Press_hPa/es_hPa-avrh)
       lv=(2500.25-2.365*temp_C)*1000
       qm_gkg=qm_gkg_zm-psyh*qe_use/(k*UStar*avdens*lv)
       tp_K=tm_K
       qp_gkg=qm_gkg
    ELSEIF(InitialData_use==0)THEN
       blh_m=241.5
       gamt_Km=0.043
       gamq_gkgm=0.0092
       tm_K_zm=(Temp_C+C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
       tm_K=tm_K_zm-psyh*qh_use/(k*UStar*avcp*avdens)
       es_hPa=sat_vap_press(Temp_C,Press_hPa,1,dectime)
       qm_gkg_zm=622*avrh/(100*Press_hPa/es_hPa-avrh)
       lv=(2500.25-2.365*temp_C)*1000
       qm_gkg=es_hPa-psyh*qe_use/(k*UStar*avdens*lv)
       tp_K=tm_K
       qp_gkg=qm_gkg
    ENDIF

    gamq_kgkgm=gamq_gkgm/1000.
    qp_kgkg=qp_gkg/1000    
    qm_kgkg=qm_gkg/1000    
    tp_C=tp_K-C2K
    tm_C=tm_K-C2K

    
    IF(sondeflag==1 .AND. IniCBLdata(id,2)/=-999) THEN
       
       
       CALL sonde(id)
       gamt_Km=0
       gamq_kgkgm=0
    ENDIF

    
    IF(qp_kgkg.GT.qsatf(tp_C,Press_hPa).OR.qp_kgkg.LT.0)THEN
       qp_kgkg = qsatf(tp_C,Press_hPa)
    ENDIF
    IF(qm_kgkg.GT.qsatf(tm_C,Press_hPa).OR.qm_kgkg.LT.0) THEN
       qm_kgkg = qsatf(tm_C,Press_hPa)
    ENDIF

    
    
    
    startflag=0


  END SUBROUTINE CBL_initial

  SUBROUTINE NBL(qh_use,qe_use,tm_K_zm,qm_gkg_zm,startflag,Gridiv)

    USE mod_z
    USE mod_k
    USE gas
    USE time
    USE data_in
    USE sues_data
    USE moist
    USE allocateArray
    USE defaultNotUsed
    USE cbl_module
    USE gis_data
    USE WhereWhen
    USE AtmMoist_module,ONLY:sat_vap_press

    IMPLICIT NONE
    REAL(KIND(1d0))::qh_use,qe_use,tm_K_zm,qm_gkg_zm
    REAL(KIND(1d0))::lv
    INTEGER::i,nLineDay,Gridiv,startflag

    qh_use=qhforCBL(Gridiv)   
    qe_use=qeforCBL(Gridiv)
    IF(qh_use<-900.OR.qe_use<-900)THEN  
       CALL ErrorHint(22,'Unrealistic qh or qe_value for CBL.',qh_use,qe_use,qh_choice)
    ENDIF

    nLineDay=0
    DO i=1,nlineInData
       IF (INT(IniCBLdata(i,1))<=id)THEN
          nLineDay=nLineDay+1
       ENDIF
    ENDDO

    
    
    
    

    
    tm_K=IniCBLdata(nLineDay,7)
    qm_gkg=IniCBLdata(nLineDay,8)

    
    blh_m=200

    
    gamt_Km=IniCBLdata(nLineDay,3)
    gamq_gkgm=IniCBLdata(nLineDay,4)
    tp_K=IniCBLdata(nLineDay,5)
    qp_gkg=IniCBLdata(nLineDay,6)
    tm_K=IniCBLdata(nLineDay,7)
    qm_gkg=IniCBLdata(nLineDay,8)

    iCBLcount=iCBLcount+1
    
    
    

    Temp_C=tm_K/((1000/Press_hPa)**(gas_ct_dry/avcp))-C2K
    es_hPa=sat_vap_press(Temp_C,Press_hPa,1,dectime)
    lv=(2500.25-2.365*Temp_C)*1000
    
    avrh=100*((qm_gkg*Press_hPa/(622+qm_gkg))/es_hPa) 
    IF(avrh>100)THEN
       CALL errorHint(34,'subroutine CBL dectime, relative humidity',dectime,avrh,100)
       avrh=100
    ENDIF

    dataOutBL(iCBLcount,1:ncolumnsdataOutBL,Gridiv)=&
         (/REAL(iy,8),REAL(id,8),REAL(it,8),REAL(imin,8),dectime,&
         blh_m,tm_K,qm_gkg,&
         tp_K,qp_gkg,&
         Temp_C,avrh,qh_use,qe_use,Press_hPa,avu1,UStar,avdens,lv_J_kg,avcp,&
         gamt_Km,gamq_kgkgm/)

    IF(InitialData_use==2) THEN
       blh_m=IniCBLdata(nLineDay,2)
       gamt_Km=IniCBLdata(nLineDay,3)
       gamq_gkgm=IniCBLdata(nLineDay,4)
       tp_K=IniCBLdata(nLineDay,5)
       qp_gkg=IniCBLdata(nLineDay,6)
       tm_K=IniCBLdata(nLineDay,7)
       qm_gkg=IniCBLdata(nLineDay,8)
    ELSEIF(InitialData_use==1 .AND. IniCBLdata(nlineDay,1)==id)THEN   
       blh_m=IniCBLdata(nLineDay,2)
       gamt_Km=IniCBLdata(nLineDay,3)
       gamq_gkgm=IniCBLdata(nLineDay,4)
       tm_K_zm=(Temp_C+C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
       tm_K=tm_K_zm-psyh*qh_use/(k*UStar*avcp*avdens)
       es_hPa=sat_vap_press(Temp_C,Press_hPa,1,dectime)
       qm_gkg_zm=622*avrh/(100*Press_hPa/es_hPa-avrh)
       lv=(2500.25-2.365*temp_C)*1000
       qm_gkg=qm_gkg_zm-psyh*qe_use/(k*UStar*avdens*lv)
       tp_K=tm_K
       qp_gkg=qm_gkg
    ELSEIF(InitialData_use==0)THEN
       blh_m=241.5
       gamt_Km=0.043
       gamq_gkgm=0.0092
       tm_K_zm=(Temp_C+C2K)*((1000/Press_hPa)**(gas_ct_dry/avcp))
       tm_K=tm_K_zm-psyh*qh_use/(k*UStar*avcp*avdens)
       es_hPa=sat_vap_press(Temp_C,Press_hPa,1,dectime)
       qm_gkg_zm=622*avrh/(100*Press_hPa/es_hPa-avrh)
       lv=(2500.25-2.365*temp_C)*1000
       qm_gkg=es_hPa-psyh*qe_use/(k*UStar*avdens*lv)
       tp_K=tm_K
       qp_gkg=qm_gkg
    ENDIF

    gamq_kgkgm=gamq_gkgm/1000.
    qp_kgkg=qp_gkg/1000    
    qm_kgkg=qm_gkg/1000    
    tp_C=tp_K-C2K
    tm_C=tm_K-C2K

    
    IF(sondeflag==1 .AND. IniCBLdata(id,2)/=-999) THEN
       
       
       CALL sonde(id)
       gamt_Km=0
       gamq_kgkgm=0
    ENDIF

    
    IF(qp_kgkg.GT.qsatf(tp_C,Press_hPa).OR.qp_kgkg.LT.0)THEN
       qp_kgkg = qsatf(tp_C,Press_hPa)
    ENDIF
    IF(qm_kgkg.GT.qsatf(tm_C,Press_hPa).OR.qm_kgkg.LT.0) THEN
       qm_kgkg = qsatf(tm_C,Press_hPa)
    ENDIF

    startflag=0
  END SUBROUTINE NBL

  


  
  
  
  
  
  SUBROUTINE RKUTTA(neqn,XA,XB,Y,NSTEPS)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    IMPLICIT NONE
    INTEGER::ns,nsteps, nj,n,neqn
    REAL(KIND(1D0)), DIMENSION (neqn):: y
    REAL(KIND(1D0)), DIMENSION (21):: dydx,arg
    REAL(KIND(1D0)), DIMENSION (21,5):: rk
    REAL(KIND(1D0)), DIMENSION (4):: coef
    REAL (KIND(1D0)):: XA,XB,step,X,xx

    coef(1)=1.0
    coef(2)=0.5
    coef(3)=0.5
    coef(4)=1.0
    
    STEP = (XB-XA)/NSTEPS

    DO NS = 1,NSTEPS
       DO  NJ = 1,nEqn
          RK(NJ,1) = 0
       ENDDO
       X = XA+(NS-1)*STEP
       DO N = 1,4
          IF (N.EQ.1)THEN
             XX = X
          ELSEIF (N.GT.1)THEN
             XX = X + COEF(N)*STEP
          ENDIF

          DO NJ = 1,nEqn
             ARG(NJ) = Y(NJ) + COEF(N)*RK(NJ,N)
          ENDDO

          CALL DIFF(xx,ARG,DYDX)

          DO NJ = 1,nEqn
             RK(NJ,N+1) = STEP*DYDX(NJ)
          ENDDO
       ENDDO

       DO  NJ = 1,nEqn
          DO  N = 1,4
             Y(NJ) = Y(NJ) + RK(NJ,N+1)/(6*COEF(N))
          ENDDO
       ENDDO
    ENDDO

    RETURN
  END SUBROUTINE RKUTTA
  
  

  SUBROUTINE diff(s,y1,dyds)
    
    

    
    
    
    
    
    USE data_in
    USE sues_data
    
    USE time
    USE CBL_MODULE
    USE defaultnotUsed
    USE mod_grav

    IMPLICIT NONE
    REAL(KIND(1D0)), DIMENSION(neqn)::dyds,y1
    REAL(KIND(1d0)) :: zero=0.0
    REAL(KIND(1d0)) :: h1,t_K,q_kgkg,c,cp,ws,s,foo
    
    REAL(KIND(1D0)):: delt_K,delq_kgkg,delc
    REAL(KIND(1D0)):: gamtv_Km,deltv_K,ftv_Kms
    REAL(KIND(1D0)):: ftva_Kms,delb,qs2,qs3
    REAL(KIND(1D0)):: dhds,dtds,dqds,dcds,dtpds,dqpds
    REAL(KIND(1D0)):: conk,conn,cona,conc,cont

    
    foo=s
    
    h1     = y1(1)
    t_K    = y1(2)
    q_kgkg = y1(3)
    c      = y1(4)
    tp_K   = y1(5)
    qp_kgkg= y1(6)

    
    
    

    cp        = 0 

    delt_K    = tp_K    - t_K
    delq_kgkg = qp_kgkg - q_kgkg
    delc      = cp - c

    
    ftv_Kms  = fhbl_Kms + 0.61 * tm_K * febl_kgkgms
    gamtv_Km = gamt_Km  + 0.61 * tm_K * gamq_kgkgm
    deltv_K  = delt_K   + 0.61 * tm_K * delq_kgkg

    
    ftva_Kms = MAX(ftv_Kms,zero) 
    ws = (h1*ftva_Kms*grav/tm_K)**0.3333333333

    
    IF (EntrainmentType.EQ.2) THEN
       
       dhds = ftva_Kms/(h1*gamtv_Km)

    ELSE IF (EntrainmentType.EQ.1) THEN
       
       IF (deltv_K.LE.0.01) THEN
          dhds = ftva_Kms/(h1*gamtv_Km)
          CALL errorHint(30,"subroutine diff [CBL: Deltv_K<0.01 EntrainmentType=1], deltv_K,delt_K,",deltv_K,delt_K,notUsedI)
          CALL errorHint(30,"subroutine diff [CBL: Deltv_K<0.01 EntrainmentType=1], tm_K,TPP_K,y1",tm_K,TPP_K, notUsedI)
          
       ELSE
          delb = grav*deltv_K/tm_K
          conc = 0.2
          cona = 5.0
          dhds = (conc*ws**3 + cona*cbldata(8)**3)/(h1*delb)
       END IF

    ELSE IF (EntrainmentType.EQ.4) THEN
       
       alpha3=0.2   
       IF (deltv_K.LE.0.01) THEN
          dhds = ftva_Kms/(h1*gamtv_Km)
          CALL ErrorHint(31, 'subroutine difflfnout: [CBL: deltv_K<0.01 EntrainmentType=4],deltv_K',&
               deltv_K,notUsed,notUsedI)
       ELSE
          
          IF (isubs.EQ.1) THEN
             dhds = alpha3*ftva_Kms/deltv_k + wsb
          ELSE
             dhds = alpha3*ftva_Kms/deltv_K
          END IF
       END IF

       

    ELSE IF (EntrainmentType.EQ.3) THEN
       
       conn = 1.33
       conk = 0.18
       cont = 0.80
       qs3 = ws**3 + (conn*cbldata(8))**3
       qs2 = qs3**(0.6666666667)

       IF (deltv_K.LE.0.01) THEN
          dhds = ftva_Kms/(h1*gamtv_Km)
          CALL ErrorHint(31, 'subroutine difflfnout: [CBL: deltv_K<0.01 EntrainmentType=3],deltv_K',&
               deltv_K,notUsed,notUsedI)

       ELSE
          delb = grav*deltv_K/tm_K
          dhds = (conk*qs3) / (cont*qs2 + h1*delb)
       END IF

    ELSE
       CALL ErrorHint(24, 'BLUEWS_DIff- CBL- illegal alpha',notUsed,notUsed,notUsedI)
    END IF
    
    
    IF (isubs.EQ.1) THEN
       dtds = fhbl_Kms/h1    + delt_K    *(dhds-wsb)/h1
       dqds = febl_kgkgms/h1 + delq_kgkg *(dhds-wsb)/h1
       dcds = fcbl/h1        + delc      *(dhds-wsb)/h1
       
       dtpds = gamt_Km * (dhds-wsb)
       dqpds = gamq_kgkgm * (dhds-wsb)
    ELSE
       dtds = fhbl_Kms/h1    + delt_K    *(dhds)/h1
       dqds = febl_kgkgms/h1 + delq_kgkg *(dhds)/h1
       dcds = fcbl/h1        + delc      *(dhds)/h1
       
       dtpds = gamt_Km * (dhds)
       dqpds = gamq_kgkgm * (dhds)
    END IF

    dyds(1) = dhds
    dyds(2) = dtds
    dyds(3) = dqds
    dyds(4) = dcds
    dyds(5) = dtpds
    dyds(6) = dqpds


    RETURN
  END SUBROUTINE diff

  
  
  SUBROUTINE sonde(id)
    
    
    USE data_in
    USE cbl_module
    IMPLICIT NONE
    INTEGER::i,fn=101,izm=500,notUsedI=-9999,id
    CHARACTER (len=200)::FileN
    REAL (KIND(1d0)):: dxx
    REAL (KIND(1d0)),PARAMETER::notUsed=-9999.99

    FileN=TRIM(FileInputPath)//TRIM(FileSonde(id))
    OPEN(fn,file=FileN,status="old",err=24)
    
    READ(fn,*)
    READ(fn,*)
    READ(fn,*)

    DO i=1,1000
       READ(fn,*,END=900,err=25)gtheta(i,1),dxx,gtheta(i,2),ghum(i,1),dxx,ghum(i,2)
       ghum(i,2) = ghum(i,2)
    ENDDO
900 zmax=i-1
    IF(zmax.GT.izm)THEN
       CALL ErrorHint(23,FileN,REAL(zmax,KIND(1D0)),notUsed,izm)
    ENDIF
    CLOSE(fn)
    RETURN
24  CALL ErrorHint(24,FileN,notUsed,notUsed, notUsedI)
25  CALL ErrorHint(25,FileN,notUsed,notUsed,i)
    RETURN
  END SUBROUTINE sonde
  
  
  SUBROUTINE gamma_sonde
    USE cbl_module
    

    IMPLICIT NONE
    REAL(KIND(1D0))::gamtt,gamqq
    INTEGER::j
    
    
    
    IF (sondeflag.EQ.1) THEN
       DO j=2,zmax
          IF (blh_m.GE.gtheta(j-1,1)) THEN
             gamtt = gtheta(j-1,2)
          ENDIF
          gamt_Km = gamtt
       ENDDO

       DO j=2,zmax
          IF (blh_m.GE.ghum(j-1,1)) THEN
             gamqq = ghum(j-1,2)
          ENDIF
          gamq_kgkgm = gamqq/1000.
       ENDDO
    ENDIF
    RETURN

  END SUBROUTINE gamma_sonde




END MODULE BLUEWS_module


