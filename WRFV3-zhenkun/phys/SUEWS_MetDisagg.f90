MODULE MetDisagg
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  USE AllocateArray
  USE ColNamesInputFiles
  USE Data_In
  USE Initial
  USE NARP_MODULE,ONLY:NARP_cal_SunPosition

  IMPLICIT NONE

CONTAINS

  
  SUBROUTINE DisaggregateMet(iBlock, igrid)
    
    
    

    USE Sues_Data
    USE DefaultNotUsed

    IMPLICIT NONE

    INTEGER:: lunit = 100
    INTEGER:: tdiff   
    INTEGER:: i,ii  
    INTEGER:: iBlock, igrid
    INTEGER,DIMENSION(Nper):: seq1Nper
    INTEGER,DIMENSION(nsd):: seq1nsd
    INTEGER,DIMENSION(nColumnsMetForcingData):: MetDisaggMethod   
    REAL(KIND(1d0)),DIMENSION(nColumnsMetForcingData):: MetArrayOrig
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData*Nper,ncolumnsMetForcingData):: Met_tt
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData*Nper):: Met_tt_kdownAdj
    CHARACTER(LEN=9),DIMENSION(ncolumnsMetForcingData):: HeaderMet
    CHARACTER(LEN=10*ncolumnsMetForcingData):: HeaderMetOut
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData):: dectimeOrig
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData*Nper):: dectimeDscd, dectimeFast
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigMetData*Nper):: idectime 

    INTEGER, DIMENSION(Nper):: temp_iy, temp_id, temp_ih, temp_im, temp_ihm

    
    ALLOCATE(MetForDisagg(ReadLinesOrigMetData,nColumnsMetForcingData))
    ALLOCATE(MetForDisaggPrev(nColumnsMetForcingData))
    ALLOCATE(MetForDisaggNext(nColumnsMetForcingData))
    MetForDisagg(:,:) = -999
    MetForDisaggPrev(:) = -999
    MetForDisaggNext(:) = -999
    
    Met_tt = -999
    
    MetDisaggMethod = -999

    
    seq1Nper = (/(i, i=1,Nper, 1)/)
    seq1nsd = (/(i, i=1,nsd, 1)/)

    
    IF(DiagnoseDisagg==1) WRITE(*,*) 'DisaggMethod: ',DisaggMethod, 'RainDisaggMethod:',RainDisaggMethod
    IF(DisaggMethod == 1) THEN
       MetDisaggMethod(:) = 10   
    ELSEIF(DisaggMethod == 2) THEN
       MetDisaggMethod(:) = 20   
    ELSEIF(DisaggMethod == 3) THEN   
       MetDisaggMethod(:) = 10   
       MetDisaggMethod(10:13) = 20   
    ELSE
       CALL errorHint(2,'Problem in SUEWS_MetDisagg: DisaggMethod value should be 1, 2, or 3', &
            NotUsed,NotUsed,DisaggMethod)
    ENDIF
    
    MetDisaggMethod(14) = RainDisaggMethod


    
    IF(DiagnoseDisagg==1) WRITE(*,*) 'Reading file: ', TRIM(FileOrigMet)
    OPEN(lunit,file=TRIM(FileOrigMet),status='old')
    
    READ(lunit,*) HeaderMet
    
    
    IF (SkippedLinesOrig>0) THEN
       DO i=1,skippedLinesOrig-1   
          READ(lunit,*)
       ENDDO
       
       CALL MetRead(lunit,MetArrayOrig,InputmetFormat,ldown_option,NetRadiationMethod,&
            snowUse,SMDMethod,SoilDepthMeas,SoilRocks,SoilDensity,SmCap)
       MetForDisaggPrev(1:ncolumnsMetForcingData) = MetArrayOrig
    ENDIF


    
    DO i=1, ReadLinesOrigMetDataMax
       CALL MetRead(lunit,MetArrayOrig,InputmetFormat,ldown_option,NetRadiationMethod,&
            snowUse,SMDMethod,SoilDepthMeas,SoilRocks,SoilDensity,SmCap)
       MetForDisagg(i,1:ncolumnsMetForcingData) = MetArrayOrig
    ENDDO

    
    IF(iBlock/=ReadBlocksOrigMetData) THEN
       CALL MetRead(lunit,MetArrayOrig,InputmetFormat,ldown_option,NetRadiationMethod,&
            snowUse,SMDMethod,SoilDepthMeas,SoilRocks,SoilDensity,SmCap)
       MetForDisaggNext(1:ncolumnsMetForcingData) = MetArrayOrig
    ENDIF
    CLOSE(lunit)

    
    
    tdiff = INT(MetForDisagg(2,4)-MetForDisagg(1,4))   
    IF(tdiff == 0) tdiff = INT(MetForDisagg(2,3)-MetForDisagg(1,3))*60   
    IF(tdiff < 0) THEN   
       tdiff = INT(MetForDisagg(3,4)-MetForDisagg(2,4))
       IF(tdiff == 0) tdiff = INT(MetForDisagg(3,3)-MetForDisagg(2,3))*60   
    ENDIF
    
    IF(tdiff /= ResolutionFilesIn/60) THEN
       CALL errorHint(2,'Problem in SUEWS_MetDisagg: timestamps in met forcing file inconsistent with ResolutionFilesIn', &
            REAL(ResolutionFilesIn,KIND(1d0)),NotUsed,tdiff*60)
    ENDIF

    
    
    IF(ANY(MetForDisagg(1:(ReadLinesOrigMetDataMax-1),1) /= MetForDisagg(1,1))) THEN
       CALL errorHint(3,'Problem in SUEWS_MetDisagg: multiple years found in original met forcing file.', &
            MetForDisagg(1,1),NotUsed,NotUsedI)
    ENDIF

    
    IF (Diagnose==1) WRITE(*,*) 'Disaggregating met forcing data (',TRIM(FileOrigMet),') to model time-step...'
    
    dectimeOrig = MetForDisagg(:,2) + MetForDisagg(:,3)/24.0 + MetForDisagg(:,4)/(60.0*24.0)

    DO i=1,ReadLinesOrigMetDataMax
       
       dectimeDscd(Nper*(i-1)+Seq1Nper) = dectimeOrig(i) - (tstep/60.0)/(60.0*24.0)*(/(ii, ii=(Nper-1),0, -1)/)
       
       temp_iy   = INT(MetForDisagg(i,1))   
       temp_id   = FLOOR(dectimeDscd(Nper*(i-1)+Seq1Nper))   
       
       
       temp_ihm  = NINT(((dectimeDscd(Nper*(i-1)+Seq1Nper) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   
       temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   
       temp_im = MOD(temp_ihm,60)   

       IF(dectimeOrig(i) == 1.0000 .AND. i > 1) THEN   
          IF (Diagnose==1) WRITE(*,*) 'Year change encountered: ', dectimeOrig(i), dectimeOrig(i-1)
          
          dectimeDscd(Nper*(i-1)+Seq1Nper) = dectimeOrig(i-1) + (tstep/60.0)/(60.0*24.0)*Seq1Nper
          
          temp_iy   = INT(MetForDisagg(i,1))   
          temp_id   = FLOOR(dectimeDscd(Nper*(i-1)+Seq1Nper))   
          temp_ihm  = NINT(((dectimeDscd(Nper*(i-1)+Seq1Nper) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   
          temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   
          temp_im = MOD(temp_ihm,60)   
          
          temp_iy(1:(Nper-1)) = temp_iy(1:(Nper-1)) - 1  
          temp_id(Nper) = 1  
       ENDIF

       
       
       
       
       
       
       

       
       Met_tt(Nper*(i-1)+Seq1Nper,1) = temp_iy
       Met_tt(Nper*(i-1)+Seq1Nper,2) = temp_id
       Met_tt(Nper*(i-1)+Seq1Nper,3) = temp_ih
       Met_tt(Nper*(i-1)+Seq1Nper,4) = temp_im

    ENDDO

    
    DO ii=5,ncolumnsMetForcingData
       IF(ii == 14) THEN  
          IF(MetDisaggMethod(14) == 100) THEN
             Met_tt(:,14) = DisaggP_amongN(MetForDisagg(:,14),Nper,Nper,ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
             IF(ALL(MetForDisagg(:,16)==-999)) THEN
                Met_tt(:,16) = -999
             ELSE
                Met_tt(:,16) = DisaggP_amongN(MetForDisagg(:,16),Nper,Nper,ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
             ENDIF
          ELSEIF(MetDisaggMethod(14) == 101) THEN
             IF(RainAmongN == -999) THEN
                CALL ErrorHint(2,'Problem in SUEWS_MetDisagg: RainDisaggMethod requires RainAmongN', &
                     REAL(RainAmongN,KIND(1d0)),NotUsed,RainDisaggMethod)
             ELSEIF(RainAmongN > Nper) THEN
                CALL ErrorHint(2,'Problem in SUEWS_MetDisagg: RainAmongN > Nper',REAL(Nper,KIND(1d0)),NotUsed,RainAmongN)
             ELSE
                Met_tt(:,14) = DisaggP_amongN(MetForDisagg(:,14),RainAmongN, Nper,ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
                IF(ALL(MetForDisagg(:,16)==-999)) THEN
                   Met_tt(:,16) = -999
                ELSE
                   Met_tt(:,16) = DisaggP_amongN(MetForDisagg(:,16),RainAmongN,Nper,ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
                ENDIF
             ENDIF
          ELSEIF(MetDisaggMethod(14) == 102) THEN
             IF(ALL(MultRainAmongN == -999)) THEN
                CALL ErrorHint(2,'Problem in SUEWS_MetDisagg: RainDisaggMethod requires MultRainAmongN', &
                     REAL(MultRainAmongN(1),KIND(1d0)),NotUsed,RainDisaggMethod)
             ELSEIF(ALL(MultRainAmongNUpperI == -999)) THEN
                CALL ErrorHint(2,'Problem in SUEWS_MetDisagg: RainDisaggMethod requires MultRainAmongNUpperI', &
                     MultRainAmongNUpperI(1),NotUsed,RainDisaggMethod)
             ELSEIF(ANY(MultRainAmongN > Nper)) THEN
                CALL ErrorHint(2,'Problem in SUEWS_MetDisagg: MultRainAmongN > Nper',REAL(Nper,KIND(1d0)),NotUsed, &
                     MAXVAL(MultRainAmongN))
             ELSE
                Met_tt(:,14) = DisaggP_amongNMult(MetForDisagg(:,14),MultRainAmongNUpperI,MultRainAmongN, Nper,&
                     ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
                IF(ALL(MetForDisagg(:,16)==-999)) THEN
                   Met_tt(:,16) = -999
                ELSE
                   Met_tt(:,16) = DisaggP_amongNMult(MetForDisagg(:,16),MultRainAmongNUpperI,MultRainAmongN,Nper, &
                        ReadLinesOrigMetData,ReadLinesOrigMetDataMax)
                ENDIF
             ENDIF
          ELSE
             WRITE(*,*) 'Disaggregation code for rain not recognised'
          ENDIF
       ELSEIF(ii == 24) THEN  
          IF(ANY(MetForDisagg(:,ii)/=-999)) THEN
             WRITE(*,*) 'Disaggregation of wind direction not currently implemented!'
          ENDIF
       ELSE
          IF(ALL(MetForDisagg(:,ii)==-999)) THEN

             Met_tt(:,ii) = -999
          ELSE
             Met_tt(:,ii) = Disagg_Lin(MetForDisagg(:,ii),MetForDisaggPrev(ii),MetForDisaggNext(ii),MetDisaggMethod(ii), &
                  Nper,ReadLinesOrigMetData,ReadLinesOrigMetDataMax,iBlock)
          ENDIF
       ENDIF
    ENDDO

    
    IF(KdownZen == 1) THEN
       IF(DiagnoseDisagg==1) WRITE(*,*) 'Adjusting disaggregated kdown using zenith angle'
       Met_tt_kdownAdj(:) = Met_tt(:,15)
       
       lat = SurfaceChar(igrid,c_lat)
       lng = SurfaceChar(igrid,c_lng)
       timezone = SurfaceChar(igrid,c_tz)
       alt = SurfaceChar(igrid,c_Alt)
       
       dectimeFast(:) = Met_tt(:,2) + Met_tt(:,3)/24.0 + Met_tt(:,4)/(60.0*24.0)
       idectime=dectimeFast-halftimestep
       DO i=1,(ReadLinesOrigMetDataMax*Nper)
          CALL NARP_cal_SunPosition(Met_tt(i,2),idectime(i),timezone,lat,lng,alt,azimuth,zenith_deg)
          
          IF(zenith_deg > 90) THEN
             
             Met_tt_kdownAdj(i) = 0.0
          ENDIF
       ENDDO
       
       DO i=1,(ReadLinesOrigMetDataMax*Nper/nsd) 
          Met_tt_kdownAdj((i-1)*nsd+seq1nsd) = Met_tt_kdownAdj( (i-1)*nsd+seq1nsd) * &
               SUM(Met_tt((i-1)*nsd+seq1nsd,15 ))/SUM(Met_tt_kdownAdj((i-1)*nsd+seq1nsd))
       ENDDO
       
       Met_tt(:,15) = Met_tt_kdownAdj(:)
    ENDIF

    
    MetForcingData(:,1:24,GridCounter) = Met_tt(:,1:24)

    
    IF(ALL(MetForcingData(:,16,GridCounter) == -999)) MetForcingData(:,16,GridCounter)=0

    
    Met_tt(:,13) = Met_tt(:,13)/10.0

    
    IF(KeepTstepFilesIn == 1) THEN
       IF (iBlock==1) THEN
          
          DO i=1,ncolumnsMetForcingData
             IF(i==1) THEN
                HeaderMetOut=ADJUSTL(HeaderMet(i))
             ELSE
                HeaderMetOut=TRIM(HeaderMetOut)//' '//ADJUSTL(HeaderMet(i))
             ENDIF
          ENDDO
          
          OPEN(78,file=TRIM(FileDscdMet),err=112)
          WRITE(78,'(a)') HeaderMetOut
       ELSE
          OPEN(78,file=TRIM(FileDscdMet),position='append')
       ENDIF
       
       DO i=1,(ReadLinesOrigMetDataMax*Nper)
          WRITE(78,303) (INT(Met_tt(i,ii)), ii=1,4), Met_tt(i,5:ncolumnsMetForcingData)
       ENDDO
       


       
       CLOSE (78)   
    ENDIF


303 FORMAT((i4,1X), 3(i3,1X), 9(f12.6,1X), (f9.4,1X), 10(f9.4,1X))  

    
    DEALLOCATE(MetForDisagg)
    DEALLOCATE(MetForDisaggPrev)
    DEALLOCATE(MetForDisaggNext)

    RETURN

112 CALL ErrorHint(52,TRIM(FileDscdMet),notUsed,notUsed,notUsedI)

  ENDSUBROUTINE DisaggregateMet
  

  
  SUBROUTINE DisaggregateESTM(iBlock)
    
    
    

    USE Sues_Data
    USE DefaultNotUsed

    IMPLICIT NONE

    INTEGER:: lunit = 101
    INTEGER:: tdiff   
    INTEGER:: i,ii  
    INTEGER:: iBlock
    INTEGER,DIMENSION(NperESTM):: seq1NperESTM
    INTEGER,DIMENSION(nsd):: seq1nsd
    INTEGER,DIMENSION(ncolsESTMdata):: ESTMDisaggMethod   
    REAL(KIND(1d0)),DIMENSION(ncolsESTMdata):: ESTMArrayOrig
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigESTMData*NperESTM,ncolsESTMdata):: ESTM_tt
    CHARACTER(LEN=9),DIMENSION(ncolsESTMdata):: HeaderESTM
    CHARACTER(LEN=10*ncolsESTMdata):: HeaderESTMOut
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigESTMData):: dectimeOrig
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrigESTMData*NperESTM):: dectimeDscd
    INTEGER::iostat_var

    INTEGER, DIMENSION(NperESTM):: temp_iy, temp_id, temp_ih, temp_im, temp_ihm

    
    ALLOCATE(ESTMForDisagg(ReadLinesOrigESTMData,ncolsESTMdata))
    ALLOCATE(ESTMForDisaggPrev(ncolsESTMdata))
    ALLOCATE(ESTMForDisaggNext(ncolsESTMdata))
    ESTMForDisagg(:,:) = -999
    ESTMForDisaggPrev(:) = -999
    ESTMForDisaggNext(:) = -999
    
    ESTM_tt = -999
    
    ESTMDisaggMethod = -999

    
    seq1NperESTM = (/(i, i=1,NperESTM, 1)/)
    seq1nsd = (/(i, i=1,nsd, 1)/)

    
    
    IF(DisaggMethodESTM == 1) THEN
       ESTMDisaggMethod(:) = 10   
    ELSEIF(DisaggMethodESTM == 2) THEN
       ESTMDisaggMethod(:) = 20   
    ELSE
       CALL errorHint(2,'Problem in SUEWS_ESTMDisagg: DisaggMethodESTM value should be 1 or 2', &
            NotUsed,NotUsed,DisaggMethodESTM)
    ENDIF

    
    IF(DiagnoseDisaggESTM==1) WRITE(*,*) 'Reading file: ', TRIM(FileOrigESTM)
    OPEN(lunit,file=TRIM(FileOrigESTM),status='old')
    
    READ(lunit,*) HeaderESTM
    
    
    IF (SkippedLinesOrigESTM>0) THEN
       DO i=1,skippedLinesOrigESTM-1   
          READ(lunit,*)
       ENDDO
       
       READ(lunit,*,iostat=iostat_var) ESTMArrayOrig
       ESTMForDisaggPrev(1:ncolsESTMdata) = ESTMArrayOrig
    ENDIF
    
    DO i=1, ReadLinesOrigESTMDataMax
       READ(lunit,*,iostat=iostat_var) ESTMArrayOrig
       ESTMForDisagg(i,1:ncolsESTMdata) = ESTMArrayOrig
    ENDDO
    
    IF(iBlock/=ReadBlocksOrigMetData) THEN
       READ(lunit,*,iostat=iostat_var) ESTMArrayOrig
       ESTMForDisaggNext(1:ncolsESTMdata) = ESTMArrayOrig
    ENDIF
    CLOSE(lunit)

    
    
    tdiff = INT(ESTMForDisagg(2,4)-ESTMForDisagg(1,4))   
    IF(tdiff == 0) tdiff = INT(ESTMForDisagg(2,3)-ESTMForDisagg(1,3))*60   
    IF(tdiff < 0) THEN   
       tdiff = INT(ESTMForDisagg(3,4)-ESTMForDisagg(2,4))
       IF(tdiff == 0) tdiff = INT(ESTMForDisagg(3,3)-ESTMForDisagg(2,3))*60   
    ENDIF
    
    IF(tdiff /= ResolutionFilesInESTM/60) THEN
       CALL errorHint(2,'Problem in SUEWS_ESTMDisagg: timestamps in ESTM forcing file inconsistent with ResolutionFilesInESTM', &
            REAL(ResolutionFilesInESTM,KIND(1d0)),NotUsed,tdiff*60)
    ENDIF

    
    IF ( Diagnose==1 ) THEN
       WRITE(*,*) 'Disaggregating ESTM forcing data (',TRIM(FileOrigESTM),') to model time-step...'
    END IF
    
    dectimeOrig = ESTMForDisagg(:,2) + ESTMForDisagg(:,3)/24.0 + ESTMForDisagg(:,4)/(60.0*24.0)

    DO i=1,ReadLinesOrigESTMDataMax
       
       dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) = dectimeOrig(i) - (tstep/60.0)/(60.0*24.0)*(/(ii, ii=(NperESTM-1),0, -1)/)
       
       temp_iy   = INT(ESTMForDisagg(i,1))   
       temp_id   = FLOOR(dectimeDscd(NperESTM*(i-1)+Seq1NperESTM))   
       
       
       temp_ihm  = NINT(((dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   
       temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   
       temp_im = MOD(temp_ihm,60)   

       IF(dectimeOrig(i) == 1.0000 .AND. i > 1) THEN   
          WRITE(*,*) 'Year change encountered: ', dectimeOrig(i), dectimeOrig(i-1)
          
          dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) = dectimeOrig(i-1) + (tstep/60.0)/(60.0*24.0)*Seq1NperESTM
          
          temp_iy   = INT(ESTMForDisagg(i,1))   
          temp_id   = FLOOR(dectimeDscd(NperESTM*(i-1)+Seq1NperESTM))   
          temp_ihm  = NINT(((dectimeDscd(NperESTM*(i-1)+Seq1NperESTM) - temp_id/1.0)*60.0*24.0)*1000.0)/1000   
          temp_ih = (temp_ihm-MOD(temp_ihm,60))/60   
          temp_im = MOD(temp_ihm,60)   
          
          temp_iy(1:(NperESTM-1)) = temp_iy(1:(NperESTM-1)) - 1  
          temp_id(NperESTM) = 1  
       ENDIF

       
       
       
       
       
       
       

       
       ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,1) = temp_iy
       ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,2) = temp_id
       ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,3) = temp_ih
       ESTM_tt(NperESTM*(i-1)+Seq1NperESTM,4) = temp_im

    ENDDO


    
    
    DO ii=5,ncolsESTMdata
       IF(ALL(ESTMForDisagg(:,ii)==-999)) THEN

          ESTM_tt(:,ii) = -999
       ELSE
          ESTM_tt(:,ii) = Disagg_Lin(ESTMForDisagg(:,ii),ESTMForDisaggPrev(ii),ESTMForDisaggNext(ii),ESTMDisaggMethod(ii), &
               NperESTM,ReadLinesOrigESTMData,ReadLinesOrigESTMDataMax,iBlock)
       ENDIF
    ENDDO

    
    ESTMForcingData(:,1:ncolsESTMdata,GridCounter) = ESTM_tt(:,1:ncolsESTMdata)

    
    IF(KeepTstepFilesIn == 1) THEN
       IF (iBlock==1) THEN
          
          DO i=1,ncolsESTMdata
             IF(i==1) THEN
                HeaderESTMOut=ADJUSTL(HeaderESTM(i))
             ELSE
                HeaderESTMOut=TRIM(HeaderESTMOut)//' '//ADJUSTL(HeaderESTM(i))
             ENDIF
          ENDDO
          
          OPEN(78,file=TRIM(FileDscdESTM),err=113)
          WRITE(78,'(a)') HeaderESTMOut
       ELSE
          OPEN(78,file=TRIM(FileDscdESTM),position='append')
       ENDIF
       
       DO i=1,(ReadLinesOrigESTMDataMax*NperESTM)
          WRITE(78,304) (INT(ESTM_tt(i,ii)), ii=1,4), ESTM_tt(i,5:ncolsESTMdata)
       ENDDO
       


       
       CLOSE (78)   
    ENDIF


304 FORMAT((i4,1X), 3(i3,1X), 9(f9.4,1X))

    
    DEALLOCATE(ESTMForDisagg)
    DEALLOCATE(ESTMForDisaggPrev)
    DEALLOCATE(ESTMForDisaggNext)

    RETURN

113 CALL ErrorHint(52,TRIM(FileDscdESTM),notUsed,notUsed,notUsedI)

  ENDSUBROUTINE DisaggregateESTM
  


  
  FUNCTION Disagg_Lin(Slow,SlowPrev,SlowNext,DisaggType,Nper_loc,ReadLinesOrig_loc,ReadLinesOrigMax_loc,iBlock) RESULT(Fast)

    USE DefaultNotUsed
    USE sues_data

    IMPLICIT NONE

    INTEGER:: DisaggType   
    INTEGER:: Nper_loc     
    INTEGER:: ReadLinesOrig_loc,ReadLinesOrigMax_loc   
    INTEGER:: iBlock
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc*Nper_loc):: Fast  
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc):: Slow   
    REAL(KIND(1d0)):: SlowPrev, SlowNext
    INTEGER,DIMENSION(Nper_loc):: FastRows   
    INTEGER,DIMENSION(FLOOR(Nper_loc/2.0)):: FirstRows10   
    INTEGER,DIMENSION(Nper_loc-FLOOR(Nper_loc/2.0)):: LastRows10    
    INTEGER,DIMENSION(Nper_loc):: FirstRows20   
    INTEGER,DIMENSION(Nper_loc):: seq1Nper_loc   
    INTEGER:: XNper_loc   
    INTEGER:: i,ii   

    
    IF(MOD(Nper_loc,2)==0) XNper_loc=2
    IF(MOD(Nper_loc,2)==1) XNper_loc=1

    seq1Nper_loc = (/(i, i=1,Nper_loc, 1)/)

    
    IF(DisaggType==10) THEN
       FastRows = FLOOR(Nper_loc/2.0) + seq1Nper_loc  
       FirstRows10 = (/(i, i=1,(FastRows(1)-1), 1)/)   
       LastRows10 =  (/(i, i=Nper_loc*(ReadLinesOrigMax_loc-1-1)+FastRows(Nper_loc)+1,(ReadLinesOrigMax_loc*Nper_loc),1)/)  
    ELSEIF(DisaggType==20) THEN
       FastRows = Nper_loc + seq1Nper_loc   
       FirstRows20 = (/(i, i=1,(FastRows(1)-1), 1)/)   
    ENDIF

    
    Fast = -999
    
    IF(DisaggType==10) THEN   
       IF(DiagnoseDisagg==1) WRITE(*,*) 'Linearly disaggregating averaged variable'
       DO i=1,(ReadLinesOrigMax_loc-1)
          Fast(Nper_loc*(i-1)+FastRows) = Slow(i) - &
               (Slow(i+1)-Slow(i))/(XNper_loc*Nper_loc) + &
               (Slow(i+1)-Slow(i))/Nper_loc*(/(ii, ii=1,Nper_loc, 1)/)
       ENDDO

       
       IF(iBlock==1) THEN
          Fast(FirstRows10) = Fast(FastRows(1))   
       ELSE
          Fast(FirstRows10) = SlowPrev - &
               (Slow(1)-SlowPrev)/(XNper_loc*Nper_loc) + &
               (Slow(1)-SlowPrev)/Nper_loc * &
               (/(ii, ii=(Nper_loc-SIZE(FirstRows10)+1),Nper_loc, 1)/)
       ENDIF
       
       IF(iBlock==ReadBlocksOrigMetData) THEN
          Fast(LastRows10) = Fast(Nper_loc*(ReadLinesOrigMax_loc-1-1)+FastRows(Nper_loc))   
       ELSE
          Fast(LastRows10) = Slow(ReadLinesOrigMax_loc) - &
               (SlowNext-Slow(ReadLinesOrigMax_loc))/(XNper_loc*Nper_loc) + &
               (SlowNext-Slow(ReadLinesOrigMax_loc))/Nper_loc * &
               (/(ii, ii=1,SIZE(LastRows10), 1)/)
       ENDIF
    ELSEIF(DisaggType==20) THEN   
       IF(DiagnoseDisagg==1) WRITE(*,*) 'Linearly disaggregating instantaneous variable'
       DO i=1,(ReadLinesOrigMax_loc-1)
          Fast(Nper_loc*(i-1)+FastRows) = (Slow(i) + &
               (Slow(i+1)-Slow(i))/Nper_loc*2*(seq1Nper_loc-1) + &
               Slow(i))/2
       ENDDO
       
       IF(iBlock==1) THEN
          Fast(FirstRows20) = Fast(FastRows(1))   
       ELSE
          Fast(FirstRows20) = (SlowPrev + &
               (Slow(1)-SlowPrev)/Nper_loc*2 * &
               ((/(ii, ii=(Nper_loc-SIZE(FirstRows20)+1),Nper_loc, 1)/)-1) + &
               SlowPrev)/2
       ENDIF
       
       
       
       
       
       
       
       
       
    ENDIF

    IF(ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
       WRITE(*,*) 'Problem: -999s (',COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc)==-999),') in disaggregated data.'
       CALL errorHint(13,'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.',NotUsed,NotUsed,NotUsedI)
    ENDIF

  ENDFUNCTION Disagg_Lin
  

  
  FUNCTION DisaggP_amongN(Slow,amongN, Nper_loc, ReadLinesOrig_loc, ReadLinesOrigMax_loc) RESULT(Fast)
    
    
    
    
    

    USE DefaultNotUsed
    USE sues_data

    IMPLICIT NONE

    INTEGER:: amongN       
    INTEGER:: Nper_loc     
    INTEGER:: ReadLinesOrig_loc,ReadLinesOrigMax_loc   
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc*Nper_loc):: Fast  
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc):: Slow   
    INTEGER,DIMENSION(:),ALLOCATABLE:: Subintervals  
    INTEGER,DIMENSION(Nper_loc):: seq1Nper_loc   
    INTEGER:: i

    
    ALLOCATE(Subintervals(amongN))
    Subintervals(:) = -999

    seq1Nper_loc = (/(i, i=1,Nper_loc, 1)/)

    IF(DiagnoseDisagg==1) WRITE(*,*) 'Distributing over ',amongN,' subintervals for variable'

    IF(amongN == Nper_loc) THEN
       Subintervals(:) = seq1Nper_loc
    ENDIF
    IF(amongN > Nper_loc) &
         CALL errorHint(2,'Problem in SUEWS_MetDisagg: no. of rainy periods cannot exceed number of subintervals', &
         REAL(Nper_loc,KIND(1d0)),NotUsed,amongN)


    
    Fast = -999
    DO i=1,ReadLinesOrigMax_loc
       Fast(Nper_loc*(i-1)+seq1Nper_loc) = 0   
       IF(Slow(i) > 0) THEN   
          IF(amongN < Nper_loc) THEN
             Subintervals(:) = -999
             Subintervals = RandomSamples(amongN,Nper_loc)
          ENDIF
          Fast(Nper_loc*(i-1)+SubIntervals) = Slow(i)/amongN
       ENDIF
    ENDDO

    IF(ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
       WRITE(*,*) 'Problem: -999s (',COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999),') in disaggregated data'
       CALL errorHint(13,'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.',NotUsed,NotUsed,NotUsedI)
    ENDIF

  ENDFUNCTION DisaggP_amongN
  

  
  FUNCTION DisaggP_amongNMult(Slow,multupperI, multamongN, Nper_loc, ReadLinesOrig_loc, ReadLinesOrigMax_loc) RESULT(Fast)
    
    
    
    
    

    USE DefaultNotUsed
    USE sues_data

    IMPLICIT NONE

    REAL(KIND(1d0)),DIMENSION(5):: multupperI     
    INTEGER,DIMENSION(5):: multamongN       
    INTEGER:: thisamongN       
    INTEGER:: Nper_loc     
    INTEGER:: ReadLinesOrig_loc,ReadLinesOrigMax_loc   
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc*Nper_loc):: Fast  
    REAL(KIND(1d0)),DIMENSION(ReadLinesOrig_loc):: Slow   
    INTEGER,DIMENSION(:),ALLOCATABLE:: Subintervals  
    INTEGER,DIMENSION(Nper_loc):: seq1Nper_loc   
    INTEGER:: i

    seq1Nper_loc = (/(i, i=1,Nper_loc, 1)/)

    IF(DiagnoseDisagg==1) WRITE(*,*) 'Distributing over variable subintervals depending on intensity for variable'

    
    Fast = -999
    DO i=1,ReadLinesOrigMax_loc
       Fast(Nper_loc*(i-1)+seq1Nper_loc) = 0   
       IF(Slow(i) > 0) THEN   
          
          IF(Slow(i) <= multupperI(1)) THEN
             thisamongN = multamongN(1)
          ELSEIF(Slow(i) > multupperI(1) .AND. Slow(i) <= multupperI(2)) THEN
             thisamongN = multamongN(2)
          ELSEIF(Slow(i) > multupperI(2) .AND. Slow(i) <= multupperI(3)) THEN
             thisamongN = multamongN(3)
          ELSEIF(Slow(i) > multupperI(3) .AND. Slow(i) <= multupperI(4)) THEN
             thisamongN = multamongN(4)
          ELSEIF(Slow(i) > multupperI(4) .AND. Slow(i) <= multupperI(5)) THEN
             thisamongN = multamongN(5)
          ELSEIF(Slow(i) > multupperI(5)) THEN
             thisamongN = multamongN(5)
             CALL errorHint(4,'Precip in met forcing file exceeds maxiumum MultRainAmongNUpperI',&
                  Slow(i),MultRainAmongNUpperI(5),NotUsed)
          ENDIF

          
          ALLOCATE(Subintervals(thisamongN))
          Subintervals(:) = -999

          IF(thisamongN > Nper_loc) CALL errorHint(2,'Problem in SUEWS_MetDisagg: no. of rainy periods cannot exceed ',&
               'number of subintervals', REAL(Nper_loc,KIND(1d0)),NotUsed,thisamongN)

          IF(thisamongN == Nper_loc) THEN   
             Subintervals(:) = seq1Nper_loc
          ELSEIF(thisamongN < Nper_loc) THEN
             Subintervals = RandomSamples(thisamongN,Nper_loc)
          ENDIF
          Fast(Nper_loc*(i-1)+SubIntervals) = Slow(i)/thisamongN
          
          DEALLOCATE(Subintervals)
       ENDIF
    ENDDO

    IF(ANY(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999)) THEN
       WRITE(*,*) 'Problem: -999s (',COUNT(Fast(1:ReadLinesOrigMax_loc*Nper_loc) == -999),') in disaggregated data'
       CALL errorHint(13,'Problem in SUEWS_MetDisagg: -999 values in disaggregated data.',NotUsed,NotUsed,NotUsedI)
    ENDIF

  ENDFUNCTION DisaggP_amongNMult
  

  
  FUNCTION RandomSamples(N,OutOf) RESULT(Samples)
    
    
    
    

    IMPLICIT NONE

    INTEGER:: i   
    INTEGER:: N   
    INTEGER:: OutOf   
    INTEGER:: X   
    REAL(KIND(1D0)):: r   
    INTEGER,DIMENSION(:),ALLOCATABLE:: Samples   

    
    ALLOCATE(Samples(N))
    Samples(:) = -999

    
    i=0 
    DO WHILE (ANY(Samples == -999))
       CALL RANDOM_NUMBER(r)
       X = INT(r*OutOf)+1
       
       
       IF(COUNT(Samples == X) == 0) THEN
          
          i=i+1
          Samples(i)=X
       ENDIF
       
    ENDDO

  ENDFUNCTION RandomSamples
  



ENDMODULE MetDisagg



