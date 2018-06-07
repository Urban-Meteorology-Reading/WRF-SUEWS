MODULE DailyState_module
  USE allocateArray,ONLY:&
       ndays,nsurf,nvegsurf,ivConif,ivDecid,ivGrass,ncolumnsDataOutDailyState


  IMPLICIT NONE
  
  
  

CONTAINS

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  SUBROUTINE SUEWS_cal_DailyState(&
       iy,id,it,imin,tstep,DayofWeek_id,&
       WaterUseMethod,snowUse,Ie_start,Ie_end,&
       LAICalcYes,LAIType,&
       nsh_real,avkdn,Temp_C,Precip,BaseTHDD,&
       lat,Faut,LAI_obs,tau_a,tau_f,tau_r,&
       SnowDensMax,SnowDensMin,SnowAlbMin,&
       alBMax_DecTr,alBMax_EveTr,alBMax_Grass,&
       AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       CapMax_dec,CapMin_dec,PorMax_dec,PorMin_dec,&
       Ie_a,Ie_m,DayWatPer,DayWat,SnowPack,&
       BaseT,BaseTe,GDDFull,SDDFull,LAIMin,LAIMax,LAIPower,&
       SnowAlb,DecidCap,albDecTr,albEveTr,albGrass,&
       porosity,GDD,HDD,SnowDens,LAI,WU_Day,&
       deltaLAI)


    IMPLICIT NONE
    
    
    
    
    
    


    INTEGER,INTENT(IN)::iy
    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::it
    INTEGER,INTENT(IN)::imin

    
    INTEGER,INTENT(IN)::tstep
    INTEGER,INTENT(IN)::WaterUseMethod
    INTEGER,INTENT(IN)::snowUse
    INTEGER,INTENT(IN)::Ie_start   
    INTEGER,INTENT(IN)::Ie_end       
    
    
    
    INTEGER,INTENT(IN)::LAICalcYes


    INTEGER,DIMENSION(nvegsurf),INTENT(IN):: LAIType                  

    

    REAL(KIND(1d0)),INTENT(IN)::nsh_real
    REAL(KIND(1d0)),INTENT(IN)::avkdn
    REAL(KIND(1d0)),INTENT(IN)::Temp_C
    REAL(KIND(1d0)),INTENT(IN)::Precip
    REAL(KIND(1d0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::Faut
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs
    REAL(KIND(1D0)),INTENT(IN)::tau_a
    REAL(KIND(1D0)),INTENT(IN)::tau_f
    REAL(KIND(1D0)),INTENT(IN)::tau_r
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMax
    REAL(KIND(1D0)),INTENT(IN)::SnowDensMin
    REAL(KIND(1D0)),INTENT(IN)::SnowAlbMin
    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec
    

    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN) ::Ie_a
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN) ::Ie_m 
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN) ::DayWatPer 
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN) ::DayWat 


    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(IN)      ::SnowPack
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::BaseT 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::BaseTe 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::GDDFull 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::SDDFull 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::LAIMin 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)   ::LAIMax 
    REAL(KIND(1d0)),DIMENSION(4,nvegsurf),INTENT(IN) ::LAIPower 


    

    
    

    
    
    
    
    REAL(KIND(1d0)),INTENT(INOUT)::SnowAlb

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::porosity
    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT):: GDD 
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(INOUT):: HDD          

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(INOUT)::SnowDens
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT):: LAI 
    INTEGER,DIMENSION(3),INTENT(in)::DayofWeek_id

    
    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(INOUT):: WU_Day
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI
    

    INTEGER::date



    

    
    

    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    CALL init_DailyState(&
         id,& 
         avkdn,&
         Temp_C,&
         Precip,&
         BaseTHDD,&
         nsh_real,&
         GDD,&
         HDD)


    
    IF (snowUse==1) CALL SnowUpdate(&
         nsurf,tstep,Temp_C,tau_a,tau_f,tau_r,&
         SnowDensMax,SnowDensMin,SnowAlbMin,SnowPack,&
         SnowAlb,SnowDens)

    
    

    
    
    IF (it==0.AND.imin==0) THEN
       
       
       

       
       
       
       
    ELSEIF (it==23 .AND. imin==(nsh_real-1)/nsh_real*60) THEN
       CALL Cal_DailyStateEnd(&
            id,it,imin,tstep,&
            LAIType,Ie_end,Ie_start,LAICalcYes,&
            WaterUseMethod,DayofWeek_id,&
            alBMax_DecTr,alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
            BaseT,BaseTe,CapMax_dec,CapMin_dec,DayWat,DayWatPer,Faut,GDDFull,&
            Ie_a,Ie_m,LAIMax,LAIMin,LAIPower,lat,PorMax_dec,PorMin_dec,SDDFull,LAI_obs,&
            albDecTr,albEveTr,albGrass,porosity,DecidCap,&
            GDD,HDD,LAI,WU_Day,&
            deltaLAI)
       
    ENDIF   

    RETURN

  END SUBROUTINE SUEWS_cal_DailyState


  SUBROUTINE Cal_DailyStateEnd(&
       id,it,imin,tstep,&
       LAIType,Ie_end,Ie_start,LAICalcYes,&
       WaterUseMethod,DayofWeek_id,&
       alBMax_DecTr,alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
       BaseT,BaseTe,CapMax_dec,CapMin_dec,DayWat,DayWatPer,Faut,GDDFull,&
       Ie_a,Ie_m,LAIMax,LAIMin,LAIPower,lat,PorMax_dec,PorMin_dec,SDDFull,LAI_obs,&
       albDecTr,albEveTr,albGrass,porosity,DecidCap,&
       GDD,HDD,LAI,WU_Day,&
       deltaLAI)
    IMPLICIT NONE
    
    


    
    
    
    
    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::it
    INTEGER,INTENT(IN)::imin
    INTEGER,INTENT(IN)::tstep
    INTEGER,INTENT(IN)::LAIType(nvegsurf)
    INTEGER,INTENT(IN)::Ie_end
    INTEGER,INTENT(IN)::Ie_start
    INTEGER,INTENT(IN)::LAICalcYes
    INTEGER,INTENT(IN)::WaterUseMethod
    INTEGER,INTENT(in)::DayofWeek_id(3)

    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::BaseT(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::BaseTe(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    
    REAL(KIND(1d0)),INTENT(IN)::DayWat(7)
    REAL(KIND(1d0)),INTENT(IN)::DayWatPer(7)
    REAL(KIND(1d0)),INTENT(IN)::Faut
    REAL(KIND(1d0)),INTENT(IN)::GDDFull(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::Ie_a(3)
    REAL(KIND(1d0)),INTENT(IN)::Ie_m(3)
    REAL(KIND(1d0)),INTENT(IN)::LAIMax(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAIMin(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAIPower(4,nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec
    REAL(KIND(1d0)),INTENT(IN)::SDDFull(nvegsurf)
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs

    
    
    
    REAL(KIND(1d0)),INTENT(INOUT)::albDecTr( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::albEveTr( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::albGrass( 0:ndays)
    
    REAL(KIND(1d0)),INTENT(INOUT)::porosity( 0:ndays)
    REAL(KIND(1d0)),INTENT(INOUT)::DecidCap( 0:ndays)
    
    REAL(KIND(1d0)),INTENT(INOUT)::GDD( 0:ndays, 5)
    REAL(KIND(1d0)),INTENT(INOUT)::HDD(-4:ndays, 6)
    REAL(KIND(1d0)),INTENT(INOUT)::LAI(-4:ndays, nvegsurf)

    REAL(KIND(1d0)),INTENT(INOUT):: WU_Day(0:ndays,9)
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI
    




    CALL update_HDD(&
         id,it,imin,tstep,& 
         HDD) 

    

    
    CALL update_WaterUse(&
         id,WaterUseMethod,DayofWeek_id,lat,Faut,HDD,&
         Ie_a,Ie_m,Ie_start,Ie_end,DayWatPer,DayWat,&
         WU_Day) 

    
    
    
    
    CALL update_GDDLAI(&
         id,LAICalcYes,& 
         lat,LAI_obs,&
         BaseT,&
         BaseTe,&
         GDDFull,&
         SDDFull,&
         LAIMin,&
         LAIMax,&
         LAIPower,LAIType,&
         GDD,LAI) 

    CALL update_Veg(&
         id,&
         LAImax,&
         LAIMin,&
         alBMax_DecTr,&
         alBMax_EveTr,&
         alBMax_Grass,&
         AlbMin_DecTr,&
         AlbMin_EveTr,&
         AlbMin_Grass,&
         CapMax_dec,&
         CapMin_dec,&
         PorMax_dec,&
         PorMin_dec,&
         DecidCap,&
         albDecTr,&
         albEveTr,&
         albGrass,&
         porosity,&
         LAI,&
         deltaLAI)

    
    
    
    
    


  END SUBROUTINE Cal_DailyStateEnd


  SUBROUTINE init_DailyState(&
       id,& 
       avkdn,&
       Temp_C,&
       Precip,&
       BaseTHDD,&
       nsh_real,&
       GDD,&
       HDD)
    IMPLICIT NONE
    

    INTEGER,INTENT(IN)::id
    
    
    
    
    REAL(KIND(1d0)),INTENT(IN)::avkdn
    REAL(KIND(1d0)),INTENT(IN)::Temp_C
    REAL(KIND(1d0)),INTENT(IN)::Precip
    REAL(KIND(1d0)),INTENT(IN)::BaseTHDD
    REAL(KIND(1d0)),INTENT(IN)::nsh_real

    
    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT):: GDD 
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(INOUT):: HDD          

    INTEGER::gamma1
    INTEGER::gamma2
    

    
    
    
    
    

    
    GDD(id,3) = MIN(Temp_C,GDD(id,3))     
    GDD(id,4) = MAX(Temp_C,GDD(id,4))     
    IF (avkdn>10) THEN
       GDD(id,5) = GDD(id,5)+1/nsh_real   
    ENDIF

    
    
    IF ((BaseTHDD-Temp_C)>=0) THEN   
       gamma1=1
    ELSE
       gamma1=0
    ENDIF

    IF ((Temp_C-BaseTHDD)>=0) THEN   
       gamma2=1
    ELSE
       gamma2=0
    ENDIF

    
    
    

    HDD(id,1)=HDD(id,1) + gamma1*(BaseTHDD-Temp_C)   
    HDD(id,2)=HDD(id,2) + gamma2*(Temp_C-BaseTHDD)   
    HDD(id,3)=HDD(id,3) + Temp_C                     
    
    HDD(id,5)=HDD(id,5) + Precip                     
    

  END SUBROUTINE init_DailyState


  SUBROUTINE update_AnOHM(&
       Gridiv,id,& 
       ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids,dataOutSUEWS,&
                                
       xBo,xmAH) 
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: Gridiv
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::ReadLinesMetdata
    INTEGER,INTENT(IN) ::ncolumnsDataOutSUEWS
    INTEGER,INTENT(IN) ::NumberOfGrids

    REAL(KIND(1d0)),DIMENSION(ReadLinesMetdata,ncolumnsDataOutSUEWS,NumberOfGrids),INTENT(IN)::dataOutSUEWS

    
    
    

    REAL(KIND(1d0)),INTENT(OUT)::xBo
    REAL(KIND(1d0)),INTENT(OUT)::xmAH
    
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE :: subMet 

    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE :: xQH
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE ::xQE
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE ::xAH
    REAL(KIND(1d0))::mxQH
    REAL(KIND(1d0))::mxQE
    
    
    

    INTEGER:: err
    INTEGER:: lenMetData
    INTEGER::nVar


    LOGICAL, ALLOCATABLE :: metMask(:)


    
    IF (ALLOCATED(metMask)) DEALLOCATE(metMask, STAT=err)
    ALLOCATE(metMask(SIZE(dataOutSUEWS, dim=1)))
    metMask=(dataOutSUEWS(:,2,Gridiv)==id & 
         .AND. dataOutSUEWS(:,4,Gridiv)==0)

    
    lenMetData = COUNT(metMask)

    
    nVar=3

    
    IF (ALLOCATED(subMet)) DEALLOCATE(subMet, STAT=err)
    ALLOCATE(subMet(lenMetData,nVar))
    subMet=RESHAPE(PACK(dataOutSUEWS(:,(/14,15,16/),Gridiv),&
         SPREAD(metMask, dim=2, ncopies=nVar)),& 
         (/lenMetData,nVar/)) 

    
    IF (ALLOCATED(xQH)) DEALLOCATE(xQH, STAT=err)
    ALLOCATE(xQH(lenMetData))
    IF (ALLOCATED(xQE)) DEALLOCATE(xQE, STAT=err)
    ALLOCATE(xQE(lenMetData))
    IF (ALLOCATED(xAH)) DEALLOCATE(xAH, STAT=err)
    ALLOCATE(xAH(lenMetData))

    xQH  = subMet(:,1)
    xQE  = subMet(:,2)
    mxQH = SUM(xQH(10:16))/7
    mxQE = SUM(xQE(10:16))/7
    
    IF ( ABS(mxQE) < 0.1 ) mxQE = 0.1
    xBo  = mxQH/mxQE

    
    xAH  = subMet(:,3)
    xmAH  = SUM(xAH(:))/24

    
    
    

  END SUBROUTINE update_AnOHM


  SUBROUTINE update_Veg(&
       id,&
       LAImax,&
       LAIMin,&
       alBMax_DecTr,&
       alBMax_EveTr,&
       alBMax_Grass,&
       AlbMin_DecTr,&
       AlbMin_EveTr,&
       AlbMin_Grass,&
       CapMax_dec,&
       CapMin_dec,&
       PorMax_dec,&
       PorMin_dec,&
       DecidCap,&
       albDecTr,&
       albEveTr,&
       albGrass,&
       porosity,&
       LAI,&
       deltaLAI)

    IMPLICIT NONE
    
    
    
    
    

    INTEGER,INTENT(IN)::id
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)::LAImax
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)::LAIMin

    REAL(KIND(1d0)),INTENT(IN)::alBMax_DecTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_EveTr
    REAL(KIND(1d0)),INTENT(IN)::alBMax_Grass
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_DecTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_EveTr
    REAL(KIND(1d0)),INTENT(IN)::AlbMin_Grass
    REAL(KIND(1d0)),INTENT(IN)::CapMax_dec
    REAL(KIND(1d0)),INTENT(IN)::CapMin_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMax_dec
    REAL(KIND(1d0)),INTENT(IN)::PorMin_dec

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(INOUT)::porosity

    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT)::LAI
    REAL(KIND(1d0)),INTENT(OUT)::deltaLAI

    INTEGER::iv

    REAL(KIND(1d0))::albChangeDecTr
    REAL(KIND(1d0))::albChangeEveTr
    REAL(KIND(1d0))::albChangeGrass
    REAL(KIND(1d0))::CapChange

    REAL(KIND(1d0))::deltaLAIEveTr
    REAL(KIND(1d0))::deltaLAIGrass
    REAL(KIND(1d0))::porChange
    
    
    
    
    
    
    deltaLAI=0
    deltaLAIEveTr=0
    deltaLAIGrass=0
    CapChange=0
    porChange=0
    albChangeDecTr=0
    albChangeEveTr=0
    albChangeGrass=0

    iv=ivDecid
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAI=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeDecTr=(alBMax_DecTr-AlbMin_DecTr)* deltaLAI
       CapChange=(CapMin_dec-CapMax_dec)* deltaLAI
       porChange=(PorMin_dec-PorMax_dec)* deltaLAI
    ENDIF

    iv=ivConif
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAIEveTr=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeEveTr=(alBMax_EveTr-AlbMin_EveTr)* deltaLAIEveTr    
    ENDIF

    iv=ivGrass
    IF((LAI(ID,iv)-LAI(ID-1,iv))/=0) THEN
       deltaLAIGrass=(LAI(id,iv)-LAI(id-1,iv))/(LAImax(iv)-LAIMin(iv))
       albChangeGrass=(alBMax_Grass-AlbMin_Grass)* deltaLAIGrass    
    ENDIF

    iv=ivDecid

    

    DecidCap(id) = DecidCap(id-1) - CapChange
    albDecTr(id) = albDecTr(id-1) + albChangeDecTr
    porosity(id) = porosity(id-1) + porChange 
    
    albEveTr(id) = albEveTr(id-1) + albChangeEveTr
    albGrass(id) = albGrass(id-1) + albChangeGrass

  END SUBROUTINE update_Veg



  SUBROUTINE update_GDDLAI(&
       id,LAICalcYes,& 
       lat,LAI_obs,&
       BaseT,&
       BaseTe,&
       GDDFull,&
       SDDFull,&
       LAIMin,&
       LAIMax,&
       LAIPower,LAIType,&
       GDD,LAI) 
    IMPLICIT NONE

    
    
    
    
    
    
    
    
    
    

    INTEGER,INTENT(IN)::id
    INTEGER,INTENT(IN)::LAICalcYes

    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::LAI_obs

    
    
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: BaseT          
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: BaseTe         
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: GDDFull        
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: SDDFull        
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: LAIMin         
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(IN)  :: LAIMax         
    REAL(KIND(1d0)),DIMENSION(4,nvegsurf),INTENT(IN):: LAIPower       
    
    INTEGER,DIMENSION(nvegsurf),INTENT(IN):: LAIType                  

    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(INOUT)       :: GDD 
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(INOUT):: LAI 

    REAL(KIND(1d0)):: no   
    REAL(KIND(1d0))::yes   
    REAL(KIND(1d0))::indHelp   

    INTEGER:: critDays
    INTEGER::iv


    critDays=50   

    
    DO iv=1,NVegSurf
       
       yes =((GDD(id,3)+GDD(id,4))/2-BaseT(iv))    
       no  =((GDD(id,3)+GDD(id,4))/2-BaseTe(iv))   

       indHelp = 0   

       IF(yes<0) THEN   
          indHelp=yes   
          yes=0
       ENDIF

       IF(no>0) no=0    

       
       GDD(id,1) = GDD(id-1,1)+yes
       GDD(id,2) = GDD(id-1,2)+no

       
       IF(GDD(id,2)<=SDDFull(iv).AND.indHelp<0) THEN
          GDD(id,1)=0
       ENDIF

       IF(GDD(id,1)>=GDDFull(iv)) THEN   
          GDD(id,1)=GDDFull(iv)          
          IF(GDD(id,2)<-critDays) GDD(id,1)=0
       ENDIF

       IF (GDD(id,2)<=SDDFull(iv)) THEN   
          GDD(id,2)=SDDFull(iv)           
          IF(GDD(id,1)>critDays) GDD(id,2)=0
       ENDIF

       
       IF(GDD(id,2)<-critDays.AND.GDD(id,2)>SDDFull(iv))  GDD(id,1)=0
       IF(GDD(id,1)> critDays.AND.GDD(id,1)<GDDFull(iv))  GDD(id,2)=0

       
       IF(lat>=0) THEN   
          IF (id==140.AND.GDD(id,2)/=0)  GDD(id,2)=0  
          
          IF (GDD(id,1)> critDays.AND.id<170) GDD(id,2)=0
          
          IF (GDD(id,2)<-critDays.AND.id>170) GDD(id,1)=0

          IF (LAItype(iv) < 0.5) THEN   
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN       
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN   
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(3,iv)*GDD(id,2)*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ELSEIF (LAItype(iv)>=0.5) THEN
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN        
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
                
             ELSEIF (GDD(id,5)<=12.AND.GDD(id,2)>SDDFull(iv)) THEN 
                LAI(id,iv)=(LAI(id-1,iv)*LAIPower(3,iv)*(1-GDD(id,2))*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ENDIF

       ELSEIF (lat<0) THEN   
          IF (id==300.AND.GDD(id,2)/=0)  GDD(id,2)=0   
          
          IF (GDD(id,1)> critDays.AND.id>250) GDD(id,2)=0
          
          IF (GDD(id,2)<-critDays.AND.id<250) GDD(id,1)=0

          IF (LAItype(iv) < 0.5) THEN   
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(3,iv)*GDD(id,2)*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ELSE
             IF(GDD(id,1)>0.AND.GDD(id,1)<GDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)**LAIPower(1,iv)*GDD(id,1)*LAIPower(2,iv))+LAI(id-1,iv)
                
             ELSEIF(GDD(id,2)<0.AND.GDD(id,2)>SDDFull(iv)) THEN
                LAI(id,iv)=(LAI(id-1,iv)*LAIPower(3,iv)*(1-GDD(id,2))*LAIPower(4,iv))+LAI(id-1,iv)
             ELSE
                LAI(id,iv)=LAI(id-1,iv)
             ENDIF
          ENDIF
       ENDIF   

       
       IF(LAI(id,iv).GT.LAImax(iv))THEN
          LAI(id,iv)=LAImax(iv)
       ELSEIF(LAI(id,iv).LT.LAImin(iv))THEN
          LAI(id,iv)=LAImin(iv)
       ENDIF

    ENDDO   

    IF(LAICalcYes==0)THEN 
       LAI(id-1,:)=LAI_obs 
    ENDIF
    

  END SUBROUTINE update_GDDLAI


  SUBROUTINE update_WaterUse(&
       id,WaterUseMethod,DayofWeek_id,lat,Faut,HDD,&
       Ie_a,Ie_m,Ie_start,Ie_end,DayWatPer,DayWat,&
       WU_Day) 

    IMPLICIT NONE
    

    INTEGER,INTENT(IN) :: id
    INTEGER,INTENT(IN) :: WaterUseMethod
    INTEGER,INTENT(IN)::Ie_start   
    INTEGER,INTENT(IN)::Ie_end       
    INTEGER,DIMENSION(3),INTENT(IN)::DayofWeek_id

    REAL(KIND(1d0)),INTENT(IN)::lat
    REAL(KIND(1d0)),INTENT(IN)::Faut          

    REAL(KIND(1d0)),DIMENSION(-4:366,6),INTENT(IN)::HDD
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN)::Ie_a
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN)::Ie_m   
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::DayWatPer  
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::DayWat       

    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(INOUT):: WU_Day       

    INTEGER::wd        
    INTEGER::&
         calc        

    IF (WaterUseMethod==0) THEN   

       wd=DayofWeek_id(1)

       IF (DayWat(wd)==1.0) THEN      
          calc=0
          IF (lat>=0) THEN            
             IF (id>=Ie_start-1.AND.id<=Ie_end+1) calc=1   
          ELSE                        
             calc=1
             IF (id>=Ie_end.AND.id<=Ie_start) calc=0       
          ENDIF

          IF(calc==1) THEN
             
             
             

             
             WU_day(id,2) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,2)<0) WU_Day(id,2)=0   

             
             WU_day(id,3) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,3)<0) WU_Day(id,3)=0   

             
             WU_Day(id,1)=(WU_day(id,2)+WU_day(id,3))

             
             WU_day(id,5) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,5)<0) WU_Day(id,5)=0   

             
             WU_day(id,6) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,6)<0) WU_Day(id,6)=0   

             
             WU_Day(id,4)=(WU_day(id,5)+WU_day(id,6))

             
             WU_day(id,8) = Faut*(Ie_a(1)+Ie_a(2)*HDD(id,3)+Ie_a(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,8)<0) WU_Day(id,8)=0   

             
             WU_day(id,9) = (1-Faut)*(Ie_m(1)+Ie_m(2)*HDD(id,3)+Ie_m(3)*HDD(id,6))*DayWatPer(wd)
             IF (WU_Day(id,9)<0) WU_Day(id,9)=0   

             
             WU_Day(id,7)=(WU_day(id,8)+WU_day(id,9))

          ELSE   
             WU_Day(id,1)=0
             WU_Day(id,2)=0
             WU_Day(id,3)=0
             WU_Day(id,4)=0
             WU_Day(id,5)=0
             WU_Day(id,6)=0
             WU_Day(id,7)=0
             WU_Day(id,8)=0
             WU_Day(id,9)=0
          ENDIF
       ENDIF
    ENDIF

  END SUBROUTINE update_WaterUse


  SUBROUTINE update_HDD(&
       id,it,imin,tstep,& 
       HDD) 
    IMPLICIT NONE
    INTEGER,INTENT(IN)::id,it,imin,tstep

    REAL(KIND(1d0)),DIMENSION(-4:366,6),INTENT(INOUT):: HDD

    INTEGER:: jj
    REAL(KIND(1d0))::tstepcount

    
    tstepcount=(it*60+imin)*60/tstep*1.
    
    HDD(id,1)=HDD(id,1)/tstepcount   
    HDD(id,2)=HDD(id,2)/tstepcount   
    HDD(id,3)=HDD(id,3)/tstepcount   

    
    DO jj=1,5
       HDD(id,4)=HDD(id,4) + HDD(id-(jj-1),3)
    ENDDO
    HDD(id,4) = HDD(id,4)/5

    
    IF(HDD(id,5)>0) THEN        
       HDD(id,6)=0
    ELSE
       HDD(id,6)=HDD(id-1,6)+1  
    ENDIF

  END SUBROUTINE update_HDD


  SUBROUTINE Cal_DailyStateStart(&
       id,iy,lat,&
       dayofWeek_id)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: id
    INTEGER,INTENT(IN) ::iy
    REAL(KIND(1d0)),INTENT(IN) ::lat

    INTEGER,DIMENSION(3),INTENT(OUT) ::dayofWeek_id

    INTEGER::wd
    INTEGER::mb
    INTEGER::date
    INTEGER::seas

    CALL day2month(id,mb,date,seas,iy,lat) 
    CALL Day_of_Week(date,mb,iy,wd)        

    dayofWeek_id(1)=wd      
    dayofWeek_id(2)=mb      
    dayofweek_id(3)=seas    

  END SUBROUTINE Cal_DailyStateStart

  SUBROUTINE SUEWS_update_DailyState(&
       iy,id,it,imin,dectime,&
       Gridiv,NumberOfGrids,nsh_real,&
       DailyStateLine,&
       dataOutDailyState)

    IMPLICIT NONE
    
    
    
    

    INTEGER,INTENT(IN) ::iy
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::it
    INTEGER,INTENT(IN) ::imin
    REAL(KIND(1d0)),INTENT(IN)::dectime

    
    
    
    
    
    
    
    
    
    
    
    REAL(KIND(1d0)),INTENT(IN) ::nsh_real
    
    
    
    
    
    
    

    INTEGER,INTENT(IN)::Gridiv
    INTEGER,INTENT(IN)::NumberOfGrids
    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5),INTENT(IN) :: DailyStateLine
    

    
    


    REAL(KIND(1d0)),DIMENSION(ndays,ncolumnsDataOutDailyState,NumberOfGrids),INTENT(INOUT):: dataOutDailyState


    

    
    
    
    
    
    
    
    
    
    



    
    dataOutDailyState(id,1:4,Gridiv)=[iy,id,it,imin]
    dataOutDailyState(id,5,Gridiv)=dectime
    
    dataOutDailyState(id,6:ncolumnsDataOutDailyState,Gridiv)=DailyStateLine

    
    
    
    
    
    
  END SUBROUTINE SUEWS_update_DailyState


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  SUBROUTINE update_DailyState(&
       iy,id,it,imin,nsh_real,&
       GDD,HDD,LAI,&
       DecidCap,albDecTr,albEveTr,albGrass,porosity,&
       WU_Day,&
       deltaLAI,VegPhenLumps,&
       SnowAlb,SnowDens,&
       a1,a2,a3,&
       DailyStateLine)

    IMPLICIT NONE
    
    

    INTEGER,INTENT(IN) ::iy
    INTEGER,INTENT(IN) ::id
    INTEGER,INTENT(IN) ::it
    INTEGER,INTENT(IN) ::imin
    REAL(KIND(1d0)),INTENT(IN) ::nsh_real

    REAL(KIND(1d0)),DIMENSION( 0:ndays, 5),INTENT(IN):: GDD          
    REAL(KIND(1d0)),DIMENSION(-4:ndays, 6),INTENT(IN):: HDD          
    REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf),INTENT(IN):: LAI   

    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::DecidCap
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albDecTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albEveTr
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::albGrass
    REAL(KIND(1d0)),DIMENSION( 0:ndays),INTENT(IN) ::porosity
    REAL(KIND(1d0)),DIMENSION(0:ndays,9),INTENT(IN):: WU_Day 

    REAL(KIND(1d0)),INTENT(IN) ::deltaLAI
    REAL(KIND(1d0)),INTENT(IN) ::VegPhenLumps
    REAL(KIND(1d0)),INTENT(IN) ::SnowAlb
    REAL(KIND(1d0)),DIMENSION(7),INTENT(IN)::SnowDens
    REAL(KIND(1d0)),INTENT(IN) ::a1
    REAL(KIND(1d0)),INTENT(IN) ::a2
    REAL(KIND(1d0)),INTENT(IN) ::a3

    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutDailyState-5),INTENT(OUT) :: DailyStateLine

    
    DailyStateLine=-999
    IF (it==23 .AND. imin==(nsh_real-1)/nsh_real*60) THEN
       
       
       DailyStateLine(1:6)   = HDD(id,1:6)
       DailyStateLine(6+1:6+5)  = GDD(id,1:5)
       DailyStateLine(11+1:11+3) = LAI(id,1:nvegsurf)
       DailyStateLine(14+1:14+5) = [DecidCap(id),Porosity(id),AlbEveTr(id),AlbDecTr(id),AlbGrass(id)]
       DailyStateLine(19+1:19+9) = WU_day(id-1,1:9)
       DailyStateLine(28+1)    = deltaLAI
       DailyStateLine(29+1)    = VegPhenLumps
       DailyStateLine(30+1:30+8) = [SnowAlb,SnowDens(1:7)]
       DailyStateLine(38+1:38+3) = [a1,a2,a3]

    END IF


  END SUBROUTINE update_DailyState


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  

  

  

  
  
  












  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


END MODULE DailyState_module
