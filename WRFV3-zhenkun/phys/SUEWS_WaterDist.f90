MODULE WaterDist_module

  IMPLICIT NONE
CONTAINS

  SUBROUTINE drainage(&
       is,& 
       state_is,&
       StorCap,&
       DrainEq,&
       DrainCoef1,&
       DrainCoef2,&
       nsh_real,&
       drain_is)

    
    
    
    
    
    
    
    
    
    

    
    
    
    

    IMPLICIT NONE
    INTEGER,INTENT(in)::&
         is 
    REAL (KIND(1d0)),INTENT(in)::&
         state_is,  &
         StorCap,   &
         DrainCoef1,&
         DrainCoef2,&
         DrainEq,   &
         nsh_real    
    REAL (KIND(1d0)),INTENT(out)::&
         drain_is


    
    IF(state_is<0.000000001) THEN
       drain_is=0.0
    ELSE
       IF(INT(DrainEq)==1) THEN   

          IF (state_is<StorCap) THEN
             drain_is=0   
          ELSE
             drain_is=(DrainCoef1*(state_is-StorCap)**DrainCoef2)/nsh_real
          ENDIF

       ELSEIF(INT(DrainEq)==2) THEN   
          drain_is=(DrainCoef1*(EXP(DrainCoef2*state_is)-1))/nsh_real
          

       ELSEIF(INT(DrainEq)==3) THEN   
          drain_is=(DrainCoef1*(state_is**DrainCoef2))/nsh_real

          
          
          
          
       ENDIF

       
       
       
       
       IF (drain_is>state_is) THEN

          CALL ErrorHint(61,'SUEWS_drain: drain_is > state_is for surface is ',drain_is,state_is,is)
          drain_is=state_is   
       ELSEIF(drain_is<0.0001) THEN
          drain_is=0
       ENDIF
    ENDIF

    RETURN

  END SUBROUTINE drainage
  

  SUBROUTINE soilstore(&
       is,& 
       sfr,&
       PipeCapacity,&
       RunoffToWater,&
       pin,&
       wu_EveTr,&
       wu_DecTr,&
       wu_Grass,&
       drain,&
       AddWater,&
       addImpervious,&
       nsh_real,&
       stateOld,&
       AddWaterRunoff,&
       PervFraction,&
       addVeg,&
       soilstoreCap,&
       addWaterBody,&
       FlowChange,&
       StateLimit,&
       runoffAGimpervious,&
       surplusWaterBody,&
       runoffAGveg,&
       runoffPipes,&
       ev,&
       soilmoist,&
       SurplusEvap,&
       runoffWaterBody,&
       runoff_per_interval,&
       p_mm,&
       chang,&
       runoff,&
       state&
       )
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    

    IMPLICIT NONE

    
    
    INTEGER, PARAMETER:: nsurf=7                
    
    

    INTEGER:: PavSurf   = 1,&   
         BldgSurf  = 2,&
         ConifSurf = 3,&
         DecidSurf = 4,&
         GrassSurf = 5,&   
         BSoilSurf = 6,&   
         WaterSurf = 7
    
    
    
    
    

    INTEGER,INTENT(in)::is 


    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr(nsurf)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWater(nsurf)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::stateOld(nsurf)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWaterRunoff(nsurf)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilstoreCap(nsurf)
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::StateLimit(nsurf)

    REAL(KIND(1d0)),INTENT(in)::PipeCapacity
    REAL(KIND(1d0)),INTENT(in)::RunoffToWater
    REAL(KIND(1d0)),INTENT(in)::pin
    REAL(KIND(1d0)),INTENT(in)::wu_EveTr
    REAL(KIND(1d0)),INTENT(in)::wu_DecTr
    REAL(KIND(1d0)),INTENT(in)::wu_Grass
    REAL(KIND(1d0)),INTENT(in)::addImpervious
    REAL(KIND(1d0)),INTENT(in)::nsh_real
    REAL(KIND(1d0)),INTENT(in)::PervFraction
    REAL(KIND(1d0)),INTENT(in)::addVeg
    REAL(KIND(1d0)),INTENT(in)::addWaterBody
    REAL(KIND(1d0)),INTENT(in)::FlowChange


    REAL(KIND(1d0)),INTENT(inout)::runoffAGimpervious
    REAL(KIND(1d0)),INTENT(inout)::surplusWaterBody
    REAL(KIND(1d0)),INTENT(inout)::runoffAGveg
    REAL(KIND(1d0)),INTENT(inout)::runoffPipes
    REAL(KIND(1d0)),INTENT(inout)::ev
    REAL(KIND(1d0)),INTENT(inout)::runoffWaterBody
    REAL(KIND(1d0)),INTENT(inout)::runoff_per_interval

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::soilmoist  
    REAL(KIND(1d0)),DIMENSION(2),INTENT(inout)    ::SurplusEvap

    REAL(KIND(1d0)),INTENT(out)::p_mm

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::chang 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoff
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::drain 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::state 

    
    REAL(KIND(1d0)):: EvPart
    REAL(KIND(1d0)),PARAMETER:: NotUsed=-55.5,&
         IPThreshold_mmhr=10

    
    EvPart=0

    

    
    
    
    IF(is==ConifSurf) THEN
       p_mm=pin+wu_EveTr
    ELSEIF(is==DecidSurf) THEN
       p_mm=pin+wu_DecTr
    ELSEIF(is==GrassSurf) THEN
       p_mm=pin+wu_Grass
    ELSE
       p_mm=pin
    ENDIF

    
    
    
    p_mm=p_mm+AddWater(is)

    
    IF(is==PavSurf.OR.is==BldgSurf) THEN

       
       
       IF (is==PavSurf) THEN
          IF(sfr(PavSurf)/=0) THEN   
             p_mm=p_mm+addImpervious/sfr(PavSurf)
          ENDIF
       ENDIF

       
       chang(is)=p_mm-(drain(is)+ev)

       
       
       IF(p_mm>IPThreshold_mmhr/nsh_real) THEN
          runoff(is)=runoff(is)+(p_mm-IPThreshold_mmhr/nsh_real)
          chang(is)=IPThreshold_mmhr/nsh_real-(drain(is)+ev)
       ENDIF

       
       state(is)=state(is)+chang(is)

       
       IF(state(is)<0.0) THEN   

          
          SurplusEvap(is)=ABS(state(is))   
          ev = ev-SurplusEvap(is)          
          state(is)=0.0                    
          
          
          
          
          
       ENDIF

       
       chang(is) = state(is)-stateOld(is)

       
       
       runoff(is)=runoff(is)+drain(is)*AddWaterRunoff(is)   

       
       
       
       

       
    ELSEIF(is>=3) THEN

       
       IF(PervFraction/=0) THEN   
          EvPart=(SurplusEvap(PavSurf)*sfr(PavSurf)+SurplusEvap(BldgSurf)*sfr(BldgSurf))/PervFraction
       ELSE         
          EvPart=0  
       ENDIF

       
       ev=ev+EvPart

       
       IF (is/=WaterSurf) THEN

          
          
          IF (is==GrassSurf.OR.is==BSoilSurf) THEN
             IF ((sfr(GrassSurf)+sfr(BSoilSurf))/=0) THEN
                p_mm=p_mm+addVeg/(sfr(GrassSurf)+sfr(BSoilSurf))
             ENDIF
          ENDIF

          
          chang(is)=p_mm-(drain(is)+ev)

          
          
          IF (p_mm>IPThreshold_mmhr/nsh_real) THEN
             runoff(is)=runoff(is)+(p_mm-IPThreshold_mmhr/nsh_real)
             chang(is)=IPThreshold_mmhr/nsh_real-(drain(is)+ev)
          ENDIF

          
          state(is)=state(is)+chang(is)

          
          IF(state(is)<0.0) THEN   
             
             
             IF((soilmoist(is)+state(is))>=0) THEN
                soilmoist(is)=soilmoist(is)+state(is)
                state(is)=0.0

             ELSE
                ev=ev-ABS(state(is))   
                state(is)=0.0          
             ENDIF
             
             
             
             
             
             
             
          ENDIF

          
          chang(is) = state(is)-stateOld(is)

          
          
          
          
          soilmoist(is)=soilmoist(is)+drain(is)*AddWaterRunoff(is)

          
          IF(soilmoist(is)>soilstoreCap(is)) THEN              
             runoff(is)=runoff(is)+(soilmoist(is)-soilstoreCap(is))
             soilmoist(is)=soilstoreCap(is)
          ELSEIF (soilmoist(is)<0) THEN   
             CALL ErrorHint(62,'SUEWS_store: soilmoist(is) < 0 ',soilmoist(is),NotUsed,is)

             
          ENDIF

          
       ELSEIF (is==WaterSurf) THEN

          IF(sfr(WaterSurf)/=0)THEN

             
             p_mm=p_mm+addWaterBody/sfr(WaterSurf)

             
             
             
             chang(is)=p_mm+FlowChange/nsh_real-(ev)

             
             state(is)=state(is)+chang(is)

             
             IF(state(is)<0.0) THEN   

                ev=ev-ABS(state(is))   
                state(is)=0.0          
                
                
                
                
                
             ENDIF

             
             chang(is) = state(is)-stateOld(is)

             
             IF (state(WaterSurf)>StateLimit(WaterSurf)) THEN
                runoff(WaterSurf)=runoff(WaterSurf)+(state(WaterSurf)-StateLimit(WaterSurf))
                state(WaterSurf)=StateLimit(WaterSurf)
                runoffWaterBody=runoffWaterBody+runoff(WaterSurf)*sfr(WaterSurf)
             ELSE
                state(WaterSurf)=state(WaterSurf)+surplusWaterBody
                IF (state(WaterSurf)>StateLimit(WaterSurf)) THEN
                   runoffWaterBody=runoffWaterBody+(state(WaterSurf)-StateLimit(WaterSurf))*sfr(WaterSurf)
                   state(WaterSurf)=StateLimit(WaterSurf)
                ENDIF
             ENDIF

             
             chang(is) = state(is)-stateOld(is)
          ENDIF

       ENDIF   

    ENDIF   

    
    

    
    
    
    

    IF(is<WaterSurf) THEN   

       
       runoffPipes=runoffPipes+(runoff(is)*sfr(is))
       
       CALL updateFlood(&
                                
            nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf,&
            sfr,PipeCapacity,RunoffToWater,&
                                
            runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes&
            )
    ENDIF

    runoff_per_interval=runoff_per_interval+(runoff(is)*sfr(is)) 

  END SUBROUTINE soilstore
  
  
  SUBROUTINE updateFlood(&

                                
       nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf,&
       sfr,PipeCapacity,RunoffToWater,&
                                
       runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes&
       )

    
    

    IMPLICIT NONE
    INTEGER, INTENT(in) :: nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf
    REAL(KIND(1d0)), INTENT(in) :: sfr(nsurf),PipeCapacity,RunoffToWater
    REAL(KIND(1d0)), INTENT(inout) :: runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes

    
    
    IF(runoffPipes>PipeCapacity) THEN

       
       IF(is==PavSurf.OR.is==BldgSurf) THEN
          IF(sfr(WaterSurf)>0.0000001) THEN
             
             
             runoffAGimpervious=runoffAGimpervious+(runoffPipes-PipeCapacity)*(1-RunoffToWater)
             surplusWaterBody=surplusWaterBody+(runoffPipes-PipeCapacity)*RunoffToWater
          ELSE
             
             runoffAGimpervious=runoffAGimpervious+(runoffPipes-PipeCapacity)
          ENDIF
          
       ELSEIF(is>=ConifSurf.AND.is<=BSoilSurf) THEN
          IF(sfr(WaterSurf)>0.0000001) THEN
             
             runoffAGveg=runoffAGveg+(runoffPipes-PipeCapacity)*(1-RunoffToWater)
             surplusWaterBody=surplusWaterBody+(runoffPipes-PipeCapacity)*RunoffToWater
          ELSE
             
             runoffAGveg=runoffAGveg+(runoffPipes-PipeCapacity)
          ENDIF
       ENDIF

       runoffPipes=PipeCapacity   

    ENDIF   

  END SUBROUTINE updateFlood

  SUBROUTINE ReDistributeWater(&
                                
       nsurf,& 
       WaterSurf,&
       snowUse,&
       WaterDist,  &
       sfr,   &
       Drain,&
                                
       AddWaterRunoff,&
       AddWater&
       )
    
    
    
    

    
    
    
    

    IMPLICIT NONE

    INTEGER,INTENT(in)::nsurf 
    INTEGER,INTENT(in)::WaterSurf
    INTEGER,INTENT(in)::snowUse

    REAL (KIND(1d0)),INTENT(in)::WaterDist(nsurf+1,nsurf-1) 
    REAL (KIND(1d0)),INTENT(in)::sfr(nsurf)                
    REAL (KIND(1d0)),INTENT(in)::Drain(nsurf)               

    REAL (KIND(1d0)),INTENT(out)::AddWaterRunoff(nsurf)
    REAL (KIND(1d0)),INTENT(out)::AddWater(nsurf)        

    INTEGER::ii,jj,&
         NSurfDoNotReceiveDrainage=0

    
    DO ii=1,nsurf-1   
       AddWaterRunoff(ii)=WaterDist(8,ii)
    ENDDO
    AddWaterRunoff(WaterSurf)=0
    AddWater=0

    DO ii=1,nsurf-NSurfDoNotReceiveDrainage 
       DO jj=1,nsurf-(NSurfDoNotReceiveDrainage+1) 

          IF (sfr(ii)/=0) THEN 

             
             IF (snowUse==0) THEN
                AddWater(ii)=AddWater(ii)+(Drain(jj)*sfr(jj)/sfr(ii))*WaterDist(ii,jj) 

                
             ELSE
                AddWaterRunoff(jj)=AddWaterRunoff(jj)+WaterDist(ii,jj) 
             ENDIF

          ELSE
             AddWaterRunoff(jj)=AddWaterRunoff(jj)+WaterDist(ii,jj) 
             
          ENDIF
       ENDDO
    ENDDO
  END SUBROUTINE ReDistributeWater


  SUBROUTINE SUEWS_update_SoilMoist(&
       NonWaterFraction,&
       soilstoreCap,sfr,soilmoist,&
       SoilMoistCap,SoilState,&
       vsmd,smd)
    IMPLICIT NONE
    INTEGER,PARAMETER ::nsurf     = 7 
    INTEGER,PARAMETER ::ConifSurf = 3 
    INTEGER,PARAMETER ::DecidSurf = 4 
    INTEGER,PARAMETER ::GrassSurf = 5

    
    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in),DIMENSION(nsurf)::soilstoreCap,sfr,soilmoist

    REAL(KIND(1d0)),INTENT(out)::SoilMoistCap,SoilState
    REAL(KIND(1d0)),INTENT(out)::vsmd,smd

    INTEGER :: is


    SoilMoistCap=0   
    SoilState=0      

    IF (NonWaterFraction/=0) THEN 
       DO is=1,nsurf-1   
          SoilMoistCap=SoilMoistCap+(soilstoreCap(is)*sfr(is)/NonWaterFraction)
          SoilState=SoilState+(soilmoist(is)*sfr(is)/NonWaterFraction)
       ENDDO
    ENDIF

    
    
    smd=SoilMoistCap-SoilState
    

    
    vsmd=0
    DO is=ConifSurf,GrassSurf  
       IF ( sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf) ==0 ) THEN
          vsmd=0
       ELSE
          vsmd=vsmd+(soilstoreCap(is) - soilmoist(is))*sfr(is)/(sfr(ConifSurf) + sfr(DecidSurf) + sfr(GrassSurf))
       END IF
       
    ENDDO

  END SUBROUTINE SUEWS_update_SoilMoist


  
  SUBROUTINE SUEWS_cal_SoilMoist(&
       SMDMethod,xsmd,NonWaterFraction,SoilMoistCap,&
       SoilStoreCap,surf_chang_per_tstep,&
       soilmoist,soilmoistOld,sfr,&
       smd,smd_nsurf,tot_chang_per_tstep,SoilState)

    IMPLICIT NONE
    INTEGER,PARAMETER::nsurf=7


    INTEGER,INTENT(in) ::SMDMethod
    REAL(KIND(1d0)),INTENT(in)::xsmd
    REAL(KIND(1d0)),INTENT(in)::NonWaterFraction
    REAL(KIND(1d0)),INTENT(in)::SoilMoistCap

    REAL(KIND(1d0)),INTENT(in)::surf_chang_per_tstep
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilmoist
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilmoistOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::SoilStoreCap        

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::smd_nsurf
    REAL(KIND(1d0)),INTENT(out)::SoilState
    REAL(KIND(1d0)),INTENT(out)::smd
    REAL(KIND(1d0)),INTENT(out)::tot_chang_per_tstep

    REAL(KIND(1d0)),PARAMETER::NotUsed=-999
    REAL(KIND(1d0)),PARAMETER::NAN=-999
    INTEGER :: is

    SoilState=0       
    IF (NonWaterFraction/=0) THEN 
       DO is=1,nsurf-1   
          SoilState=SoilState+(soilmoist(is)*sfr(is)/NonWaterFraction)
          IF (SoilState<0) THEN
             CALL ErrorHint(62,'SUEWS_Calculations: total SoilState < 0 (just added surface is) ',SoilState,NotUsed,is)
          ELSEIF (SoilState>SoilMoistCap) THEN
             CALL ErrorHint(62,'SUEWS_Calculations: total SoilState > capacity (just added surface is) ',SoilState,NotUsed,is)
             
          ENDIF
       ENDDO  
    ENDIF

    
    smd=SoilMoistCap-SoilState   
    smd_nsurf=SoilstoreCap-soilmoist   

    
    
    tot_chang_per_tstep = surf_chang_per_tstep   
    DO is=1,(nsurf-1)   
       tot_chang_per_tstep = tot_chang_per_tstep + ((SoilMoist(is)-soilmoistOld(is))*sfr(is))   
    ENDDO

    IF (SMDMethod>0) THEN
       
       smd_nsurf=NAN
       smd=xsmd
    ENDIF


  END SUBROUTINE SUEWS_cal_SoilMoist


END MODULE WaterDist_module
