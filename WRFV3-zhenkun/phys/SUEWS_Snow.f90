MODULE Snow_module

  IMPLICIT NONE
CONTAINS
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  SUBROUTINE Snow_cal_MeltHeat(&
       snowUse,&
       lvS_J_kg,lv_J_kg,tstep_real,RadMeltFact,TempMeltFact,SnowAlbMax,&
       SnowDensMin,Temp_C,Precip,PrecipLimit,PrecipLimitAlb,&
       nsh_real,sfr,Tsurf_ind,Tsurf_ind_snow,state,qn1_ind_snow,&
       kup_ind_snow,Meltwaterstore,deltaQi,&
       SnowPack,snowFrac,SnowAlb,SnowDens,SnowfallCum,&
       mwh,fwh,Qm,QmFreez,QmRain,&
       veg_fr,snowCalcSwitch,Qm_melt,Qm_freezState,Qm_rain,FreezMelt,&
       FreezState,FreezStateVol,rainOnSnow,SnowDepth,mw_ind,&
       dataOutLineSnow)

    IMPLICIT NONE
    INTEGER,PARAMETER::nsurf=7
    INTEGER,PARAMETER::PavSurf=1
    INTEGER,PARAMETER::BldgSurf=2
    INTEGER,PARAMETER::WaterSurf=7
    INTEGER,PARAMETER::ncolumnsDataOutSnow=102-5
    REAL(KIND(1d0)),PARAMETER::waterDens=999.8395 

    
    INTEGER,INTENT(in)::snowUse
    
    
    
    

    REAL(KIND(1d0)),INTENT(in)::lvS_J_kg
    REAL(KIND(1d0)),INTENT(in)::lv_J_kg
    REAL(KIND(1d0)),INTENT(in)::tstep_real
    REAL(KIND(1d0)),INTENT(in)::RadMeltFact
    REAL(KIND(1d0)),INTENT(in)::TempMeltFact
    REAL(KIND(1d0)),INTENT(in)::SnowAlbMax
    REAL(KIND(1d0)),INTENT(in)::SnowDensMin
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::Precip
    REAL(KIND(1d0)),INTENT(in)::PrecipLimit
    REAL(KIND(1d0)),INTENT(in)::PrecipLimitAlb
    REAL(KIND(1d0)),INTENT(in)::nsh_real
    

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Tsurf_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Tsurf_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::qn1_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::kup_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Meltwaterstore
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::deltaQi


    
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowPack
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::snowFrac
    REAL(KIND(1d0)),INTENT(inout)::SnowAlb
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens
    REAL(KIND(1d0)),INTENT(inout)::SnowfallCum

    
    REAL(KIND(1d0)),INTENT(out)::mwh
    REAL(KIND(1d0)),INTENT(out)::fwh
    REAL(KIND(1d0)),INTENT(out)::Qm
    REAL(KIND(1d0)),INTENT(out)::QmFreez
    REAL(KIND(1d0)),INTENT(out)::QmRain

    REAL(KIND(1d0)),INTENT(out)::veg_fr

    INTEGER,DIMENSION(nsurf),INTENT(out)::snowCalcSwitch

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_melt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_freezState
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_rain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezMelt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezState
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezStateVol
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::rainOnSnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::SnowDepth
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::mw_ind

    REAL(KIND(1d0)),DIMENSION(ncolumnsDataOutSnow),INTENT(out) :: dataOutLineSnow

    IF ( snowUse==1 ) THEN

       CALL MeltHeat(&
            bldgsurf,nsurf,PavSurf,WaterSurf,&
            lvS_J_kg,lv_J_kg,tstep_real,RadMeltFact,TempMeltFact,&
            SnowAlbMax,SnowDensMin,Temp_C,Precip,PrecipLimit,PrecipLimitAlb,&
            nsh_real,waterdens,sfr,Tsurf_ind,state,qn1_ind_snow,&
            Meltwaterstore,deltaQi,SnowPack,snowFrac,SnowAlb,SnowDens,SnowfallCum,&
            mwh,fwh,Qm,QmFreez,QmRain,snowCalcSwitch,&
            Qm_melt,Qm_freezState,Qm_rain,FreezMelt,FreezState,FreezStateVol,&
            rainOnSnow,SnowDepth,mw_ind)

       CALL veg_fr_snow(&
            sfr,snowFrac,nsurf,&
            veg_fr)


    ELSE 
       mwh=0
       fwh=0
       Qm=0
       QmFreez=0
       QmRain=0
       SnowfallCum=0
       snowCalcSwitch=0
       Qm_melt=0
       Qm_freezState=0
       Qm_rain=0
       FreezMelt=0
       FreezState=0
       FreezStateVol=0
       rainOnSnow=0
       SnowDepth=0
       mw_ind=0

       
       snowFrac=0
       CALL veg_fr_snow(&
            sfr,snowFrac,nsurf,&
            veg_fr)

    END IF

    
    dataOutLineSnow=[&
         SnowPack(1:nsurf),mw_ind(1:nsurf),Qm_melt(1:nsurf),            & 
         Qm_rain(1:nsurf),Qm_freezState(1:nsurf),snowFrac(1:(nsurf-1)), & 
         rainOnSnow(1:nsurf),                                           & 
         qn1_ind_snow(1:nsurf),kup_ind_snow(1:nsurf),freezMelt(1:nsurf),& 
         MeltWaterStore(1:nsurf),SnowDens(1:nsurf),                     & 
         snowDepth(1:nsurf),Tsurf_ind_snow(1:nsurf)]
    

  END SUBROUTINE Snow_cal_MeltHeat


  SUBROUTINE MeltHeat(&
       bldgsurf,&
       nsurf,&
       PavSurf,&
       WaterSurf,&
       lvS_J_kg,&
       lv_J_kg,&
       tstep_real,&
       RadMeltFact,&
       TempMeltFact,&
       SnowAlbMax,&
       SnowDensMin,&
       Temp_C,&
       Precip,&
       PrecipLimit,&
       PrecipLimitAlb,&
       nsh_real,&
       waterdens,&
       sfr,&
       Tsurf_ind,&
       state,&
       qn1_ind_snow,&
       Meltwaterstore,&
       deltaQi,&
       SnowPack,&
       snowFrac,&
       SnowAlb,&
       SnowDens,&
       SnowfallCum,&
       mwh,&
       fwh,&
       Qm,&
       QmFreez,&
       QmRain,&
       snowCalcSwitch,&
       Qm_melt,&
       Qm_freezState,&
       Qm_rain,&
       FreezMelt,&
       FreezState,&
       FreezStateVol,&
       rainOnSnow,&
       SnowDepth,&
       mw_ind)

    IMPLICIT NONE

    
    INTEGER,INTENT(in)::bldgsurf
    INTEGER,INTENT(in)::nsurf
    INTEGER,INTENT(in)::PavSurf
    INTEGER,INTENT(in)::WaterSurf

    REAL(KIND(1d0)),INTENT(in)::lvS_J_kg
    REAL(KIND(1d0)),INTENT(in)::lv_J_kg
    REAL(KIND(1d0)),INTENT(in)::tstep_real
    REAL(KIND(1d0)),INTENT(in)::RadMeltFact
    REAL(KIND(1d0)),INTENT(in)::TempMeltFact
    REAL(KIND(1d0)),INTENT(in)::SnowAlbMax
    REAL(KIND(1d0)),INTENT(in)::SnowDensMin
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::Precip
    REAL(KIND(1d0)),INTENT(in)::PrecipLimit
    REAL(KIND(1d0)),INTENT(in)::PrecipLimitAlb
    REAL(KIND(1d0)),INTENT(in)::nsh_real
    REAL(KIND(1d0)),INTENT(in)::waterdens

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Tsurf_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::qn1_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Meltwaterstore
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::deltaQi


    
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowPack
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::snowFrac
    REAL(KIND(1d0)),INTENT(inout)::SnowAlb
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens
    REAL(KIND(1d0)),INTENT(inout)::SnowfallCum

    
    REAL(KIND(1d0)),INTENT(out)::mwh
    REAL(KIND(1d0)),INTENT(out)::fwh
    REAL(KIND(1d0)),INTENT(out)::Qm
    REAL(KIND(1d0)),INTENT(out)::QmFreez
    REAL(KIND(1d0)),INTENT(out)::QmRain

    
    INTEGER,DIMENSION(nsurf),INTENT(out)::snowCalcSwitch

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_melt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_freezState
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::Qm_rain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezMelt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezState
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::FreezStateVol
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::rainOnSnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::SnowDepth
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::mw_ind

    
    REAL(KIND(1d0))::AdjMeltFact
    REAL(KIND(1d0))::Watfreeze

    REAL(KIND(1d0)),PARAMETER::cw=4190  

    INTEGER::is,xx

    
    mwh= 0 
    fwh=0
    Qm=0
    QmFreez=0
    QmRain=0
    snowCalcSwitch=0
    Qm_melt=0
    Qm_freezState=0
    Qm_rain=0
    FreezMelt=0
    FreezState=0
    FreezStateVol=0
    rainOnSnow = 0
    SnowDepth = 0
    mw_ind=0

    
    xx=bldgsurf
    xx=PavSurf
    


    
    DO is=1,nsurf  
       IF (sfr(is)/=0) THEN  

          IF (SnowPack(is)>0) THEN  

             SnowDepth(is) = (SnowPack(is)/1000)*waterDens/SnowDens(is) 

             

             
             IF (Temp_C>=0) THEN
                IF (qn1_ind_snow(is)<0) THEN
                   mw_ind(is) = TempMeltFact*Temp_C             
                ELSE
                   mw_ind(is) = RadMeltFact*(qn1_ind_snow(is))  
                ENDIF

             ELSE  
                AdjMeltFact=1  
                mw_ind(is) = TempMeltFact*Temp_C*AdjMeltFact 
             ENDIF
             
             mw_ind(is) = mw_ind(is)/nsh_real

             IF (mw_ind(is)>SnowPack(is)) mw_ind(is) = SnowPack(is)

             
             
             
             Qm_melt(is) = waterDens*((mw_ind(is)/tstep_real)/1000)*(lvS_J_kg-lv_J_kg)

             
             IF (mw_ind(is)<0) THEN

                FreezMelt(is) = -mw_ind(is) 
                mw_ind(is) = 0

                
                IF (FreezMelt(is)>Meltwaterstore(is)) FreezMelt(is) = Meltwaterstore(is)

                
                Qm_melt(is) = waterDens*((-FreezMelt(is)/tstep_real)/1000)*(lvS_J_kg-lv_J_kg)
             ENDIF

             
             
             
             
             IF (Temp_C>=PrecipLimit.AND.Precip>0) THEN
                Qm_rain(is) = waterDens*cw*(Temp_C-PrecipLimit)*(Precip*0.001/tstep_real)  
                IF (Qm_rain(is)<0) THEN 
                   Qm_rain(is) = 0
                ELSE
                   rainOnSnow(is) = Precip 
                ENDIF
             ENDIF

          ENDIF 

          

          
          IF (Tsurf_ind(is)<0.AND.state(is)>0) THEN

             snowCalcSwitch(is)=1 

             
             IF (is/=WaterSurf) THEN

                
                
                
                FreezState(is) = -TempMeltFact*Tsurf_ind(is)/nsh_real

                
                IF (FreezState(is)>state(is)) FreezState(is) = state(is)

                IF (SnowPack(is)==0.OR.snowfrac(is)==0) THEN 
                   FreezStateVol(is) = FreezState(is)
                ELSE                                         
                   FreezStateVol(is) = FreezState(is)*(1-snowFrac(is))/snowFrac(is)
                ENDIF

                
                
                IF (FreezStateVol(is)<0.00000000001.AND.FreezState(is)<state(is)) THEN
                   FreezState(is) = 0
                   FreezStateVol(is) = 0
                ENDIF

                
                Qm_freezState(is) = -waterDens*(FreezState(is)/tstep_real/1000)*(lvS_J_kg-lv_J_kg)

                
             ELSE
                
                
                
                
                
                Watfreeze = 100*(0-Temp_C)/(waterDens*(lvS_J_kg-lv_J_kg))
                FreezState(is) = Watfreeze
                Qm_freezState(is) = -waterDens*(Watfreeze/tstep_real/1000)*(lvS_J_kg-lv_J_kg)
             ENDIF

          ENDIF

          
          
          
          IF (is/=WaterSurf) THEN
             IF (SnowPack(is)>0.OR.(Precip>0.AND.Tsurf_ind(is)<0)) THEN
                snowCalcSwitch(is)=1
             ENDIF
          ELSE       
             IF (SnowPack(WaterSurf)>0.OR.FreezState(WaterSurf)>0) THEN
                snowCalcSwitch(WaterSurf)=1
             ENDIF
          ENDIF

          
          IF (Precip>0.AND.Tsurf_ind(is)<0.AND.SnowPack(is)>0) THEN
             SnowDens(is) = SnowDens(is)*SnowPack(is)/(SnowPack(is)+Precip)+SnowDensMin*Precip/(SnowPack(is)+Precip)
          ENDIF

          
          mwh = mwh + mw_ind(is)*sfr(is)*snowFrac(is)        
          fwh = fwh + FreezMelt(is)*sfr(is)*snowFrac(is)     
          Qm = Qm + Qm_melt(is)*sfr(is)*snowFrac(is)         
          QmRain = QmRain + Qm_rain(is)*sfr(is)*snowFrac(is) 
          QmFreez=QmFreez+deltaQi(is)*sfr(is)*snowFrac(is)+Qm_freezState(is)*sfr(is)*(1-snowFrac(is)) 
       ENDIF

    ENDDO 

    
    IF (Precip>0.AND.SUM(SnowPack)>0.AND.Temp_C<0) THEN

       SnowfallCum=SnowfallCum + Precip

       IF (SnowfallCum>PrecipLimitAlb) THEN

          SnowAlb=SnowAlbMax
          SnowfallCum=0
       ENDIF
    ELSE

       SnowfallCum=0
    ENDIF



  END SUBROUTINE MeltHeat


  
  
  SUBROUTINE SnowCalc(&
       id,& 
       tstep,imin,it,dectime,is,&
       ity,CRWmin,CRWmax,nsh_real,lvS_J_kg,lv_j_kg,avdens,&
       avRh,Press_hPa,Temp_C,RAsnow,psyc_hPa,avcp,sIce_hPa,&
       PervFraction,vegfraction,addimpervious,&
       numPM,s_hPa,ResistSurf,sp,ra,rb,tlv,snowdensmin,SnowProf,precip,&
       PipeCapacity,RunoffToWater,runoffAGimpervious,runoffAGveg,&
       addVeg,surplusWaterBody,SnowLimPaved,SnowLimBuild,FlowChange,drain,&
       WetThresh,stateOld,mw_ind,soilstorecap,rainonsnow,&
       freezmelt,freezstate,freezstatevol,&
       Qm_Melt,Qm_rain,Tsurf_ind,sfr,dayofWeek_id,surf,snowD,&
       AddWater,addwaterrunoff,&
       SnowPack,SurplusEvap,&
       snowFrac,MeltWaterStore,iceFrac,SnowDens,&
       runoffSnow,& 
       runoff,runoffSoil,chang,changSnow,SnowToSurf,state,ev_snow,soilmoist,&
       SnowDepth,SnowRemoval,swe,ev,chSnow_per_interval,&
       ev_per_tstep,qe_per_tstep,runoff_per_tstep,surf_chang_per_tstep,&
       runoffPipes,mwstore,runoffwaterbody)

    
    
    
    

    
    
    
    USE WaterDist_module,ONLY:updateFlood


    IMPLICIT NONE
    INTEGER,PARAMETER::nsurf=7
    INTEGER,PARAMETER::PavSurf   = 1  
    INTEGER,PARAMETER::BldgSurf  = 2  
    INTEGER,PARAMETER::ConifSurf = 3  
    
    
    INTEGER,PARAMETER::BSoilSurf = 6
    INTEGER,PARAMETER::WaterSurf = 7

    INTEGER,PARAMETER::snowfractionchoice=2 
    REAL(KIND(1d0)),PARAMETER::waterDens=999.8395 

    INTEGER,INTENT(in)::id
    
    INTEGER,INTENT(in)::tstep
    INTEGER,INTENT(in)::imin
    INTEGER,INTENT(in)::it
    INTEGER,INTENT(in)::is

    
    
    
    
    
    INTEGER,INTENT(in)::ity
    INTEGER,DIMENSION(3),INTENT(in)  ::DayofWeek_id

    REAL(KIND(1d0)),INTENT(in)::dectime
    REAL(KIND(1d0)),INTENT(in)::CRWmin
    REAL(KIND(1d0)),INTENT(in)::CRWmax
    REAL(KIND(1d0)),INTENT(in)::nsh_real
    REAL(KIND(1d0)),INTENT(in)::lvS_J_kg
    REAL(KIND(1d0)),INTENT(in)::lv_j_kg
    REAL(KIND(1d0)),INTENT(in)::avdens
    
    REAL(KIND(1d0)),INTENT(in)::avRh
    REAL(KIND(1d0)),INTENT(in)::Press_hPa
    REAL(KIND(1d0)),INTENT(in)::Temp_C
    REAL(KIND(1d0)),INTENT(in)::RAsnow
    REAL(KIND(1d0)),INTENT(in)::psyc_hPa
    REAL(KIND(1d0)),INTENT(in)::avcp
    REAL(KIND(1d0)),INTENT(in)::sIce_hPa
    REAL(KIND(1d0)),INTENT(in)::PervFraction
    REAL(KIND(1d0)),INTENT(in)::vegfraction
    REAL(KIND(1d0)),INTENT(in)::addimpervious
    REAL(KIND(1d0)),INTENT(in)::numPM
    REAL(KIND(1d0)),INTENT(in)::s_hPa
    REAL(KIND(1d0)),INTENT(in)::ResistSurf
    REAL(KIND(1d0)),INTENT(in)::sp
    REAL(KIND(1d0)),INTENT(in)::ra
    REAL(KIND(1d0)),INTENT(in)::rb
    REAL(KIND(1d0)),INTENT(in)::tlv
    REAL(KIND(1d0)),INTENT(in)::snowdensmin
    REAL(KIND(1d0)),INTENT(in)::precip
    REAL(KIND(1d0)),INTENT(in)::PipeCapacity
    REAL(KIND(1d0)),INTENT(in)::RunoffToWater
    REAL(KIND(1d0)),INTENT(in)::addVeg
    REAL(KIND(1d0)),INTENT(in)::SnowLimPaved
    REAL(KIND(1d0)),INTENT(in)::SnowLimBuild
    REAL(KIND(1d0)),INTENT(in)::FlowChange

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::drain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::WetThresh
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::stateOld
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::mw_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::soilstorecap
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::rainonsnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezmelt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezstate
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::freezstatevol
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Qm_Melt
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Qm_rain
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::Tsurf_ind
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::snowD
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::AddWater
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in)::addwaterrunoff
    REAL(KIND(1d0)),DIMENSION(6,nsurf),INTENT(in)::surf
    REAL(KIND(1d0)),DIMENSION(0:23,2),INTENT(in)::snowProf

    
    REAL(KIND(1d0)),INTENT(inout)::runoffAGveg
    REAL(KIND(1d0)),INTENT(inout)::runoffAGimpervious
    REAL(KIND(1d0)),INTENT(inout)::surplusWaterBody

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowPack
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::snowFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::MeltWaterStore
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::iceFrac
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(inout)::SnowDens

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoffSnow 
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoff
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::runoffSoil
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::chang
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::changSnow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::SnowToSurf
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::state
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::SnowDepth
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::ev_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out)::soilmoist
    REAL(KIND(1d0)),DIMENSION(2),INTENT(out)::SnowRemoval



    REAL(KIND(1d0)),INTENT(out)::swe
    REAL(KIND(1d0)),INTENT(out)::ev
    REAL(KIND(1d0)),INTENT(out)::chSnow_per_interval
    REAL(KIND(1d0)),INTENT(out)::ev_per_tstep
    REAL(KIND(1d0)),INTENT(out)::qe_per_tstep
    REAL(KIND(1d0)),INTENT(out)::runoff_per_tstep
    REAL(KIND(1d0)),INTENT(out)::surf_chang_per_tstep
    REAL(KIND(1d0)),INTENT(out)::runoffPipes
    REAL(KIND(1d0)),INTENT(out)::mwstore
    REAL(KIND(1d0)),INTENT(out)::runoffwaterbody


    REAL(KIND(1d0)),DIMENSION(2),INTENT(inout):: SurplusEvap


    REAL(KIND(1d0))::qe
    REAL(KIND(1d0))::rss

    
    REAL(KIND(1d0))::MeltExcess      
    REAL(KIND(1d0))::snowTotInit
    REAL(KIND(1d0))::EvPart
    REAL(KIND(1d0))::runoffTest
    REAL(KIND(1d0))::snowFracFresh1   
    REAL(KIND(1d0))::snowFracFresh2   
    REAL(KIND(1d0))::snowFracOld
    REAL(KIND(1d0))::WaterHoldCapFrac
    REAL(KIND(1d0))::FWC                
    

    INTEGER:: iu                        
    REAL(KIND(1d0)),PARAMETER :: IPThreshold_mmhr = 10   
    
    
    ev_per_tstep         = 0
    qe_per_tstep         = 0
    runoff_per_tstep     = 0
    surf_chang_per_tstep = 0

    
    iu=1     
    IF(DayofWeek_id(1)==1.OR.DayofWeek_id(1)==7) iu=2  

    
    runoffSnow(is)=0 
    runoff(is)=0
    runoffSoil(is)=0
    chang(is)=0
    changSnow(is)=0
    runoffTest=0
    SnowToSurf(is)=0
    EvPart=0
    ev=0
    snowFracFresh1=0
    snowFracFresh2=0
    snowFracOld=0

    
    snowTotInit=SnowPack(is)+MeltWaterStore(is)

    
    IF (SnowDens(is)>=200) THEN
       WaterHoldCapFrac=CRWmin
    ELSE
       WaterHoldCapFrac=CRWmin+(CRWmax-CRWmin)*(200-SnowDens(is))/200
    ENDIF

    
    
    

    IF (snowFrac(is)<1) CALL Evap_SUEWS(&

                                
         ity,&
         state(is),& 
         WetThresh(is),&
         surf(6,is),& 
         numPM,&
         s_hPa,&
         psyc_hPa,&
         ResistSurf,&
         sp,&
         ra,&
         rb,&
         tlv,&

                                
         rss,&
         ev,&
         qe) 

    IF (snowFrac(is)>0) THEN
       ev_snow(is) = Evap_SUEWS_Snow(Qm_Melt(is),Qm_rain(is),lvS_J_kg,avdens,avRh,Press_hPa,Temp_C,RAsnow,&
            psyc_hPa,tstep,avcp,sIce_hPa,dectime)
    ENDIF

    
    
    IF (is>2) THEN
       IF  (PervFraction/=0) THEN
          EvPart=(SurplusEvap(PavSurf)*sfr(PavSurf)+SurplusEvap(BldgSurf)*sfr(BldgSurf))/PervFraction
       ENDIF
    ENDIF


    
    
    IF (is==WaterSurf.AND.sfr(WaterSurf)>0) GO TO 606

    
    
    
    

    
    
    IF (SnowPack(is)>0.AND.snowFrac(is)==1) THEN

       ev_snow(is)=ev_snow(is)+EvPart 

       
       changSnow(is)=(Precip+freezMelt(is))-(mw_ind(is)+ev_snow(is)) 

       
       IF (rainOnSnow(is)>0) THEN
          changSnow(is)=changSnow(is)-Precip
          MeltWaterStore(is) = MeltWaterStore(is)+rainOnSnow(is)
       ENDIF

       SnowPack(is)=SnowPack(is)+changSnow(is)  

       
       IF (SnowPack(is)>0) THEN

          
          MeltWaterStore(is) = MeltWaterStore(is) + mw_ind(is) - freezMelt(is)

          
          FWC = WaterHoldCapFrac*SnowPack(is)

          
          IF (MeltWaterStore(is)>=FWC) THEN
             MeltExcess = 0                      
             MeltExcess = MeltWaterStore(is)-FWC 
             MeltWaterStore(is) = FWC            
             runoffSnow(is) = runoffSnow(is) + MeltExcess
          ENDIF

          
          IF (SnowProf(it,iu)==1.AND.is<3.AND.(imin==(nsh_real-1)/nsh_real*60))  &
               CALL snowRem(&
               is,PavSurf,BldgSurf,nsurf,&
               snowfrac,sfr,&
               SnowPack, SnowRemoval,&
               SnowLimPaved,SnowLimBuild)
          
       ELSEIF (SnowPack(is)<0) THEN

          
          MeltWaterStore(is)=MeltWaterStore(is)-freezMelt(is)+mw_ind(is)+SnowPack(is)
          SnowPack(is)=0.0   
          snowFracOld=1
          snowFrac(is)=0
          snowDens(is)=0

          IF (MeltWaterStore(is)<0) THEN 
             ev_snow(is)=ev_snow(is)+MeltWaterStore(is) 
             IF (ev_snow(is)<0) ev_snow(is)=0
             changSnow(is)=changSnow(is)+MeltWaterStore(is)
             MeltWaterStore(is)=0
          ELSE
             chang(is)=MeltWaterStore(is)  
             state(is)=state(is)+chang(is)
             MeltWaterStore(is)=0
          ENDIF
       ENDIF 


       
       
    ELSEIF (snowFrac(is)<1) THEN

       
       IF (SnowPack(is)>0) THEN
          ev_snow(is)=ev_snow(is)+EvPart 


          
          
          changSnow(is)=(Precip+freezMelt(is)+freezStateVol(is))-(mw_ind(is)+ev_snow(is)) 

          
          IF (rainOnSnow(is)>0) THEN
             changSnow(is)=changSnow(is)-Precip
             MeltWaterStore(is) = MeltWaterStore(is)+rainOnSnow(is)
          ENDIF
          SnowPack(is)=SnowPack(is)+changSnow(is)


          
          
          
          
          
          IF (Precip>0.AND.FreezState(is)==state(is)) THEN 
             snowFracFresh1=1
          ELSEIF (Precip==0.AND.FreezState(is)>0.AND.FreezState(is)==state(is)) THEN
             snowFracFresh1=1

             
             
          ELSEIF (FreezState(is)>0.AND.FreezState(is)<state(is)) THEN 
             snowFracFresh1=0.95 
             
             
             
             
          ENDIF

          
          
          
          
       ELSEIF (SnowPack(is)==0.AND.Tsurf_ind(is)<0) THEN

          
          
          
          IF ((Precip>0.AND.state(is)==0).OR.(Precip==0.AND.FreezState(is)==state(is)).OR.&
               (Precip>0.AND.FreezState(is)==state(is))) THEN

             
             changSnow(is)=Precip+FreezStateVol(is)
             SnowPack(is)=SnowPack(is)+changSnow(is)  

             snowFracFresh1=1
             iceFrac(is)=FreezState(is)/(FreezState(is)+Precip)
             SnowDens(is)=SnowDensMin
          ENDIF

          IF (FreezState(is)>0.AND.FreezState(is)<state(is)) THEN

             changSnow(is)=Precip+freezStateVol(is)
             SnowPack(is)=SnowPack(is)+changSnow(is)  
             snowFracFresh2=0.95 

             
             
             iceFrac(is)=1
             SnowDens(is)=SnowDensMin
             
             

          ENDIF
       ENDIF

       
       IF (SnowPack(is)>0) THEN

          
          MeltWaterStore(is) = MeltWaterStore(is) + mw_ind(is) - freezMelt(is)

          
          FWC = WaterHoldCapFrac*SnowPack(is)

          
          IF (MeltWaterStore(is)>=FWC) THEN
             MeltExcess = 0                      
             MeltExcess = MeltWaterStore(is)-FWC 
             MeltWaterStore(is) = FWC            

             
             
             
             IF ((snowFrac(is)>0.9.AND.is/=BldgSurf).OR.(is==BldgSurf)) THEN
                runoffSnow(is) = runoffSnow(is) + MeltExcess
             ELSE
                SnowToSurf(is) = SnowToSurf(is) + MeltExcess*snowFrac(is)/(1-snowFrac(is))
             ENDIF
          ENDIF

          
          IF (SnowProf(it,iu)==1.AND.is<3.AND.(imin==(nsh_real-1)/nsh_real*60))  &
               CALL snowRem(&
               is,PavSurf,BldgSurf,nsurf,&
               snowfrac,sfr,&
               SnowPack, SnowRemoval,&
               SnowLimPaved,SnowLimBuild)

          
       ELSEIF (SnowPack(is)<0) THEN

          
          MeltWaterStore(is)=MeltWaterStore(is)-freezMelt(is)+mw_ind(is)+SnowPack(is)

          SnowPack(is)=0.0   
          snowFracFresh1=0
          snowFracFresh2=0
          snowDens(is)=0

          IF (MeltWaterStore(is)<0) THEN 
             ev_snow(is)=ev_snow(is)+MeltWaterStore(is) 
             IF (ev_snow(is)<0) ev_snow(is)=0
             changSnow(is)=changSnow(is)+MeltWaterStore(is)
             MeltWaterStore(is)=0
          ELSE
             SnowToSurf(is)=SnowToSurf(is)+MeltWaterStore(is)*snowFrac(is)/(1-snowFrac(is))
             MeltWaterStore(is)=0
          ENDIF
       ENDIF 


       
       
       IF ((is==PavSurf.OR.is==BldgSurf).AND.snowFrac(is)<1) THEN  

          
          
          IF (precip>IPThreshold_mmhr/nsh_real) THEN
             
             runoff(is)=runoff(is)+(Precip+SnowToSurf(is)+AddWater(is)-IPThreshold_mmhr/nsh_real)
             chang(is)=IPThreshold_mmhr/nsh_real-(drain(is)+ev+freezState(is))
          ELSE
             
             chang(is)=Precip+SnowToSurf(is)+AddWater(is)-(drain(is)+ev+freezState(is))
          ENDIF

          state(is)=state(is)+chang(is) 

          
          
          IF (is==PavSurf.AND.sfr(PavSurf)>0) state(is)=state(is)+(addImpervious)/sfr(PavSurf)

          runoff(is)=runoff(is)+drain(is)*AddWaterRunoff(is) 

          IF(state(is)<0.0) THEN  
             SurplusEvap(is)=ABS(state(is)) 
             ev = ev-SurplusEvap(is)
             state(is)=0.0
          ENDIF

       ELSEIF(is>=3.AND.snowFrac(is)<1) THEN 

          ev=ev+EvPart

          
          IF ( VegFraction>0 ) THEN
             IF (Precip+addVeg*(sfr(is)/VegFraction)>(IPThreshold_mmhr/nsh_real)) THEN 
                runoff(is)=runoff(is)+(Precip+addVeg*(sfr(is)/VegFraction)+SnowToSurf(is)+AddWater(is)-(IPThreshold_mmhr/nsh_real))
                chang(is)=(IPThreshold_mmhr/nsh_real)-(drain(is)+ev+freezState(is))
             ELSE
                chang(is)=Precip+addVeg*(sfr(is)/VegFraction)+SnowToSurf(is)+AddWater(is)-(drain(is)+ev+freezState(is))
             ENDIF
          ELSE
             chang(is)=Precip+SnowToSurf(is)+AddWater(is)-(drain(is)+ev+freezState(is))
          END IF


          state(is)=state(is)+chang(is)

          
          IF (Temp_C>0) THEN
             soilmoist(is)=soilmoist(is)+Drain(is)*AddWaterRunoff(is)*(1-snowFrac(is))
          ELSE
             runoff(is)=runoff(is)+Drain(is)*AddWaterRunoff(is)
          ENDIF

          
          IF(state(is)<0.0) THEN

             IF ((soilmoist(is)+state(is))>=0.AND.Temp_C>0) THEN 

                soilmoist(is)=soilmoist(is)+state(is)*(1-snowFrac(is))
                state(is)=0.0

             ELSE 
                chang(is)=chang(is)+state(is)
                ev=ev+state(is)
                state(is)=0.0
             ENDIF
          ENDIF 

          
          IF (soilmoist(is)>soilstoreCap(is)) THEN
             runoffTest=runoffTest+(soilmoist(is)-soilstoreCap(is))
             soilmoist(is)=soilstoreCap(is)
          ELSEIF (soilmoist(is)<0) THEN
             soilmoist(is)=0
          ENDIF

       ENDIF 

    ENDIF 

    

    
    
    IF (snowFracFresh2>0) THEN
       surf_chang_per_tstep=surf_chang_per_tstep+(state(is)-stateOld(is))*sfr(is)*(1-snowFrac(is))&
            -Precip*sfr(is)*(1-snowFracFresh2)
       chSnow_per_interval=chSnow_per_interval+((SnowPack(is)+MeltWaterstore(is))-snowTotInit)*sfr(is)*(1-snowFrac(is))&
            -Precip*sfr(is)*snowFracFresh2
    ELSE
       surf_chang_per_tstep=surf_chang_per_tstep+(state(is)-stateOld(is))*sfr(is)*(1-snowFrac(is))
       chSnow_per_interval=chSnow_per_interval+((SnowPack(is)+MeltWaterstore(is))-snowTotInit)*sfr(is)*MAX(snowFrac(is),snowfracOld)
    ENDIF

    
    IF (is==BldgSurf.OR.is==PavSurf) THEN
       ev_per_tstep=ev_per_tstep+ev*sfr(is)*(1-snowFrac(is))+ev_snow(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)
       qe_per_tstep=qe_per_tstep+ev_snow(is)*lvS_J_kg*sfr(is)*snowFrac(is)&
            +ev*lv_J_kg*sfr(is)*(1-snowFrac(is))
    ELSE
       ev_per_tstep=ev_per_tstep+ev*sfr(is)*(1-snowFrac(is))+ev_snow(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)
       qe_per_tstep=qe_per_tstep+ev_snow(is)*lvS_J_kg*sfr(is)*MAX(snowFrac(is),snowfracOld)+ev*lv_J_kg*sfr(is)*(1-snowFrac(is))
    ENDIF

    

    
    runoffPipes=runoffPipes+runoffSnow(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)+runoff(is)*sfr(is)*(1-snowFrac(is))&
         +runoffTest*sfr(is)
    CALL updateFlood(&
                                
         nsurf,is,PavSurf,BldgSurf,WaterSurf,ConifSurf,BSoilSurf,&
         sfr,PipeCapacity,RunoffToWater,&
                                
         runoffAGimpervious,surplusWaterBody,runoffAGveg,runoffPipes)

    runoff_per_tstep=runoff_per_tstep+runoffSnow(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)+runoff(is)*sfr(is)*(1-snowFrac(is))&
         +runoffTest*sfr(is)

    
    IF (SnowDens(is)/=0) THEN
       SnowDepth(is) = SnowPack(is)*waterDens/SnowDens(is)
    ENDIF

    
    swe = swe + SnowPack(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)
    MwStore = MwStore + MeltWaterStore(is)*sfr(is)*MAX(snowFrac(is),snowfracOld)

    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    IF (snowFracFresh1>0) snowFrac(is)=snowFracFresh1
    IF (snowFracFresh2>0) snowFrac(is)=snowFracFresh2

    
    
    
    
    
    
    IF (SnowFractionChoice==2) THEN
       IF (SnowPack(is)>0.AND.mw_ind(is)>0) THEN
          snowFrac(is) = SnowDepletionCurve(is,SnowPack(is),snowD(is))
          IF (snowFrac(is)<0.001) snowFrac(is)=0.001  
       ELSEIF (SnowPack(is)==0) THEN
          snowFrac(is)=0
       ENDIF
    ENDIF

    RETURN

    
    
    
606 changSnow(WaterSurf)=(Precip+freezMelt(WaterSurf)+freezState(WaterSurf))-&
         (mw_ind(WaterSurf)+ev_snow(WaterSurf))

    SnowPack(WaterSurf)=SnowPack(WaterSurf)+changSnow(WaterSurf) 
    state(WaterSurf)=state(WaterSurf)+FlowChange-freezState(WaterSurf)  

    
    IF (SnowPack(WaterSurf)>0) THEN

       
       MeltWaterStore(WaterSurf)=MeltWaterStore(WaterSurf)+mw_ind(WaterSurf)-freezMelt(WaterSurf)

       
       FWC = WaterHoldCapFrac*SnowPack(WaterSurf)

       
       IF (MeltWaterStore(WaterSurf)>=FWC.AND.Temp_C>=0) THEN
          state(WaterSurf)=state(WaterSurf)+(MeltWaterStore(WaterSurf)-FWC)
          MeltWaterStore(WaterSurf) = FWC
       ENDIF

       
    ELSEIF (SnowPack(is)<0) THEN

       
       
       MeltWaterStore(WaterSurf) = MeltWaterStore(WaterSurf)-freezMelt(WaterSurf) &
            + mw_ind(WaterSurf)

       state(WaterSurf)=state(WaterSurf)+MeltWaterStore(WaterSurf)+SnowPack(WaterSurf) 
       SnowPack(WaterSurf)=0
       IF (state(WaterSurf)<0) ev_snow(WaterSurf)=ev_snow(WaterSurf)+state(WaterSurf)

    ENDIF 

    
    IF (state(WaterSurf)>Surf(5,WaterSurf)) THEN
       runoff(WaterSurf)=runoff(WaterSurf)+(state(WaterSurf)-Surf(5,WaterSurf))
       state(WaterSurf)=Surf(5,WaterSurf)
       runoffWaterBody=runoffWaterBody+runoff(WaterSurf)*sfr(WaterSurf)
    ELSE
       state(WaterSurf)=state(WaterSurf)+surplusWaterBody

       IF (state(WaterSurf)>Surf(5,WaterSurf)) THEN
          runoffWaterBody=runoffWaterBody+(state(WaterSurf)-Surf(5,WaterSurf))*sfr(WaterSurf)
          state(WaterSurf)=Surf(5,WaterSurf)
       ENDIF
    ENDIF


    
    chSnow_per_interval=chSnow_per_interval+((SnowPack(WaterSurf)+MeltWaterstore(WaterSurf))-snowTotInit)*sfr(WaterSurf)
    
    surf_chang_per_tstep=surf_chang_per_tstep+(state(WaterSurf)-stateOld(WaterSurf))*sfr(WaterSurf)

    
    ev_per_tstep=ev_per_tstep+ev*sfr(WaterSurf)+ev_snow(WaterSurf)*sfr(WaterSurf)
    qe_per_tstep=qe_per_tstep+ev_snow(WaterSurf)*lvS_J_kg*sfr(WaterSurf)+ev*lv_J_kg*sfr(WaterSurf)
    runoff_per_tstep=runoff_per_tstep+(runoff(is)*sfr(is)) 

    IF (SnowPack(WaterSurf)>0) THEN     
       snowFrac(WaterSurf)=1
    ELSE
       snowFrac(WaterSurf)=0
    ENDIF

  END SUBROUTINE SnowCalc


  
  
  

  FUNCTION Evap_SUEWS_Snow(Qm,QP,lvS_J_kg,avdens,avRh,Press_hPa,Temp_C,RAsnow,psyc_hPa,&
       tstep,avcp,sIce_hPa,dectime) RESULT(ev_snow)

    USE AtmMoist_module,ONLY:sat_vap_pressice
    IMPLICIT NONE

    
    REAL (KIND(1d0))::Qm,QP,&        
         lvS_J_kg,avdens,avRh,&   
         Press_hPa,Temp_C,&       
         RAsnow,psyc_hPa,&        
         avcp,sIce_hPa,&            
         dectime

    
    REAL (KIND(1d0))::e_snow,&     
         sae_snow,&   
         qe_snow,&    
         ev_snow,&    
         vdrcIce,&    
         esIce_hPa,&  
         EaIce_hPa,&  
         tlv_sub,&    
         tstep_real   

    

    INTEGER:: tstep,from=1
    

    tstep_real = REAL(tstep,KIND(1d0))

    sae_snow=sIce_hPa*(Qp-Qm)   

    esIce_hPa= sat_vap_pressIce(Temp_C,Press_hPa,from,dectime) 
    EaIce_hPa=avRh/100*esIce_hPa                       
    vdrcIce=(esIce_hPa-eaIce_hpa)*avdens*avcp          
    tlv_sub=lvS_J_kg/tstep_real                        
    e_snow=sae_snow+vdrcIce/RAsnow                     
    qe_snow=e_snow/(sIce_hPa+psyc_hPa)                 
    ev_snow=qe_snow/tlv_sub                            

    RETURN

  END FUNCTION Evap_SUEWS_Snow

  
  
  
  SUBROUTINE snowRem(&
       is,PavSurf,BldgSurf,nsurf,&
       snowfrac,sfr,&
       SnowPack, SnowRemoval,&
       SnowLimPaved,SnowLimBuild)

    IMPLICIT NONE
    INTEGER,INTENT(in)                          :: is,PavSurf,BldgSurf,nsurf
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) :: snowfrac,sfr
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out):: SnowPack, SnowRemoval
    REAL(KIND(1d0)),INTENT(in)                  :: SnowLimPaved,SnowLimBuild
    

    IF (is==PavSurf) THEN
       IF (SnowPack(PavSurf)>SnowLimPaved) THEN
          SnowRemoval(PavSurf) = (SnowPack(PavSurf)-SnowLimPaved)*sfr(PavSurf)*snowfrac(PavSurf)
          SnowPack(PavSurf)=SnowLimPaved
          
       ENDIF
    ENDIF
    IF (is==BldgSurf)THEN
       IF (SnowPack(BldgSurf)>SnowLimBuild) THEN
          SnowRemoval(2) = (SnowPack(BldgSurf)-SnowLimBuild)*sfr(BldgSurf)*snowfrac(BldgSurf)
          SnowPack(BldgSurf)=SnowLimBuild
          
       ENDIF
    ENDIF
    
    
  END SUBROUTINE snowRem

  
  
  FUNCTION   SnowDepletionCurve(is,swe,sweD) RESULT(asc)
    
    
    
    
    

    USE allocateArray

    IMPLICIT  NONE

    INTEGER::is
    REAL (KIND(1d0))::asc,sweD,swe


    
    IF (is==PavSurf) THEN

       IF (swe<=sweD) THEN      
          asc=((swe/sweD))**2
       ELSE
          asc=1
       ENDIF

       
    ELSEIF (is==BldgSurf) THEN

       IF (swe<=sweD) THEN
          IF ((swe/sweD)<0.9) THEN
             asc=(swe/sweD)*0.5
          ELSE
             asc=(swe/sweD)**8
          ENDIF
       ELSE
          asc=1
       ENDIF
    ELSEIF (is==WaterSurf) THEN
       IF (swe>0) asc=1

       
    ELSE
       IF (swe<=sweD) THEN

          asc=1-((1/3.1416)*ACOS(2*(swe/sweD)-1))**1.7
       ELSE
          asc=1
       ENDIF

    ENDIF

    

    RETURN
  END FUNCTION SnowDepletionCurve


  SUBROUTINE veg_fr_snow(&
       sfr,snowFrac,nsurf,&
       veg_fr)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: nsurf 

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) :: sfr      
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) :: snowFrac 

    REAL(KIND(1d0)),INTENT(out) :: veg_fr   

    veg_fr = DOT_PRODUCT(sfr(3:7),1-snowFrac(3:7))

  END SUBROUTINE veg_fr_snow


END MODULE Snow_module
