
















MODULE allocateArray

  IMPLICIT NONE

  

  INTEGER, PARAMETER:: MaxNumberOfGrids=10000   

  INTEGER, PARAMETER:: MaxLinesMet=8640        

  
  INTEGER, PARAMETER:: ncolumnsSiteSelect=101       
  INTEGER, PARAMETER:: ncolumnsNonVeg=24            
  INTEGER, PARAMETER:: ncolumnsVeg=38               
  INTEGER, PARAMETER:: ncolumnsWater=22             
  INTEGER, PARAMETER:: ncolumnsSnow=25              
  INTEGER, PARAMETER:: ncolumnsSoil=9               
  INTEGER, PARAMETER:: ncolumnsConductance=13       
  INTEGER, PARAMETER:: ncolumnsOHMCoefficients=4    
  INTEGER, PARAMETER:: ncolumnsESTMCoefficients=52  
  INTEGER, PARAMETER:: ncolumnsAnthropogenic=34     
  INTEGER, PARAMETER:: ncolumnsIrrigation=25        
  INTEGER, PARAMETER:: ncolumnsProfiles=25          
  INTEGER, PARAMETER:: ncolumnsWGWaterDist=10       
  INTEGER, PARAMETER:: ncolumnsBiogen=9             
  INTEGER, PARAMETER:: ncolumnsMetForcingData=24    
  INTEGER, PARAMETER:: ncolsESTMdata=13             


  
  INTEGER, PARAMETER:: ncolumnsDataOutSUEWS=84,&    
       ncolumnsDataOutSnow=102,&
       ncolumnsdataOutSOL=31,&
       ncolumnsdataOutBL=22,&
       ncolumnsDataOutESTM=32,&
       ncolumnsDataOutDailyState=46

  
  CHARACTER(len=20),DIMENSION(ncolumnsSiteSelect)::        HeaderSiteSelect_File          
  CHARACTER(len=20),DIMENSION(ncolumnsNonVeg)::            HeaderNonVeg_File              
  CHARACTER(len=20),DIMENSION(ncolumnsNonVeg)::            HeaderNonVeg_Reqd              
  CHARACTER(len=20),DIMENSION(ncolumnsVeg)::               HeaderVeg_File                 
  CHARACTER(len=20),DIMENSION(ncolumnsVeg)::               HeaderVeg_Reqd                 
  CHARACTER(len=20),DIMENSION(ncolumnsWater)::             HeaderWater_File               
  CHARACTER(len=20),DIMENSION(ncolumnsWater)::             HeaderWater_Reqd               
  CHARACTER(len=20),DIMENSION(ncolumnsSnow)::              HeaderSnow_File                
  CHARACTER(len=20),DIMENSION(ncolumnsSnow)::              HeaderSnow_Reqd                
  CHARACTER(len=20),DIMENSION(ncolumnsSoil)::              HeaderSoil_File                
  CHARACTER(len=20),DIMENSION(ncolumnsSoil)::              HeaderSoil_Reqd                
  CHARACTER(len=20),DIMENSION(ncolumnsConductance)::       HeaderCond_File                
  CHARACTER(len=20),DIMENSION(ncolumnsConductance)::       HeaderCond_Reqd                
  CHARACTER(len=20),DIMENSION(ncolumnsOHMCoefficients)::   HeaderOHMCoefficients_File     
  CHARACTER(len=20),DIMENSION(ncolumnsOHMCoefficients)::   HeaderOHMCoefficients_Reqd     
  CHARACTER(len=20),DIMENSION(ncolumnsESTMCoefficients)::  HeaderESTMCoefficients_File    
  CHARACTER(len=20),DIMENSION(ncolumnsESTMCoefficients)::  HeaderESTMCoefficients_Reqd    
  CHARACTER(len=20),DIMENSION(ncolumnsAnthropogenic)::     HeaderAnthropogenic_File       
  CHARACTER(len=20),DIMENSION(ncolumnsAnthropogenic)::     HeaderAnthropogenic_Reqd       
  CHARACTER(len=20),DIMENSION(ncolumnsIrrigation)::        HeaderIrrigation_File          
  CHARACTER(len=20),DIMENSION(ncolumnsIrrigation)::        HeaderIrrigation_Reqd          
  CHARACTER(len=20),DIMENSION(ncolumnsProfiles)::          HeaderProfiles_File            
  CHARACTER(len=20),DIMENSION(ncolumnsProfiles)::          HeaderProfiles_Reqd            
  CHARACTER(len=20),DIMENSION(ncolumnsWGWaterDist)::       HeaderWGWaterDist_File         
  CHARACTER(len=20),DIMENSION(ncolumnsWGWaterDist)::       HeaderWGWaterDist_Reqd         
  CHARACTER(len=20),DIMENSION(ncolumnsBiogen)::            HeaderBiogen_File              
  CHARACTER(len=20),DIMENSION(ncolumnsBiogen)::            HeaderBiogen_Reqd              

  
  INTEGER,DIMENSION(:),ALLOCATABLE:: UseColumnsDataOut       
  
  CHARACTER(len=14*ncolumnsDataOutSUEWS):: HeaderUse,FormatUse,HeaderUseNoSep,FormatUseNoSep    
  CHARACTER(len=52*ncolumnsDataOutSUEWS):: LongNmUse
  CHARACTER(len=14*ncolumnsDataOutSUEWS):: UnitsUse
  CHARACTER(len=3*ncolumnsDataOutSUEWS):: AggregUse
  CHARACTER(len=4*ncolumnsDataOutSUEWS):: ColNosUse

  
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::SiteSelect                
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::NonVeg_Coeff              
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Veg_Coeff                 
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Water_Coeff               
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Snow_Coeff                
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Soil_Coeff                
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Conductance_Coeff         
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::OHMCoefficients_Coeff     
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::ESTMCoefficients_Coeff    
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Anthropogenic_Coeff       
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Irrigation_Coeff          
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Profiles_Coeff            
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::WGWaterDist_Coeff         
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::Biogen_Coeff              

  
  INTEGER,DIMENSION(:), ALLOCATABLE:: GridIDmatrix         
  INTEGER,DIMENSION(:), ALLOCATABLE:: GridIDmatrix0        
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: SurfaceChar          
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: MetForcingData      
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE  :: MetForcingData_grid 
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: ESTMForcingData      
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: ModelDailyState      
  REAL(KIND(1d0)),DIMENSION(:),    ALLOCATABLE:: DailyStateFirstOpen
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: ModelOutputData      
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutSUEWS              
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutBL            
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutSOL           
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutSnow          
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutESTM          
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: dataOutDailyState    

  REAL(KIND(1D0)),DIMENSION(5)::datetimeLine
  REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSUEWS-5)::dataOutLineSUEWS
  REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutSnow-5)::dataOutLineSnow
  REAL(KIND(1D0)),DIMENSION(ncolumnsDataOutDailyState-5)::DailyStateLine

  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: MetForDisagg           
  REAL(KIND(1d0)),DIMENSION(:),  ALLOCATABLE:: MetForDisaggPrev,MetForDisaggNext 

  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: ESTMForDisagg           
  REAL(KIND(1d0)),DIMENSION(:),  ALLOCATABLE:: ESTMForDisaggPrev,ESTMForDisaggNext 

  
  REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE:: TstepProfiles
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: AHProf_tstep
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: WUProfM_tstep, WUProfA_tstep
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: HumActivity_tstep
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: TraffProf_tstep
  REAL(KIND(1d0)),DIMENSION(:,:),  ALLOCATABLE:: PopProf_tstep

  
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:,:):: Ts5mindata   
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:)  :: ts5mindata_ir 
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:)  :: Tair24HR
  REAL(KIND(1d0)),DIMENSION(27)  :: dataOutLineESTM 

  
  INTEGER:: cTP_EnUseWD  = 1,&
       cTP_EnUseWE       = 2,&
       cTP_WUManuWD      = 3,&
       cTP_WUManuWE      = 4,&
       cTP_WUAutoWD      = 5,&
       cTP_WUAutoWE      = 6,&
       cTP_SnowCWD       = 7,&
       cTP_SnowCWE       = 8,&
       cTP_HumActivityWD = 9,&
       cTP_HumActivityWE = 10,&
       cTP_TraffProfWD   = 11,&
       cTP_TraffProfWE   = 12,&
       cTP_PopProfWD     = 13,&
       cTP_PopProfWE     = 14
  

  
  INTEGER, PARAMETER:: nsurf=7                
  INTEGER, PARAMETER:: NVegSurf=3             
  INTEGER, PARAMETER:: nsurfIncSnow=nsurf+1   

  INTEGER,PARAMETER:: PavSurf   = 1,&   
       BldgSurf  = 2,&
       ConifSurf = 3,&
       DecidSurf = 4,&
       GrassSurf = 5,&   
       BSoilSurf = 6,&   
       WaterSurf = 7,&
       ExcessSurf= 8,&   
       NSurfDoNotReceiveDrainage=0,&   
       ivConif = 1,&     
       ivDecid = 2,&
       ivGrass = 3

  REAL(KIND(1d0)),DIMENSION(nsurf):: sfr   

  
  
  REAL(KIND(1d0)),DIMENSION(nsurf):: AddWater       
  REAL(KIND(1d0)),DIMENSION(nsurf):: AddWaterRunoff 
  
  REAL(KIND(1d0)),DIMENSION(nsurf):: chang          
  REAL(KIND(1d0)),DIMENSION(nsurf):: drain          
  REAL(KIND(1d0)),DIMENSION(nsurf):: evap           
  REAL(KIND(1d0)),DIMENSION(nsurf):: runoff         
  REAL(KIND(1d0)),DIMENSION(nsurf):: runoffSoil     
  REAL(KIND(1d0)),DIMENSION(nsurf):: smd_nsurf      
  REAL(KIND(1d0)),DIMENSION(nsurf):: smd_nsurfOut   
  REAL(KIND(1d0)),DIMENSION(nsurf):: soilmoist      
  REAL(KIND(1d0)),DIMENSION(nsurf):: soilmoistOld   
  REAL(KIND(1d0)),DIMENSION(nsurf):: state          
  REAL(KIND(1d0)),DIMENSION(nsurf):: stateOut       
  REAL(KIND(1d0)),DIMENSION(nsurf):: stateOld       
  REAL(KIND(1D0)),DIMENSION(nsurf):: rss_nsurf      

  REAL(KIND(1d0)),DIMENSION(nsurf):: WetThresh      
  REAL(KIND(1d0)),DIMENSION(nsurf):: StateLimit     

  REAL(KIND(1d0)),DIMENSION(1)::     WaterDepth     

  
  REAL(KIND(1d0)),DIMENSION(nsurf):: SatHydraulicConduct 
  REAL(KIND(1d0)),DIMENSION(nsurf):: SoilDepth           
  REAL(KIND(1d0)),DIMENSION(nsurf):: SoilStoreCap        

  
  REAL(KIND(1d0)),DIMENSION(nsurf+1,nsurf-1)::WaterDist 

  
  REAL(KIND(1d0)),DIMENSION(6,nsurf):: surf   
  
  
  
  
  
  
  

  
  INTEGER, PARAMETER:: ndays = 366   
  
  REAL(KIND(1d0)),DIMENSION( 0:ndays, 5):: GDD          
  REAL(KIND(1d0)),DIMENSION(-4:ndays, 6):: HDD          
  REAL(KIND(1d0)),DIMENSION( 0:ndays, 9):: WU_Day       
  REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf):: LAI   

  
  REAL(KIND(1d0)),DIMENSION( 0:ndays):: DecidCap   
  REAL(KIND(1d0)),DIMENSION( 0:ndays):: porosity   

  REAL(KIND(1d0)),DIMENSION( 0:ndays):: albDecTr     
  REAL(KIND(1d0)),DIMENSION( 0:ndays):: albEveTr     
  REAL(KIND(1d0)),DIMENSION( 0:ndays):: albGrass   

  REAL(KIND(1d0)):: AlbMin_DecTr,&   
       AlbMax_DecTr,&   
       CapMin_dec,&   
       CapMax_dec,&   
       PorMin_dec,&  
       PorMax_dec,&    
       AlbMin_EveTr,&   
       AlbMax_EveTr,&   
       AlbMin_Grass,&   
       AlbMax_Grass     

  
  
  REAL(KIND(1d0)),DIMENSION( 0:ndays, 5,MaxNumberOfGrids):: GDD_grids
  REAL(KIND(1d0)),DIMENSION(-4:ndays, 6,MaxNumberOfGrids):: HDD_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays, 9,MaxNumberOfGrids):: WU_Day_grids
  REAL(KIND(1d0)),DIMENSION(-4:ndays, nvegsurf,MaxNumberOfGrids):: LAI_grids

  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albDecTr_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: DecidCap_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: porosity_grids

  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albEveTr_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: albGrass_grids

  
  
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: Bo_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: mAH_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a1AnOHM_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a2AnOHM_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids):: a3AnOHM_grids
  REAL(KIND(1d0)),DIMENSION( 0:ndays,MaxNumberOfGrids,nsurf,3):: a123AnOHM_gs
  REAL(KIND(1d0)):: xBo 
  
  
  


  
  INTEGER,DIMENSION(0:ndays,3)::DayofWeek   
  

  
  
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: BaseT            
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: BaseTe           
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: GDDFull          
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: SDDFull          
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: LaiMin           
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: LaiMax           
  REAL(KIND(1d0)),DIMENSION(nvegsurf):: MaxConductance   
  REAL(KIND(1d0)),DIMENSION(4,nvegsurf):: LaiPower       
  
  INTEGER,DIMENSION(nvegsurf):: LAIType                  
  

  REAL(KIND(1d0)),DIMENSION(nvegsurf):: BiogenCO2Code,&    
       alpha_bioCO2,&
       beta_bioCO2,&
       theta_bioCO2,&
       alpha_enh_bioCO2,&
       beta_enh_bioCO2,&
       resp_a,&
       resp_b,&
       min_res_bioCO2

  
  
  
  
  

  
  REAL(KIND(1d0)),DIMENSION(nsurf):: alb    
  REAL(KIND(1d0)),DIMENSION(nsurf):: emis   

  REAL(KIND(1d0)):: bulkalbedo 

  
  REAL(KIND(1d0)),DIMENSION(nsurf):: Tsurf_ind,&        
       Tsurf_ind_snow,&   
       Tsurf_ind_nosnow
  REAL(KIND(1d0)),DIMENSION(nsurf):: kup_ind,&          
       kup_ind_snow,&     
       kup_ind_nosnow
  REAL(KIND(1d0)),DIMENSION(nsurf):: lup_ind,&          
       lup_ind_snow,&     
       lup_ind_nosnow
  REAL(KIND(1d0)),DIMENSION(nsurf):: qn1_ind,&          
       qn1_ind_snow,&     
       qn1_ind_nosnow

  
  REAL(KIND(1d0))             :: NARP_LAT,NARP_LONG,NARP_YEAR,NARP_TZ,&
       NARP_ALB_SNOW,NARP_EMIS_SNOW,NARP_TRANS_SITE
  REAL(KIND(1D0))             :: NARP_G(365)   
  INTEGER                     :: NARP_NPERHOUR
  REAL(KIND(1D0)),ALLOCATABLE :: NARP_KDOWN_HR(:)
  
  REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292,&
       RAD2DEG=57.29577951,&
       SIGMA_SB=5.67E-8
  

  
  REAL(KIND(1d0)),DIMENSION(nsurf+1,4,3):: OHM_coef   
  REAL(KIND(1d0)),DIMENSION(nsurf+1)::     OHM_threshSW, OHM_threshWD   
  REAL(KIND(1d0)):: a1,a2,a3   
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: qn1_store, qn1_S_store   
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE:: qn1_av_store, qn1_S_av_store  
  

  
  REAL(KIND(1d0)),DIMENSION(nsurf):: changSnow,&       
       maxSnowVol,&      
       MeltWaterStore,&  
       ev_snow,&          
       mw_ind,&           
       mw_indDay,&        
       runoffSnow,&       
       SnowDens,&        
       SnowFrac,&         
       iceFrac,&
       snowInit,&
       snowDepth,&       
       SnowToSurf,&      
       volSWE,&
       StateFraction,&   
       freezMelt,&       
       Qm_freezState,&   
       freezState,&      
       FreezStateVol,&
       Qm_melt,&         
       Qm_rain,&         
       rainOnSnow,&      
       snowD,&
       deltaQi

  REAL(KIND(1d0)),DIMENSION(nsurf):: snowPack,&        
       snowPackOld
  INTEGER,DIMENSION(nsurf):: heiG,&                    
       snowCoverForms,&
       snowCalcSwitch=0          
  

  
  
  
  INTEGER,PARAMETER:: nconns = 8   
  REAL(KIND(1d0)),DIMENSION(nconns):: GridToFrac   
  REAL(KIND(1d0)),DIMENSION(nconns):: GridTo       
  
  
  

  
  REAL(KIND(1d0)),DIMENSION(MaxNumberOfGrids) :: a1AnOHM,a2AnOHM,a3AnOHM   
  REAL(KIND(1d0)),DIMENSION(MaxNumberOfGrids) :: mAHAnOHM           
  REAL(KIND(1d0)),DIMENSION(MaxNumberOfGrids) :: BoAnOHMStart  
  REAL(KIND(1d0)),DIMENSION(MaxNumberOfGrids) :: BoAnOHMEnd  
  REAL(KIND(1d0)),DIMENSION(nsurf):: cpAnOHM      
  REAL(KIND(1d0)),DIMENSION(nsurf):: kkAnOHM       
  REAL(KIND(1d0)),DIMENSION(nsurf):: chAnOHM      
  

  
  REAL(KIND(1d0)),DIMENSION(5,nsurfIncSnow):: zSurf_SUEWSsurfs, &
       kSurf_SUEWSsurfs, &
       rSurf_SUEWSsurfs
  
  

  
  
  INTEGER:: cc    
  INTEGER,PARAMETER:: ccEndSI=ncolumnsSiteSelect

  
  INTEGER,DIMENSION(nsurf):: c_AlbMin     = (/(cc, cc=ccEndSI+ 0*nsurf+1,ccEndSI+ 0*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_AlbMax     = (/(cc, cc=ccEndSI+ 1*nsurf+1,ccEndSI+ 1*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_Emis       = (/(cc, cc=ccEndSI+ 2*nsurf+1,ccEndSI+ 2*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_StorMin    = (/(cc, cc=ccEndSI+ 3*nsurf+1,ccEndSI+ 3*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_StorMax    = (/(cc, cc=ccEndSI+ 4*nsurf+1,ccEndSI+ 4*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_WetThresh  = (/(cc, cc=ccEndSI+ 5*nsurf+1,ccEndSI+ 5*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_StateLimit = (/(cc, cc=ccEndSI+ 6*nsurf+1,ccEndSI+ 6*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_DrEq       = (/(cc, cc=ccEndSI+ 7*nsurf+1,ccEndSI+ 7*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_DrCoef1    = (/(cc, cc=ccEndSI+ 8*nsurf+1,ccEndSI+ 8*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_DrCoef2    = (/(cc, cc=ccEndSI+ 9*nsurf+1,ccEndSI+ 9*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_SoilTCode  = (/(cc, cc=ccEndSI+10*nsurf+1,ccEndSI+10*nsurf+nsurf, 1)/)  

  
  INTEGER,DIMENSION(nsurf):: c_SnowLimPat =(/(cc, cc=ccEndSI+11*nsurf+1,ccEndSI+11*nsurf+nsurf, 1)/) 
  
  INTEGER,DIMENSION(nsurf):: c_SnowLimRem =(/(cc, cc=ccEndSI+12*nsurf+1,ccEndSI+12*nsurf+nsurf, 1)/) 
  
  INTEGER,DIMENSION(nsurf):: c_CpAnOHM = (/(cc, cc=ccEndSI+13*nsurf+1,ccEndSI+13*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_KkAnOHM = (/(cc, cc=ccEndSI+14*nsurf+1,ccEndSI+14*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_ChAnOHM = (/(cc, cc=ccEndSI+15*nsurf+1,ccEndSI+15*nsurf+nsurf, 1)/) 



  
  INTEGER,PARAMETER:: ccEndI = (ccEndSI+15*nsurf+nsurf) 

  
  INTEGER,DIMENSION(NVegSurf):: c_BaseT   =(/(cc, cc=ccEndI+ 0*nvegsurf+1,ccEndI+ 0*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_BaseTe  =(/(cc, cc=ccEndI+ 1*nvegsurf+1,ccEndI+ 1*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_GDDFull =(/(cc, cc=ccEndI+ 2*nvegsurf+1,ccEndI+ 2*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_SDDFull =(/(cc, cc=ccEndI+ 3*nvegsurf+1,ccEndI+ 3*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LAIMin  =(/(cc, cc=ccEndI+ 4*nvegsurf+1,ccEndI+ 4*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LAIMax  =(/(cc, cc=ccEndI+ 5*nvegsurf+1,ccEndI+ 5*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_PorosityMin  =(/(cc, cc=ccEndI+ 6*nvegsurf+1,ccEndI+ 6*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_PorosityMax  =(/(cc, cc=ccEndI+ 7*nvegsurf+1,ccEndI+ 7*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_GsMax   =(/(cc, cc=ccEndI+ 8*nvegsurf+1,ccEndI+ 8*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LAIEq   =(/(cc, cc=ccEndI+ 9*nvegsurf+1,ccEndI+ 9*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LeafGP1 =(/(cc, cc=ccEndI+10*nvegsurf+1,ccEndI+10*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LeafGP2 =(/(cc, cc=ccEndI+11*nvegsurf+1,ccEndI+11*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LeafOP1 =(/(cc, cc=ccEndI+12*nvegsurf+1,ccEndI+12*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_LeafOP2 =(/(cc, cc=ccEndI+13*nvegsurf+1,ccEndI+13*nvegsurf+nvegsurf, 1)/) 
  INTEGER,DIMENSION(NVegSurf):: c_BiogenCO2Code = (/(cc, cc=ccEndI+14*nvegsurf+1,ccEndI+14*nvegsurf+nvegsurf, 1)/) 
  
  INTEGER,PARAMETER:: ccEndP = (ccEndI+14*nvegsurf+nvegsurf)

  
  INTEGER:: c_WaterDepth = (ccEndP+1)

  
  INTEGER,PARAMETER:: ccEndW = (ccEndP+1)

  
  INTEGER:: c_SnowRMFactor = (ccEndW+ 1)
  INTEGER:: c_SnowTMFactor = (ccEndW+ 2)
  INTEGER:: c_SnowAlbMin   = (ccEndW+ 3)
  INTEGER:: c_SnowAlbMax   = (ccEndW+ 4)
  
  INTEGER:: c_SnowEmis     = (ccEndW+ 6)
  INTEGER:: c_Snowtau_a    = (ccEndW+ 7)
  INTEGER:: c_Snowtau_f    = (ccEndW+ 8)
  INTEGER:: c_SnowPLimAlb  = (ccEndW+ 9)
  INTEGER:: c_SnowSDMin    = (ccEndW+10)
  INTEGER:: c_SnowSDMax    = (ccEndW+11)
  INTEGER:: c_Snowtau_r    = (ccEndW+12)
  INTEGER:: c_SnowCRWMin   = (ccEndW+13)
  INTEGER:: c_SnowCRWMax   = (ccEndW+14)
  INTEGER:: c_SnowPLimSnow = (ccEndW+15)

  
  INTEGER,PARAMETER:: ccEndSn = (ccEndW+15)

  
  INTEGER,DIMENSION(nsurf):: c_SoilDepth    = (/(cc, cc=ccEndSn+ 0*nsurf+1,ccEndSn+ 0*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_SoilStCap = (/(cc, cc=ccEndSn+ 1*nsurf+1,ccEndSn+ 1*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_KSat         = (/(cc, cc=ccEndSn+ 2*nsurf+1,ccEndSn+ 2*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_SoilDens     = (/(cc, cc=ccEndSn+ 3*nsurf+1,ccEndSn+ 3*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_SoilInfRate  = (/(cc, cc=ccEndSn+ 4*nsurf+1,ccEndSn+ 4*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_ObsSMDepth   = (/(cc, cc=ccEndSn+ 5*nsurf+1,ccEndSn+ 5*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_ObsSMMax     = (/(cc, cc=ccEndSn+ 6*nsurf+1,ccEndSn+ 6*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: c_ObsSNRFrac   = (/(cc, cc=ccEndSn+ 7*nsurf+1,ccEndSn+ 7*nsurf+nsurf, 1)/)  

  
  INTEGER,PARAMETER:: ccEndSo = (ccEndSn+ 7*nsurf+nsurf)

  
  INTEGER:: c_GsG1  = (ccEndSo+ 1)
  INTEGER:: c_GsG2  = (ccEndSo+ 2)
  INTEGER:: c_GsG3  = (ccEndSo+ 3)
  INTEGER:: c_GsG4  = (ccEndSo+ 4)
  INTEGER:: c_GsG5  = (ccEndSo+ 5)
  INTEGER:: c_GsG6  = (ccEndSo+ 6)
  INTEGER:: c_GsTH  = (ccEndSo+ 7)
  INTEGER:: c_GsTL  = (ccEndSo+ 8)
  INTEGER:: c_GsS1  = (ccEndSo+ 9)
  INTEGER:: c_GsS2  = (ccEndSo+10)
  INTEGER:: c_GsKmax  = (ccEndSo+11)
  INTEGER:: c_gsModel  = (ccEndSo+12)

  
  INTEGER,PARAMETER:: ccEndGs = (ccEndSo+12)

  
  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMCode_SWet  =(/(cc, cc=ccEndGs+ 0*nsurfIncSnow+1,&
       ccEndGs+ 0*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMCode_SDry  =(/(cc, cc=ccEndGs+ 1*nsurfIncSnow+1,&
       ccEndGs+ 1*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMCode_WWet  =(/(cc, cc=ccEndGs+ 2*nsurfIncSnow+1,&
       ccEndGs+ 2*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMCode_WDry  =(/(cc, cc=ccEndGs+ 3*nsurfIncSnow+1,&
       ccEndGs+ 3*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a1_SWet       =(/(cc, cc=ccEndGs+ 4*nsurfIncSnow+1,&
       ccEndGs+ 4*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a2_SWet       =(/(cc, cc=ccEndGs+ 5*nsurfIncSnow+1,&
       ccEndGs+ 5*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a3_SWet       =(/(cc, cc=ccEndGs+ 6*nsurfIncSnow+1,&
       ccEndGs+ 6*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a1_SDry       =(/(cc, cc=ccEndGs+ 7*nsurfIncSnow+1,&
       ccEndGs+ 7*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a2_SDry       =(/(cc, cc=ccEndGs+ 8*nsurfIncSnow+1,&
       ccEndGs+ 8*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a3_SDry       =(/(cc, cc=ccEndGs+ 9*nsurfIncSnow+1,&
       ccEndGs+ 9*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a1_WWet       =(/(cc, cc=ccEndGs+10*nsurfIncSnow+1,&
       ccEndGs+10*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a2_WWet       =(/(cc, cc=ccEndGs+11*nsurfIncSnow+1,&
       ccEndGs+11*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a3_WWet       =(/(cc, cc=ccEndGs+12*nsurfIncSnow+1,&
       ccEndGs+12*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a1_WDry       =(/(cc, cc=ccEndGs+13*nsurfIncSnow+1,&
       ccEndGs+13*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a2_WDry       =(/(cc, cc=ccEndGs+14*nsurfIncSnow+1,&
       ccEndGs+14*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_a3_WDry       =(/(cc, cc=ccEndGs+15*nsurfIncSnow+1,&
       ccEndGs+15*nsurfIncSnow+nsurfIncSnow, 1)/)  

  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMThresh_SW  =(/(cc, cc=ccEndGs+ 16*nsurfIncSnow+1,&
       ccEndGs+ 16*nsurfIncSnow+nsurfIncSnow, 1)/)  
  INTEGER,DIMENSION(nsurfIncSnow):: c_OHMThresh_WD  =(/(cc, cc=ccEndGs+ 17*nsurfIncSnow+1,&
       ccEndGs+ 17*nsurfIncSnow+nsurfIncSnow, 1)/)  

  
  INTEGER,DIMENSION(nsurfIncSnow):: c_ESTMCode      = (/(cc, cc=ccEndGs+18*nsurfIncSnow+1,&
       ccEndGs+18*nsurfIncSnow+nsurfIncSnow, 1)/)  

  
  INTEGER,PARAMETER:: ccEndO = (ccEndGs+18*nsurfIncSnow+nsurfIncSnow)

  
  INTEGER :: c_BaseTHDD               = (ccEndO+ 1)
  INTEGER :: c_QF_A1                  = (ccEndO+ 2)
  INTEGER :: c_QF_B1                  = (ccEndO+ 3)
  INTEGER :: c_QF_C1                  = (ccEndO+ 4)
  INTEGER :: c_QF_A2                  = (ccEndO+ 5)
  INTEGER :: c_QF_B2                  = (ccEndO+ 6)
  INTEGER :: c_QF_C2                  = (ccEndO+ 7)
  INTEGER :: c_AHMin_WD               = (ccEndO+ 8)
  INTEGER :: c_AHMin_WE               = (ccEndO+ 9)
  INTEGER :: c_AHSlopeHeating_WD      = (ccEndO+10)
  INTEGER :: c_AHSlopeHeating_WE      = (ccEndO+11)
  INTEGER :: c_AHSlopeCooling_WD      = (ccEndO+12)
  INTEGER :: c_AHSlopeCooling_WE      = (ccEndO+13)
  INTEGER :: c_TCriticHeating_WE      = (ccEndO+14)
  INTEGER :: c_TCriticHeating_WD      = (ccEndO+15)
  INTEGER :: c_TCriticCooling_WE      = (ccEndO+16)
  INTEGER :: c_TCriticCooling_WD      = (ccEndO+17)
  INTEGER :: c_EnProfWD               = (ccEndO+18)
  INTEGER :: c_EnProfWE               = (ccEndO+19)
  INTEGER :: c_CO2mWD                 = (ccEndO+20)
  INTEGER :: c_CO2mWE                 = (ccEndO+21)
  INTEGER :: c_TraffProfWD            = (ccEndO+22)
  INTEGER :: c_TraffProfWE            = (ccEndO+23)
  INTEGER :: c_PopProfWD              = (ccEndO+24)
  INTEGER :: c_PopProfWE              = (ccEndO+25)
  INTEGER :: c_MinQFMetab             = (ccEndO+26)
  INTEGER :: c_MaxQFMetab             = (ccEndO+27)
  INTEGER :: c_FrFossilFuel_Heat      = (ccEndO+28)
  INTEGER :: c_FrFossilFuel_NonHeat   = (ccEndO+29)
  INTEGER :: c_EF_umolCO2perJ         = (ccEndO+30)
  INTEGER :: c_EnEF_v_Jkm             = (ccEndO+31)
  INTEGER :: c_FcEF_v_kgkm            = (ccEndO+32)
  INTEGER :: c_TrafficUnits           = (ccEndO+33)

  
  INTEGER,PARAMETER:: ccEndA = (ccEndO+33)

  
  INTEGER :: c_IeStart    = (ccEndA+ 1)
  INTEGER :: c_IeEnd    = (ccEndA+ 2)
  INTEGER :: c_IntWU    = (ccEndA+ 3)
  INTEGER :: c_Faut    = (ccEndA+ 4)
  INTEGER,DIMENSION(3):: c_Ie_a      = (/(cc, cc=ccEndA+4+ 0*3+1, ccEndA+4 + 0*3+3, 1)/)  
  INTEGER,DIMENSION(3):: c_Ie_m      = (/(cc, cc=ccEndA+4+ 1*3+1, ccEndA+4 + 1*3+3, 1)/)  
  INTEGER,DIMENSION(7):: c_DayWat    = (/(cc, cc=ccEndA+10+ 0*7+1,ccEndA+10+ 0*7+7, 1)/)  
  INTEGER,DIMENSION(7):: c_DayWatPer = (/(cc, cc=ccEndA+10+ 1*7+1,ccEndA+10+ 1*7+7, 1)/)  

  
  INTEGER,PARAMETER:: ccEndIr = (ccEndA+10+ 1*7+7)

  
  INTEGER,DIMENSION(24):: c_HrProfEnUseWD  = (/(cc, cc=ccEndIr+ 0*24+1, ccEndIr+ 0*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfEnUseWE  = (/(cc, cc=ccEndIr+ 1*24+1, ccEndIr+ 1*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfWUManuWD = (/(cc, cc=ccEndIr+ 2*24+1, ccEndIr+ 2*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfWUManuWE = (/(cc, cc=ccEndIr+ 3*24+1, ccEndIr+ 3*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfWUAutoWD = (/(cc, cc=ccEndIr+ 4*24+1, ccEndIr+ 4*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfWUAutoWE = (/(cc, cc=ccEndIr+ 5*24+1, ccEndIr+ 5*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfSnowCWD  = (/(cc, cc=ccEndIr+ 6*24+1, ccEndIr+ 6*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfSnowCWE  = (/(cc, cc=ccEndIr+ 7*24+1, ccEndIr+ 7*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfHumActivityWD = (/(cc, cc=ccEndIr+ 8*24+1, ccEndIr+ 8*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfHumActivityWE = (/(cc, cc=ccEndIr+ 9*24+1, ccEndIr+ 9*24+24, 1)/)  
  INTEGER,DIMENSION(24):: c_HrProfTraffWD  = (/(cc, cc=ccEndIr+ 10*24+1, ccEndIr+ 10*24+24, 1)/) 
  INTEGER,DIMENSION(24):: c_HrProfTraffWE  = (/(cc, cc=ccEndIr+ 11*24+1, ccEndIr+ 11*24+24, 1)/) 
  INTEGER,DIMENSION(24):: c_HrProfPopWD    = (/(cc, cc=ccEndIr+ 12*24+1, ccEndIr+ 12*24+24, 1)/) 
  INTEGER,DIMENSION(24):: c_HrProfPopWE    = (/(cc, cc=ccEndIr+ 13*24+1, ccEndIr+ 13*24+24, 1)/) 


  
  INTEGER,PARAMETER:: ccEndPr = (ccEndIr+ 13*24+24)

  
  INTEGER,DIMENSION(nsurf):: c_WGToPaved = (/(cc, cc=ccEndPr+ 0*nsurf+1,ccEndPr+ 0*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToBldgs = (/(cc, cc=ccEndPr+ 1*nsurf+1,ccEndPr+ 1*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToEveTr = (/(cc, cc=ccEndPr+ 2*nsurf+1,ccEndPr+ 2*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToDecTr = (/(cc, cc=ccEndPr+ 3*nsurf+1,ccEndPr+ 3*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToGrass = (/(cc, cc=ccEndPr+ 4*nsurf+1,ccEndPr+ 4*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToBSoil = (/(cc, cc=ccEndPr+ 5*nsurf+1,ccEndPr+ 5*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToWater = (/(cc, cc=ccEndPr+ 6*nsurf+1,ccEndPr+ 6*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToRunoff    = (/(cc, cc=ccEndPr+ 7*nsurf+1,ccEndPr+ 7*nsurf+nsurf, 1)/) 
  INTEGER,DIMENSION(nsurf):: c_WGToSoilStore = (/(cc, cc=ccEndPr+ 8*nsurf+1,ccEndPr+ 8*nsurf+nsurf, 1)/) 

  
  INTEGER,PARAMETER:: cBEndWG = (ccEndPr+ 8*nsurf+nsurf)

  
  INTEGER,DIMENSION(nvegsurf):: c_alpha_bioCO2     = (/(cc, cc=cBEndWG+ 0*nvegsurf+1,cBEndWG+ 0*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_beta_bioCO2      = (/(cc, cc=cBEndWG+ 1*nvegsurf+1,cBEndWG+ 1*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_theta_bioCO2     = (/(cc, cc=cBEndWG+ 2*nvegsurf+1,cBEndWG+ 2*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_alpha_enh_bioCO2 = (/(cc, cc=cBEndWG+ 3*nvegsurf+1,cBEndWG+ 3*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_beta_enh_bioCO2  = (/(cc, cc=cBEndWG+ 4*nvegsurf+1,cBEndWG+ 4*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_resp_a           = (/(cc, cc=cBEndWG+ 5*nvegsurf+1,cBEndWG+ 5*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_resp_b           = (/(cc, cc=cBEndWG+ 6*nvegsurf+1,cBEndWG+ 6*nvegsurf+nvegsurf, 1)/)
  INTEGER,DIMENSION(nvegsurf):: c_min_res_bioCO2   = (/(cc, cc=cBEndWG+ 7*nvegsurf+1,cBEndWG+ 7*nvegsurf+nvegsurf, 1)/)

  
  INTEGER,PARAMETER:: ccEndB = (cBEndWG+7*nvegsurf+nvegsurf)

  
  
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_thick1  = (/(cc, cc=ccEndB+ 0*nsurfIncSnow+1,ccEndB+ 0*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_k1      = (/(cc, cc=ccEndB+ 1*nsurfIncSnow+1,ccEndB+ 1*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_rhoCp1  = (/(cc, cc=ccEndB+ 2*nsurfIncSnow+1,ccEndB+ 2*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_thick2  = (/(cc, cc=ccEndB+ 3*nsurfIncSnow+1,ccEndB+ 3*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_k2      = (/(cc, cc=ccEndB+ 4*nsurfIncSnow+1,ccEndB+ 4*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_rhoCp2  = (/(cc, cc=ccEndB+ 5*nsurfIncSnow+1,ccEndB+ 5*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_thick3  = (/(cc, cc=ccEndB+ 6*nsurfIncSnow+1,ccEndB+ 6*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_k3      = (/(cc, cc=ccEndB+ 7*nsurfIncSnow+1,ccEndB+ 7*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_rhoCp3  = (/(cc, cc=ccEndB+ 8*nsurfIncSnow+1,ccEndB+ 8*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_thick4  = (/(cc, cc=ccEndB+ 9*nsurfIncSnow+1,ccEndB+ 9*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_k4      = (/(cc, cc=ccEndB+10*nsurfIncSnow+1,ccEndB+10*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_rhoCp4  = (/(cc, cc=ccEndB+11*nsurfIncSnow+1,ccEndB+11*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_thick5  = (/(cc, cc=ccEndB+12*nsurfIncSnow+1,ccEndB+12*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_k5      = (/(cc, cc=ccEndB+13*nsurfIncSnow+1,ccEndB+13*nsurfIncSnow+nsurfIncSnow, 1)/)
  INTEGER,DIMENSION(nsurfIncSnow):: c_Surf_rhoCp5  = (/(cc, cc=ccEndB+14*nsurfIncSnow+1,ccEndB+14*nsurfIncSnow+nsurfIncSnow, 1)/)
  
  INTEGER,PARAMETER:: ccEndESTMB = (ccEndB+14*nsurfIncSnow+nsurfIncSnow)
  
  INTEGER:: c_Wall_thick1  = (ccEndESTMB+ 1)
  INTEGER:: c_Wall_k1      = (ccEndESTMB+ 2)
  INTEGER:: c_Wall_rhoCp1  = (ccEndESTMB+ 3)
  INTEGER:: c_Wall_thick2  = (ccEndESTMB+ 4)
  INTEGER:: c_Wall_k2      = (ccEndESTMB+ 5)
  INTEGER:: c_Wall_rhoCp2  = (ccEndESTMB+ 6)
  INTEGER:: c_Wall_thick3  = (ccEndESTMB+ 7)
  INTEGER:: c_Wall_k3      = (ccEndESTMB+ 8)
  INTEGER:: c_Wall_rhoCp3  = (ccEndESTMB+ 9)
  INTEGER:: c_Wall_thick4  = (ccEndESTMB+10)
  INTEGER:: c_Wall_k4      = (ccEndESTMB+11)
  INTEGER:: c_Wall_rhoCp4  = (ccEndESTMB+12)
  INTEGER:: c_Wall_thick5  = (ccEndESTMB+13)
  INTEGER:: c_Wall_k5      = (ccEndESTMB+14)
  INTEGER:: c_Wall_rhoCp5  = (ccEndESTMB+15)
  INTEGER:: c_Internal_thick1  = (ccEndESTMB+16)
  INTEGER:: c_Internal_k1      = (ccEndESTMB+17)
  INTEGER:: c_Internal_rhoCp1  = (ccEndESTMB+18)
  INTEGER:: c_Internal_thick2  = (ccEndESTMB+19)
  INTEGER:: c_Internal_k2      = (ccEndESTMB+20)
  INTEGER:: c_Internal_rhoCp2  = (ccEndESTMB+21)
  INTEGER:: c_Internal_thick3  = (ccEndESTMB+22)
  INTEGER:: c_Internal_k3      = (ccEndESTMB+23)
  INTEGER:: c_Internal_rhoCp3  = (ccEndESTMB+24)
  INTEGER:: c_Internal_thick4  = (ccEndESTMB+25)
  INTEGER:: c_Internal_k4      = (ccEndESTMB+26)
  INTEGER:: c_Internal_rhoCp4  = (ccEndESTMB+27)
  INTEGER:: c_Internal_thick5  = (ccEndESTMB+28)
  INTEGER:: c_Internal_k5      = (ccEndESTMB+29)
  INTEGER:: c_Internal_rhoCp5  = (ccEndESTMB+30)
  INTEGER:: c_nroom      =  (ccEndESTMB+31)
  INTEGER:: c_alb_ibld   =  (ccEndESTMB+32)
  INTEGER:: c_em_ibld    =  (ccEndESTMB+33)
  INTEGER:: c_CH_iwall   =  (ccEndESTMB+34)
  INTEGER:: c_CH_iroof   =  (ccEndESTMB+35)
  INTEGER:: c_CH_ibld    =  (ccEndESTMB+36)
  
  INTEGER,PARAMETER:: ccEndESTMM = (ccEndESTMB+36)
  
  INTEGER,DIMENSION(3):: c_Surf_thick1_Paved  = (/(cc, cc=ccEndESTMM+ 0*3+1,ccEndESTMM+ 0*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_k1_Paved      = (/(cc, cc=ccEndESTMM+ 1*3+1,ccEndESTMM+ 1*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_rhoCp1_Paved  = (/(cc, cc=ccEndESTMM+ 2*3+1,ccEndESTMM+ 2*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_thick2_Paved  = (/(cc, cc=ccEndESTMM+ 3*3+1,ccEndESTMM+ 3*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_k2_Paved      = (/(cc, cc=ccEndESTMM+ 4*3+1,ccEndESTMM+ 4*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_rhoCp2_Paved  = (/(cc, cc=ccEndESTMM+ 5*3+1,ccEndESTMM+ 5*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_thick3_Paved  = (/(cc, cc=ccEndESTMM+ 6*3+1,ccEndESTMM+ 6*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_k3_Paved      = (/(cc, cc=ccEndESTMM+ 7*3+1,ccEndESTMM+ 7*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_rhoCp3_Paved  = (/(cc, cc=ccEndESTMM+ 8*3+1,ccEndESTMM+ 8*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_thick4_Paved  = (/(cc, cc=ccEndESTMM+ 9*3+1,ccEndESTMM+ 9*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_k4_Paved      = (/(cc, cc=ccEndESTMM+10*3+1,ccEndESTMM+10*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_rhoCp4_Paved  = (/(cc, cc=ccEndESTMM+11*3+1,ccEndESTMM+11*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_thick5_Paved  = (/(cc, cc=ccEndESTMM+12*3+1,ccEndESTMM+12*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_k5_Paved      = (/(cc, cc=ccEndESTMM+13*3+1,ccEndESTMM+13*3+3, 1)/)
  INTEGER,DIMENSION(3):: c_Surf_rhoCp5_Paved  = (/(cc, cc=ccEndESTMM+14*3+1,ccEndESTMM+14*3+3, 1)/)
  
  INTEGER,PARAMETER:: ccEndESTMMP = (ccEndESTMM+14*3+3)
  
  INTEGER,DIMENSION(5):: c_Surf_thick1_Bldgs  = (/(cc, cc=ccEndESTMMP+ 0*5+1,ccEndESTMMP+ 0*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_k1_Bldgs      = (/(cc, cc=ccEndESTMMP+ 1*5+1,ccEndESTMMP+ 1*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_rhoCp1_Bldgs  = (/(cc, cc=ccEndESTMMP+ 2*5+1,ccEndESTMMP+ 2*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_thick2_Bldgs  = (/(cc, cc=ccEndESTMMP+ 3*5+1,ccEndESTMMP+ 3*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_k2_Bldgs      = (/(cc, cc=ccEndESTMMP+ 4*5+1,ccEndESTMMP+ 4*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_rhoCp2_Bldgs  = (/(cc, cc=ccEndESTMMP+ 5*5+1,ccEndESTMMP+ 5*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_thick3_Bldgs  = (/(cc, cc=ccEndESTMMP+ 6*5+1,ccEndESTMMP+ 6*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_k3_Bldgs      = (/(cc, cc=ccEndESTMMP+ 7*5+1,ccEndESTMMP+ 7*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_rhoCp3_Bldgs  = (/(cc, cc=ccEndESTMMP+ 8*5+1,ccEndESTMMP+ 8*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_thick4_Bldgs  = (/(cc, cc=ccEndESTMMP+ 9*5+1,ccEndESTMMP+ 9*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_k4_Bldgs      = (/(cc, cc=ccEndESTMMP+10*5+1,ccEndESTMMP+10*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_rhoCp4_Bldgs  = (/(cc, cc=ccEndESTMMP+11*5+1,ccEndESTMMP+11*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_thick5_Bldgs  = (/(cc, cc=ccEndESTMMP+12*5+1,ccEndESTMMP+12*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_k5_Bldgs      = (/(cc, cc=ccEndESTMMP+13*5+1,ccEndESTMMP+13*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Surf_rhoCp5_Bldgs  = (/(cc, cc=ccEndESTMMP+14*5+1,ccEndESTMMP+14*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_thick1_Bldgs  = (/(cc, cc=ccEndESTMMP+15*5+1,ccEndESTMMP+15*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_k1_Bldgs      = (/(cc, cc=ccEndESTMMP+16*5+1,ccEndESTMMP+16*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_rhoCp1_Bldgs  = (/(cc, cc=ccEndESTMMP+17*5+1,ccEndESTMMP+17*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_thick2_Bldgs  = (/(cc, cc=ccEndESTMMP+18*5+1,ccEndESTMMP+18*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_k2_Bldgs      = (/(cc, cc=ccEndESTMMP+19*5+1,ccEndESTMMP+19*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_rhoCp2_Bldgs  = (/(cc, cc=ccEndESTMMP+20*5+1,ccEndESTMMP+20*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_thick3_Bldgs  = (/(cc, cc=ccEndESTMMP+21*5+1,ccEndESTMMP+21*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_k3_Bldgs      = (/(cc, cc=ccEndESTMMP+22*5+1,ccEndESTMMP+22*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_rhoCp3_Bldgs  = (/(cc, cc=ccEndESTMMP+23*5+1,ccEndESTMMP+23*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_thick4_Bldgs  = (/(cc, cc=ccEndESTMMP+24*5+1,ccEndESTMMP+24*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_k4_Bldgs      = (/(cc, cc=ccEndESTMMP+25*5+1,ccEndESTMMP+25*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_rhoCp4_Bldgs  = (/(cc, cc=ccEndESTMMP+26*5+1,ccEndESTMMP+26*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_thick5_Bldgs  = (/(cc, cc=ccEndESTMMP+27*5+1,ccEndESTMMP+27*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_k5_Bldgs      = (/(cc, cc=ccEndESTMMP+28*5+1,ccEndESTMMP+28*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Wall_rhoCp5_Bldgs  = (/(cc, cc=ccEndESTMMP+29*5+1,ccEndESTMMP+29*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_thick1_Bldgs  = (/(cc, cc=ccEndESTMMP+30*5+1,ccEndESTMMP+30*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_k1_Bldgs      = (/(cc, cc=ccEndESTMMP+31*5+1,ccEndESTMMP+31*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_rhoCp1_Bldgs  = (/(cc, cc=ccEndESTMMP+32*5+1,ccEndESTMMP+32*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_thick2_Bldgs  = (/(cc, cc=ccEndESTMMP+33*5+1,ccEndESTMMP+33*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_k2_Bldgs      = (/(cc, cc=ccEndESTMMP+34*5+1,ccEndESTMMP+34*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_rhoCp2_Bldgs  = (/(cc, cc=ccEndESTMMP+35*5+1,ccEndESTMMP+35*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_thick3_Bldgs  = (/(cc, cc=ccEndESTMMP+36*5+1,ccEndESTMMP+36*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_k3_Bldgs      = (/(cc, cc=ccEndESTMMP+37*5+1,ccEndESTMMP+37*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_rhoCp3_Bldgs  = (/(cc, cc=ccEndESTMMP+38*5+1,ccEndESTMMP+38*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_thick4_Bldgs  = (/(cc, cc=ccEndESTMMP+39*5+1,ccEndESTMMP+39*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_k4_Bldgs      = (/(cc, cc=ccEndESTMMP+40*5+1,ccEndESTMMP+40*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_rhoCp4_Bldgs  = (/(cc, cc=ccEndESTMMP+41*5+1,ccEndESTMMP+41*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_thick5_Bldgs  = (/(cc, cc=ccEndESTMMP+42*5+1,ccEndESTMMP+42*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_k5_Bldgs      = (/(cc, cc=ccEndESTMMP+43*5+1,ccEndESTMMP+43*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_Internal_rhoCp5_Bldgs  = (/(cc, cc=ccEndESTMMP+44*5+1,ccEndESTMMP+44*5+5, 1)/)
  INTEGER,DIMENSION(5):: c_nroom_Bldgs      =  (ccEndESTMMP+44*5+5+ 1)
  INTEGER,DIMENSION(5):: c_alb_ibld_Bldgs   =  (ccEndESTMMP+44*5+5+ 2)
  INTEGER,DIMENSION(5):: c_em_ibld_Bldgs    =  (ccEndESTMMP+44*5+5+ 3)
  INTEGER,DIMENSION(5):: c_CH_iwall_Bldgs   =  (ccEndESTMMP+44*5+5+ 4)
  INTEGER,DIMENSION(5):: c_CH_iroof_Bldgs   =  (ccEndESTMMP+44*5+5+ 5)
  INTEGER,DIMENSION(5):: c_CH_ibld_Bldgs    =  (ccEndESTMMP+44*5+5+ 6)

  
  INTEGER,PARAMETER:: MaxNCols_c = (ccEndESTMMP+44*5+5+ 6)
  

  
  
  INTEGER,PARAMETER:: ccMOD = 32
  INTEGER,DIMENSION(nsurf):: cMOD_State          =(/(cc, cc=ccMOD+ 0*nsurf+1,ccMOD+ 0*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: cMOD_SoilState      =(/(cc, cc=ccMOD+ 1*nsurf+1,ccMOD+ 1*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: cMOD_SnowWaterState =(/(cc, cc=ccMOD+ 2*nsurf+1,ccMOD+ 2*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: cMOD_SnowPack       =(/(cc, cc=ccMOD+ 3*nsurf+1,ccMOD+ 3*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: cMOD_SnowFrac       =(/(cc, cc=ccMOD+ 4*nsurf+1,ccMOD+ 4*nsurf+nsurf, 1)/)  
  INTEGER,DIMENSION(nsurf):: cMOD_SnowDens       =(/(cc, cc=ccMOD+ 5*nsurf+1,ccMOD+ 5*nsurf+nsurf, 1)/)  

  
  INTEGER,PARAMETER:: MaxNCols_cMOD = ccMOD+ 5*nsurf+nsurf
  

  
  
  INTEGER,PARAMETER:: ccMDS = 30
  INTEGER,DIMENSION(nsurf):: cMDS_SnowDens       =(/(cc, cc=ccMDS+ 0*nsurf+1,ccMDS+ 0*nsurf+nsurf, 1)/)  

  
  INTEGER,PARAMETER:: MaxNCols_cMDS = ccMDS+ 0*nsurf+nsurf
  

  
  
  INTEGER, PARAMETER:: cTs_iy = 1
  INTEGER, PARAMETER:: cTs_id = 2
  INTEGER, PARAMETER:: cTs_it = 3
  INTEGER, PARAMETER:: cTs_imin = 4
  INTEGER, PARAMETER:: cTs_Tiair = 5
  INTEGER, PARAMETER:: cTs_Tsurf = 6
  INTEGER, PARAMETER:: cTs_Troof = 7
  INTEGER, PARAMETER:: cTs_Troad = 8
  INTEGER, PARAMETER:: cTs_Twall = 9
  INTEGER, PARAMETER:: cTs_Twall_n = 10
  INTEGER, PARAMETER:: cTs_Twall_e = 11
  INTEGER, PARAMETER:: cTs_Twall_s = 12
  INTEGER, PARAMETER:: cTs_Twall_w = 13


END MODULE allocateArray



MODULE Initial

  IMPLICIT NONE

  INTEGER::FirstYear,&          
       LastYear,&           
       FirstGrid,&    
       LastGrid,&            
       NumberOfGrids,&      
       GridCounter,&        

       ReadBlocksMetData,&  
       ReadBlocksOrigMetData,&  
       ReadLinesMetData,&   
       ReadLinesOrigMetData,&   
       ReadLinesOrigESTMData,&   
       ReadLinesOrigMetDataMax,&   
       ReadLinesOrigESTMDataMax,&   
       nlinesOrigMetData,&        
       nlinesOrigESTMData,&        
       nlinesMetData,&            
       nlinesESTMdata,&           
       nlinesSiteSelect,&         
       nlinesNonVeg,&             
       nlinesVeg,&                
       nlinesWater,&             
       nlinesSnow,&               
       nlinesSoil,&               
       nlinesConductance,&        
       nlinesOHMCoefficients,&    
       nlinesESTMCoefficients,&   
       nlinesAnthropogenic,&      
       nlinesIrrigation,&         
       nlinesProfiles,&           
       nlinesWGWaterDist,&        
       nlinesBiogen,&             
       nlines,&                   
       SkippedLines,&             
       SkippedLinesOrig,&         
       SkippedLinesOrigESTM,&         
       iv5            

END MODULE Initial



MODULE data_in

  IMPLICIT NONE

  CHARACTER (len=90)::progname='SUEWS V2018a'  

  
  CHARACTER (len=20)::  FileCode   
  CHARACTER (len=150):: FileInputPath,&   
       FileOutputPath    
  
  CHARACTER (len=150):: FileOut,&         
       FileChoices,&     
       FileMet,&         
       FileOrigMet,&     
       FileOrigESTM,&    
       FileDscdMet,&     
       FileDscdESTM,&    
       FileDaily,&       
       FileESTMTs,&      
       SOLWEIGpoiOut,&   
       BLout,&             
       FileOut_tt,&        
       ESTMOut_tt

  INTEGER:: SkipHeaderSiteInfo = 2   
  INTEGER:: SkipHeaderMet = 1        

  
  INTEGER:: EmissionsMethod,& 
       CBLuse,&               
       MultipleMetFiles,&     
       MultipleInitFiles,&      
       MultipleESTMFiles,&    
       KeepTstepFilesIn,&     
       KeepTstepFilesOut,&    
       ResolutionFilesIn,&    
       ResolutionFilesOut,&   
       ResolutionFilesInESTM,&
       WriteOutOption,&         
       NetRadiationMethod,&   
       OHMIncQF,&             
       StorageHeatMethod,&             
       SNOWuse,&              
       SOLWEIGuse,&           
       SMDMethod,&           
       WaterUseMethod,&            
       RoughLenMomMethod,&              
       DisaggMethod,&         
       DisaggMethodESTM,&         
       RainDisaggMethod,&     
       RainAmongN,&           
       KdownZen,&             
       SuppressWarnings,&     
       Diagnose,&             
       DiagnoseDisagg,&       
       ncMode,&               
       nRow,&                 
       nCol,&                 
       DiagnoseDisaggESTM,&   
       DiagQN, DiagQS         

  
  INTEGER, DIMENSION(5):: MultRainAmongN           
  REAL(KIND(1d0)),DIMENSION(5):: MultRainAmongNUpperI   

  
  INTEGER:: AlbedoChoice,&         
                                
       InputMetFormat,&       
                                
       ity,&                  
                                
       LAIcalcYes,&           
                                
       WriteDailyState        
  

  
  INTEGER:: ldown_option           

  
  INTEGER:: lfnout,&               
       lfnoutC,&              
       lfnOld                 

  INTEGER:: OutputFormats   


  
  REAL (KIND(1d0)):: timezone      

  
  
  REAL (KIND(1d0))::  alpha_qhqe,& 
       alt,&                        
       avdens,&    
       avkdn,&     
       avrh,&      
       avts,&      
       avu1,&      
       avU10_ms,&   
       azimuth,&   
       BaseTHDD,&  
       BuildEnergyUse,&  
       E_mod,&     
       emis_snow,& 
       Fc,&        
       Fc_anthro,& 
       Fc_biogen,& 
       Fc_photo,&  
       Fc_respi,&  
       Fc_metab,&  
       Fc_traff,&  
       Fc_build,&  
       fcld,&      
       fcld_obs,&  
       h_mod,&     
       kclear,&    
       kdiff,&     
       kdir,&      
       kup,&       
       LAI_obs,&   
       lat,&       
       ldown, &    
       ldown_obs,& 
       lng,&       
       lup,&       
       NumCapita,& 
       PopDensDaytime,&   
       PopDensNighttime,& 
       Precip,&    
       Precip_hr,&    
       Press_hPa,&  
       Pres_kPa,&   
       q2_gkg,&    
       qe,&        
       qe_obs,&
       qf,&        
       QF_SAHP,&    
       QF_SAHP_base,&    
       QF_SAHP_heat,&    
       QF_SAHP_ac,& 
       qh,&        
       qh_obs,&
       QH_r,&      
       qn1,&       
       qn1_bup,&
       qn1_obs,&   
       qn1_S,&     
       qn1_SF,&    
       qs,&        
       QSanOHM,&   
       QSestm,&    
       snow,&      
       snow_obs,&  
       CO2mWD,&
       CO2mWE,&
       EF_umolCO2perJ,&
       EnEF_v_Jkm,&
       EnProfWD, &
       EnProfWE,&
       TrafficUnits,&
       TraffProfWD,&
       TraffProfWE,&
       PopProfWD,&
       PopProfWE,&
       FcEF_v_kgkm,&
       FrFossilFuel_Heat,&
       FrFossilFuel_NonHeat,&
       MinQFMetab,&
       MaxQFMetab,&
       QF_build,&
       QF_metab,&
       QF_traff,&
       Temp_C,&    
       t2_C,&     
       trans_site,&  
       tsurf,&   
       wdir,&      
       wu_m3,&     
       xsmd,&      
       year,&      
       zenith_deg  

  REAL(KIND(1d0)),DIMENSION(2)::Qf_A,Qf_B,Qf_C,&   
       AH_MIN,&    
       AH_SLOPE_Heating,&  
       AH_SLOPE_Cooling,&
       T_CRITIC_Heating,& 
       T_CRITIC_Cooling,& 
       TrafficRate,& 
       QF0_BEU

  REAL(KIND(1d0)),DIMENSION(0:23,2):: AHPROF     
  REAL(KIND(1d0)),DIMENSION(0:23,2):: HumActivityProf   
  REAL(KIND(1d0)),DIMENSION(0:23,2):: TraffProf   
  REAL(KIND(1d0)),DIMENSION(0:23,2):: PopProf   


  
  INTEGER::startDLS   
  INTEGER::endDLS   

  INTEGER::nCBLstep  


  REAL (KIND(1D0)):: DRAINRT,&      
       RAINBUCKET,&   
       RAINCOVER,&
       RAINMAXRES,&   
       RAINRES,&      
       TEMPVEG        

  
  REAL(KIND(1D0)):: absL,&             
       absK,&             
       heightgravity,&    
       TransMin,&         
       TransMax           

  INTEGER:: Posture,&                
       usevegdem,&          
       row,&                    
       col,&                    
       onlyglobal,&             
       SOLWEIGpoi_out,&         
       Tmrt_out,&               
       Lup2d_out,&              
       Ldown2d_out,&            
       Kup2d_out,&              
       Kdown2d_out,&            
       GVF_out,&                
       SOLWEIG_ldown,&          
       OutInterval,&            
       RunForGrid               

  CHARACTER (len=150):: DSMPath,&    
       DSMname,&    
       CDSMname,&   
       TDSMname,&   
       SVFPath,&    
       SVFsuffix,&  
       buildingsname

  
  

END MODULE data_in



MODULE cbl_MODULE

  INTEGER::EntrainmentType,&  
       CO2_included,&     
       InitialData_use,&  
                                
       sondeflag,&      
       isubs          

  INTEGER,DIMENSION(366)::cblday=0

  CHARACTER (len=200), DIMENSION(366)::FileSonde=""
  CHARACTER (len=200)::InitialDataFileName
  REAL(KIND(1D0)):: wsb       
  REAL(KIND(1d0)),DIMENSION(1:10):: cbldata
  REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::IniCBLdata

  
  INTEGER::zmax,&
       nEqn=6,&  
       iCBLcount,&
       nlineInData
  REAL(KIND(1d0))::C2K=273.16

  REAL (KIND(1D0)):: usbl,ftbl,fqbl,fcbl,gamt,gamq,gamc,tpp,qpp,cp0

  REAL(KIND(1D0))::alpha3,&
       blh_m,&    
       blh1_m,&
       cm,&       
                                
       gamt_Km,&  
       gamq_gkgm,&
       gamq_kgkgm,&
                                
       tm_C,&     
       tm_K,&     
       tmp_K,&
       tp_C,&     
       tp_K,&     
       tpp_K,&
       febl_kgkgms,&
       fhbl_Kms,&   
       qm_gkg,&   
       qm_kgkg,&  
       qp_gkg,&   
       qp_kgkg,&  
       qpp_kgkg


  REAL (KIND(1D0)), DIMENSION (0:500,2):: gtheta,ghum 
  REAL (KIND(1D0)), DIMENSION(6)::y  

END   MODULE cbl_MODULE


MODULE snowMod
  IMPLICIT NONE

  REAL (KIND(1D0))::AdjMeltFact,&    
       SnowfallCum,&        
       fwh,&              
       lvS_J_kg,&         
       mwh,&              
       MwStore,&              
       PrecipLimit,&      
       PrecipLimitAlb,&   
       Qm,&               
       QmFreez,&          
       QmRain,&
       qn1_snow,&         
       qn1_nosnow,&       
       RadMeltFact,&      
       SnowAlb,&          
       SnowAlbMin,&       
       SnowAlbMax,&       
       SnowDensMin,&      
       SnowDensMax,&      
       SnowLimBuild,&     
       SnowLimPaved,&     
       swe,&        
       tau_a,&            
       tau_f,&
       tau_r,&            
       TempMeltFact,&     
       volDay,&           
       zf,&
       WaterHoldCapFrac,& 
       CRWmin,& 
       CRWmax  

  REAL(KIND(1D0)), DIMENSION(2)::  SnowRemoval=0 
  REAL(KIND(1d0)), DIMENSION(0:23,2):: snowProf  

  INTEGER::SnowFractionChoice=2   

END MODULE snowMod



MODULE defaultNotUsed
  IMPLICIT NONE
  REAL (KIND(1d0)):: notUsed=-55.55,reall,NAN=-999,pNAN=999
  INTEGER:: notUsedI=-55, ios_out
  INTEGER:: errorChoice, warningChoice  
END MODULE defaultNotUsed



MODULE time
  INTEGER:: iy,&            
       id,&            
       it,&            
       imin,&          
       DLS             

  REAL(KIND(1d0)):: dectime        
  REAL (KIND(1d0)):: tstepcount    
  INTEGER:: nofDaysThisYear        

  INTEGER:: iy_prev_t, id_prev_t   

END MODULE time



MODULE mod_grav
  REAL (KIND(1d0)):: grav=9.80665  
END MODULE mod_grav


MODULE mod_k
  REAL(KIND(1d0)) :: k=0.4,&             
       k2=0.16,&           
       neut_limit=0.001000 
END MODULE mod_k


MODULE Thresh
  REAL(KIND(1d0)) :: IPThreshold_mmhr = 10   

END MODULE Thresh



MODULE gas
  
  IMPLICIT NONE
  REAL (KIND(1d0))::  comp=0.9995
  REAL (KIND(1d0))::  epsil=0.62197   
  REAL (KIND(1d0))::  epsil_gkg=621.97   
  REAL (KIND(1d0))::  dry_gas=8.31451 
  REAL (KIND(1d0))::  gas_ct_wat=461.05 
  REAL (KIND(1d0))::  molar=0.028965 
  REAL (KIND(1d0))::  molar_wat_vap=0.0180153 
  REAL (KIND(1d0))::  gas_ct_dry=8.31451/0.028965 
  REAL (KIND(1d0))::  gas_ct_wv=8.31451/0.0180153 
END MODULE gas


MODULE mod_z
  REAL (KIND(1d0)) :: zzd,&  
       z0m,&  
       zdm,&  
       z      
  REAL(KIND(1E10))::z0V      
END MODULE mod_z


MODULE resist  
  IMPLICIT NONE
  REAL (KIND(1d0)):: th,&             
       tl,&             
       Kmax,&           
       g1,g2,g3,g4,&    
       g5,g6,s1,s2,&    
       tc,&             
       tc2              
  INTEGER:: gsModel     
END MODULE resist


MODULE moist
  IMPLICIT NONE

  REAL (KIND(1d0))::avcp,&        
       dens_dry,&    
       dq,&          
       Ea_hPa,&      
       Es_hPa,&      
       lv_J_kg,&     
       tlv,&         
                                
       psyc_hPa,&    
       psycIce_hPa,& 
       s_Pa,&        
       s_hpa,&       
       sIce_hpa,&    
       vpd_hPa,&     
       vpd_pa,&      
       waterDens=999.8395 

END MODULE moist


MODULE gis_data
  IMPLICIT NONE

  REAL(KIND(1d0)):: areaunir,&                   
       areair,&                     
       bldgH,&                      
       FAIbldg,&                    
       FAItree,&                    
       FAIEveTree,&                    
       FAIDecTree,&                    
       grassfractionirrigated,&     
       pavedfractionirrigated,&     
       TreeH,&                      
       EveTreeH,&                     
       DecTreeH,&                     
       treefractionirrigated,&      
       veg_fr,&                     
                                
       VegFraction, &               
       ImpervFraction,&             
       PervFraction,&               
       NonWaterFraction,&           
       areaZh                       

  INTEGER:: idgis,&      
       itgis,&      
       Veg_type=1    

END MODULE gis_data


MODULE sues_data
  IMPLICIT NONE

  INTEGER:: tstep,&    
       nsh,&      
       nsd,&      
       nsdorig,&  
       t_interval,&   
       Nper, NperESTM   

  REAL(KIND(1d0)):: nsh_real,&   
       tstep_real,&   
       Nper_real, NperESTM_real   

  REAL(KIND(1d0)):: halftimestep   

  
  INTEGER:: StabilityMethod,&   
       RoughLenHeatMethod     


  INTEGER:: in
  INTEGER:: is      

  
  INTEGER::AerodynamicResistanceMethod=2 

  INTEGER::Ie_start,&   
       Ie_end       

  REAL(KIND(1d0)),DIMENSION(2):: SurplusEvap 
  

  
  REAL (KIND(1d0))::FlowChange,&         
       PipeCapacity,&       
       RunoffToWater,&      
       SmCap,&              
       SoilDensity,&        
       SoilDepthMeas,&      
       SoilRocks,&          
       SurfaceArea,&        
       SurfaceArea_ha,&     
       WaterBodyType,&      
       WaterStorCap,&       
       WUAreaEveTr_m2,&     
       WUAreaDecTr_m2,&     
       WUAreaGrass_m2,&     
       WUAreaTotal_m2,&     
       wu_EveTr,&              
       wu_DecTr,&              
       wu_Grass                

  
  REAL (KIND(1d0))::AdditionalWater,&     
       ch_per_interval,&     
       chSnow_per_interval,& 
       dI_dt,&               
       dr_per_interval,&     
       ev_per_interval,&     
       surf_chang_per_tstep,& 
       tot_chang_per_tstep,&  
       NWstate_per_tstep,&     
       state_per_tstep,&     
       drain_per_tstep,&     
       runoff_per_tstep,&     
       runoffSoil_per_tstep,& 
       ev_per_tstep,&     
       qe_per_tstep,&     
       p_mm,&                
       pin,&                 
       planF,&               
       rb,&                  
                                
       runoffAGimpervious,&     
       runoffAGveg,&            
       runoffWaterBody,&        
       runoffPipes,&            
       runoffAGimpervious_m3,&  
       runoffAGveg_m3,&         
       runoffWaterBody_m3,&     
       runoffPipes_m3,&         
       runoff_per_interval,&  
       addImpervious,&      
       addVeg,&             
       addWaterbody,&       
       addPipes,&           

       runoffSoil_per_interval,&
       qe_per_interval,&     
       SoilMoistCap,&
       SoilState,&        
       st_per_interval,&
       surplusWaterBody,&  
       tlv_sub,&
       overuse=0,&
       Zh               

  
  REAL (KIND(1d0))::H,&          
       l_mod,&      
       psim,&       
       psyh,&       
       RA,&         
       RAsnow,&     
       tstar,&      
       UStar,&      
       z0_gis       

  
  REAL (KIND(1d0))::resistsurf,& 
       gdq,&        
       qnm,&        
       gq,&         
       gtemp,&      
       gl,&         
       sdp,&        
       smd,&        
       vsmd,&       
       gs,&         
       gsc,&        
       rss          

  
  REAL (KIND(1d0))::  vdrc,&     
       numPM,&    
       sp,&       
       sae,&      
       ev,&       
       rst,&      
       qeph,&     
       qeOut      

  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: qhforCBL, qeforCBL   
  INTEGER:: qh_choice        

  
  REAL (KIND(1d0)):: ext_wu,&         
       Faut,&           
       int_wu,&         
       IrrFracConif,&  
       IrrFracDecid,&  
       IrrFracGrass,&  
       InternalWaterUse_h 

  
  REAL(KIND(1d0)),DIMENSION(7)::DayWatPer,&  
       DayWat       
  REAL(KIND(1d0)),DIMENSION(0:23,2):: WUProfM,&   
       WUProfA   


  REAL (KIND(1d0)),DIMENSION(3)::Ie_a,Ie_m   

END MODULE sues_data



MODULE VegPhenogy
  IMPLICIT NONE
  REAL (KIND(1d0)):: VegPhenLumps,deltaLAI
END MODULE VegPhenogy

MODULE filename
  CHARACTER (len=90)::  smithfile     
END MODULE filename


MODULE InitialCond

  REAL (KIND(1d0))::LAIinitialEveTr,&
       LAIinitialDecTr,&
       LAIinitialGrass,&
       porosity0,&
       DecidCap0,&
       albDecTr0,&
       albEveTr0,&
       albGrass0,&
       Temp_C0,&
       GDD_1_0,&
       GDD_2_0,&
       SoilStorePavedState,&
       SoilStoreBldgsState,&
       SoilStoreEveTrState,&
       SoilStoreDecTrstate,&
       SoilStoreGrassState,&
       SoilStoreBSoilState,&
       SnowWaterPavedState,&
       SnowWaterBldgsState,&
       SnowWaterEveTrState,&
       SnowWaterDecTrState,&
       SnowWaterGrassState,&
       SnowWaterBSoilState,&
       SnowWaterWaterstate,&
       SnowPackPaved,&
       SnowPackBldgs,&
       SnowPackEveTr,&
       SnowPackDecTr,&
       SnowPackGrass,&
       SnowPackBSoil,&
       SnowPackWater,&
       SnowAlb0
       

  INTEGER::ID_Prev

END MODULE InitialCond






MODULE ColNamesModelDailyState

  IMPLICIT NONE

  

  INTEGER:: cMDS_id_prev    = 3, &
       cMDS_HDD1            = 4, &
       cMDS_HDD2            = 5, &
       cMDS_TempC           = 6, &
       cMDS_TempCRM         = 7, &
       cMDS_Precip          = 8, &
       cMDS_DaysSinceRain   = 9, &
       cMDS_TempCOld1       = 10,&
       cMDS_TempCOld2       = 11,&
       cMDS_TempCOld3       = 12,&
       cMDS_GDDMin          = 13,&
       cMDS_GDDMax          = 14,&
       cMDS_GDD1_0          = 15,&
       cMDS_GDD2_0          = 16,&
       cMDS_LAIInitialEveTr = 17,&
       cMDS_LAIInitialDecTr = 18,&
       cMDS_LAIInitialGrass = 19,&
       cMDS_porosity        = 20,&
       cMDS_albEveTr        = 21,&
       cMDS_albDecTr        = 22,&
       cMDS_albGrass        = 23,&
       cMDS_DecidCap        = 24,&
       cMDS_SnowfallCum     = 25,&
       cMDS_LAIEveTr        = 26,&
       cMDS_LAIDecTr        = 27,&
       cMDS_LAIGrass        = 28,&
       cMDS_SnowAlb         = 29,&
       cMDS_BoRatio         = 30,& 
       cMDS_a1AnOHM         = 31,& 
       cMDS_a2AnOHM         = 32,& 
       cMDS_a3AnOHM         = 33 


END MODULE ColNamesModelDailyState



MODULE ColNamesInputFiles

  IMPLICIT NONE

  INTEGER:: ccc    

  

  
  
  INTEGER::c_Grid = 1,&
       c_Year     = 2,&
       c_StartDLS = 3,&
       c_EndDLS   = 4,&
                                
       c_lat  = 5,&
       c_lng  = 6,&
       c_tz   = 7,&
       c_Area = 8,&
       c_Alt  = 9,&
       c_z    = 10,&
                                
       c_id   = 11,&
       c_it   = 12,&
       c_imin = 13,&
                                
       c_FrPaved = 14,&
       c_FrBldgs = 15,&
       c_FrEveTr = 16,&
       c_FrDecTr = 17,&
       c_FrGrass = 18,&
       c_FrBSoil = 19,&
       c_FrWater = 20,&
                                
       c_IrrEveTrFrac = 21,&
       c_IrrDecTrFrac = 22,&
       c_IrrGrassFrac = 23,&
                                
       c_HBldgs   = 24,&
       c_HEveTr   = 25,&
       c_HDecTr   = 26,&
       c_z0m      = 27,&
       c_zdm      = 28,&
       c_FAIBldgs = 29,&
       c_FAIEveTr = 30,&
       c_FAIDecTr = 31,&
                                
       c_PopDensDay   = 32,&
       c_PopDensNight = 33,&
       c_TrafficRate_WD  = 34,&    
       c_TrafficRate_WE  = 35,&    
       c_QF0_BEU_WD  = 36,&    
       c_QF0_BEU_WE = 37,&
                                
       c_PavedCode = 38,&  
       c_BldgsCode = 39,&  
       c_EveTrCode = 40,&  
       c_DecTrCode = 41,&    
       c_GrassCode = 42,&     
       c_BSoilCode = 43,&  
       c_WaterCode = 44,&       
                                
       c_LUMPSDr     = 45,&
       c_LUMPSCover  = 46,&
       c_LUMPSMaxRes = 47,&
                                
       c_NARPTrans   = 48,&
                                
       c_CondCode    = 49,&       
                                
       c_SnowCode    = 50,&    
                                
       c_SnowProfWD  = 51,&  
       c_SnowProfWE  = 52,&  
       c_QFCode      = 53,&  
       c_IrrCode     = 54,&  
       c_WProfManuWD = 55,&  
       c_WProfManuWE = 56,&  
       c_WProfAutoWD = 57,&  
       c_WProfAutoWE = 58,&  
                                
       c_FlowChange    =59,&  
       c_RunoffToWater =60,&    
       c_PipeCapacity  =61,&  
                                
       c_GridConnection1of8 = 62,&
       c_Fraction1of8       = 63,&
       c_GridConnection2of8 = 64,&
       c_Fraction2of8       = 65,&
       c_GridConnection3of8 = 66,&
       c_Fraction3of8       = 67,&
       c_GridConnection4of8 = 68,&
       c_Fraction4of8       = 69,&
       c_GridConnection5of8 = 70,&
       c_Fraction5of8       = 71,&
       c_GridConnection6of8 = 72,&
       c_Fraction6of8       = 73,&
       c_GridConnection7of8 = 74,&
       c_Fraction7of8       = 75,&
       c_GridConnection8of8 = 76,&
       c_Fraction8of8       = 77,&
                                
       c_WGPavedCode = 78,&   
       c_WGBldgsCode = 79,&   
       c_WGEveTrCode = 80,&   
       c_WGDecTrCode = 81,&   
       c_WGGrassCode = 82,&   
       c_WGBSoilCode = 83,&   
       c_WGWaterCode = 84,&   
                                
       c_AreaWall = 85   

  INTEGER,DIMENSION(3):: c_Fr_ESTMClass_Paved =   (/(ccc,ccc=86,88,1)/) 
  INTEGER,DIMENSION(3)::         c_Code_ESTMClass_Paved = (/(ccc,ccc=89,91,1)/) 
  INTEGER,DIMENSION(5):: c_Fr_ESTMClass_Bldgs =   (/(ccc,ccc=92,96,1)/) 
  INTEGER,DIMENSION(5)::         c_Code_ESTMClass_Bldgs = (/(ccc,ccc=97,101,1)/) 

  
  INTEGER :: ci_Code   = 1, &
       ci_AlbMin       = 2, &
       ci_AlbMax       = 3, &
       ci_Emis         = 4, &
       ci_StorMin      = 5, &
       ci_StorMax      = 6, &
       ci_WetThresh    = 7, &
       ci_StateLimit   = 8, &
       ci_DrEq         = 9, &
       ci_DrCoef1      = 10,&
       ci_DrCoef2      = 11,&
       ci_SoilTCode    = 12,&
       ci_SnowLimPat   = 13,&
       ci_SnowLimRem   = 14,&
       ci_OHMCode_SWet = 15,&
       ci_OHMCode_SDry = 16,&
       ci_OHMCode_WWet = 17,&
       ci_OHMCode_WDry = 18,&
       ci_OHMThresh_SW = 19,&
       ci_OHMThresh_WD = 20,&
       ci_ESTMCode     = 21,& 
       ci_CpAnOHM      = 22,& 
       ci_KkAnOHM      = 23,& 
       ci_ChAnOHM      = 24 

  
  INTEGER :: cp_Code   = 1, &
       cp_AlbMin       = 2, &
       cp_AlbMax       = 3, &
       cp_Emis         = 4, &
       cp_StorMin      = 5, &
       cp_StorMax      = 6, &
       cp_WetThresh    = 7, &
       cp_StateLimit   = 8, &
       cp_DrEq         = 9, &
       cp_DrCoef1      = 10,&
       cp_DrCoef2      = 11,&
       cp_SoilTCode    = 12,&
       cp_SnowLimPat   = 13,&
       cp_BaseT        = 14,&
       cp_BaseTe       = 15,&
       cp_GDDFull      = 16,&
       cp_SDDFull      = 17,&
       cp_LAIMin       = 18,&
       cp_LAIMax       = 19,&
       cp_PorosityMin  = 20,&
       cp_PorosityMax  = 21,&
       cp_GsMax        = 22,&
       cp_LAIEq        = 23,&
       cp_LeafGP1      = 24,&
       cp_LeafGP2      = 25,&
       cp_LeafOP1      = 26,&
       cp_LeafOP2      = 27,&
       cp_OHMCode_SWet = 28,&
       cp_OHMCode_SDry = 29,&
       cp_OHMCode_WWet = 30,&
       cp_OHMCode_WDry = 31,&
       cp_OHMThresh_SW = 32,&
       cp_OHMThresh_WD = 33,&
       cp_ESTMCode     = 34,&
       cp_CpAnOHM      = 35,& 
       cp_KkAnOHM      = 36,& 
       cp_ChAnOHM      = 37,& 
       cp_BiogenCO2Code = 38


  
  INTEGER :: cw_Code   = 1, &
       cw_AlbMin       = 2, &
       cw_AlbMax       = 3, &
       cw_Emis         = 4, &
       cw_StorMin      = 5, &
       cw_StorMax      = 6, &
       cw_WetThresh    = 7, &
       cw_StateLimit   = 8, &
       cw_WaterDepth   = 9, &
       cw_DrEq         = 10, &
       cw_DrCoef1      = 11,&
       cw_DrCoef2      = 12,&
       cw_OHMCode_SWet = 13,&
       cw_OHMCode_SDry = 14,&
       cw_OHMCode_WWet = 15,&
       cw_OHMCode_WDry = 16,&
       cw_OHMThresh_SW = 17,&
       cw_OHMThresh_WD = 18,&
       cw_ESTMCode     = 19,&
       cw_CpAnOHM      = 20,& 
       cw_KkAnOHM      = 21,& 
       cw_ChAnOHM      = 22 

  
  INTEGER :: cs_Code   = 1, &
       cs_SnowRMFactor = 2, &
       cs_SnowTMFactor = 3, &
       cs_SnowAlbMin   = 4, &
       cs_SnowAlbMax   = 5, &
       cs_SnowEmis     = 6, &
       cs_Snowtau_a    = 7, &
       cs_Snowtau_f    = 8, &
       cs_SnowPLimAlb  = 9, &
       cs_SnowSDMin    = 10,&
       cs_SnowSDMax    = 11,&
       cs_Snowtau_r    = 12,&
       cs_SnowCRWMin   = 13,&
       cs_SnowCRWMax   = 14,&
       cs_SnowPLimSnow = 15,&
       cs_OHMCode_SWet = 16,&
       cs_OHMCode_SDry = 17,&
       cs_OHMCode_WWet = 18,&
       cs_OHMCode_WDry = 19,&
       cs_OHMThresh_SW = 20,&
       cs_OHMThresh_WD = 21,&
       cs_ESTMCode     = 22,&
       cs_CpAnOHM      = 23,& 
       cs_KkAnOHM      = 24,& 
       cs_ChAnOHM      = 25 

  
  INTEGER :: cSo_Code        =  1,&
       cSo_SoilDepth   =  2,&
       cSo_SoilStCap   =  3,&
       cSo_KSat         =  4,&
       cSo_SoilDens    =  5,&
       cSo_SoilInfRate =  6,&
       cSo_ObsSMDepth  =  7,&
       cSo_ObsSMMax    =  8,&
       cSo_ObsSNRFrac  =  9

  
  INTEGER :: cc_Code   =  1,&
       cc_GsG1   =  2,&
       cc_GsG2   =  3,&
       cc_GsG3   =  4,&
       cc_GsG4   =  5,&
       cc_GsG5   =  6,&
       cc_GsG6   =  7,&
       cc_GsTH   =  8,&
       cc_GsTL   =  9,&
       cc_GsS1   =  10,&
       cc_GsS2   =  11,&
       cc_GsKmax =  12,&
       cc_gsModel = 13      

  
  INTEGER :: cO_Code = 1,&
       cO_a1  = 2,&
       cO_a2  = 3,&
       cO_a3  = 4

  
  INTEGER::cE_Code    = 1, &
       cE_Surf_thick1 = 2, &  
       cE_Surf_k1     = 3, &
       cE_Surf_rhoCp1 = 4, &
       cE_Surf_thick2 = 5, &
       cE_Surf_k2     = 6, &
       cE_Surf_rhoCp2 = 7, &
       cE_Surf_thick3 = 8, &
       cE_Surf_k3     = 9, &
       cE_Surf_rhoCp3 = 10, &
       cE_Surf_thick4 = 11, &
       cE_Surf_k4     = 12, &
       cE_Surf_rhoCp4 = 13, &
       cE_Surf_thick5 = 14, &
       cE_Surf_k5     = 15, &
       cE_Surf_rhoCp5 = 16, &
       cE_Wall_thick1 = 17, &   
       cE_Wall_k1     = 18, &
       cE_Wall_rhoCp1 = 19, &
       cE_Wall_thick2 = 20, &
       cE_Wall_k2     = 21, &
       cE_Wall_rhoCp2 = 22, &
       cE_Wall_thick3 = 23, &
       cE_Wall_k3     = 24, &
       cE_Wall_rhoCp3 = 25, &
       cE_Wall_thick4 = 26, &
       cE_Wall_k4     = 27, &
       cE_Wall_rhoCp4 = 28, &
       cE_Wall_thick5 = 29, &
       cE_Wall_k5     = 30, &
       cE_Wall_rhoCp5 = 31, &
       cE_Internal_thick1 = 32, &   
       cE_Internal_k1     = 33, &
       cE_Internal_rhoCp1 = 34, &
       cE_Internal_thick2 = 35, &
       cE_Internal_k2     = 36, &
       cE_Internal_rhoCp2 = 37, &
       cE_Internal_thick3 = 38, &
       cE_Internal_k3     = 39, &
       cE_Internal_rhoCp3 = 40, &
       cE_Internal_thick4 = 41, &
       cE_Internal_k4     = 42, &
       cE_Internal_rhoCp4 = 43, &
       cE_Internal_thick5 = 44, &
       cE_Internal_k5     = 45, &
       cE_Internal_rhoCp5 = 46, &
       cE_nroom    = 47,&
       cE_alb_ibld = 48,&
       cE_em_ibld  = 49,&
       cE_CH_iwall = 50,&
       cE_CH_iroof = 51,&
       cE_CH_ibld  = 52

  
  INTEGER ::   cA_Code    = 1,&
       cA_BaseTHDD               = 2,&
       cA_QF_A1                  = 3,&   
       cA_QF_B1                  = 4,&   
       cA_QF_C1                  = 5,&   
       cA_QF_A2                  = 6,&   
       cA_QF_B2                  = 7,&   
       cA_QF_C2                  = 8,&   
       cA_AHMin_WD               = 9,&   
       cA_AHMin_WE               = 10,&  
       cA_AHSlopeHeating_WD      = 11,&  
       cA_AHSlopeHeating_WE      = 12,&  
       cA_AHSlopeCooling_WD      = 13,&  
       cA_AHSlopeCooling_WE      = 14,&  
       cA_TCriticHeating_WD      = 15,&  
       cA_TCriticHeating_WE      = 16,&  
       cA_TCriticCooling_WD      = 17,&  
       cA_TCriticCooling_WE      = 18,&  
       cA_EnProfWD               = 19,&  
       cA_EnProfWE               = 20,&  
       cA_CO2mWD                 = 21,&  
       cA_CO2mWE                 = 22,&  
       cA_TraffProfWD            = 23,&  
       cA_TraffProfWE            = 24,&  
       cA_PopProfWD              = 25,&  
       cA_PopProfWE              = 26,&  
       cA_MinQFMetab             = 27,&
       cA_MaxQFMetab             = 28,&
       cA_FrFossilFuel_Heat      = 29,&
       cA_FrFossilFuel_NonHeat   = 30,&
       cA_EF_umolCO2perJ         = 31,&
       cA_EnEF_v_Jkm             = 32,&
       cA_FcEF_v_kgkm            = 33,&
       cA_TrafficUnits           = 34


  

  INTEGER ::  cIr_Code     = 1,&
       cIr_IeStart     = 2,&
       cIr_IeEnd       = 3,&
       cIr_IntWU       = 4,&
       cIr_Faut        = 5,&
       cIr_Ie_a1       = 6,&
       cIr_Ie_a2       = 7,&
       cIr_Ie_a3       = 8,&
       cIr_Ie_m1       = 9,&
       cIr_Ie_m2       = 10,&
       cIr_Ie_m3       = 11,&
       cIr_DayWat1     = 12,&
       cIr_DayWat2     = 13,&
       cIr_DayWat3     = 14,&
       cIr_DayWat4     = 15,&
       cIr_DayWat5     = 16,&
       cIr_DayWat6     = 17,&
       cIr_DayWat7     = 18,&
       cIr_DayWatPer1  = 19,&
       cIr_DayWatPer2  = 20,&
       cIr_DayWatPer3  = 21,&
       cIr_DayWatPer4  = 22,&
       cIr_DayWatPer5  = 23,&
       cIr_DayWatPer6  = 24,&
       cIr_DayWatPer7  = 25

  

  INTEGER:: cc   

  INTEGER:: cPr_Code = 1
  INTEGER,DIMENSION(24):: cPr_Hours = (/(cc, cc=2,25, 1)/)  

  

  INTEGER::   cWG_Code     = 1,&
       cWG_ToPaved     = 2,&
       cWG_ToBldgs     = 3,&
       cWG_ToEveTr     = 4,&
       cWG_ToDecTr     = 5,&
       cWG_ToGrass     = 6,&
       cWG_ToBSoil     = 7,&
       cWG_ToWater     = 8,&
       cWG_ToRunoff    = 9,&
       cWG_ToSoilStore = 10

  
  INTEGER ::   cB_Code    = 1,&
       cB_alpha           = 2,&
       cB_beta            = 3,&
       cB_theta           = 4,&
       cB_alpha_enh       = 5,&
       cB_beta_enh        = 6,&
       cB_resp_a          = 7,&
       cB_resp_b          = 8,&
       cB_min_r           = 9


END MODULE ColNamesInputFiles


MODULE ESTM_data 

  
  INTEGER ::        evolveTibld,&
       TsurfChoice,&
       ibldCHmod

  REAL(KIND(1d0)):: LBC_soil,&        
       THEAT_ON,&
       THEAT_OFF,&
       THEAT_fix,&
       ivf_iw,&    
       ivf_ir,&
       ivf_ii,&
       ivf_if,&
       ivf_ww,&    
       ivf_wr,&
       ivf_wi,&
       ivf_wf,&
       ivf_rw,&    
       ivf_ri,&
       ivf_rf,&
       ivf_fw,&    
       ivf_fr,&
       ivf_fi


  
  INTEGER:: Nibld,&           
       Nwall,&           
       Nroof,&           
       Nground           

  REAL(KIND(1d0)),DIMENSION(5):: zibld,&    
       zwall,&    
       zroof,&    
       zground,&  
       kibld,&    
       kwall,&    
       kroof,&    
       kground,&  
       ribld,&    
       rwall,&    
       rroof,&    
       rground    

  
  
  REAL(KIND(1d0)),DIMENSION(5,3):: zSurf_Paved
  REAL(KIND(1d0)),DIMENSION(5,3):: kSurf_Paved
  REAL(KIND(1d0)),DIMENSION(5,3):: rSurf_Paved
  
  REAL(KIND(1d0)),DIMENSION(5,5):: zSurf_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: kSurf_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: rSurf_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: zwall_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: kwall_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: rwall_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: zibld_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: kibld_Bldgs
  REAL(KIND(1d0)),DIMENSION(5,5):: ribld_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: nroom_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: alb_ibld_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: em_ibld_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: CH_iwall_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: CH_iroof_Bldgs
  REAL(KIND(1d0)),DIMENSION(5):: CH_ibld_Bldgs

  REAL(KIND(1d0))::   nroom,&      
       alb_ibld,& 
       em_ibld,&  
       CH_iroof,& 
       CH_iwall,& 
       CH_ibld,&  
       fwall,&    
       AreaWall   

  
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:)::  Tibld,Twall,Troof,Tground
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:,:)::  Tw_4

  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:,:)  ::  Tibld_grids,Twall_grids,Troof_grids,Tground_grids
  REAL(KIND(1d0)),ALLOCATABLE,DIMENSION(:,:,:)::  Tw_4_grids

  
  REAL(KIND(1d0))                           ::  alb_avg,&
       alb_ground,&   
       alb_roof,&     
       alb_veg,&      
       CHAIR,&
       CHR,&
       em_ground,&    
       em_roof,&      
       em_veg,&       
       em_r,&         
       em_w,&         
       em_i,&         
       em_f,&         
       fair,&         
       fground,&      
       fibld,&        
       finternal,&    
       froof,&        
       fveg,&         
       HW,&           
       LUP_ground,&
       LUP_ROOF,&
       LUP_VEG,&
       LUP_WALL,&
       minshc_airbld,&
       Pcoeff(5),&
       Qsground,&     
       Qsroof,&       
       Qswall,&       
       Qs_4(4),&      
       Qsair,&        
       Qsibld,&       
       RVF_ground,&
       RVF_WALL,&
       RVF_ROOF,&
       RVF_CANYON,&
       RVF_VEG,&
       SHC_air,&
       SVF_ground,&   
       SVF_wall,&     
       SVF_roof,&     
       TANZENITH,&    
       Tair1,&
       Tair2,&
       Tairday,&      
       Tfloor,&
       Tievolve,&
       TN_roof,&
       TN_wall,&
       T0_wall,&
       T0_roof,&
       T0_ground,&
       T0_ibld,&
       WS,&           
       xvf_wall,&
       ZREF,&         
       zvf_ground,&   
       zvf_WALL     

  
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: Tair2_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: lup_ground_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: lup_wall_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: lup_roof_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: Tievolve_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: T0_wall_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: T0_roof_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: T0_ground_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: T0_ibld_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: TN_roof_grids
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE:: TN_wall_grids

  
  REAL(KIND(1d0)),DIMENSION(3):: ESTMsfr_Paved
  REAL(KIND(1d0)),DIMENSION(5):: ESTMsfr_Bldgs

  LOGICAL             ::bctype(2),&
       CFLfail=.FALSE.,&
       diagnoseTi=.FALSE.,&
       first,&
       HVAC=.FALSE.,&
       SPINDONE=.FALSE.

  REAL(KIND(1d0)),PARAMETER::alb_wall=0.23,em_wall=0.9  
  INTEGER, PARAMETER::        maxiter=100
  REAL(KIND(1d0)),PARAMETER:: conv=0.0001

  
  INTEGER             ::nalb,&
       nemis
  REAL(KIND(1d0))     ::sumalb,&
       sumemis

END MODULE ESTM_data


MODULE WhereWhen
  

  INTEGER(KIND(1d0)):: GridID   
  CHARACTER(LEN=10):: GridID_text 
  CHARACTER(LEN=15):: datetime  

END MODULE WhereWhen


MODULE MathConstants

  REAL (KIND(1d0)),PARAMETER ::pi=3.14159265359
  REAL (KIND(1d0)),PARAMETER ::dtr=0.0174532925, rtd=57.2957795

END MODULE MathConstants


MODULE PhysConstants

  REAL (KIND(1d0)),PARAMETER :: C2K = 273.15   
  REAL (KIND(1d0)),PARAMETER :: SBConst = 5.67051e-8   
  REAL (KIND(1d0)),PARAMETER :: JtoumolPAR = 4.6   
  REAL (KIND(1d0)),PARAMETER :: KdntoPAR = 0.46    

END MODULE PhysConstants


