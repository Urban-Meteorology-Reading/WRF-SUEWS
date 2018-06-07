MODULE ctrl_output
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  USE allocateArray
  USE cbl_module
  USE data_in
  USE defaultNotUsed
  USE ESTM_data
  USE gis_data
  USE initial
  USE solweig_module
  USE sues_data
  USE time
  USE strings

  IMPLICIT NONE


  INTEGER :: i

  CHARACTER(len=10),PARAMETER:: & 
       fy   = '(i0004,1X)',& 
       ft   = '(i0004,1X)',& 
       fd   = '(f08.4,1X)',& 
       f94  = '(f09.4,1X)',& 
       f104 = '(f10.4,1X)',& 
       f106 = '(f10.6,1X)',& 
       f146 = '(f14.6,1X)'   

  CHARACTER(len= 1),PARAMETER:: & 
       aT = 'T',&   
       aA = 'A',&   
       aS = 'S',&   
       aL = 'L'     

  CHARACTER(len= 3):: itext

  
  TYPE varAttr
     CHARACTER(len = 15) :: header 
     CHARACTER(len = 12) :: unit   
     CHARACTER(len = 14) :: fmt    
     CHARACTER(len = 50) :: longNm 
     CHARACTER(len = 1)  :: aggreg 
     CHARACTER(len = 10) :: group  
     INTEGER             :: level  
  END TYPE varAttr

  
  TYPE(varAttr) :: varList(300)

  
  DATA(varList(i), i=1,5)/&
       varAttr('Year'    , 'YYYY' , fy , 'Year'         , aT , 'datetime' , 0),&
       varAttr('DOY'     , 'DOY'  , ft , 'Day of Year'  , aT , 'datetime' , 0),&
       varAttr('Hour'    , 'HH'   , ft , 'Hour'         , aT , 'datetime' , 0),&
       varAttr('Min'     , 'MM'   , ft , 'Minute'       , aT , 'datetime' , 0),&
       varAttr('Dectime' , '-'    , fd , 'Decimal time' , aT , 'datetime' , 0)&
       /

  
  DATA(varList(i), i=5+1,ncolumnsDataOutSUEWS)/&
       varAttr('Kdown'      , 'W m-2'        , f104 , 'Incoming shortwave radiation'                     , aA , 'SUEWS' , 0)     , &
       varAttr('Kup'        , 'W m-2'        , f104 , 'Outgoing shortwave radiation'                     , aA , 'SUEWS' , 0)     , &
       varAttr('Ldown'      , 'W m-2'        , f104 , 'Incoming longwave radiation'                      , aA , 'SUEWS' , 0)     , &
       varAttr('Lup'        , 'W m-2'        , f104 , 'Outgoing longwave radiation'                      , aA , 'SUEWS' , 0)     , &
       varAttr('Tsurf'      , 'degC'         , f104 , 'Bulk surface temperature'                         , aA , 'SUEWS' , 0)     , &
       varAttr('QN'         , 'W m-2'        , f104 , 'Net all-wave radiation'                           , aA , 'SUEWS' , 0)     , &
       varAttr('QF'         , 'W m-2'        , f104 , 'Anthropogenic heat flux'                          , aA , 'SUEWS' , 0)     , &
       varAttr('QS'         , 'W m-2'        , f104 , 'Net storage heat flux'                            , aA , 'SUEWS' , 0)     , &
       varAttr('QH'         , 'W m-2'        , f104 , 'Sensible heat flux'                               , aA , 'SUEWS' , 0)     , &
       varAttr('QE'         , 'W m-2'        , f104 , 'Latent heat flux'                                 , aA , 'SUEWS' , 0)     , &
       varAttr('QHlumps'    , 'W m-2'        , f104 , 'Sensible heat flux (using LUMPS)'                 , aA , 'SUEWS' , 1)     , &
       varAttr('QElumps'    , 'W m-2'        , f104 , 'Latent heat flux (using LUMPS)'                   , aA , 'SUEWS' , 1)     , &
       varAttr('QHresis'    , 'W m-2'        , f104 , 'Sensible heat flux (resistance method)'           , aA , 'SUEWS' , 1)     , &
       varAttr('Rain'       , 'mm'           , f106 , 'Rain'                                             , aS , 'SUEWS' , 0)     , &
       varAttr('Irr'        , 'mm'           , f106 , 'Irrigation'                                       , aS , 'SUEWS' , 0)     , &
       varAttr('Evap'       , 'mm'           , f106 , 'Evaporation'                                      , aS , 'SUEWS' , 0)     , &
       varAttr('RO'         , 'mm'           , f106 , 'Runoff'                                           , aS , 'SUEWS' , 0)     , &
       varAttr('TotCh'      , 'mm'           , f106 , 'Surface and soil moisture change'                 , aS , 'SUEWS' , 0)     , &
       varAttr('SurfCh'     , 'mm'           , f106 , 'Surface moisture change'                          , aS , 'SUEWS' , 0)     , &
       varAttr('State'      , 'mm'           , f104 , 'Surface Wetness State'                            , aL , 'SUEWS' , 0)     , &
       varAttr('NWtrState'  , 'mm'           , f106 , 'Surface wetness state (non-water surfaces)'       , aL , 'SUEWS' , 0)     , &
       varAttr('Drainage'   , 'mm'           , f106 , 'Drainage'                                         , aS , 'SUEWS' , 0)     , &
       varAttr('SMD'        , 'mm'           , f94  , 'Soil Moisture Deficit'                            , aL , 'SUEWS' , 0)     , &
       varAttr('FlowCh'     , 'mm'           , f104 , 'Additional flow into water body'                  , aS , 'SUEWS' , 1)     , &
       varAttr('AddWater'   , 'mm'           , f104 , 'Addtional water from other grids'                 , aS , 'SUEWS' , 1)     , &
       varAttr('ROSoil'     , 'mm'           , f106 , 'Runoff to soil'                                   , aS , 'SUEWS' , 1)     , &
       varAttr('ROPipe'     , 'mm'           , f106 , 'Runoff to pipes'                                  , aS , 'SUEWS' , 1)     , &
       varAttr('ROImp'      , 'mm'           , f106 , 'Runoff over impervious surfaces'                  , aS , 'SUEWS' , 1)     , &
       varAttr('ROVeg'      , 'mm'           , f106 , 'Runoff over vegetated surfaces'                   , aS , 'SUEWS' , 1)     , &
       varAttr('ROWater'    , 'mm'           , f106 , 'Runoff for water surface'                         , aS , 'SUEWS' , 1)     , &
       varAttr('WUInt'      , 'mm'           , f94  , 'InternalWaterUse'                                 , aS , 'SUEWS' , 1)     , &
       varAttr('WUEveTr'    , 'mm'           , f94  , 'Water use for evergreen trees'                    , aS , 'SUEWS' , 1)     , &
       varAttr('WUDecTr'    , 'mm'           , f94  , 'Water use for deciduous trees'                    , aS , 'SUEWS' , 1)     , &
       varAttr('WUGrass'    , 'mm'           , f94  , 'Water use for grass'                              , aS , 'SUEWS' , 1)     , &
       varAttr('SMDPaved'   , 'mm'           , f94  , 'Soil moisture deficit for paved surface'          , aL , 'SUEWS' , 1)     , &
       varAttr('SMDBldgs'   , 'mm'           , f94  , 'Soil moisture deficit for building surface'       , aL , 'SUEWS' , 1)     , &
       varAttr('SMDEveTr'   , 'mm'           , f94  , 'Soil moisture deficit for evergreen tree surface' , aL , 'SUEWS' , 1)     , &
       varAttr('SMDDecTr'   , 'mm'           , f94  , 'Soil moisture deficit for deciduous tree surface' , aL , 'SUEWS' , 1)     , &
       varAttr('SMDGrass'   , 'mm'           , f94  , 'Soil moisture deficit for grass surface'          , aL , 'SUEWS' , 1)     , &
       varAttr('SMDBSoil'   , 'mm'           , f94  , 'Soil moisture deficit for bare soil surface'      , aL , 'SUEWS' , 1)     , &
       varAttr('StPaved'    , 'mm'           , f94  , 'Surface wetness state for paved surface'          , aL , 'SUEWS' , 1)     , &
       varAttr('StBldgs'    , 'mm'           , f94  , 'Surface wetness state for building surface'       , aL , 'SUEWS' , 1)     , &
       varAttr('StEveTr'    , 'mm'           , f94  , 'Surface wetness state for evergreen tree surface' , aL , 'SUEWS' , 1)     , &
       varAttr('StDecTr'    , 'mm'           , f94  , 'Surface wetness state for deciduous tree surface' , aL , 'SUEWS' , 1)     , &
       varAttr('StGrass'    , 'mm'           , f94  , 'Surface wetness state for grass surface'          , aL , 'SUEWS' , 1)     , &
       varAttr('StBSoil'    , 'mm'           , f94  , 'Surface wetness state for bare soil surface'      , aL , 'SUEWS' , 1)     , &
       varAttr('StWater'    , 'mm'           , f104 , 'Surface wetness state for water surface'          , aL , 'SUEWS' , 1)     , &
       varAttr('Zenith'     , 'degree'       , f94  , 'Solar zenith angle'                               , aL , 'SUEWS' , 0)     , &
       varAttr('Azimuth'    , 'degree'       , f94  , 'Solar azimuth angle'                              , aL , 'SUEWS' , 0)     , &
       varAttr('AlbBulk'    , '1'            , f94  , 'Bulk albedo'                                      , aA , 'SUEWS' , 0)     , &
       varAttr('Fcld'       , '1'            , f94  , 'Cloud fraction'                                   , aA , 'SUEWS' , 0)     , &
       varAttr('LAI'        , 'm2 m-2'       , f94  , 'Leaf area index'                                  , aA , 'SUEWS' , 0)     , &
       varAttr('z0m'        , 'm'            , f94  , 'Roughness length for momentum'                    , aA , 'SUEWS' , 1)     , &
       varAttr('zdm'        , 'm'            , f94  , 'Zero-plane displacement height'                   , aA , 'SUEWS' , 1)     , &
       varAttr('UStar'      , 'm s-1'        , f94  , 'Friction velocity'                                , aA , 'SUEWS' , 0)     , &
       varAttr('Lob'        , 'm'            , f104 , 'Obukhov length'                                   , aA , 'SUEWS' , 0)     , &
       varAttr('ra'         , 's m-1'        , f94  , 'Aerodynamic resistance'                           , aA , 'SUEWS' , 1)     , &
       varAttr('rs'         , 's m-1'        , f94  , 'Surface resistance'                               , aA , 'SUEWS' , 1)     , &
       varAttr('Fc'         , 'umol m-2 s-1' , f94  , 'CO2 flux'                                         , aA , 'SUEWS' , 0)     , &
       varAttr('FcPhoto'    , 'umol m-2 s-1' , f94  , 'CO2 flux from photosynthesis'                     , aA , 'SUEWS' , 1)     , &
       varAttr('FcRespi'    , 'umol m-2 s-1' , f94  , 'CO2 flux from respiration'                        , aA , 'SUEWS' , 1)     , &
       varAttr('FcMetab'    , 'umol m-2 s-1' , f94  , 'CO2 flux from metabolism'                         , aA , 'SUEWS' , 1)     , &
       varAttr('FcTraff'    , 'umol m-2 s-1' , f94  , 'CO2 flux from traffic'                            , aA , 'SUEWS' , 1)     , &
       varAttr('FcBuild'    , 'umol m-2 s-1' , f94  , 'CO2 flux from buildings'                          , aA , 'SUEWS' , 1)     , &
       varAttr('QNSnowFr'   , 'W m-2'        , f94  , 'Net all-wave radiation for non-snow area'         , aA , 'SUEWS' , 2)     , &
       varAttr('QNSnow'     , 'W m-2'        , f94  , 'Net all-wave radiation for snow area'             , aA , 'SUEWS' , 2)     , &
       varAttr('AlbSnow'    , '-'            , f94  , 'Snow albedo'                                      , aA , 'SUEWS' , 2)     , &
       varAttr('QM'         , 'W m-2'        , f106 , 'Snow-related heat exchange'                       , aA , 'SUEWS' , 2)     , &
       varAttr('QMFreeze'   , 'W m-2'        , f106 , 'Internal energy change'                           , aA , 'SUEWS' , 2)     , &
       varAttr('QMRain'     , 'W m-2'        , f106 , 'Heat released by rain on snow'                    , aA , 'SUEWS' , 2)     , &
       varAttr('SWE'        , 'mm'           , f104 , 'Snow water equivalent'                            , aA , 'SUEWS' , 2)     , &
       varAttr('MeltWater'  , 'mm'           , f104 , 'Meltwater'                                        , aA , 'SUEWS' , 2)     , &
       varAttr('MeltWStore' , 'mm'           , f104 , 'Meltwater store'                                  , aA , 'SUEWS' , 2)     , &
       varAttr('SnowCh'     , 'mm'           , f104 , 'Change in snow pack'                              , aA , 'SUEWS' , 2)     , &
       varAttr('SnowRPaved' , 'mm'           , f94  , 'Snow removed from paved surface'                  , aS , 'SUEWS' , 2)     , &
       varAttr('SnowRBldg'  , 'mm'           , f94  , 'Snow removed from building surface'               , aS , 'SUEWS' , 2)     , &
       varAttr('T2'         , 'degC'         , f94  , 'Air temperature at 2 m'                           , aA , 'SUEWS' , 0)     , &
       varAttr('Q2'         , 'g kg-1'       , f94  , 'Specific humidity at 2 m'                         , aA , 'SUEWS' , 0)     , &
       varAttr('U10'        , 'm s-1'        , f94  , 'Wind speed at 10 m'                               , aA , 'SUEWS' , 0)   &
       /

  
  DATA(varList(i), i=84+1,84+ncolumnsdataOutSOL-5)/&
       varAttr('azimuth'    , 'to_add' , f106 , 'azimuth'    , aA , 'SOLWEIG' , 0)  , &
       varAttr('altitude'   , 'to_add' , f106 , 'altitude'   , aA , 'SOLWEIG' , 0)  , &
       varAttr('GlobalRad'  , 'to_add' , f106 , 'GlobalRad'  , aA , 'SOLWEIG' , 0)  , &
       varAttr('DiffuseRad' , 'to_add' , f106 , 'DiffuseRad' , aA , 'SOLWEIG' , 0)  , &
       varAttr('DirectRad'  , 'to_add' , f106 , 'DirectRad'  , aA , 'SOLWEIG' , 0)  , &
       varAttr('Kdown2d'    , 'to_add' , f106 , 'Kdown2d'    , aA , 'SOLWEIG' , 0)  , &
       varAttr('Kup2d'      , 'to_add' , f106 , 'Kup2d'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Ksouth'     , 'to_add' , f106 , 'Ksouth'     , aA , 'SOLWEIG' , 0)  , &
       varAttr('Kwest'      , 'to_add' , f106 , 'Kwest'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Knorth'     , 'to_add' , f106 , 'Knorth'     , aA , 'SOLWEIG' , 0)  , &
       varAttr('Keast'      , 'to_add' , f106 , 'Keast'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Ldown2d'    , 'to_add' , f106 , 'Ldown2d'    , aA , 'SOLWEIG' , 0)  , &
       varAttr('Lup2d'      , 'to_add' , f106 , 'Lup2d'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Lsouth'     , 'to_add' , f106 , 'Lsouth'     , aA , 'SOLWEIG' , 0)  , &
       varAttr('Lwest'      , 'to_add' , f106 , 'Lwest'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Lnorth'     , 'to_add' , f106 , 'Lnorth'     , aA , 'SOLWEIG' , 0)  , &
       varAttr('Least'      , 'to_add' , f106 , 'Least'      , aA , 'SOLWEIG' , 0)  , &
       varAttr('Tmrt'       , 'to_add' , f106 , 'Tmrt'       , aA , 'SOLWEIG' , 0)  , &
       varAttr('I0'         , 'to_add' , f106 , 'I0'         , aA , 'SOLWEIG' , 0)  , &
       varAttr('CI'         , 'to_add' , f106 , 'CI'         , aA , 'SOLWEIG' , 0)  , &
       varAttr('gvf'        , 'to_add' , f106 , 'gvf'        , aA , 'SOLWEIG' , 0)  , &
       varAttr('shadow'     , 'to_add' , f106 , 'shadow'     , aA , 'SOLWEIG' , 0)  , &
       varAttr('svf'        , 'to_add' , f106 , 'svf'        , aA , 'SOLWEIG' , 0)  , &
       varAttr('svfbuveg'   , 'to_add' , f106 , 'svfbuveg'   , aA , 'SOLWEIG' , 0)  , &
       varAttr('Ta'         , 'to_add' , f106 , 'Ta'         , aA , 'SOLWEIG' , 0)  , &
       varAttr('Tg'         , 'to_add' , f106 , 'Tg'         , aA , 'SOLWEIG' , 0)&
       /

  
  DATA(varList(i), i=110+1,110+ncolumnsdataOutBL-5)/&
       varAttr('z'         , 'to_add' , f104 , 'z'         , aA , 'BL' , 0)  , &
       varAttr('theta'     , 'to_add' , f104 , 'theta'     , aA , 'BL' , 0)  , &
       varAttr('q'         , 'to_add' , f104 , 'q'         , aA , 'BL' , 0)  , &
       varAttr('theta+'    , 'to_add' , f104 , 'theta+'    , aA , 'BL' , 0)  , &
       varAttr('q+'        , 'to_add' , f104 , 'q+'        , aA , 'BL' , 0)  , &
       varAttr('Temp_C'    , 'to_add' , f104 , 'Temp_C'    , aA , 'BL' , 0)  , &
       varAttr('rh'        , 'to_add' , f104 , 'rh'        , aA , 'BL' , 0)  , &
       varAttr('QH_use'    , 'to_add' , f104 , 'QH_use'    , aA , 'BL' , 0)  , &
       varAttr('QE_use'    , 'to_add' , f104 , 'QE_use'    , aA , 'BL' , 0)  , &
       varAttr('Press_hPa' , 'to_add' , f104 , 'Press_hPa' , aA , 'BL' , 0)  , &
       varAttr('avu1'      , 'to_add' , f104 , 'avu1'      , aA , 'BL' , 0)  , &
       varAttr('UStar'     , 'to_add' , f104 , 'UStar'     , aA , 'BL' , 0)  , &
       varAttr('avdens'    , 'to_add' , f104 , 'avdens'    , aA , 'BL' , 0)  , &
       varAttr('lv_J_kg'   , 'to_add' , f146 , 'lv_J_kg'   , aA , 'BL' , 0)  , &
       varAttr('avcp'      , 'to_add' , f104 , 'avcp'      , aA , 'BL' , 0)  , &
       varAttr('gamt'      , 'to_add' , f104 , 'gamt'      , aA , 'BL' , 0)  , &
       varAttr('gamq'      , 'to_add' , f104 , 'gamq'      , aA , 'BL' , 0)&
       /

  
  DATA(varList(i), i=127+1,127+ncolumnsDataOutSnow-5)/&
       varAttr('SWE_Paved'      , 'to_add' , f106 , 'SWE_Paved'      , aA , 'snow' , 0)  , &
       varAttr('SWE_Bldgs'      , 'to_add' , f106 , 'SWE_Bldgs'      , aA , 'snow' , 0)  , &
       varAttr('SWE_EveTr'      , 'to_add' , f106 , 'SWE_EveTr'      , aA , 'snow' , 0)  , &
       varAttr('SWE_DecTr'      , 'to_add' , f106 , 'SWE_DecTr'      , aA , 'snow' , 0)  , &
       varAttr('SWE_Grass'      , 'to_add' , f106 , 'SWE_Grass'      , aA , 'snow' , 0)  , &
       varAttr('SWE_BSoil'      , 'to_add' , f106 , 'SWE_BSoil'      , aA , 'snow' , 0)  , &
       varAttr('SWE_Water'      , 'to_add' , f106 , 'SWE_Water'      , aA , 'snow' , 0)  , &
       varAttr('Mw_Paved'       , 'to_add' , f106 , 'Mw_Paved'       , aA , 'snow' , 0)  , &
       varAttr('Mw_Bldgs'       , 'to_add' , f106 , 'Mw_Bldgs'       , aA , 'snow' , 0)  , &
       varAttr('Mw_EveTr'       , 'to_add' , f106 , 'Mw_EveTr'       , aA , 'snow' , 0)  , &
       varAttr('Mw_DecTr'       , 'to_add' , f106 , 'Mw_DecTr'       , aA , 'snow' , 0)  , &
       varAttr('Mw_Grass'       , 'to_add' , f106 , 'Mw_Grass'       , aA , 'snow' , 0)  , &
       varAttr('Mw_BSoil'       , 'to_add' , f106 , 'Mw_BSoil'       , aA , 'snow' , 0)  , &
       varAttr('Mw_Water'       , 'to_add' , f106 , 'Mw_Water'       , aA , 'snow' , 0)  , &
       varAttr('Qm_Paved'       , 'to_add' , f106 , 'Qm_Paved'       , aA , 'snow' , 0)  , &
       varAttr('Qm_Bldgs'       , 'to_add' , f106 , 'Qm_Bldgs'       , aA , 'snow' , 0)  , &
       varAttr('Qm_EveTr'       , 'to_add' , f106 , 'Qm_EveTr'       , aA , 'snow' , 0)  , &
       varAttr('Qm_DecTr'       , 'to_add' , f106 , 'Qm_DecTr'       , aA , 'snow' , 0)  , &
       varAttr('Qm_Grass'       , 'to_add' , f106 , 'Qm_Grass'       , aA , 'snow' , 0)  , &
       varAttr('Qm_BSoil'       , 'to_add' , f106 , 'Qm_BSoil'       , aA , 'snow' , 0)  , &
       varAttr('Qm_Water'       , 'to_add' , f106 , 'Qm_Water'       , aA , 'snow' , 0)  , &
       varAttr('Qa_Paved'       , 'to_add' , f106 , 'Qa_Paved'       , aA , 'snow' , 0)  , &
       varAttr('Qa_Bldgs'       , 'to_add' , f106 , 'Qa_Bldgs'       , aA , 'snow' , 0)  , &
       varAttr('Qa_EveTr'       , 'to_add' , f106 , 'Qa_EveTr'       , aA , 'snow' , 0)  , &
       varAttr('Qa_DecTr'       , 'to_add' , f106 , 'Qa_DecTr'       , aA , 'snow' , 0)  , &
       varAttr('Qa_Grass'       , 'to_add' , f106 , 'Qa_Grass'       , aA , 'snow' , 0)  , &
       varAttr('Qa_BSoil'       , 'to_add' , f106 , 'Qa_BSoil'       , aA , 'snow' , 0)  , &
       varAttr('Qa_Water'       , 'to_add' , f106 , 'Qa_Water'       , aA , 'snow' , 0)  , &
       varAttr('QmFr_Paved'     , 'to_add' , f106 , 'QmFr_Paved'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_Bldgs'     , 'to_add' , f106 , 'QmFr_Bldgs'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_EveTr'     , 'to_add' , f106 , 'QmFr_EveTr'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_DecTr'     , 'to_add' , f106 , 'QmFr_DecTr'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_Grass'     , 'to_add' , f106 , 'QmFr_Grass'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_BSoil'     , 'to_add' , f106 , 'QmFr_BSoil'     , aA , 'snow' , 0)  , &
       varAttr('QmFr_Water'     , 'to_add' , f106 , 'QmFr_Water'     , aA , 'snow' , 0)  , &
       varAttr('fr_Paved'       , 'to_add' , f106 , 'fr_Paved'       , aA , 'snow' , 0)  , &
       varAttr('fr_Bldgs'       , 'to_add' , f106 , 'fr_Bldgs'       , aA , 'snow' , 0)  , &
       varAttr('fr_EveTr'       , 'to_add' , f106 , 'fr_EveTr'       , aA , 'snow' , 0)  , &
       varAttr('fr_DecTr'       , 'to_add' , f106 , 'fr_DecTr'       , aA , 'snow' , 0)  , &
       varAttr('fr_Grass'       , 'to_add' , f106 , 'fr_Grass'       , aA , 'snow' , 0)  , &
       varAttr('fr_BSoil'       , 'to_add' , f106 , 'fr_BSoil'       , aA , 'snow' , 0)  , &
       varAttr('RainSn_Paved'   , 'to_add' , f146 , 'RainSn_Paved'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_Bldgs'   , 'to_add' , f146 , 'RainSn_Bldgs'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_EveTr'   , 'to_add' , f146 , 'RainSn_EveTr'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_DecTr'   , 'to_add' , f146 , 'RainSn_DecTr'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_Grass'   , 'to_add' , f146 , 'RainSn_Grass'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_BSoil'   , 'to_add' , f146 , 'RainSn_BSoil'   , aA , 'snow' , 0)  , &
       varAttr('RainSn_Water'   , 'to_add' , f146 , 'RainSn_Water'   , aA , 'snow' , 0)  , &
       varAttr('Qn_PavedSnow'   , 'to_add' , f146 , 'Qn_PavedSnow'   , aA , 'snow' , 0)  , &
       varAttr('Qn_BldgsSnow'   , 'to_add' , f146 , 'Qn_BldgsSnow'   , aA , 'snow' , 0)  , &
       varAttr('Qn_EveTrSnpw'   , 'to_add' , f146 , 'Qn_EveTrSnpw'   , aA , 'snow' , 0)  , &
       varAttr('Qn_DecTrSnow'   , 'to_add' , f146 , 'Qn_DecTrSnow'   , aA , 'snow' , 0)  , &
       varAttr('Qn_GrassSnpw'   , 'to_add' , f146 , 'Qn_GrassSnpw'   , aA , 'snow' , 0)  , &
       varAttr('Qn_BSoilSnow'   , 'to_add' , f146 , 'Qn_BSoilSnow'   , aA , 'snow' , 0)  , &
       varAttr('Qn_WaterSnow'   , 'to_add' , f146 , 'Qn_WaterSnow'   , aA , 'snow' , 0)  , &
       varAttr('kup_PavedSnow'  , 'to_add' , f146 , 'kup_PavedSnow'  , aA , 'snow' , 0)  , &
       varAttr('kup_BldgsSnow'  , 'to_add' , f146 , 'kup_BldgsSnow'  , aA , 'snow' , 0)  , &
       varAttr('kup_EveTrSnpw'  , 'to_add' , f146 , 'kup_EveTrSnpw'  , aA , 'snow' , 0)  , &
       varAttr('kup_DecTrSnow'  , 'to_add' , f146 , 'kup_DecTrSnow'  , aA , 'snow' , 0)  , &
       varAttr('kup_GrassSnpw'  , 'to_add' , f146 , 'kup_GrassSnpw'  , aA , 'snow' , 0)  , &
       varAttr('kup_BSoilSnow'  , 'to_add' , f146 , 'kup_BSoilSnow'  , aA , 'snow' , 0)  , &
       varAttr('kup_WaterSnow'  , 'to_add' , f146 , 'kup_WaterSnow'  , aA , 'snow' , 0)  , &
       varAttr('frMelt_Paved'   , 'to_add' , f146 , 'frMelt_Paved'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_Bldgs'   , 'to_add' , f146 , 'frMelt_Bldgs'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_EveTr'   , 'to_add' , f146 , 'frMelt_EveTr'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_DecTr'   , 'to_add' , f146 , 'frMelt_DecTr'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_Grass'   , 'to_add' , f146 , 'frMelt_Grass'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_BSoil'   , 'to_add' , f146 , 'frMelt_BSoil'   , aA , 'snow' , 0)  , &
       varAttr('frMelt_Water'   , 'to_add' , f146 , 'frMelt_Water'   , aA , 'snow' , 0)  , &
       varAttr('MwStore_Paved'  , 'to_add' , f146 , 'MwStore_Paved'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_Bldgs'  , 'to_add' , f146 , 'MwStore_Bldgs'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_EveTr'  , 'to_add' , f146 , 'MwStore_EveTr'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_DecTr'  , 'to_add' , f146 , 'MwStore_DecTr'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_Grass'  , 'to_add' , f146 , 'MwStore_Grass'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_BSoil'  , 'to_add' , f146 , 'MwStore_BSoil'  , aA , 'snow' , 0)  , &
       varAttr('MwStore_Water'  , 'to_add' , f146 , 'MwStore_Water'  , aA , 'snow' , 0)  , &
       varAttr('SnowDens_Paved' , 'to_add' , f146 , 'SnowDens_Paved' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_Bldgs' , 'to_add' , f146 , 'SnowDens_Bldgs' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_EveTr' , 'to_add' , f146 , 'SnowDens_EveTr' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_DecTr' , 'to_add' , f146 , 'SnowDens_DecTr' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_Grass' , 'to_add' , f146 , 'SnowDens_Grass' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_BSoil' , 'to_add' , f146 , 'SnowDens_BSoil' , aA , 'snow' , 0)  , &
       varAttr('SnowDens_Water' , 'to_add' , f146 , 'SnowDens_Water' , aA , 'snow' , 0)  , &
       varAttr('Sd_Paved'       , 'to_add' , f106 , 'Sd_Paved'       , aA , 'snow' , 0)  , &
       varAttr('Sd_Bldgs'       , 'to_add' , f106 , 'Sd_Bldgs'       , aA , 'snow' , 0)  , &
       varAttr('Sd_EveTr'       , 'to_add' , f106 , 'Sd_EveTr'       , aA , 'snow' , 0)  , &
       varAttr('Sd_DecTr'       , 'to_add' , f106 , 'Sd_DecTr'       , aA , 'snow' , 0)  , &
       varAttr('Sd_Grass'       , 'to_add' , f106 , 'Sd_Grass'       , aA , 'snow' , 0)  , &
       varAttr('Sd_BSoil'       , 'to_add' , f106 , 'Sd_BSoil'       , aA , 'snow' , 0)  , &
       varAttr('Sd_Water'       , 'to_add' , f106 , 'Sd_Water'       , aA , 'snow' , 0)  , &
       varAttr('Tsnow_Paved'    , 'to_add' , f146 , 'Tsnow_Paved'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_Bldgs'    , 'to_add' , f146 , 'Tsnow_Bldgs'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_EveTr'    , 'to_add' , f146 , 'Tsnow_EveTr'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_DecTr'    , 'to_add' , f146 , 'Tsnow_DecTr'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_Grass'    , 'to_add' , f146 , 'Tsnow_Grass'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_BSoil'    , 'to_add' , f146 , 'Tsnow_BSoil'    , aA , 'snow' , 0)  , &
       varAttr('Tsnow_Water'    , 'to_add' , f146 , 'Tsnow_Water'    , aA , 'snow' , 0)&
       /

  
  DATA(varList(i), i=224+1,224+ncolumnsDataOutESTM-5)/&
       varAttr('QS'       , 'W m-2' , f104 , 'Total Storage'                            , aA , 'ESTM' , 0) , &
       varAttr('QSAir'    , 'W m-2' , f104 , 'Storage air'                              , aA , 'ESTM' , 0) , &
       varAttr('QSWall'   , 'W m-2' , f104 , 'Storage Wall'                             , aA , 'ESTM' , 0) , &
       varAttr('QSRoof'   , 'W m-2' , f104 , 'Storage Roof'                             , aA , 'ESTM' , 0) , &
       varAttr('QSGround' , 'W m-2' , f104 , 'Storage Ground'                           , aA , 'ESTM' , 0) , &
       varAttr('QSIBld'   , 'W m-2' , f104 , 'Storage Internal building'                , aA , 'ESTM' , 0) , &
       varAttr('TWALL1'   , 'degK'  , f104 , 'Temperature in wall layer 1'              , aA , 'ESTM' , 0) , &
       varAttr('TWALL2'   , 'degK'  , f104 , 'Temperature in wall layer 2'              , aA , 'ESTM' , 0) , &
       varAttr('TWALL3'   , 'degK'  , f104 , 'Temperature in wall layer 3'              , aA , 'ESTM' , 0) , &
       varAttr('TWALL4'   , 'degK'  , f104 , 'Temperature in wall layer 4'              , aA , 'ESTM' , 0) , &
       varAttr('TWALL5'   , 'degK'  , f104 , 'Temperature in wall layer 5'              , aA , 'ESTM' , 0) , &
       varAttr('TROOF1'   , 'degK'  , f104 , 'Temperature in roof layer 1'              , aA , 'ESTM' , 0) , &
       varAttr('TROOF2'   , 'degK'  , f104 , 'Temperature in roof layer 2'              , aA , 'ESTM' , 0) , &
       varAttr('TROOF3'   , 'degK'  , f104 , 'Temperature in roof layer 3'              , aA , 'ESTM' , 0) , &
       varAttr('TROOF4'   , 'degK'  , f104 , 'Temperature in roof layer 4'              , aA , 'ESTM' , 0) , &
       varAttr('TROOF5'   , 'degK'  , f104 , 'Temperature in roof layer 5'              , aA , 'ESTM' , 0) , &
       varAttr('TGROUND1' , 'degK'  , f104 , 'Temperature in ground layer 1'            , aA , 'ESTM' , 0) , &
       varAttr('TGROUND2' , 'degK'  , f104 , 'Temperature in ground layer 2'            , aA , 'ESTM' , 0) , &
       varAttr('TGROUND3' , 'degK'  , f104 , 'Temperature in ground layer 3'            , aA , 'ESTM' , 0) , &
       varAttr('TGROUND4' , 'degK'  , f104 , 'Temperature in ground layer 4'            , aA , 'ESTM' , 0) , &
       varAttr('TGROUND5' , 'degK'  , f104 , 'Temperature in ground layer 5'            , aA , 'ESTM' , 0) , &
       varAttr('TiBLD1'   , 'degK'  , f104 , 'Temperature in internal building layer 1' , aA , 'ESTM' , 0) , &
       varAttr('TiBLD2'   , 'degK'  , f104 , 'Temperature in internal building layer 2' , aA , 'ESTM' , 0) , &
       varAttr('TiBLD3'   , 'degK'  , f104 , 'Temperature in internal building layer 3' , aA , 'ESTM' , 0) , &
       varAttr('TiBLD4'   , 'degK'  , f104 , 'Temperature in internal building layer 4' , aA , 'ESTM' , 0) , &
       varAttr('TiBLD5'   , 'degK'  , f104 , 'Temperature in internal building layer 5' , aA , 'ESTM' , 0) , &
       varAttr('TaBLD'    , 'degK'  , f104 , 'Indoor air temperature'                   , aA , 'ESTM' , 0) &
       /

  
  DATA(varList(i), i=251+1,251+ncolumnsDataOutDailyState-5)/&
       varAttr('HDD1_h'     , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('HDD2_c'     , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('HDD3_Tmean' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('HDD4_T5d'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('P_day'      , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DaysSR'     , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('GDD1_g'     , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('GDD2_s'     , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('GDD3_Tmin'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('GDD4_Tmax'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('GDD5_DLHrs' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('LAI_EveTr'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('LAI_DecTr'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('LAI_Grass'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DecidCap'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('Porosity'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('AlbEveTr'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('AlbDecTr'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('AlbGrass'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_EveTr1'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_EveTr2'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_EveTr3'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_DecTr1'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_DecTr2'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_DecTr3'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_Grass1'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_Grass2'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('WU_Grass3'  , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('deltaLAI'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('LAIlumps'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('AlbSnow'    , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowPvd'   , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowBldgs' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowEveTr' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowDecTr' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowGrass' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowBSoil' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('DSnowWater' , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('a1'         , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('a2'         , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0), &
       varAttr('a3'         , 'to be added' , f104 , 'to be added' , aL , 'DailyState' , 0)  &
       /


CONTAINS
  
  SUBROUTINE SUEWS_Output(irMax,iv,Gridiv)
    IMPLICIT NONE
    INTEGER,INTENT(in) :: irMax
    INTEGER,INTENT(in) ::iv,Gridiv

    INTEGER :: xx,err,outLevel,i
    TYPE(varAttr),DIMENSION(:),ALLOCATABLE::varListX
    CHARACTER(len=10) :: grpList0(6)
    CHARACTER(len=10),DIMENSION(:),ALLOCATABLE :: grpList
    LOGICAL :: grpCond(6)

    
    SELECT CASE (WriteOutOption)
    CASE (0) 
       outLevel=1
    CASE (1) 
       outLevel=2
    CASE (2) 
       outLevel=0
    END SELECT


    
    
    grpList0(1)='SUEWS'
    grpList0(2)='SOLWEIG'
    grpList0(3)='BL'
    grpList0(4)='snow'
    grpList0(5)='ESTM'
    grpList0(6)='DailyState'
    grpCond=(/.TRUE.,&
         SOLWEIGpoi_out==1,&
         CBLuse>=1,&
         SnowUse>=1,&
         StorageHeatMethod==4 .OR. StorageHeatMethod==14,&
         .TRUE./)
    xx=COUNT(grpCond)

    

    ALLOCATE(grpList(xx), stat=err)
    IF ( err/= 0) PRINT *, "grpList: Allocation request denied"

    grpList=PACK(grpList0, mask=grpCond)

    

    
    DO i = 1, SIZE(grpList),1

       xx=COUNT(varList%group == TRIM(grpList(i)), dim=1)


       ALLOCATE(varListX(5+xx), stat=err)
       IF ( err/= 0) PRINT *, "varListX: Allocation request denied"
       
       varListX(1:5)=varList(1:5)
       
       varListX(6:5+xx)=PACK(varList, mask=(varList%group == TRIM(grpList(i))))

       IF  (TRIM(varListX(SIZE(varListX))%group) /= 'DailyState') THEN
          
          
          
          IF ( ResolutionFilesOut == Tstep .OR. KeepTstepFilesOut == 1 ) THEN
                CALL SUEWS_Output_txt_grp(iv,irMax,varListX,Gridiv,outLevel,Tstep)

          ENDIF
          
          IF ( ResolutionFilesOut /= Tstep ) THEN
                CALL SUEWS_Output_txt_grp(iv,irMax,varListX,Gridiv,outLevel,ResolutionFilesOut)
          ENDIF
       ELSE
          
             CALL SUEWS_Output_txt_grp(iv,irMax,varListX,Gridiv,outLevel,Tstep)
       ENDIF

       IF (ALLOCATED(varListX)) DEALLOCATE(varListX, stat=err)
       IF ( err/= 0) PRINT *, "varListX: Deallocation request denied"


    END DO
  END SUBROUTINE SUEWS_Output

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  






  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  


  
  SUBROUTINE SUEWS_Output_txt_grp(iv,irMax,varList,Gridiv,outLevel,outFreq_s)
    IMPLICIT NONE

    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList
    INTEGER,INTENT(in) :: iv,irMax,Gridiv,outLevel,outFreq_s

    INTEGER :: err,idMin,idMax

    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::dataOutX
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::dataOutX_agg

    IF (.NOT. ALLOCATED(dataOutX)) THEN
       ALLOCATE(dataOutX(irMax,SIZE(varList)), stat=err)
       IF ( err/= 0) PRINT *, "dataOutX: Allocation request denied"
    ENDIF

    
    SELECT CASE (TRIM(varList(SIZE(varList))%group))
    CASE ('SUEWS') 
       dataOutX=dataOutSUEWS(1:irMax,1:SIZE(varList),Gridiv)

    CASE ('SOLWEIG') 
       
       dataOutX=dataOutSOL(1:irMax,1:SIZE(varList),Gridiv)

    CASE ('BL') 
       dataOutX=dataOutBL(1:irMax,1:SIZE(varList),Gridiv)

    CASE ('snow')    
       dataOutX=dataOutSnow(1:irMax,1:SIZE(varList),Gridiv)

    CASE ('ESTM')    
       dataOutX=dataOutESTM(1:irMax,1:SIZE(varList),Gridiv)

    CASE ('DailyState')    
       
       
       
       idMin=MAX(1, &
            INT(MINVAL(dataOutSUEWS(1:irMax,2,Gridiv))), &
            INT(MINVAL(PACK(dataOutDailyState(:,2,Gridiv), &
            mask=(dataOutDailyState(:,6,Gridiv)/=-999)))))
       idMax=MIN(366,&
            INT(MAXVAL(dataOutSUEWS(1:irMax,2,Gridiv))), &
            INT(MAXVAL(PACK(dataOutDailyState(:,2,Gridiv), &
            mask=(dataOutDailyState(:,6,Gridiv)/=-999)))))


       IF (ALLOCATED(dataOutX)) THEN
          DEALLOCATE(dataOutX)
          IF ( err/= 0) PRINT *, "dataOutX: Deallocation request denied"
       ENDIF

       IF (.NOT. ALLOCATED(dataOutX)) THEN
          ALLOCATE(dataOutX(idMax-idMin+1,SIZE(varList)), stat=err)
          IF ( err/= 0) PRINT *, "dataOutX: Allocation request denied"
       ENDIF

       dataOutX=dataOutDailyState(idMin:idMax,1:SIZE(varList),Gridiv)


    END SELECT









    

    IF  (TRIM(varList(SIZE(varList))%group) /= 'DailyState') THEN

       CALL SUEWS_Output_Agg(dataOutX_agg,dataOutX,varList,irMax,outFreq_s)

    ELSE
       IF (.NOT. ALLOCATED(dataOutX_agg)) THEN
          ALLOCATE(dataOutX_agg(SIZE(dataOutX, dim=1),SIZE(varList)), stat=err)
          IF ( err/= 0) PRINT *, ": Allocation request denied"
       ENDIF
       dataOutX_agg=dataOutX
    ENDIF

    
    

    IF ( iv == 1 ) CALL SUEWS_Output_Init(dataOutX_agg,varList,Gridiv,outLevel)

    
    CALL SUEWS_Write_txt(dataOutX_agg,varList,Gridiv,outLevel)

  END SUBROUTINE SUEWS_Output_txt_grp

  
  SUBROUTINE SUEWS_Output_Init(dataOutX,varList,Gridiv,outLevel)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::dataOutX
    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList
    INTEGER,INTENT(in) :: Gridiv,outLevel

    TYPE(varAttr),DIMENSION(:),ALLOCATABLE::varListSel
    INTEGER :: xx,err,fn,i,nargs
    CHARACTER(len=100) :: FileOut
    CHARACTER(len=3) :: itext
    CHARACTER(len=6) :: args(5)
    CHARACTER(len=16*SIZE(varList)) :: FormatOut
    CHARACTER(len=16) :: formatX
    CHARACTER(len=16), DIMENSION(:), ALLOCATABLE:: headerOut

    
    xx=COUNT((varList%level<= outLevel), dim=1)
    WRITE(itext,'(i3)') xx
    ALLOCATE(varListSel(xx), stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Allocation request denied"
    varListSel=PACK(varList, mask=(varList%level<= outLevel))


    
    CALL filename_gen(dataOutX,varList,Gridiv,FileOut)



    
    ALLOCATE(headerOut(xx), stat=err)
    IF ( err/= 0) PRINT *, "headerOut: Allocation request denied"

    
    DO i = 1, SIZE(varListSel)
       CALL parse(varListSel(i)%fmt,'if.,',args,nargs)
       formatX=ADJUSTL('(a'//TRIM(args(2))//',1x)')
       
       WRITE(headerOut(i),formatX) ADJUSTR(TRIM(ADJUSTL(varListSel(i)%header)))
       IF ( i==1 ) THEN
          FormatOut=ADJUSTL(TRIM(formatX))
       ELSE
          FormatOut=TRIM(FormatOut)//' '//ADJUSTL(TRIM(formatX))
       END IF
    END DO
    FormatOut='('//TRIM(ADJUSTL(FormatOut))//')'

    
    fn=9
    OPEN(fn,file=TRIM(ADJUSTL(FileOut)),status='unknown')

    
    WRITE(fn, FormatOut) headerOut
    CLOSE(fn)

    
    CALL formatFile_gen(dataOutX,varList,Gridiv,outLevel)

    
    IF (ALLOCATED(varListSel)) DEALLOCATE(varListSel, stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Deallocation request denied"
    IF (ALLOCATED(headerOut)) DEALLOCATE(headerOut, stat=err)
    IF ( err/= 0) PRINT *, "headerOut: Deallocation request denied"

  END SUBROUTINE SUEWS_Output_Init

  
  SUBROUTINE formatFile_gen(dataOutX,varList,Gridiv,outLevel)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::dataOutX
    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList
    INTEGER,INTENT(in) :: Gridiv,outLevel

    TYPE(varAttr),DIMENSION(:),ALLOCATABLE::varListSel
    INTEGER :: xx,err,fn
    CHARACTER(len=100) :: FileOut
    CHARACTER(len=50*300) :: str_cat
    CHARACTER(len=50) :: str_x=''
    CHARACTER(len=3) :: itext

    
    CALL filename_gen(dataOutX,varList,Gridiv,FileOut,1)

    
    xx=COUNT((varList%level<= outLevel), dim=1)
    ALLOCATE(varListSel(xx), stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Allocation request denied"
    varListSel=PACK(varList, mask=(varList%level<= outLevel))

    
    fn=9
    OPEN(fn,file=TRIM(ADJUSTL(FileOut)),status='unknown')

    
    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       WRITE(itext,'(i3)') i
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(itext))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(itext)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       str_x=varListSel(i)%header
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(str_x))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(str_x)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       str_x=varListSel(i)%longNm
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(str_x))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(str_x)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       str_x=varListSel(i)%unit
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(str_x))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(str_x)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       str_x=varListSel(i)%fmt
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(str_x))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(str_x)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    str_cat=''
    DO i = 1, SIZE(varListSel)
       str_x=varListSel(i)%aggreg
       IF ( i==1 ) THEN
          str_cat=TRIM(ADJUSTL(str_x))
       ELSE
          str_cat=TRIM(str_cat)//';'//ADJUSTL(str_x)
       ENDIF
    END DO
    WRITE(fn,'(a)') TRIM(str_cat)

    
    CLOSE(fn)

    
    IF (ALLOCATED(varListSel)) DEALLOCATE(varListSel, stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Deallocation request denied"

  END SUBROUTINE formatFile_gen

  
  SUBROUTINE SUEWS_Output_Agg(dataOut_agg,dataOutX,varList,irMax,outFreq_s)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::dataOutX
    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList
    INTEGER,INTENT(in) :: irMax,outFreq_s
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE,INTENT(out)::dataOut_agg

    INTEGER ::  nlinesOut,i,j,x
    REAL(KIND(1d0))::dataOut_aggX(1:SIZE(varList))
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::dataOut_agg0
    nlinesOut=INT(nsh/(60.*60/outFreq_s))
    

    ALLOCATE(dataOut_agg(INT(irMax/nlinesOut),SIZE(varList)))
    ALLOCATE(dataOut_agg0(nlinesOut,SIZE(varList)))


    DO i=nlinesOut,irMax,nlinesOut
       x=i/nlinesOut
       dataOut_agg0=dataOutX(i-nlinesOut+1:i,:)
       DO j = 1, SIZE(varList), 1
          
          SELECT CASE (varList(j)%aggreg)
          CASE (aT) 
             dataOut_aggX(j)=dataOut_agg0(nlinesOut,j)
          CASE (aA) 
             dataOut_aggX(j)=SUM(dataOut_agg0(:,j))/nlinesOut
          CASE (aS) 
             dataOut_aggX(j)=SUM(dataOut_agg0(:,j))
          CASE (aL) 
             dataOut_aggX(j)=dataOut_agg0(nlinesOut,j)
          END SELECT

          IF ( Diagnose==1 .AND. i==irMax ) THEN
             
             PRINT*, 'raw data of ',j,':'
             PRINT*, dataOut_agg0(:,j)
             PRINT*, 'aggregated with method: ',varList(j)%aggreg
             PRINT*, dataOut_aggX(j)
             PRINT*, ''
          END IF
       END DO
       dataOut_agg(x,:)=dataOut_aggX
    END DO

  END SUBROUTINE SUEWS_Output_Agg


  
  SUBROUTINE SUEWS_Write_txt(dataOutX,varList,Gridiv,outLevel)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::dataOutX
    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList
    INTEGER,INTENT(in) :: Gridiv,outLevel

    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::dataOutSel
    TYPE(varAttr),DIMENSION(:),ALLOCATABLE::varListSel
    CHARACTER(len=100) :: FileOut
    INTEGER :: fn,i,xx,err
    CHARACTER(len=12*SIZE(varList)) :: FormatOut

    IF(Diagnose==1) WRITE(*,*) 'Writting data of group: ',varList(SIZE(varList))%group

    
    xx=COUNT((varList%level<= outLevel), dim=1)
    ALLOCATE(varListSel(xx), stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Allocation request denied"
    varListSel=PACK(varList, mask=(varList%level<= outLevel))

    
    ALLOCATE(dataOutSel(SIZE(dataOutX, dim=1),xx), stat=err)
    IF ( err/= 0) PRINT *, "dataOutSel: Allocation request denied"
    
    
    dataOutSel=dataOutX(:,PACK((/(i,i=1,SIZE(varList%level))/), varList%level <= outLevel))


    
    DO i = 1, SIZE(varListSel)
       IF ( i==1 ) THEN
          FormatOut=ADJUSTL(varListSel(i)%fmt)
       ELSE
          FormatOut=TRIM(FormatOut)//' '//ADJUSTL(varListSel(i)%fmt)
       END IF
    END DO
    FormatOut='('//TRIM(ADJUSTL(FormatOut))//')'

    
    CALL filename_gen(dataOutSel,varListSel,Gridiv,FileOut)


    
    fn=50
    OPEN(fn,file=TRIM(fileout),position='append')
    DO i=1,SIZE(dataOutSel,dim=1)

       
       WRITE(fn,FormatOut) &
            INT(dataOutSel(i,1:4)),&
            dataOutSel(i,5:SIZE(varListSel))
    ENDDO
    CLOSE (fn)

    IF (ALLOCATED(varListSel)) DEALLOCATE(varListSel, stat=err)
    IF ( err/= 0) PRINT *, "varListSel: Deallocation request denied"

    IF (ALLOCATED(dataOutSel)) DEALLOCATE(dataOutSel, stat=err)
    IF ( err/= 0) PRINT *, "dataOutSel: Deallocation request denied"

  END SUBROUTINE SUEWS_Write_txt


  SUBROUTINE filename_gen(dataOutX,varList,Gridiv,FileOut,opt_fmt)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:,:),INTENT(in)::dataOutX 
    TYPE(varAttr),DIMENSION(:),INTENT(in)::varList 
    INTEGER,INTENT(in) :: Gridiv 
    INTEGER,INTENT(in),OPTIONAL :: opt_fmt 
    CHARACTER(len=100),INTENT(out) :: FileOut 

    CHARACTER(len=20):: str_out_min,str_grid,&
         str_date,str_year,str_DOY,str_grp,str_sfx
    INTEGER :: year_int,DOY_int,val_fmt

    
    val_fmt=-999

    IF( PRESENT(opt_fmt) ) val_fmt = opt_fmt

    


    
    year_int=INT(dataOutX(1,1))
    DOY_int=INT(dataOutX(1,2))
    WRITE(str_year,'(i4)') year_int
    WRITE(str_DOY,'(i3.3)') DOY_int
    str_date='_'//TRIM(ADJUSTL(str_year))


    
    IF ( varList(6)%group == 'DailyState' ) THEN
       str_out_min='' 
    ELSE
       WRITE(str_out_min,'(i4)') &
            INT(dataOutX(2,3)-dataOutX(1,3))*60& 
            +INT(dataOutX(2,4)-dataOutX(1,4))     
       str_out_min='_'//TRIM(ADJUSTL(str_out_min))
    ENDIF



    
    str_grp=varList(6)%group
    IF ( LEN(TRIM(str_grp)) > 0 ) str_grp='_'//TRIM(ADJUSTL(str_grp))

    
    WRITE(str_grid,'(i10)') GridIDmatrix(Gridiv)

    
    str_sfx='.txt'

    
    FileOut=TRIM(FileOutputPath)//&
         TRIM(FileCode)//&
         TRIM(ADJUSTL(str_grid))//&
         TRIM(ADJUSTL(str_date))//&
         TRIM(ADJUSTL(str_grp))//&
         TRIM(ADJUSTL(str_out_min))//&
         TRIM(ADJUSTL(str_sfx))

    
    IF ( val_fmt==1 ) THEN
       FileOut=TRIM(FileOutputPath)//&
            TRIM(FileCode)//&
            TRIM(ADJUSTL(str_grp))//&
            '_OutputFormat.txt'
    END IF


  END SUBROUTINE filename_gen


  
  FUNCTION count_lines(filename) RESULT(nlines)
    
    

    
    IMPLICIT NONE
    CHARACTER(len=*)    :: filename
    INTEGER             :: nlines
    INTEGER             :: io,iv

    OPEN(10,file=filename, iostat=io, status='old')

    
    IF (io/=0) THEN
       PRINT*, 'io', io, 'for', filename
       STOP 'Cannot open file! '
    ENDIF

    nlines = 0
    DO
       READ(10,*,iostat=io) iv
       IF (io < 0 .OR. iv ==-9) EXIT

       nlines = nlines + 1
    END DO
    CLOSE(10)
    nlines=nlines-1 
  END FUNCTION count_lines



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
  
  


END MODULE ctrl_output
