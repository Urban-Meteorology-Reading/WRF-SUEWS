MODULE NARP_MODULE
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  

  
  

  IMPLICIT NONE

CONTAINS
  
  SUBROUTINE RadMethod(&
       NetRadiationMethod,&
       snowUse,&
       NetRadiationMethodX,AlbedoChoice,ldown_option)
    IMPLICIT NONE
    INTEGER,INTENT(in) :: NetRadiationMethod 
    INTEGER,INTENT(in) ::snowUse
    INTEGER,INTENT(out)::NetRadiationMethodX 
    INTEGER,INTENT(out)::AlbedoChoice,ldown_option
    
    
    AlbedoChoice=0
    ldown_option=0
    IF(NetRadiationMethod==0)THEN    
       NetRadiationMethodX=0
       

       IF(snowUse==1) THEN            
          
          NetRadiationMethodX=3000
          ldown_option=3              
          
       ENDIF

    ELSEIF(NetRadiationMethod>0)THEN  
       AlbedoChoice=-9
       IF(NetRadiationMethod<10) THEN
          AlbedoChoice=0
          IF(NetRadiationMethod==1)ldown_option=1
          IF(NetRadiationMethod==2)ldown_option=2
          IF(NetRadiationMethod==3)ldown_option=3
          NetRadiationMethodX=NetRadiationMethod

       ELSEIF(NetRadiationMethod>=100.AND.NetRadiationMethod<1000) THEN
          AlbedoChoice=1
          IF(NetRadiationMethod==100)ldown_option=1
          IF(NetRadiationMethod==200)ldown_option=2
          IF(NetRadiationMethod==300)ldown_option=3
          
          NetRadiationMethodX=NetRadiationMethod/100
       ENDIF

       
       IF(NetRadiationMethodX>3.OR. AlbedoChoice==-9)THEN
          WRITE(*,*) 'NetRadiationMethod=',NetRadiationMethodX
          WRITE(*,*) 'Value not usable'
          STOP
       ENDIF
    ENDIF


  END SUBROUTINE RadMethod




  
  SUBROUTINE NARP(&
       nsurf,sfr,snowFrac,alb,emis,IceFrac,&
       NARP_TRANS_SITE,NARP_EMIS_SNOW,&
       DTIME,ZENITH_deg,kdown,Temp_C,RH,Press_hPa,qn1_obs,&
       SnowAlb,&
       AlbedoChoice,ldown_option,&
       NetRadiationMethodX,DiagQN,&
       QSTARall,QSTAR_SF,QSTAR_S,kclear,KUPall,LDOWN,LUPall,fcld,TSURFall,&
       qn1_ind_snow,kup_ind_snow,Tsurf_ind_snow,Tsurf_ind)
    
    
    

    
    
    

    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    


    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::sfr
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::snowFrac
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::alb
    REAL(KIND(1D0)),DIMENSION(nsurf),INTENT(in) ::emis
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(in) ::IceFrac
    

    REAL(KIND(1D0)),INTENT(in) ::DTIME
    REAL(KIND(1D0)),INTENT(in) ::ZENITH_deg
    REAL(KIND(1D0)),INTENT(in) ::kdown
    REAL(KIND(1D0)),INTENT(in) ::Temp_C
    REAL(KIND(1D0)),INTENT(in) ::RH
    REAL(KIND(1D0)),INTENT(in) ::Press_hPa
    REAL(KIND(1D0)),INTENT(in) ::qn1_obs
    REAL(KIND(1D0)),INTENT(in) ::SnowAlb
    REAL(KIND(1D0)),INTENT(in) ::NARP_TRANS_SITE
    REAL(KIND(1D0)),INTENT(in) ::NARP_EMIS_SNOW

    INTEGER,INTENT(in) ::nsurf
    INTEGER,INTENT(in) ::NetRadiationMethodX 
    INTEGER,INTENT(in) ::AlbedoChoice
    INTEGER,INTENT(in) ::ldown_option
    INTEGER,INTENT(in) ::DiagQN

    REAL(KIND(1D0)),INTENT(out) ::QSTARall
    REAL(KIND(1D0)),INTENT(out) ::QSTAR_SF
    REAL(KIND(1D0)),INTENT(out) ::QSTAR_S
    REAL(KIND(1D0)),INTENT(out) ::kclear
    REAL(KIND(1D0)),INTENT(out) ::KUPall
    REAL(KIND(1D0)),INTENT(out) ::LDOWN
    REAL(KIND(1D0)),INTENT(out) ::LUPall
    REAL(KIND(1D0)),INTENT(out) ::fcld
    REAL(KIND(1D0)),INTENT(out) ::TSURFall

    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::qn1_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::kup_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::Tsurf_ind_snow
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(out) ::Tsurf_ind

    REAL(KIND(1d0)),DIMENSION(nsurf) ::qn1_ind
    REAL(KIND(1d0)),DIMENSION(nsurf) ::kup_ind
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind


    REAL(KIND(1d0)),DIMENSION(nsurf) ::qn1_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::kup_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::Tsurf_ind_nosnow
    REAL(KIND(1d0)),DIMENSION(nsurf) ::lup_ind_snow


    REAL(KIND(1D0)) ::Temp_K,TD,ZENITH,QSTAR,QSTAR_SNOW,KUP_SNOW,LUP_SNOW,TSURF_SNOW,KUP,LUP,TSURF
    REAL(KIND(1D0)) ::ALB0,EMIS0,EMIS_A,TRANS
    REAL(KIND(1D0)) ::LUPCORR,SIGMATK4,KDOWN_HR=0.
    INTEGER         ::DOY, is

    REAL(KIND(1D0))::qn1_cum,kup_cum,lup_cum,tsurf_cum,&   
         qn1_is,kup_is,lup_is,tsurf_is,&       
         SF_all,ALB1

    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292,&
                                
         SIGMA_SB=5.67E-8

    
    
    REAL(KIND(1D0)),DIMENSION(365),PARAMETER ::  NARP_G=3.0
    
    
    
    
    Temp_K=Temp_C+273.16
    SIGMATK4=SIGMA_SB*Temp_K**4
    TD=DEWPOINT(Temp_C,RH)
    
    
    
    ZENITH=ZENITH_deg*DEG2RAD
    DOY=INT(DTIME)
    IF(DOY==366)doy=365

    
    
    qn1_cum=0
    kup_cum=0
    lup_cum=0
    tsurf_cum=0

    QSTAR_SF=0
    QSTAR_S=0

    
    SF_all=0
    DO is = 1,nsurf
       IF (sfr(is)/=0) SF_all = SF_all + sfr(is)*(1-snowFrac(is))
    ENDDO

    DO is=1,nsurf
       IF(DiagQN==1) WRITE(*,*) 'is ',is
       
       EMIS_A=PRATA_EMIS(Temp_K,Press_hPa)

       
       

       IF (AlbedoChoice==1.AND.180*ZENITH/ACOS(0.0)<90) THEN
          ALB0=ALB(is)+0.5e-16*(180*ZENITH/ACOS(0.0))**8 
       ELSE
          ALB0=ALB(is)
       ENDIF
       EMIS0=EMIS(is)

       
       IF((ldown_option==4) .OR. (ldown_option==5)) THEN 
          IF (ZENITH<1.5) THEN 
             TRANS=TRANSMISSIVITY(Press_hPa,TD,NARP_G(DOY),ZENITH)
             KCLEAR=ISURFACE(DOY,ZENITH)*TRANS*NARP_TRANS_SITE
             IF (KCLEAR>50.) THEN
                FCLD=CLOUD_FRACTION(KDOWN,KCLEAR)
                EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
             ELSE
                IF(ldown_option==5) THEN 
                   FCLD=WC_fraction(RH,Temp_C)
                   EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
                ELSE
                   
                   EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
                ENDIF
             ENDIF
          ELSE 
             IF(ldown_option==4) THEN
                
                EMIS_A=EMIS_CLOUD_SQ(EMIS_A,FCLD)
             ELSEIF((ldown_option==5)) THEN 
                FCLD=WC_fraction(RH,Temp_C)
                EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
             ENDIF
          ENDIF
       ELSEIF(ldown_option==3) THEN 
          FCLD=WC_fraction(RH,Temp_C)
          EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
       ELSEIF(ldown_option==2) THEN 
          EMIS_A=EMIS_CLOUD(EMIS_A,FCLD)
       ENDIF
       IF(DiagQN==1) WRITE(*,*) 'ldown_option: ',ldown_option,'FCLD:',FCLD

       IF(ldown_option>1) THEN 
          LDOWN=EMIS_A*SIGMATK4
          IF(DiagQN==1) WRITE(*,*) 'EMIS_A: ',EMIS_A,'SIGMATK4:',SIGMATK4,'LDOWN: ',LDOWN
       ENDIF


       
       
       KDOWN_HR=KDOWN
       IF (KDOWN_HR>0) THEN
          LUPCORR=(1-ALB0)*(0.08*KDOWN_HR)
       ELSE
          LUPCORR=0.
       ENDIF

       KUP=ALB0*KDOWN
       TSURF=((EMIS0*SIGMATK4+LUPCORR)/(EMIS0*SIGMA_SB))**0.25 

       LUP=EMIS0*SIGMATK4+LUPCORR+(1-EMIS0)*LDOWN              
       QSTAR=KDOWN-KUP+LDOWN-LUP
       TSURF=TSURF-273.16

       
       
       IF (snowFrac(is)>0) THEN
          IF (AlbedoChoice==1.AND.180*ZENITH/ACOS(0.0)<90) THEN
             ALB1=SnowAlb+0.5e-16*(180*ZENITH/ACOS(0.0))**8 
          ELSE
             ALB1=SnowAlb
          ENDIF

          KUP_SNOW = (ALB1*(snowFrac(is)-snowFrac(is)*IceFrac(is))+ALB0*snowFrac(is)*IceFrac(is))*KDOWN
          TSURF_SNOW=((NARP_EMIS_SNOW*SIGMATK4)/(NARP_EMIS_SNOW*SIGMA_SB))**0.25 

          

          
          

          LUP_SNOW = NARP_EMIS_SNOW*SIGMA_SB*TSURF_SNOW**4+(1-NARP_EMIS_SNOW)*LDOWN
          QSTAR_SNOW = KDOWN-KUP_SNOW+LDOWN-LUP_SNOW
          TSURF_SNOW = TSURF_SNOW-273.16

       ELSE
          KUP_SNOW = 0
          LUP_SNOW = 0
          TSURF_SNOW = 0
          QSTAR_SNOW = 0
          
          
       ENDIF

       qn1_ind_nosnow(is)=QSTAR          
       kup_ind_nosnow(is)=KUP
       lup_ind_nosnow(is)=LUP
       Tsurf_ind_nosnow(is)=TSURF

       qn1_ind_snow(is)=QSTAR_SNOW        
       kup_ind_snow(is)=KUP_SNOW
       lup_ind_snow(is)=LUP_SNOW
       Tsurf_ind_snow(is)=TSURF_SNOW


       IF (SF_all/=0)THEN
          QSTAR_SF = QSTAR_SF + QSTAR*sfr(is)*(1-snowFrac(is))/SF_all
       ELSE
          QSTAR_SF = QSTAR_SF + QSTAR*sfr(is)*(1-snowFrac(is))
       ENDIF

       IF ((1-SF_all)/=0)THEN
          QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr(is)*snowFrac(is)/(1-SF_all)
       ELSE
          QSTAR_S = QSTAR_S + QSTAR_SNOW*sfr(is)*snowFrac(is)
       ENDIF

       
       
       qn1_is = QSTAR*(1-snowFrac(is))+QSTAR_SNOW*snowFrac(is)
       kup_is = KUP*(1-snowFrac(is))+KUP_SNOW*snowFrac(is)
       lup_is = LUP*(1-snowFrac(is))+LUP_SNOW*snowFrac(is)
       tsurf_is = TSURF*(1-snowFrac(is))+TSURF_SNOW*snowFrac(is)

       IF(DiagQN==1) WRITE(*,*) 'QSTAR',QSTAR,'QSTAR_SNOW',QSTAR_SNOW,'snowFrac',snowFrac(is)

       qn1_cum=qn1_cum+(qn1_is*sfr(is))  
       kup_cum=kup_cum+(kup_is*sfr(is))
       lup_cum=lup_cum+(lup_is*sfr(is))
       tsurf_cum=tsurf_cum+(tsurf_is*sfr(is))

       qn1_ind(is)=qn1_is                
       kup_ind(is)=kup_is
       lup_ind(is)=lup_is
       Tsurf_ind(is)=tsurf_is

       IF(DiagQN==1) WRITE(*,*) 'qn1_is: ',qn1_is

    ENDDO 



    
    IF (NetRadiationMethodX/=3000) THEN 
       QSTARall=qn1_cum
    ELSE
       QSTARall=qn1_obs
    ENDIF

    KUPall=kup_cum
    LUPall=lup_cum
    TSURFall=tsurf_cum

    
    
    
    
    tsurf=TSURFall
    
    
    
    

    IF(DiagQN==1) WRITE(*,*) 'kdown: ',kdown,'kup:',kup,'LDOWN: ',LDOWN,'LUP: ',LUP
    IF(DiagQN==1) WRITE(*,*) 'Qn: ',QSTARall

  END SUBROUTINE NARP


  
  SUBROUTINE NARP_cal_SunPosition(year,idectime,UTC,locationlatitude,locationlongitude,locationaltitude,sunazimuth,sunzenith)
    IMPLICIT NONE

    REAL(KIND(1D0)),INTENT(in) :: year,idectime,UTC,locationlatitude,locationlongitude,locationaltitude
    REAL(KIND(1D0)),INTENT(out) ::sunazimuth,sunzenith

    REAL(KIND(1D0)):: sec
    INTEGER :: month,day,hour,min,seas,dayofyear

    REAL(KIND(1D0)) :: juliancentury,julianday,julianephemeris_century,julianephemeris_day,&
         julianephemeris_millenium
    REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude,earth_heliocentric_positionlongitude,&
         earth_heliocentric_positionradius
    REAL(KIND(1D0)) :: sun_geocentric_positionlatitude, sun_geocentric_positionlongitude
    REAL(KIND(1D0)) :: nutationlongitude,nutationobliquity
    REAL(KIND(1D0)) :: corr_obliquity
    REAL(KIND(1D0)) :: aberration_correction
    REAL(KIND(1D0)) :: apparent_sun_longitude
    REAL(KIND(1D0)) :: apparent_stime_at_greenwich
    REAL(KIND(1D0)) :: sun_rigth_ascension
    REAL(KIND(1D0)) :: sun_geocentric_declination
    REAL(KIND(1D0)) :: observer_local_hour
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension ,topocentric_sun_positionrigth_ascension_parallax
    REAL(KIND(1D0)) :: topocentric_sun_positiondeclination
    REAL(KIND(1D0)) :: topocentric_local_hour
    

    
    
    
    
    
    
    
    
    
    
    
    


    
    CALL dectime_to_timevec(idectime,hour,min,sec)
    dayofyear=FLOOR(idectime)
    CALL day2month(dayofyear,month,day,seas,year,locationlatitude)

    
    
    CALL julian_calculation(year,month,day,hour,min,sec,UTC,juliancentury,julianday,julianephemeris_century,&
         julianephemeris_day,julianephemeris_millenium)

    
    
    CALL earth_heliocentric_position_calculation(julianephemeris_millenium,earth_heliocentric_positionlatitude,&
         &earth_heliocentric_positionlongitude,earth_heliocentric_positionradius)

    
    CALL sun_geocentric_position_calculation(earth_heliocentric_positionlongitude,earth_heliocentric_positionlatitude,&
         & sun_geocentric_positionlatitude, sun_geocentric_positionlongitude)

    
    CALL nutation_calculation(julianephemeris_century,nutationlongitude,nutationobliquity)

    
    CALL corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)

    
    CALL abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)

    
    CALL apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
         & aberration_correction, apparent_sun_longitude)

    
    CALL apparent_stime_at_greenwich_calculation(julianday,juliancentury, nutationlongitude, &
         &corr_obliquity, apparent_stime_at_greenwich)

    
    CALL sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
         &sun_rigth_ascension)

    
    
    CALL sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, sun_geocentric_positionlatitude, &
         &sun_geocentric_declination)

    
    CALL observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, sun_rigth_ascension, observer_local_hour)

    
    
    CALL topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension,&
         &topocentric_sun_positionrigth_ascension_parallax,topocentric_sun_positiondeclination,locationaltitude,&
         &locationlatitude,observer_local_hour,sun_rigth_ascension,sun_geocentric_declination,&
         &earth_heliocentric_positionradius)

    
    CALL topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
         & topocentric_local_hour)

    
    CALL sun_topocentric_zenith_angle_calculate(locationlatitude , topocentric_sun_positiondeclination,&
         & topocentric_local_hour, sunazimuth,sunzenith)


  END SUBROUTINE NARP_cal_SunPosition


  
  SUBROUTINE julian_calculation(year,month,day,hour,min,sec,UTC,juliancentury,julianday,julianephemeris_century&
       &,julianephemeris_day,julianephemeris_millenium)
    IMPLICIT NONE

    REAL(KIND(1D0)) :: A,B,D,delta_t
    REAL(KIND(1D0)) :: juliancentury
    REAL(KIND(1D0)) :: julianday
    REAL(KIND(1D0)) :: julianephemeris_century
    REAL(KIND(1D0)) :: julianephemeris_day
    REAL(KIND(1D0)) :: julianephemeris_millenium
    REAL(KIND(1D0)) :: M,sec,year,UTC
    INTEGER :: day,hour,min,month
    
    REAL(KIND(1D0)) :: ut_time ,Y 
    
    
    
    

    IF (month == 1 .OR. month == 2) THEN
       Y = year - 1.
       M = month + 12
    ELSE
       Y = year
       M = month
    END IF
    ut_time = ((float(hour) - UTC)/24.) + (float(min)/(60.*24.)) + (sec/(60.*60.*24.)) 
    D = day + ut_time 

    
    IF (year == 1582.) THEN
       IF (month == 10) THEN
          IF (day <= 4) THEN 
             B = 0
          ELSE IF (day >= 15) THEN 
             A = FLOOR(Y/100)
             B = 2 - A + FLOOR(A/4)
          ELSE

             month = 10
             day = 4
             B = 0
          END IF
       ELSE IF (month<10) THEN 
          B = 0
       ELSE 
          A = FLOOR(Y/100)
          B = 2 - A + FLOOR(A/4)
       END IF

    ELSE IF (year<1582.) THEN 
       B = 0
    ELSE
       A = FLOOR(Y/100) 
       B = 2 - A + FLOOR(A/4)
    END IF

    julianday = FLOOR(365.25*(Y+4716.)) + FLOOR(30.6001*(M+1)) + D + B - 1524.5

    delta_t = 0. 
    julianephemeris_day = julianday + (delta_t/86400)

    juliancentury = (julianday - 2451545.) / 36525.

    julianephemeris_century = (julianephemeris_day - 2451545.) / 36525.

    julianephemeris_millenium = julianephemeris_century / 10.

  END SUBROUTINE julian_calculation

  SUBROUTINE earth_heliocentric_position_calculation(julianephemeris_millenium,earth_heliocentric_positionlatitude&
       &,earth_heliocentric_positionlongitude,earth_heliocentric_positionradius)
    IMPLICIT NONE

    REAL(KIND(1D0)) :: julianephemeris_millenium      

    REAL(KIND(1D0)),DIMENSION(64) :: A0      
    REAL(KIND(1D0)),DIMENSION(34) :: A1      
    REAL(KIND(1D0)),DIMENSION(20) :: A2      
    REAL(KIND(1D0)),DIMENSION(7) :: A3      
    REAL(KIND(1D0)),DIMENSION(3) :: A4      
    REAL(KIND(1D0)) :: A5      
    REAL(KIND(1D0)),DIMENSION(64) :: B0      
    REAL(KIND(1D0)),DIMENSION(34) :: B1      
    REAL(KIND(1D0)),DIMENSION(20) :: B2      
    REAL(KIND(1D0)),DIMENSION(7) :: B3      
    REAL(KIND(1D0)),DIMENSION(3) :: B4      
    REAL(KIND(1D0)) :: B5      
    REAL(KIND(1D0)),DIMENSION(64) :: C0      
    REAL(KIND(1D0)),DIMENSION(34) :: C1      
    REAL(KIND(1D0)),DIMENSION(20) :: C2      
    REAL(KIND(1D0)),DIMENSION(7) :: C3      
    REAL(KIND(1D0)),DIMENSION(3) :: C4      
    REAL(KIND(1D0)) :: C5
    REAL(KIND(1D0)),DIMENSION(40) :: A0j      
    REAL(KIND(1D0)),DIMENSION(10) :: A1j     
    REAL(KIND(1D0)),DIMENSION(6) :: A2j      
    REAL(KIND(1D0)),DIMENSION(2) :: A3j      
    REAL(KIND(1D0)) :: A4j      
    REAL(KIND(1D0)),DIMENSION(40) :: B0j      
    REAL(KIND(1D0)),DIMENSION(10) :: B1j      
    REAL(KIND(1D0)),DIMENSION(6) :: B2j      
    REAL(KIND(1D0)),DIMENSION(2) :: B3j      
    REAL(KIND(1D0)) :: B4j      
    REAL(KIND(1D0)),DIMENSION(40) :: C0j      
    REAL(KIND(1D0)),DIMENSION(10) :: C1j      
    REAL(KIND(1D0)),DIMENSION(6) :: C2j      
    REAL(KIND(1D0)),DIMENSION(2) :: C3j      
    REAL(KIND(1D0)) :: C4j      
    REAL(KIND(1D0)),DIMENSION(5) ::  A0i
    REAL(KIND(1D0)),DIMENSION(5) ::  B0i
    REAL(KIND(1D0)),DIMENSION(5) ::  C0i
    REAL(KIND(1D0)),DIMENSION(2) ::  A1i
    REAL(KIND(1D0)),DIMENSION(2) ::  B1i
    REAL(KIND(1D0)),DIMENSION(2) ::   C1i
    REAL(KIND(1D0)) :: earth_heliocentric_positionlatitude      
    REAL(KIND(1D0)) :: earth_heliocentric_positionlongitude      
    REAL(KIND(1D0)) :: earth_heliocentric_positionradius      
    REAL(KIND(1D0)) :: JME      
    REAL(KIND(1D0)) :: L0      
    
    REAL(KIND(1D0)) :: L1      
    
    REAL(KIND(1D0)) :: L2      
    
    REAL(KIND(1D0)) :: L3      
    
    REAL(KIND(1D0)) :: L4      
    
    REAL(KIND(1D0)) :: L5      
    
    
    
    
    
    
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    
    

    A0=(/175347046,3341656,34894,3497,3418,3136,2676,2343,1324,1273,1199,990,902,857,780,753,505,&
         &492,357,317,284,271,243,206,205,202,156,132,126,115,103,102,102,99,98,86,85,85,80,79,71,&
         &74,74,70,62,61,57,56,56,52,52,51,49,41,41,39,37,37,36,36,33,30,30,25/)
    B0 = (/0.,4.669256800,4.626100,2.744100,2.828900,3.627700,4.418100,6.135200,0.7425000,2.037100,&
         &1.109600,5.233000,2.045000,3.508000,1.179000,2.533000,4.583000,4.205000,2.92,5.849000,&
         &1.899000,0.315,0.345,4.806000,1.869000,2.445800,0.833,3.411000,1.083000,0.645,0.636,0.976,&
         &4.267000,6.21,0.68,5.98,1.3,3.67,1.81,3.04,1.76,3.5,4.68,0.83,3.98,1.82,2.78,4.39,3.47,0.19,&
         &1.33,0.28,0.49,5.37,2.4,6.17,6.04,2.57,1.71,1.78,0.59,0.44,2.74,3.16/)
    C0 = (/0.,6283.075850,12566.15170,5753.384900,3.523100,77713.77150,7860.419400,3930.209700,&
         &11506.76980,529.6910,1577.343500,5884.927,26.29800,398.1490,5223.694,5507.553,&
         &18849.22800,775.5230,0.067,11790.62900,796.2980,10977.07900,5486.778,2544.314,&
         &5573.143,6069.777,213.2990,2942.463,20.77500,0.98,4694.003,15720.83900,7.114000,&
         &2146.170,155.4200,161000.6900,6275.960,71430.70,17260.15,12036.46,5088.630,3154.690,&
         &801.8200,9437.760,8827.390,7084.900,6286.600,14143.50,6279.550,12139.55,1748.020,&
         &5856.480,1194.450,8429.240,19651.05,10447.39,10213.29,1059.380,2352.870,6812.770,&
         &17789.85,83996.85,1349.870,4690.480/)
    A1 = (/628331966747.000,206059.,4303.,425.,119.,109.,93.,72.,68.,67.,59.,56.,45.,36.,29.,21.,19.,19.,17.,16.,&
         &16.,15.,12.,12.,12.,12.,11.,10.,10.,9.,9.,8.,6.,6./)
    B1 =(/ 0.,2.678235,2.635100,1.59,5.796000,2.966000,2.59,1.14,1.87,4.41,2.89,2.17,0.40,0.47,&
         &2.65,5.34,1.85,4.97,2.99,0.030,1.43,1.21,2.83,3.26,5.27,2.08,0.77,1.3,4.24,2.7,5.64,&
         &5.3,2.65,4.67/)
    C1 =(/ 0.,6283.075850,12566.15170,3.523000,26.29800,1577.344,18849.23,529.6900,398.1500,&
         &5507.550,5223.690,155.4200,796.3000,775.5200,7.11,0.98,5486.780,213.3000,6275.960,&
         &2544.310,2146.170,10977.08,1748.020,5088.630,1194.450,4694.,553.5700,3286.600,&
         &1349.870,242.7300,951.7200,2352.870,9437.760,4690.480/)
    A2 =(/ 52919,8720,309,27,16,16,10,9,7,5,4,4,3,3,3,3,3,3,2,2/)
    B2 = (/0.,1.072100,0.867,0.050,5.19,3.68,0.76,2.06,0.83,4.66,1.03,3.44,5.14,6.05,1.19,&
         &6.12,0.31,2.28,4.38,3.75/)
    C2 =(/ 0.,6283.075800,12566.15200,3.52,26.3,155.4200,18849.23,77713.77,775.5200,1577.340,&
         &7.11,5573.140,796.3000,5507.550,242.7300,529.6900,398.1500,553.5700,5223.690,0.98/)
    A3 = (/289,35,17,3,1,1,1/)
    B3 = (/5.8440,0.,5.4900,5.2000,4.7200,5.3000,5.9700/)
    C3 = (/6283.076,0.,12566.15,155.4200,3.52,18849.23,242.7300/)
    A4 = (/114,8,1/)
    B4 = (/3.1420,4.1300,3.8400/)
    C4 =  (/0.,6283.08,12566.15/)
    A5 =1.
    B5 =3.1400
    C5 =0.

    JME = julianephemeris_millenium

    
    L0 = SUM(A0 * COS(B0 + (C0 * JME)))
    L1 = SUM(A1 * COS(B1 + (C1 * JME)))
    L2 = SUM(A2 * COS(B2 + (C2 * JME)))
    L3 = SUM(A3 * COS(B3 + (C3 * JME)))
    L4 = SUM(A4 * COS(B4 + (C4 * JME)))
    L5 = A5 * COS(B5 + (C5 * JME))

    earth_heliocentric_positionlongitude = &
         &(L0 + (L1 * JME) + (L2 * JME**2) + (L3 * JME**3) + (L4 * JME**4) + (L5 * JME**5)) / 1e8
    
    earth_heliocentric_positionlongitude = earth_heliocentric_positionlongitude * 180./pi
    
    earth_heliocentric_positionlongitude=set_to_range(earth_heliocentric_positionlongitude)

    A0i = (/280,102,80,44,32/)
    B0i = (/3.19900000000000,5.42200000000000,3.88000000000000,3.70000000000000,4./)
    C0i = (/84334.6620000000,5507.55300000000,5223.69000000000,2352.87000000000,1577.34000000000/)
    A1i = (/9,6/)
    B1i =(/3.90000000000000,1.73000000000000/)
    C1i = (/5507.55000000000,5223.69000000000/)

    L0 = SUM(A0i * COS(B0i + (C0i * JME)))
    L1 = SUM(A1i * COS(B1i + (C1i * JME)))

    earth_heliocentric_positionlatitude = (L0 + (L1 * JME)) / 1e8
    
    earth_heliocentric_positionlatitude = earth_heliocentric_positionlatitude * 180/pi
    
    earth_heliocentric_positionlatitude=set_to_range(earth_heliocentric_positionlatitude)

    A0j = (/100013989,1670700,13956,3084,1628,1576,925,542,472,346,329,307,243,212,186,175,110,&
         &98,86,86,85,63,57,56,49,47,45,43,39,38,37,37,36,35,33,32,32,28,28,26/)
    B0j = (/0.,3.09846350,3.05525000,5.1985,1.1739,2.8469,5.453,4.564,3.661,0.964,5.90,0.299,&
         &4.273,5.847,5.022,3.012,5.055,0.890,5.69,1.27,0.270,0.920,2.01,5.24,3.25,2.58,5.54,&
         &6.01,5.36,2.39,0.830,4.90,1.67,1.84,0.240,0.180,1.78,1.21,1.90,4.59/)
    C0j = (/0.,6283.07585,12566.1517,77713.7715,5753.38490,7860.41940,11506.7700,3930.21000,&
         &5884.92700,5507.55300,5223.69400,5573.14300,11790.6290,1577.34400,10977.0790,18849.2280,&
         &5486.77800,6069.78000,15720.8400,161000.690,17260.1500,529.69,83996.8500,71430.7000,&
         &2544.31000,775.52,9437.76000,6275.96000,4694.,8827.39000,19651.0500,12139.5500,&
         &12036.4600,2942.46000,7084.9,5088.63000,398.15,6286.6,6279.55000,10447.3900/)
    A1j = (/103019,1721,702,32,31,25,18,10,9,9/)
    B1j = (/1.10749000,1.0644,3.142,1.02,2.84,1.32,1.42,5.91,1.42,0.270/)
    C1j = (/6283.07585,12566.1517,0.,18849.2300,5507.55000,5223.69000,1577.34000,10977.0800,&
         &6275.96000,5486.78000/)
    A2j = (/4359,124,12,9,6,3/)
    B2j = (/5.7846,5.579,3.14,3.63,1.87,5.47/)
    C2j = (/6283.07580,12566.1520,0.,77713.7700,5573.14000,18849./)
    A3j = (/145,7/)
    B3j = (/4.273,3.92/)
    C3j = (/6283.07600,12566.1500/)
    A4j = 4
    B4j = 2.56
    C4j = 6283.08000

    
    L0 = SUM(A0j * COS(B0j + (C0j * JME)))
    L1 = SUM(A1j * COS(B1j + (C1j * JME)))
    L2 = SUM(A2j * COS(B2j + (C2j * JME)))
    L3 = SUM(A3j * COS(B3j + (C3j * JME)))
    L4 = A4j * COS(B4j + (C4j * JME))

    
    earth_heliocentric_positionradius = &
         &(L0 + (L1 * JME) + (L2 * JME**2) + (L3 * JME**3) + (L4 * JME**4)) / 1e8

  END SUBROUTINE earth_heliocentric_position_calculation

  SUBROUTINE sun_geocentric_position_calculation(earth_heliocentric_positionlongitude,&
       &earth_heliocentric_positionlatitude, sun_geocentric_positionlatitude, &
       &sun_geocentric_positionlongitude)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlongitude      
    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionlatitude      
    REAL(KIND(1D0)) :: sun_geocentric_positionlatitude      
    REAL(KIND(1D0)) :: sun_geocentric_positionlongitude      

    

    sun_geocentric_positionlongitude = earth_heliocentric_positionlongitude + 180.0
    
    sun_geocentric_positionlongitude=set_to_range(sun_geocentric_positionlongitude)

    sun_geocentric_positionlatitude = -earth_heliocentric_positionlatitude
    
    sun_geocentric_positionlatitude=set_to_range(sun_geocentric_positionlatitude)
  END SUBROUTINE sun_geocentric_position_calculation

  SUBROUTINE nutation_calculation(julianephemeris_century,nutationlongitude,nutationobliquity)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: julianephemeris_century      
    REAL(KIND(1D0)), DIMENSION(63) :: delta_longitude      
    REAL(KIND(1D0)), DIMENSION(63) :: delta_obliquity      
    REAL(KIND(1D0)) :: JCE      
    REAL(KIND(1D0)) :: nutationlongitude      
    REAL(KIND(1D0)) :: nutationobliquity      
    REAL(KIND(1D0)) , DIMENSION(4) :: p0,p1,p2,p3,p4
    REAL(KIND(1D0)), DIMENSION(63) ::tabulated_argument      
    REAL(KIND(1D0)) :: X0      
    REAL(KIND(1D0)) :: X1      
    REAL(KIND(1D0)) :: X2      
    REAL(KIND(1D0)) :: X3      
    REAL(KIND(1D0)) :: X4      
    REAL(KIND(1D0)), DIMENSION(5) :: Xi      
    INTEGER, DIMENSION(315) :: Y_terms1     
    INTEGER, DIMENSION(5,63) ::Y_terms
    REAL(KIND(1D0)), DIMENSION(252) :: nutation_terms1     
    REAL(KIND(1D0)), DIMENSION(4,63) ::nutation_terms
    INTEGER :: i
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    
    

    
    JCE = julianephemeris_century

    
    p0 = (/ (1/189474.),-0.0019142,445267.11148,297.85036 /)
    
    X0 = p0(1) * JCE**3 + p0(2) * JCE**2 + p0(3) * JCE + p0(4) 

    
    p1 = (/ -(1/300000.),-0.0001603,35999.05034,357.52772 /)
    
    X1 = p1(1) * JCE**3 + p1(2) * JCE**2 + p1(3) * JCE + p1(4)

    
    p2 = (/(1/56250.),0.0086972,477198.867398,134.96298 /)
    
    X2 = p2(1) * JCE**3 + p2(2) * JCE**2 + p2(3) * JCE + p2(4)

    
    p3 = (/ (1/327270.),-0.0036825,483202.017538,93.27191 /)
    
    X3 = p3(1) * JCE**3 + p3(2) * JCE**2 + p3(3) * JCE + p3(4)


    
    p4 = (/ (1/450000.),0.0020708,-1934.136261,125.04452 /)
    
    X4 = p4(1) * JCE**3 + p4(2) * JCE**2 + p4(3) * JCE + p4(4)

    
    Y_terms1 =  (/0,0,0,0,1,-2,0,0,2,2,0,0,0,2,2,0,0,0,0,2,0,1,0,0,0,0,0,1,0,0,-2,1,0,2,2,0,0,0,2,1, &
         0,0,1,2,2,-2,-1,0,2,2,-2,0,1,0,0,-2,0,0,2,1,0,0,-1,2,2,2,0,0,0,0,0,0,1,0,1,2,0,-1,2,2,&
         0,0,-1,0,1,0,0,1,2,1,-2,0,2,0,0,0,0,-2,2,1,2,0,0,2,2,0,0,2,2,2,0,0,2,0,0,-2,0,1,2,2,0,&
         0,0,2,0,-2,0,0,2,0,0,0,-1,2,1,0,2,0,0,0,2,0,-1,0,1,-2,2,0,2,2,0,1,0,0,1,-2,0,1,0,1,0,-1,0,0,1,0,0,&
         2,-2,0,2,0,-1,2,1,2,0,1,2,2,0,1,0,2,2,-2,1,1,0,0,0,-1,0,2,2,2,0,0,2,1,2,0,1,0,0,-2,0,2,2,2,-2,0,1,&
         2,1,2,0,-2,0,1,2,0,0,0,1,0,-1,1,0,0,-2,-1,0,2,1,-2,0,0,0,1,0,0,2,2,1,-2,0,2,0,1,-2,1,0,2,1,0,0,1,-2,&
         0,-1,0,1,0,0,-2,1,0,0,0,1,0,0,0,0,0,0,1,2,0,0,0,-2,2,2,-1,-1,1,0,0,0,1,1,0,0,0,-1,1,2,2,2,-1,-1,2,2,&
         0,0,3,2,2,2,-1,0,2,2/)
    Y_terms=RESHAPE(Y_terms1,(/5,63/))
    nutation_terms1 = (/-171996.,-174.2,92025.,8.9,-13187.,-1.6,5736.,-3.1,-2274.,-0.2,977.,-0.5,2062.,0.2,-895.,0.5,&
         1426.,-3.4,54.,-0.1,712.,0.1,-7.,0.,-517.,1.2,224.,-0.6,-386.,-0.4,200.,0.,-301.,0.,129.,-0.1,&
         217.,-0.5,-95.,0.3,-158.,0.,0.,0.,129.,0.1,-70.,0.,123.,0.,-53.,0.,63.,0.,0.,0.,63.,0.1,-33.,0.,-59.,0.,26.,0.,&
         -58.,-0.1,32.,0.,-51.,0.,27.,0.,48.,0.,0.,0.,46.,0.,-24.,0.,-38.,0.,16.,0.,-31.,0.,13.,0.,29.,0.,&
         0.,0.,29.,0.,-12.,0.,26.,0.,0.,0.,-22.,0.,0.,0.,21.,0.,-10.,0.,17.,-0.1,0.,0.,16.,0.,-8.,0.,-16.,0.1,7.,0.,&
         -15.,0.,9.,0.,-13.,0.,7.,0.,-12.,0.,6.,0.,11.,0.,0.,0.,-10.,0.,5.,0.,-8.,0.,3.,0.,7.,0.,-3.,0.,-7.,0.,0.,0.,&
         -7.,0.,3.,0.,-7.,0.,3.,0.,6.,0.,0.,0.,6.,0.,-3.,0.,6.,0.,-3.,0.,-6.,0.,3.,0.,-6.,0.,3.,0.,5.,0.,0.,0.,-5.,0.,&
         3.,0.,-5.,0.,3.,0.,-5.,0.,3.,0.,4.,0.,0.,0.,4.,0.,0.,0.,4.,0.,0.,0.,-4.,0.,0.,0.,-4.,0.,0.,0.,-4.,0.,0.,0.,&
         3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0.,-3.,0.,0.,0./)
    nutation_terms=RESHAPE(nutation_terms1,(/4,63/))
    
    
    Xi = (/X0, X1, X2, X3, X4/)

    DO i=1,63
       tabulated_argument(i)=&
            &((Y_terms(1,i)*Xi(1))+(Y_terms(2,i)*Xi(2))+(Y_terms(3,i)*Xi(3))+(Y_terms(4,i)*Xi(4))+(Y_terms(5,i)*Xi(5)))*pi/180
    END DO

    delta_longitude = ((nutation_terms(1,:) + (nutation_terms(2,:) * JCE))) * SIN(tabulated_argument)
    delta_obliquity = ((nutation_terms(3,:) + (nutation_terms(4,:) * JCE))) * COS(tabulated_argument)

    
    nutationlongitude = SUM(delta_longitude) / 36000000.0

    
    nutationobliquity = SUM(delta_obliquity) / 36000000.0

  END SUBROUTINE nutation_calculation

  SUBROUTINE corr_obliquity_calculation(julianephemeris_millenium, nutationobliquity, corr_obliquity)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: corr_obliquity      
    REAL(KIND(1D0)), INTENT(in) :: julianephemeris_millenium     
    REAL(KIND(1D0)), INTENT(in) :: nutationobliquity     
    REAL(KIND(1D0)) :: mean_obliquity      
    REAL(KIND(1D0)), DIMENSION(11) :: p      
    REAL(KIND(1D0)) :: U      

    


    p = (/ 2.45,5.79,27.87,7.12,-39.05,-249.67,-51.38,1999.25,-1.55,-4680.93,84381.448 /)
    

    U = julianephemeris_millenium/10
    mean_obliquity =&
         &p(1)*U**10 + p(2)*U**9 + p(3)*U**8 + p(4)*U**7 + p(5)*U**6 + p(6)*U**5 + p(7)*U**4 + &
         &p(8)*U**3 + p(9)*U**2 + p(10)*U + p(11)

    corr_obliquity = (mean_obliquity/3600) + nutationobliquity
  END SUBROUTINE corr_obliquity_calculation

  SUBROUTINE abberation_correction_calculation(earth_heliocentric_positionradius, aberration_correction)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: aberration_correction      
    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius     

    
    

    aberration_correction = -20.4898/(3600*earth_heliocentric_positionradius)

  END SUBROUTINE abberation_correction_calculation

  SUBROUTINE apparent_sun_longitude_calculation(sun_geocentric_positionlongitude, nutationlongitude,&
       & aberration_correction, apparent_sun_longitude)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: aberration_correction      
    REAL(KIND(1D0)), INTENT(out) :: apparent_sun_longitude      
    REAL(KIND(1D0)), INTENT(in) :: nutationlongitude      
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlongitude      

    

    apparent_sun_longitude = sun_geocentric_positionlongitude + nutationlongitude + aberration_correction

  END SUBROUTINE apparent_sun_longitude_calculation

  SUBROUTINE apparent_stime_at_greenwich_calculation(julianday,juliancentury, nutationlongitude,&
       & corr_obliquity, apparent_stime_at_greenwich)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(out) :: apparent_stime_at_greenwich      
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      
    REAL(KIND(1D0)), INTENT(in) :: julianday     
    REAL(KIND(1D0)), INTENT(in) :: juliancentury     
    REAL(KIND(1D0)), INTENT(in) :: nutationlongitude      
    REAL(KIND(1D0)) :: JC      
    REAL(KIND(1D0)) :: JD      
    REAL(KIND(1D0)) :: mean_stime      
    REAL(KIND(1D0)),PARAMETER       :: pi=3.14159265358979d+0

    

    JD = julianday
    JC = juliancentury

    
    mean_stime = 280.46061837d+0 + (360.98564736629d+0*(JD-2451545.0d+0)) + (0.000387933d+0*JC**2) - (JC**3/38710000.0d+0)

    
    mean_stime=set_to_range(mean_stime)

    apparent_stime_at_greenwich = mean_stime + (nutationlongitude * COS(corr_obliquity * pi/180))
  END SUBROUTINE apparent_stime_at_greenwich_calculation

  SUBROUTINE sun_rigth_ascension_calculation(apparent_sun_longitude, corr_obliquity, &
       &sun_geocentric_positionlatitude, sun_rigth_ascension)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude      
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude      
    REAL(KIND(1D0)), INTENT(out) :: sun_rigth_ascension      
    REAL(KIND(1D0)) :: argument_denominator      
    REAL(KIND(1D0)) :: argument_numerator      
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    

    argument_numerator = (SIN(apparent_sun_longitude * pi/180.0) * COS(corr_obliquity * pi/180.0)) - &
         (TAN(sun_geocentric_positionlatitude * pi/180.0) * SIN(corr_obliquity * pi/180.0))
    argument_denominator = COS(apparent_sun_longitude * pi/180.0)

    sun_rigth_ascension = ATAN2(argument_numerator, argument_denominator) * 180.0/pi
    
    sun_rigth_ascension=set_to_range(sun_rigth_ascension)
  END SUBROUTINE sun_rigth_ascension_calculation

  SUBROUTINE sun_geocentric_declination_calculation(apparent_sun_longitude, corr_obliquity, &
       &sun_geocentric_positionlatitude, sun_geocentric_declination)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_sun_longitude      
    REAL(KIND(1D0)), INTENT(in) :: corr_obliquity      
    REAL(KIND(1D0)), INTENT(out) :: sun_geocentric_declination      
    REAL(KIND(1D0)), INTENT(in) :: sun_geocentric_positionlatitude     
    REAL(KIND(1D0)) :: argument      
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    argument = (SIN(sun_geocentric_positionlatitude * pi/180.0) * COS(corr_obliquity * pi/180.0)) + &
         (COS(sun_geocentric_positionlatitude * pi/180.0) * SIN(corr_obliquity * pi/180) * SIN(apparent_sun_longitude * pi/180.0))

    sun_geocentric_declination = ASIN(argument) * 180.0/pi
  END SUBROUTINE sun_geocentric_declination_calculation

  SUBROUTINE observer_local_hour_calculation(apparent_stime_at_greenwich, locationlongitude, &
       &sun_rigth_ascension, observer_local_hour)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: apparent_stime_at_greenwich      
    REAL(KIND(1D0)), INTENT(in) :: locationlongitude     
    REAL(KIND(1D0)), INTENT(out) :: observer_local_hour      
    REAL(KIND(1D0)), INTENT(in) :: sun_rigth_ascension      


    observer_local_hour = apparent_stime_at_greenwich + locationlongitude - sun_rigth_ascension
    
    observer_local_hour=set_to_range(observer_local_hour)
  END SUBROUTINE observer_local_hour_calculation

  SUBROUTINE topocentric_sun_position_calculate(topocentric_sun_positionrigth_ascension &
       &,topocentric_sun_positionrigth_ascension_parallax,topocentric_sun_positiondeclination,&
       &locationaltitude,locationlatitude,observer_local_hour,sun_rigth_ascension,&
       &sun_geocentric_declination,earth_heliocentric_positionradius)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: earth_heliocentric_positionradius
    REAL(KIND(1D0)), INTENT(in) :: locationlatitude      
    REAL(KIND(1D0)), INTENT(in) :: locationaltitude
    REAL(KIND(1D0)), INTENT(in) :: observer_local_hour      
    REAL(KIND(1D0)),  INTENT(in) :: sun_geocentric_declination      
    REAL(KIND(1D0)),  INTENT(in) :: sun_rigth_ascension      
    REAL(KIND(1D0)) :: denominator      
    REAL(KIND(1D0)) :: eq_horizontal_parallax      
    REAL(KIND(1D0)) :: nominator      
    REAL(KIND(1D0)) :: sun_rigth_ascension_parallax      
    REAL(KIND(1D0)) :: topocentric_sun_positiondeclination      
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension      
    REAL(KIND(1D0)) :: topocentric_sun_positionrigth_ascension_parallax      
    REAL(KIND(1D0)) :: u      
    REAL(KIND(1D0)) :: x      
    REAL(KIND(1D0)) :: y      
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0

    
    
    

    
    eq_horizontal_parallax = 8.794 / (3600 * earth_heliocentric_positionradius)

    
    u = ATAN(0.99664719 * TAN(locationlatitude * pi/180))

    
    x = COS(u) + ((locationaltitude/6378140) * COS(locationaltitude * pi/180))

    
    y = (0.99664719d+0 * SIN(u)) + ((locationaltitude/6378140) * SIN(locationlatitude * pi/180))

    
    nominator = -x * SIN(eq_horizontal_parallax * pi/180.0) * SIN(observer_local_hour * pi/180.0)
    denominator = COS(sun_geocentric_declination * pi/180.0) - &
         (x * SIN(eq_horizontal_parallax * pi/180.0) * COS(observer_local_hour * pi/180.0))
    sun_rigth_ascension_parallax = ATAN2(nominator, denominator)
    
    topocentric_sun_positionrigth_ascension_parallax = sun_rigth_ascension_parallax * 180.0/pi

    
    topocentric_sun_positionrigth_ascension = sun_rigth_ascension + (sun_rigth_ascension_parallax * 180.0/pi)

    
    nominator = (SIN(sun_geocentric_declination * pi/180.0) - (y*SIN(eq_horizontal_parallax * pi/180.0)))&
         & * COS(sun_rigth_ascension_parallax)
    denominator = COS(sun_geocentric_declination * pi/180.0) - (y*SIN(eq_horizontal_parallax * pi/180.0))&
         & * COS(observer_local_hour * pi/180.0)
    topocentric_sun_positiondeclination = ATAN2(nominator, denominator) * 180.0/pi
  END SUBROUTINE topocentric_sun_position_calculate

  SUBROUTINE topocentric_local_hour_calculate(observer_local_hour, topocentric_sun_positionrigth_ascension_parallax,&
       & topocentric_local_hour)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: observer_local_hour      
    REAL(KIND(1D0)), INTENT(out) :: topocentric_local_hour      
    REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positionrigth_ascension_parallax     

    

    topocentric_local_hour = observer_local_hour - topocentric_sun_positionrigth_ascension_parallax
  END SUBROUTINE topocentric_local_hour_calculate

  SUBROUTINE sun_topocentric_zenith_angle_calculate(locationlatitude , topocentric_sun_positiondeclination, &
       &topocentric_local_hour, sunazimuth,sunzenith)
    IMPLICIT NONE

    REAL(KIND(1D0)), INTENT(in) :: locationlatitude     
    REAL(KIND(1D0)), INTENT(in) :: topocentric_local_hour      
    REAL(KIND(1D0)), INTENT(in) :: topocentric_sun_positiondeclination
    REAL(KIND(1D0)) :: corr_elevation      
    REAL(KIND(1D0)) :: apparent_elevation      
    REAL(KIND(1D0)) :: argument      
    REAL(KIND(1D0)) :: denominator      
    REAL(KIND(1D0)) :: nominator      
    REAL(KIND(1D0)) :: refraction_corr      
    REAL(KIND(1D0)) :: sunazimuth      
    REAL(KIND(1D0)) :: sunzenith      
    REAL(KIND(1D0)),PARAMETER       :: pi=3.141592653589793d+0
    
    
    

    
    argument = (SIN(locationlatitude * pi/180.0) * SIN(topocentric_sun_positiondeclination * pi/180.0)) + &
         (COS(locationlatitude * pi/180.0) * COS(topocentric_sun_positiondeclination * pi/180.0) * &
         &COS(topocentric_local_hour * pi/180.0))
    corr_elevation = ASIN(argument) * 180.0/pi

    
    argument = corr_elevation + (10.3/(corr_elevation + 5.11))
    refraction_corr = 1.02 / (60 * TAN(argument * pi/180.0))

    
    
    

    
    apparent_elevation = corr_elevation + refraction_corr

    sunzenith = 90.0 - apparent_elevation

    
    
    
    nominator = SIN(topocentric_local_hour * pi/180.0)
    denominator = (COS(topocentric_local_hour * pi/180.0) * SIN(locationlatitude * pi/180.0)) - &
         (TAN(topocentric_sun_positiondeclination * pi/180.0) * COS(locationlatitude * pi/180.0))
    sunazimuth = (ATAN2(nominator, denominator) * 180.0/pi) + 180.0
    
    sunazimuth=set_to_range(sunazimuth)

  END SUBROUTINE sun_topocentric_zenith_angle_calculate

  FUNCTION set_to_range(var) RESULT(vari)
    

    REAL(KIND(1D0)) :: max_interval      
    REAL(KIND(1D0)) :: min_interval      
    REAL(KIND(1D0)) :: var
    REAL(KIND(1D0)) :: vari
    
    max_interval=360.0
    min_interval=0.0

    vari = var - max_interval * FLOOR(var/max_interval)

    IF (vari<min_interval) THEN
       vari = vari + max_interval
    END IF

  END FUNCTION set_to_range

  
  FUNCTION dewpoint(Temp_C,rh) RESULT(td)
    
    
    
    
    

    REAL(KIND(1d0))::rh,td,Temp_C,g
    
    g=((17.27*Temp_C)/(237.7+Temp_C))+LOG(rh/100)
    Td=(237.7*g)/(17.27-g)
    
  END FUNCTION dewpoint
  
  FUNCTION PRATA_EMIS(Temp_K,EA_hPa) RESULT(EMIS_A)
    
    REAL(KIND(1d0))::Temp_K,ea_hPa,EMIS_A
    REAL(KIND(1d0))::W

    W=46.5*(ea_hPa/Temp_K)
    EMIS_A=1.-(1.+W)*EXP(-SQRT(1.2+3.*W))
  END FUNCTION PRATA_EMIS
  
  FUNCTION EMIS_CLOUD(EMIS_A,FCLD) RESULT(em_adj)
    
    REAL(KIND(1d0))::EMIS_A,FCLD,em_adj
    
    
    em_adj=EMIS_A+(1.-EMIS_A)*FCLD
  END FUNCTION EMIS_CLOUD
  
  FUNCTION EMIS_CLOUD_SQ(EMIS_A,FCLD) RESULT(em_adj)
    
    REAL(KIND(1d0))::EMIS_A,FCLD,em_adj
    em_adj=EMIS_A+(1.-EMIS_A)*FCLD*FCLD
  END FUNCTION EMIS_CLOUD_SQ
  
  FUNCTION cloud_fraction(KDOWN,KCLEAR) RESULT(FCLD)
    REAL(KIND(1d0))::KDOWN,KCLEAR,FCLD

    FCLD=1.-KDOWN/KCLEAR
    IF(FCLD>1.) FCLD=1.
    IF(FCLD<0.) FCLD=0.
  END FUNCTION cloud_fraction
  
  FUNCTION WC_fraction(RH,Temp) RESULT(FWC)

    
    REAL(KIND(1d0)),INTENT(in)   :: RH      
    REAL(KIND(1d0)),INTENT(in)   :: Temp    

    REAL(KIND(1d0))              :: FWC     
    REAL(KIND(1d0))              :: A, B    

    
    
    

    A=0.185
    B=0.00019*Temp+0.015

    
    FWC=A * (EXP(B * RH)-1)
    IF(FWC>1.) FWC=1.
    IF(FWC<0.) FWC=0.
  END FUNCTION WC_fraction
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  FUNCTION ISURFACE(doy,zenith) RESULT(Isurf)
    
    
    
    REAL(KIND(1d0))::zenith,Isurf
    INTEGER::doy
    REAL(KIND(1d0))::Rmean, Rse, cosZ,Itoa
    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292

    Rmean = 149.6                 
    Rse=solar_ESdist(doy)
    IF(zenith<90.*DEG2RAD) THEN
       cosZ = COS(zenith)
       Itoa = 1370.*(Rmean/Rse)**2  
       Isurf = Itoa*cosZ            
    ELSE
       Isurf = 0.
    ENDIF

  END FUNCTION ISURFACE

  
  FUNCTION solar_ESdist(doy) RESULT(Rse)
    
    INTEGER          ::doy
    REAL(KIND(1d0))             ::Rse
    REAL(KIND(1d0)) ::MA,nu,e,a

    e = 0.0167
    a = 146.457

    MA = 2.*3.141592654*(doy-3)/365.25463 
    nu=MA+0.0333988*SIN(MA)+.0003486*SIN(2.*MA)+5e-6*SIN(3.*MA) 
    Rse = a*(1-e*e)/(1+e*COS(nu))

  END FUNCTION solar_ESdist

  
  FUNCTION SmithLambda(lat) RESULT(G)
    USE FileName
    USE defaultnotUsed
    
    
    
    
    INTEGER :: lat,ios,ilat
    REAL(KIND(1d0)),DIMENSION(365):: G

    
    
    OPEN(99,file=smithFile,iostat=ios)
    DO ilat=1,lat
       READ(99,*)
    ENDDO
    READ(99,*,iostat=ios)ilat, G
    IF (ios/=0) THEN
       CALL  ErrorHint(11,'reading Smith1966.grd (ios).',notUsed,notUsed,ios)
    ENDIF
    CLOSE(99)
  END FUNCTION SmithLambda

  
  FUNCTION transmissivity(Press_hPa,Temp_C_dew,G,zenith) RESULT(trans)
    
    
    
    
    
    

    REAL(KIND(1d0)) ::Press_hPa,TemP_C_dew,zenith,G,trans
    REAL(KIND(1d0))::m,TrTpg,u,Tw,Ta,cosZ
    REAL(KIND(1d0))::Tdf
    REAL(KIND(1D0)),PARAMETER   :: DEG2RAD=0.017453292


    IF (zenith>80.*DEG2RAD) THEN
       cosZ=COS(80.*DEG2RAD)
    ELSE
       cosZ=COS(zenith)
    ENDIF

    Tdf = TemP_C_dew*1.8+32. 
    
    m = 35*cosZ/SQRT(1224.*cosZ*cosZ+1)            
    
    TrTpg = 1.021-0.084*SQRT(m*(0.000949*Press_hPa+0.051)) 
    u = EXP(0.113-LOG(G+1)+0.0393*Tdf)             
    Tw = 1-0.077*(u*m)**0.3             
    Ta = 0.935**m                       
    trans = TrTpg*Tw*Ta                 
  END FUNCTION transmissivity
  
END MODULE NARP_MODULE


