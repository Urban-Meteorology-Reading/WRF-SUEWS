


















MODULE mod_interp
  
  IMPLICIT NONE

CONTAINS
  ELEMENTAL FUNCTION interp1d(x1,x2,y1,y2,xi) RESULT(yi)
    REAL(KIND(1d0)),INTENT(in) ::x1,x2,xi
    REAL(KIND(1d0)),INTENT(in) ::y1,y2
    REAL (KIND(1D0))::b0,b1
    REAL(KIND(1d0))         ::yi
    
    b1=(y2-y1)/(x2-x1)
    b0=y1-b1*x1
    yi=b0+b1*xi
  END FUNCTION interp1d
END MODULE mod_interp


MODULE mod_solver
  
  

  IMPLICIT NONE

CONTAINS

  FUNCTION NewtonPolynomial(x0,Pcoeff,conv,maxiter) RESULT(x)

    
    
    

    
    
    
    
    REAL(KIND(1d0)) ::x0,x,conv
    REAL(KIND(1d0)) ::Pcoeff(:)
    REAL(KIND(1d0)) ::e, xprev
    REAL (KIND(1D0))::f,fp
    INTEGER ::maxiter
    INTEGER ::niter
    LOGICAL ::converged=.FALSE.
    INTEGER ::n,i,j

    e=HUGE(1.)
    n=SIZE(Pcoeff)
    x=x0
    DO i=1,maxiter
       IF (ABS(e)<conv) THEN
          converged=.TRUE.
          EXIT
       ENDIF
       f=0; fp = 0
       DO j=1,n-1
          f = f+Pcoeff(j)*x**(n-j)
          fp = fp + Pcoeff(j)*(n-j)*x**(n-j-1)                              
       ENDDO

       f = f + Pcoeff(n)
       xprev = x
       IF (fp==0.) fp=TINY(1.)
       x = xprev - f/fp
       e = x-xprev
    ENDDO
    niter=i-1
    IF (.NOT.converged) THEN
       PRINT*, "Solution did not converge. Niter=", niter, " Error=", e
       x=x0
    ENDIF
  END FUNCTION NewtonPolynomial
END MODULE mod_solver



MODULE modSolarCalc
  USE MathConstants
  IMPLICIT NONE

CONTAINS
  
  FUNCTION min_zenith(lat,doy) RESULT(zmin)
    
    
    REAL(KIND(1d0)) ::lat,dectime,zmin
    REAL(KIND(1d0)) ::latr,decl
    INTEGER :: doy
    dectime=float(doy)
    latr=lat*dtr
    decl=0.409*COS(2*pi*(dectime-173)/365.25)
    zmin=pi/2.-ASIN(SIN(latr)*SIN(decl)-COS(latr)*COS(decl)*(-1))
  END FUNCTION min_zenith
  

  
  FUNCTION Local_apparent_time(lng,dectime) RESULT(la_time)
    
    REAL(KIND(1d0)) ::lng,dectime,la_time
    REAL(KIND(1d0)) ::gamma,eqtime,lmst

    lmst=dectime-4.*lng/60./1440.
    gamma=2.*pi/365.*(lmst-1.)
    eqtime=229.18*(7.5e-5+1.868e-3*COS(gamma)-0.032077*SIN(gamma)&
         &    -0.014615*COS(2.*gamma)-0.040849*SIN(2.*gamma))
    la_time=lmst+eqtime/1440.
  END FUNCTION Local_apparent_time
  

  SUBROUTINE Solar_angles(lat,lng,timezone,dectime,decl,zenith,azimuth)

    REAL, INTENT(in)  ::lat,lng,timezone,dectime
    INTEGER                 ::doy,hour,mn
    REAL(KIND(1d0)), INTENT(out)  ::decl,zenith,azimuth
    REAL (KIND(1d0))  ::ha,latr,eqtime,tst,&
         time_offset,gamma           

    latr=lat*pi/180.
    doy=FLOOR(dectime)
    hour=FLOOR((dectime-doy)*24.)
    mn=FLOOR((dectime-doy-hour/24.)*60.)   

    gamma=2.*pi/365.25463*(doy-1.+(hour-12.)/24.)
    eqtime=229.18*(7.5e-5+1.868e-3*COS(gamma)-0.032077*SIN(gamma)&
         &    -0.014615*COS(2.*gamma)-0.040849*SIN(2.*gamma))
    decl=6.918e-3-0.399912*COS(gamma)+0.070257*SIN(gamma)&
         &    -0.006758*COS(2.*gamma)+9.07e-4*SIN(2.*gamma)-2.697e-3*COS(3.*gamma)&
         &    +1.48e-3*SIN(3.*gamma)
    time_offset=eqtime-4.*lng+60.*timezone
    tst=hour*60.+mn+time_offset
    ha=(tst/4.)-180.
    ha=ha*pi/180.

    zenith=ACOS(SIN(latr)*SIN(decl)+COS(latr)*COS(decl)*COS(ha))
    azimuth=pi+ACOS((SIN(latr)*COS(zenith)-SIN(decl))/(COS(latr)*SIN(zenith)))

    RETURN
  END SUBROUTINE Solar_angles

  
  SUBROUTINE Solar_Times(lat,lng,timezone,dectime,sunrise,sunset,snoon)
    
    

    REAL(KIND(1d0)), INTENT(in)  ::lat,lng,timezone,dectime
    INTEGER                 ::doy
    REAL(KIND(1d0)), INTENT(out)  ::sunrise, sunset, snoon
    REAL(KIND(1d0))  :: ha, latr, eqtime, gamma, zenith, decl
    latr = lat*dtr
    zenith=90.833*dtr
    doy=FLOOR(dectime)
    gamma=2.*pi/365.*(float(doy)-0.5) 
    eqtime=229.18*(7.5e-5+1.868e-3*COS(gamma)-0.032077*SIN(gamma)&
         &    -0.014615*COS(2.*gamma)-0.040849*SIN(2.*gamma))
    decl=6.918e-3-0.399912*COS(gamma)+0.070257*SIN(gamma)&
         &    -0.006758*COS(2.*gamma)+9.07e-4*SIN(2.*gamma)-2.697e-3*COS(3.*gamma)&
         &    +1.48e-3*SIN(3.*gamma)
    ha=ACOS(COS(zenith)/(COS(latr)*COS(decl))-TAN(latr)*TAN(decl))
    ha=ha*rtd
    sunrise = (720.-4.*(lng-ha)-eqtime)/60.-timezone
    sunset = (720.-4.*(lng+ha)-eqtime)/60.-timezone
    snoon = (720.-4.*lng-eqtime)/60.-timezone
    RETURN
  END SUBROUTINE Solar_Times
  
  FUNCTION kdown_surface(doy,zenith) RESULT(Isurf)
    
    
    
    REAL(KIND(1d0))    ::zenith,Isurf
    INTEGER    ::doy
    REAL (KIND(1d0))::Rmean, Rse, cosZ,Itoa

    Rmean = 149.6   
    Rse=solar_ESdist(doy)
    IF(zenith<pi/2.) THEN
       cosZ = COS(zenith)
       Itoa = 1370*(Rmean/Rse)**2    
       Isurf = Itoa*cosZ      
    ELSE
       Isurf = 0.
    ENDIF

  END FUNCTION kdown_surface

  
  FUNCTION SmithLambda(lat) RESULT(G)
    
    INTEGER :: lat,ios
    REAL,DIMENSION(365):: G

    OPEN(99,file="Smith1966.grd",access="direct",action="read",recl=365*4,iostat=ios)
    IF (ios/=0) THEN
       PRINT*, "Iostat=",ios," reading Smith1966.grd"
       STOP
    ENDIF
    READ(99,rec=lat+1,iostat=ios) G
    IF (ios/=0) PRINT*, "Iostat=", ios, " reading Smith1966.grd"
    CLOSE(99)
  END FUNCTION SmithLambda
  
  FUNCTION transmissivity_CD(P,Td,G,zenith) RESULT(trans)           
    
    
    
    
    
    

    
    REAL(KIND(1d0))    ::P,Td,zenith,G,trans
    REAL (KIND(1d0))::m,TrTpg,u,Tw,Ta,cosZ
    REAL (KIND(1d0))::Tdf

    IF (zenith>80.*dtr) THEN
       cosZ=COS(80.*dtr)
    ELSE
       cosZ=COS(zenith)
    ENDIF
    Tdf = Td*1.8+32. 
    
    m = 35*cosZ/SQRT(1224.*cosZ*cosZ+1) 
    TrTpg = 1.021-0.084*SQRT(m*(0.000949*P+0.051)) 
    u = EXP(0.113-LOG(G+1)+0.0393*Tdf) 
    Tw = 1-0.077*(u*m)**0.3    
    Ta = 0.935**m        
    trans = TrTpg*Tw*Ta              
  END FUNCTION transmissivity_CD

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  FUNCTION solar_ESdist(doy) RESULT(Rse)
    
    INTEGER  ::doy
    REAL(KIND(1d0))  ::Rse
    REAL (KIND(1d0)) ::MA,nu,e,a

    e = 0.0167
    a = 146.457

    MA = 2.*pi*(doy-3)/365.25463 
    nu=MA+0.0333988*SIN(MA)+.0003486*SIN(2.*MA)+5e-6*SIN(3.*MA) 
    Rse = a*(1-e*e)/(1+e*COS(nu))

  END FUNCTION solar_ESdist

END MODULE modSolarCalc



MODULE heatflux
  IMPLICIT NONE
CONTAINS

  SUBROUTINE heatcond1d(T,Qs,dx,dt,k,rhocp,bc,bctype)
    REAL(KIND(1d0)),INTENT(inout)::T(:)
    REAL(KIND(1d0)),INTENT(in)::dx(:),dt,k(:),rhocp(:),bc(2)
    REAL(KIND(1d0)),INTENT(out)::Qs
    LOGICAL,INTENT(in)::bctype(2)
    INTEGER         ::i,n
    REAL(KIND(1d0)),ALLOCATABLE::w(:),a(:),T1(:)
    n=SIZE(T)
    ALLOCATE(w(0:n),a(n),T1(n))
    
    w(1:n)=T
    w(0)=bc(1); w(n)=bc(2)
    
    
    IF (bctype(1)) w(0)=bc(1)*0.5*dx(1)/k(1)+w(1)
    IF (bctype(2)) w(n)=bc(2)*0.5*dx(n)/k(n)+w(n)

    a=k/dx
    DO i=1,n-1
       w(i)=(T(i+1)*a(i+1)+T(i)*a(i))/(a(i)+a(i+1))
    ENDDO

    DO i=1,n
       T1(i) = (dt/rhocp(i))*(w(i-1)-2*T(i) + w(i))*2*a(i)/dx(i) + T(i)
    ENDDO

    
    Qs = (w(0)-T(1))*2*a(1) + (w(n)-T(n))*2*a(n)                           
    
    T=T1
  END SUBROUTINE heatcond1d
END MODULE heatflux



MODULE METEO

  USE MathConstants
  IMPLICIT NONE

  
  REAL (KIND(1d0)),PARAMETER ::  RAD2DEG=57.29577951
  REAL (KIND(1d0)),PARAMETER ::  DEG2RAD=0.017453292

  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_AIR=0.028965             
  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_CO2=0.04401              
  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_H2O=0.0180153            
  REAL (KIND(1d0)),PARAMETER ::  MU_H2O=MOLMASS_AIR/MOLMASS_H2O   
  REAL (KIND(1d0)),PARAMETER ::  MU_CO2=MOLMASS_AIR/MOLMASS_CO2   
  REAL (KIND(1d0)),PARAMETER ::  R_DRY_MOL=8.31451                
  REAL (KIND(1D0)),PARAMETER ::  R_DRY_MASS=R_DRY_MOL/MOLMASS_AIR 
  
  REAL (KIND(1d0)),PARAMETER ::  EPSIL=0.62197
  REAL (KIND(1d0)),PARAMETER ::  KB=1.3807E-25                    
  REAL (KIND(1d0)),PARAMETER ::  AVOGADRO=6.02252E23              

CONTAINS

  
  FUNCTION sat_vap_press(TK,P) RESULT(es)
    
    
    
    
    REAL(KIND(1d0))    :: TK,P,TC,es,e,f
    TC=TK-273.15
    IF(TC.EQ.0)THEN
       TC=0.001
    ENDIF
    
    e=6.1121*EXP(((18.729-TC/227.3)*TC)/(TC+257.87))
    f=1.00072+P*(3.2E-6+5.9E-10*TC**2)
    es=e*f
  END FUNCTION sat_vap_press

  REAL(KIND(1d0)) FUNCTION SOS_DRYAIR(TK)
    
    REAL(KIND(1d0)) ::TK
    SOS_DRYAIR=SQRT(1.4*R_DRY_MOL*TK/(MOLMASS_AIR*1000.))
  END FUNCTION SOS_DRYAIR
  
  REAL(KIND(1d0)) FUNCTION POTENTIAL_TEMP(TK,P)
    
    
    REAL(KIND(1d0))    ::TK,P
    POTENTIAL_TEMP=TK*(1000./P)**0.286
  END FUNCTION POTENTIAL_TEMP

  REAL(KIND(1d0)) FUNCTION LATENTHEAT_V(TK)
    
    
    REAL(KIND(1d0)) ::TK
    LATENTHEAT_V=2.501E6-2370.*(TK-273.15)
  END FUNCTION LATENTHEAT_V

  REAL(KIND(1d0)) FUNCTION LATENTHEAT_M(TK)
    
    
    REAL(KIND(1d0)) ::TK,TC
    TC=TK-273.15
    LATENTHEAT_M=3.3358E5+TC*(2030.-10.46*TC)
  END FUNCTION LATENTHEAT_M

  REAL(KIND(1d0)) FUNCTION SPEC_HEAT_DRYAIR(TK)
    
    REAL(KIND(1d0)) ::TK,TC
    TC=TK-273.15
    SPEC_HEAT_DRYAIR=1005.+((TC+23.15)**2)/3364.
  END FUNCTION SPEC_HEAT_DRYAIR

  REAL(KIND(1d0)) FUNCTION SPEC_HEAT_VAPOR(TK,RH)
    
    REAL(KIND(1d0)) ::TK,TC_100,RH
    TC_100=(TK-273.15)/100.
    SPEC_HEAT_VAPOR=1859.+0.13*RH+(19.3+0.569*RH)*TC_100+(10.+0.5*RH)*TC_100**2
  END FUNCTION SPEC_HEAT_VAPOR

  REAL(KIND(1d0)) FUNCTION HEATCAPACITY_AIR(TK,RH,P)
    REAL(KIND(1d0)) ::TK,RH,P
    REAL(KIND(1d0)) ::RHO_D,RHO_V
    REAL(KIND(1d0)) ::CPD,CPV
    RHO_D=DENSITY_DRYAIR(TK,P)
    RHO_V=DENSITY_VAPOR(TK,RH,P)
    CPD=SPEC_HEAT_DRYAIR(TK)
    CPV=SPEC_HEAT_VAPOR(TK,RH)
    HEATCAPACITY_AIR=RHO_D*CPD+RHO_V*CPV
  END FUNCTION HEATCAPACITY_AIR

  REAL(KIND(1d0)) FUNCTION DENSITY_MOIST(TVK,P)
    
    
    
    REAL(KIND(1d0)) ::TVK,P
    DENSITY_MOIST=P*100./(R_DRY_MASS*TVK)
  END FUNCTION DENSITY_MOIST

  REAL(KIND(1d0)) FUNCTION DENSITY_VAPOR(TK,RH,P)
    
    REAL(KIND(1d0))    ::TK,P,RH,EA
    EA=SAT_VAP_PRESS(TK,P)*RH/100.
    DENSITY_VAPOR=(EA*100.*EPSIL)/(R_DRY_MASS*TK)
  END FUNCTION DENSITY_VAPOR

  REAL(KIND(1d0)) FUNCTION DENSITY_DRYAIR(TK,P)
    REAL(KIND(1d0)) ::TK,P
    DENSITY_DRYAIR=P*100./(R_DRY_MASS*TK)
  END FUNCTION DENSITY_DRYAIR

  REAL(KIND(1d0)) FUNCTION DENSITY_GAS(TK,PP,MOLMASS)
    
    REAL(KIND(1d0)) ::TK,PP,MOLMASS
    DENSITY_GAS=PP*MOLMASS/(R_DRY_MOL*TK)
  END FUNCTION DENSITY_GAS

  REAL(KIND(1d0)) FUNCTION PARTIAL_PRESSURE(TK,N)
    
    REAL(KIND(1d0)) ::TK,N 
    PARTIAL_PRESSURE=N*KB*TK
  END FUNCTION PARTIAL_PRESSURE

  REAL(KIND(1d0)) FUNCTION SCALE_HEIGHT(TK)
    REAL(KIND(1d0)) ::TK
    
    SCALE_HEIGHT=R_DRY_MOL*TK/(MOLMASS_AIR*9.81)
  END FUNCTION SCALE_HEIGHT

  REAL(KIND(1d0)) FUNCTION VAISALA_BRUNT_F(TK)
    
    REAL(KIND(1d0)) ::TK
    VAISALA_BRUNT_F=SQRT(0.4/1.4*9.81/SCALE_HEIGHT(TK))
  END FUNCTION VAISALA_BRUNT_F

END MODULE METEO

MODULE ESTM_module
  
  
  
  

  IMPLICIT NONE


CONTAINS

  
  
  
  SUBROUTINE SUEWS_GetESTMData(lunit)
    USE allocateArray, ONLY:ncolsestmdata, estmforcingdata
    USE data_in, ONLY: fileestmts, skipheadermet
    USE sues_data, ONLY: tstep_real, tstep
    USE defaultnotUsed, ONLY: notused, ios_out
    USE Initial, ONLY: skippedlines, readlinesmetdata, gridcounter

    IMPLICIT NONE

    INTEGER,INTENT(in)::lunit
    INTEGER::i,iyy 
    INTEGER :: iostat_var
    REAL(KIND(1d0)),DIMENSION(ncolsESTMdata):: ESTMArray
    REAL(KIND(1d0)):: imin_prev, ih_prev, iday_prev, tstep_estm   

    

    
    
    OPEN(lunit,file=TRIM(FileESTMTs),status='old',err=315)
    CALL skipHeader(lunit,SkipHeaderMet)

    
    IF (skippedLines>0) THEN
       DO iyy=1,skippedLines
          READ(lunit,*)
       ENDDO
    ENDIF

    
    DO i=1,ReadlinesMetdata
       READ(lunit,*,iostat=iostat_var) ESTMArray
       ESTMForcingData(i,1:ncolsESTMdata,GridCounter) = ESTMArray
       
       IF(i==1) THEN
          imin_prev = ESTMArray(4)
          ih_prev   = ESTMArray(3)
          iday_prev = ESTMArray(2)
       ELSEIF(i==2) THEN
          tstep_estm = ((ESTMArray(4)+60*ESTMArray(3)) - (imin_prev+60*ih_prev))*60   
          IF(tstep_estm.NE.tstep_real.AND.ESTMArray(2)==iday_prev) THEN
             CALL ErrorHint(39,'TSTEP in RunControl does not match TSTEP of ESTM data (DOY).',REAL(tstep,KIND(1d0)),tstep_estm,&
                  INT(ESTMArray(2)))
          ENDIF
       ENDIF
    ENDDO

    CLOSE(lunit)

    RETURN

315 CALL errorHint(11,TRIM(fileESTMTs),notUsed,notUsed,ios_out)

  END SUBROUTINE SUEWS_GetESTMData
  


  
  SUBROUTINE ESTM_initials

    
    
    

    USE defaultNotUsed
    USE PhysConstants, ONLY: c2k
    USE ESTM_data
    USE allocateArray
    USE data_in, ONLY: fileinputpath
    USE Initial, ONLY: numberofgrids

    IMPLICIT NONE

    
    NAMELIST/ESTMinput/TsurfChoice,&
         evolveTibld,              &
         ibldCHmod,                &
         LBC_soil,                 &
         THEAT_ON,                 &
         THEAT_OFF,                &
         THEAT_fix

    OPEN(511,file=TRIM(FileInputPath)//'ESTMinput.nml',status='old')
    READ(511,nml=ESTMinput)
    CLOSE(511)

    
    THEAT_ON=THEAT_ON+C2K
    THEAT_OFF=THEAT_OFF+C2K
    THEAT_fix=THEAT_fix+C2K

    ALLOCATE(Tair2_grids(NumberOfGrids))
    ALLOCATE(lup_ground_grids(NumberOfGrids))
    ALLOCATE(lup_wall_grids(NumberOfGrids))
    ALLOCATE(lup_roof_grids(NumberOfGrids))
    ALLOCATE(Tievolve_grids(NumberOfGrids))
    ALLOCATE(T0_ibld_grids(NumberOfGrids))
    ALLOCATE(T0_ground_grids(NumberOfGrids))
    ALLOCATE(T0_wall_grids(NumberOfGrids))
    ALLOCATE(T0_roof_grids(NumberOfGrids))
    ALLOCATE(TN_wall_grids(NumberOfGrids))
    ALLOCATE(TN_roof_grids(NumberOfGrids))

  END SUBROUTINE ESTM_initials
  


  SUBROUTINE ESTM_translate(Gridiv)
    

    USE defaultNotUsed,ONLY: nan
    USE PhysConstants, ONLY: c2k, sbconst
    USE ESTM_data
    USE allocateArray
    USE gis_data, ONLY: bldgh
    USE Initial, ONLY: numberofgrids

    IMPLICIT NONE
    INTEGER :: i
    
    
    REAL(KIND(1d0))::W,WB
    
    
    
    INTEGER:: ESTMStart=0
    INTEGER:: Gridiv

    
    IF(Gridiv == 1) ESTMStart = ESTMStart+1
    IF(ESTMStart==1) THEN



       TFLOOR=20.0 
       TFLOOR=TFLOOR+C2K

       
       Tievolve=20.0 + C2K
       SHC_air=1230.0
       minshc_airbld=1300

       
       
       IVF_IW =   0.100000
       IVF_IR =   0.000000
       IVF_II =   0.900000
       IVF_IF =   0.000000
       IVF_WW =   0.050000
       IVF_WR =   0.000000
       IVF_WI =   0.950000
       IVF_WF =   0.000000
       IVF_RW =   0.050000
       IVF_RI =   0.950000
       IVF_RF =   0.000000
       IVF_FW =   0.050000
       IVF_FR =   0.000000
       IVF_FI =   0.950000

       Tair24HR=C2K

       
       
       Ts5mindata(1,1:ncolsESTMdata) = ESTMForcingData(1,1:ncolsESTMdata,Gridiv)


       
       
       IF ( .NOT. ALLOCATED(Tibld) ) THEN
          
          
          
          
          ALLOCATE(Tibld(Nibld),Twall(Nwall),Troof(Nroof),Tground(Nground),Tw_4(Nwall,4))
          ALLOCATE(Tibld_grids(Nibld,NumberOfGrids), &
               Twall_grids(Nwall,NumberOfGrids), &
               Troof_grids(Nroof,NumberOfGrids), &
               Tground_grids(Nground,NumberOfGrids), &
               Tw_4_grids(Nwall,4,NumberOfGrids))
       ENDIF

       
       
       
       
       
       
       
       DO i=1,Nground
          
          Tground(i)=(LBC_soil-Ts5mindata(1,cTs_Troad))*(i-1)/(Nground-1)+Ts5mindata(1,cTs_Troad)+C2K
       ENDDO
       DO i=1,Nwall
          Twall(i)=(Ts5mindata(1,cTs_Tiair)-Ts5mindata(1,cTs_Twall))*(i-1)/(Nwall-1)+Ts5mindata(1,cTs_Twall)+C2K
       ENDDO
       DO i=1,Nroof
          Troof(i)=(Ts5mindata(1,cTs_Tiair)-Ts5mindata(1,cTs_Troof))*(i-1)/(Nroof-1)+Ts5mindata(1,cTs_Troof)+C2K
       ENDDO
       Tibld(1:Nibld)=Ts5mindata(1,cTs_Tiair)+C2K

    ENDIF  

    
    ZREF=2.0*BldgH   

    svf_ground=1.0
    svf_roof=1.0

    
    
    alb_roof=alb(BldgSurf)
    em_roof=emis(BldgSurf)

    
    
    IF(fveg/=0) THEN
       alb_veg=(alb(ConifSurf)*sfr(ConifSurf) + alb(DecidSurf)*sfr(DecidSurf) + alb(GrassSurf)*sfr(GrassSurf))/fveg
       em_veg=(emis(ConifSurf)*sfr(ConifSurf) + emis(DecidSurf)*sfr(DecidSurf) + emis(GrassSurf)*sfr(GrassSurf))/fveg
    ELSE 
       alb_veg=NAN
       em_veg=NAN
    ENDIF

    
    
    IF(fground/=0) THEN
       alb_ground=(alb(ConifSurf)*sfr(ConifSurf)+alb(DecidSurf)*sfr(DecidSurf)&
            +alb(GrassSurf)*sfr(GrassSurf)+alb(PavSurf)*sfr(PavSurf)&
            +alb(BsoilSurf)*sfr(BsoilSurf)+alb(WaterSurf)*sfr(WaterSurf))/fground
       em_ground=(emis(ConifSurf)*sfr(ConifSurf)+emis(DecidSurf)*sfr(DecidSurf)&
            +emis(GrassSurf)*sfr(GrassSurf)+emis(PavSurf)*sfr(PavSurf)&
            +emis(BsoilSurf)*sfr(BsoilSurf)+emis(WaterSurf)*sfr(WaterSurf))/fground
    ELSE 
       alb_ground=NAN
       em_ground=NAN
    ENDIF

    IF(froof<1.0) THEN
       HW=fwall/(2.0*(1.0-froof))
    ELSE
       
       HW=0.00001  

    END IF
    HW=MAX(0.00001,HW)

    IF (Fground==1.0) THEN   
       W=1
       WB=0
       SVF_ground=1.
       zvf_WALL=0.
       SVF_WALL=0.
       SVF_ROOF=1.
       zvf_ground=0.
       xvf_wall=0.
       RVF_CANYON=1.
       RVF_ground=1.-FVEG
       RVF_ROOF=0
       RVF_WALL=0
       RVF_VEG=FVEG
    ELSE IF ( Fground==0.0 ) THEN 
       
       W=0
       WB=1
       zvf_WALL= 0 
       HW=0
       SVF_ground=MAX(COS(ATAN(2*HW)),0.00001)
       SVF_WALL=(1-zvf_WALL)/2                                                 
       zvf_ground=1-svf_ground                                                 
       xvf_wall=svf_wall                                                       
       
       
       
       
       RVF_ground=(fground-fveg)*SVF_ground
       RVF_veg=fveg*SVF_ground
       RVF_ROOF=froof
       RVF_Wall=1-RVF_ROOF-RVF_ground-RVF_VEG
    ELSE
       W=BldgH/HW   
       WB=W*SQRT(FROOF/Fground)
       SVF_ground=COS(ATAN(2*HW))                                              
       zvf_WALL=COS(ATAN(2/HW))                                                
       SVF_WALL=(1-zvf_WALL)/2                                                 
       zvf_ground=1-svf_ground                                                 
       xvf_wall=svf_wall                                                       
       
       
       
       
       RVF_ground=(fground-fveg)*SVF_ground
       RVF_veg=fveg*SVF_ground
       RVF_ROOF=froof
       RVF_Wall=1-RVF_ROOF-RVF_ground-RVF_VEG
    ENDIF

    alb_avg=alb_ground*RVF_ground + alb_wall*RVF_WALL + alb_roof*RVF_ROOF + alb_veg*RVF_VEG

    sumalb=0.; nalb=0
    sumemis=0.; nemis=0

    
    em_r = em_ibld; em_w=em_ibld; em_i=em_ibld; em_f=em_ibld

    
    IF (nroom==0) THEN
       fibld = (FLOOR(BldgH/3.1-0.5)-1)*froof
    ELSE
       fibld = (2.-2./nroom)*fwall + (FLOOR(BldgH/3.1-0.5)-1)*froof
    ENDIF

    IF (fibld==0) fibld=0.00001 
    finternal = froof+fibld+fwall
    IF (finternal==0) finternal=0.00001 
    fair=zref-BldgH*froof
    IF (fair==0) fair=0.00001 
    
    
    
    

    IF ((ivf_ii+ivf_iw+ivf_ir+ivf_if > 1.0001) .OR. &
         (ivf_wi+ivf_ww+ivf_wr+ivf_wf > 1.0001) .OR. &
         (ivf_ri+ivf_rw+ivf_rf > 1.0001) .OR. &
         (ivf_fi+ivf_fw+ivf_fr > 1.0001) .OR. &
         (ivf_ii+ivf_iw+ivf_ir+ivf_if < 0.9999) .OR. &
         (ivf_wi+ivf_ww+ivf_wr+ivf_wf < 0.9999) .OR. &
         (ivf_ri+ivf_rw+ivf_rf < 0.9999) .OR. &
         (ivf_fi+ivf_fw+ivf_fr < 0.9999)) THEN
       PRINT*, "At least one internal view factor <> 1. Check ivf in ESTMinput.nml"
    ENDIF

    
    
    



    
    
    
    
    

    

    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    

    IF(ESTMStart==1) THEN
       DO i=1,4
          Tw_4(:,i) = Twall  
       ENDDO

       
       T0_ground=Tground(1)
       T0_wall=Twall(1)
       T0_roof=Troof(1)
       T0_ibld=Tibld(1)
       TN_roof=Troof(nroof)
       TN_wall=Twall(nwall)

       
       LUP_ground=SBConst*EM_ground*T0_ground**4
       LUP_WALL=SBConst*EM_WALL*T0_WALL**4
       LUP_ROOF=SBConst*EM_ROOF*T0_ROOF**4

       

       

       




    ENDIF

    first=.TRUE.

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


    



    RETURN

    
    

  END SUBROUTINE ESTM_translate

  
  SUBROUTINE ESTM(&
       Gridiv,&
       nsh,tstep,&
       avkdn, avu1, temp_c, zenith_deg, avrh, press_hpa, ldown,&
       bldgh,Ts5mindata_ir,&
       Tair24HR,&
       dataOutLineESTM,QS)
    
    
    
    
    
    
    

    
    
    

    
    
    
    

    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    USE meteo, ONLY: pi, heatcapacity_air
    USE mod_solver
    USE modSolarCalc                                                        
    USE MathConstants                                                       
    USE PhysConstants
    USE heatflux
    USE ESTM_data

    IMPLICIT NONE
    INTEGER, PARAMETER:: ncolsESTMdata=13
    
    INTEGER, PARAMETER:: cTs_Tiair = 5
    INTEGER, PARAMETER:: cTs_Tsurf = 6
    INTEGER, PARAMETER:: cTs_Troof = 7
    INTEGER, PARAMETER:: cTs_Troad = 8
    INTEGER, PARAMETER:: cTs_Twall = 9
    INTEGER, PARAMETER:: cTs_Twall_n = 10
    INTEGER, PARAMETER:: cTs_Twall_e = 11
    INTEGER, PARAMETER:: cTs_Twall_s = 12
    INTEGER, PARAMETER:: cTs_Twall_w = 13
    REAL(KIND(1d0)),PARAMETER::NAN=-999

    INTEGER,INTENT(in)::Gridiv
    INTEGER,INTENT(in)::nsh,tstep
    
    
    
    

    REAL(KIND(1d0)),INTENT(in)::avkdn
    REAL(KIND(1d0)),INTENT(in)::avu1
    REAL(KIND(1d0)),INTENT(in)::temp_c
    REAL(KIND(1d0)),INTENT(in)::zenith_deg
    REAL(KIND(1d0)),INTENT(in)::avrh
    REAL(KIND(1d0)),INTENT(in)::press_hpa
    REAL(KIND(1d0)),INTENT(in)::ldown
    REAL(KIND(1d0)),INTENT(in)::bldgh
    
    REAL(KIND(1d0)),DIMENSION(ncolsESTMdata),INTENT(in)::  Ts5mindata_ir     
    REAL(KIND(1d0)),DIMENSION(24*nsh),INTENT(inout) ::   Tair24HR 

    REAL(KIND(1d0)),DIMENSION(27),INTENT(out):: dataOutLineESTM
    
    REAL(KIND(1d0)),INTENT(out)::QS
    


    
    INTEGER::i, ii
    INTEGER:: Tair2Set=0
    REAL(KIND(1d0))::AIREXHR, AIREXDT
    REAL(KIND(1d0)),DIMENSION(2)::bc
    REAL(KIND(1d0))::chair_ground,chair_wall
    REAL(KIND(1d0))::EM_EQUIV
    REAL(KIND(1d0))::kdz
    REAL(KIND(1d0))::kup_estm,LUP_net,kdn_estm
    REAL(KIND(1d0))::QHestm
    REAL(KIND(1d0))::QFBld 
    REAL(KIND(1d0))::shc_airbld
    REAL(KIND(1d0))::sw_hor,sw_vert
    REAL(KIND(1d0))::T0
    REAL(KIND(1d0))::Tinternal,Tsurf_all,Troof_in,Troad,Twall_all,Tw_n,Tw_e,Tw_s,Tw_w
    REAL(KIND(1d0))::Twallout(5),Troofout(5),Tibldout(5),Tgroundout(5)
    REAL(KIND(1d0))::Tadd,Tveg
    REAL(KIND(1d0))::Tairmix
    REAL(KIND(1d0))::RN
    REAL(KIND(1d0))::Rs_roof,Rl_roof,RN_ROOF
    REAL(KIND(1d0))::Rs_wall,Rl_wall,RN_WALL
    REAL(KIND(1d0))::Rs_ground,Rl_ground,RN_ground
    REAL(KIND(1d0))::Rs_ibld,Rl_ibld
    REAL(KIND(1d0))::Rs_iroof,Rl_iroof
    REAL(KIND(1d0))::Rs_iwall,Rl_iwall
    REAL(KIND(1d0))::zenith_rad
    REAL(KIND(1d0))::dum(50)
    REAL(KIND(1d0))::bldgHX 
    REAL(KIND(1d0)),PARAMETER::WSmin=0.1  
    LOGICAL::radforce, groundradforce

    radforce       = .FALSE.
    groundradforce = .FALSE. 

    bldgHX=MAX(bldgH,0.001) 

    
    
    
    
    
    
    dum=[(-999,i=1,50)]

    
    CHR=0.005
    CHAIR=CHR
    CHAIR_ground=CHAIR
    CHAIR_WALL=CHAIR

    
    kdn_estm=avkdn
    WS=avu1
    IF (WS<WSMin) WS=WSmin
    Tair1=Temp_C+C2K
    
    IF(Gridiv == 1) Tair2Set = Tair2Set+1
    IF(Tair2Set==1) THEN
       Tair2=Temp_C+C2K
    ELSE
       Tair2 = Tair2_grids(Gridiv)
       
       Tievolve = Tievolve_grids(Gridiv)
       lup_ground = lup_ground_grids(Gridiv)
       lup_wall = lup_wall_grids(Gridiv)
       lup_roof = lup_roof_grids(Gridiv)
       T0_ibld = T0_ibld_grids(Gridiv)
       T0_ground = T0_ground_grids(Gridiv)
       T0_wall = T0_wall_grids(Gridiv)
       T0_roof = T0_roof_grids(Gridiv)
       TN_wall = TN_wall_grids(Gridiv)
       TN_roof = TN_roof_grids(Gridiv)
       Tground(:) = Tground_grids(:,Gridiv)
       Twall(:) = Twall_grids(:,Gridiv)
       Troof(:) = Troof_grids(:,Gridiv)
       Tibld(:) = Tibld_grids(:,Gridiv)
       Tw_4 = Tw_4_grids(:,:,Gridiv)

    ENDIF

    
    
    
    
    
    
    
    
    
    
    
    Tinternal  = Ts5mindata_ir(cTs_Tiair)
    Tsurf_all  = Ts5mindata_ir(cTs_Tsurf)
    Troof_in   = Ts5mindata_ir(cTs_Troof)
    Troad      = Ts5mindata_ir(cTs_Troad)
    Twall_all  = Ts5mindata_ir(cTs_Twall)

    Tw_n       = Ts5mindata_ir(cTs_Twall_n)
    Tw_e       = Ts5mindata_ir(cTs_Twall_e)
    Tw_s       = Ts5mindata_ir(cTs_Twall_s)
    Tw_w       = Ts5mindata_ir(cTs_Twall_w)


    

    
    
    
    

    
    
    
    

    
    
    
    
    
    

    
    
    
    
    
    
    
    
    

    
    zenith_rad=zenith_deg/180*PI
    IF (zenith_rad>0.AND.zenith_rad<PI/2.-HW) THEN  
       tanzenith = MIN(TAN(zenith_rad),5.67) 
       tanzenith = tanzenith*kdn_estm/(1370*COS(zenith_rad)) 
    ELSE
       tanzenith = 0.
    ENDIF

    SHC_air=HEATCAPACITY_AIR(Tair1,avrh,Press_hPa)   
    Tair24HR=EOSHIFT(Tair24HR, 1, Tair1, 1) 
    Tairday=SUM(Tair24HR)/(24*nsh)


    
    SELECT CASE(evolvetibld)   
    CASE(0)
       diagnoseTi=.FALSE.
       HVAC=.FALSE. 
    CASE(1)                                                                                   
       diagnoseTi=.TRUE.
       IF (Tievolve>THEAT_OFF) THEN   
          
          HVAC=.FALSE.
       ELSEIF (Tievolve<THEAT_ON) THEN   
          
          HVAC=.TRUE.
       ENDIF
    CASE(2)
       diagnoseTi=.TRUE.                                                                 
    END SELECT

    
    IF (Tairday>20.+C2K.AND.Tievolve>25.+C2K.AND.TAIR1<Tievolve.AND..NOT.HVAC) THEN
       AIREXHR = 2.0  
    ELSEIF (Tairday<17.+C2K.OR.HVAC) THEN
       AIREXHR = 0.5 
    ELSE
       AIREXHR = 1.0
    ENDIF

    AIREXDT=AIREXHR*(Tstep/3600.0)
    shc_airbld=MAX(HEATCAPACITY_AIR(TiEVOLVE,avrh,Press_hPa),0.00001) 
    IF (shc_airbld<minshc_airbld) minshc_airbld=shc_airbld

    
    
    IF (ibldCHmod==1) THEN       
       CH_ibld  = 1.31*(ABS(T0_ibld-Tievolve))**0.25/shc_airbld
       CH_iwall = 1.31*(ABS(TN_wall-Tievolve))**0.25/shc_airbld
       CH_iroof = 1.52*(ABS(TN_roof-Tievolve))**0.25/shc_airbld
       IF (ABS(TN_roof-Tievolve)>0) CH_iroof=CH_iroof*0.39 
    ELSEIF (ibldCHmod==2) THEN   
       CH_ibld  = 1.823*(ABS(T0_ibld-Tievolve))**0.293/shc_airbld
       CH_iwall = 1.823*(ABS(TN_wall-Tievolve))**0.293/shc_airbld
       CH_iroof = 2.175*(ABS(TN_roof-Tievolve))**0.308/shc_airbld
       IF (ABS(TN_roof-Tievolve)>0) CH_iroof=0.704*(ABS(TN_roof-Tievolve))**0.133/shc_airbld 
    ENDIF

    
    
    
    Tairmix =  (Tievolve + TAIR1*AIREXDT)/(1.0+AIREXDT)
    QFBld= froof*(Tievolve-Tairmix)*shc_airbld*bldgHX/Tstep 

    
    Tievolve = Tairmix+Tstep/bldgHX/finternal* &                                                                         
         (CH_ibld*fibld*(T0_ibld-Tievolve)+CH_iroof*froof*(TN_roof-Tievolve)+CH_iwall*fwall*(TN_wall-Tievolve))      

    IF (.NOT.diagnoseTi) Tievolve=Tinternal+C2K
    IF (HVAC) THEN 
       Tadd=(SIGN(-1.0d0,THEAT_fix-Tievolve)+THEAT_fix-Tievolve)*MIN(4.*Tstep/3600.0,0.9) 
       Tievolve=Tievolve+Tadd
    ENDIF


    
    IF (kdn_estm<0) kdn_estm=0. 

    
    
    
    
    
    sw_hor =kdn_estm           
    sw_vert=kdn_estm*tanzenith 

    Rs_roof=svf_roof*(1.0-alb_roof)*sw_hor
    Rl_roof=svf_roof*em_roof*ldown

    Rs_ground=svf_ground*(1.-alb_ground)*sw_hor+&
         zvf_ground*svf_wall*alb_wall*sw_vert*(1-alb_ground)+&
         zvf_ground*svf_ground*alb_ground*sw_hor*xvf_wall*alb_wall

    Rl_ground=svf_ground*ldown*em_ground+zvf_ground*(lup_wall+svf_wall*ldown*(1-em_wall))*em_ground

    Rs_wall=svf_wall*(1.-alb_wall)*sw_vert+&
         zvf_wall*svf_wall*alb_wall*sw_vert*(1.+zvf_wall*alb_wall)+&
         xvf_wall*svf_ground*alb_ground*sw_hor*(1-alb_wall)+&
         zvf_ground*xvf_wall*svf_ground*alb_ground*sw_hor*alb_wall

    
    Rl_wall=svf_wall*ldown*em_wall+zvf_wall*svf_wall*ldown*(1-em_wall)*em_wall+&
         xvf_wall*(lup_ground+svf_ground*ldown*(1-em_ground))*em_wall

    
    kup_estm=kdn_estm-RVF_ROOF*Rs_roof-(RVF_ground+RVF_WALL)*Rs_ground/svf_ground-RVF_VEG*ALB_VEG*kdn_estm
    IF (kdn_estm > 10 .AND. kup_estm > 0) THEN
       alb_avg = kup_estm/kdn_estm
       sumalb  = sumalb+alb_avg
       Nalb    = Nalb+1
    ENDIF


    
    Rs_ibld=0 
    
    
    Rl_ibld=SBConst*(ivf_iw*em_w*TN_wall**4 +&
         ivf_ir*em_r*TN_roof**4 +&
         ivf_if*em_f*Tfloor**4)
    Rs_iwall=0
    Rl_iwall=SBConst*(ivf_wi*em_i*T0_ibld**4 +&
         ivf_wr*em_r*TN_roof**4 +&
         ivf_wf*em_f*Tfloor**4)
    Rs_iroof=0
    Rl_iroof=SBConst*(ivf_ri*em_i*T0_ibld**4 +&
         ivf_rw*em_w*TN_wall**4 +&
         ivf_rf*em_f*Tfloor**4)

    
    bctype=.FALSE.
    kdz=2*kibld(1)/zibld(1)
    Pcoeff=(/em_ibld*SBConst*(1-ivf_ii*em_ibld),0.0d0,0.0d0,kdz+shc_airbld*CH_ibld,&
         -kdz*Tibld(1)-shc_airbld*CH_ibld*Tievolve-Rs_ibld-Rl_ibld/)
    T0_ibld=NewtonPolynomial(T0_ibld,Pcoeff,conv,maxiter)
    bc(1)=T0_ibld                                                       
    bc(2)=bc(1)                                                         
    CALL heatcond1d(Tibld,Qsibld,zibld(1:Nibld),REAL(Tstep,KIND(1d0)),kibld(1:Nibld),ribld(1:Nibld),bc,bctype)

    
    bctype=.FALSE.
    kdz=2*kwall(nwall)/zwall(nwall)
    Pcoeff=(/em_ibld*SBConst*(1-ivf_ww*em_ibld),0.0d0,0.0d0,kdz+shc_airbld*CH_iwall,&
         -kdz*Twall(nwall)-shc_airbld*CH_iwall*Tievolve-Rs_iwall-Rl_iwall/)
    TN_wall=NewtonPolynomial(TN_wall,Pcoeff,conv,maxiter)
    bc(2)=TN_wall                                                       

    IF (TsurfChoice<2 .OR.radforce) THEN
       IF (radforce) THEN                                              
          kdz=2*kwall(1)/zwall(1)
          Pcoeff=(/em_wall*SBConst*(1-zvf_wall*em_wall),0.0d0,0.0d0,kdz+shc_air*chair_wall*WS,&
               -kdz*Twall(1)-shc_air*chair_wall*WS*Tair1-Rs_wall-Rl_wall/)
          T0_wall=NewtonPolynomial(T0_wall,Pcoeff,conv,maxiter)
          bc(1)=T0_wall                                               
       ELSEIF (TsurfChoice==0) THEN
          bc(1)=Tsurf_all+C2K; T0_wall=bc(1)
       ELSEIF (TsurfChoice==1) THEN
          bc(1)=Twall_all+C2K; T0_wall=bc(1)
       ENDIF                                                           

       CALL heatcond1d(Twall,Qswall,zwall(1:nwall),REAL(Tstep,KIND(1d0)),kwall(1:nwall),rwall(1:nwall),bc,bctype)     

    ELSEIF(TsurfChoice==2) THEN
       T0_wall=0.
       DO i=1,4 
          bc(1)=Tw_n+Tw_e+Tw_s+Tw_w+C2K; T0_wall=T0_wall+bc(1)
          CALL heatcond1d(Tw_4(:,i),Qs_4(i),zwall(1:nwall),REAL(Tstep,KIND(1d0)),kwall(1:nwall),rwall(1:nwall),bc,bctype)
       ENDDO
       
       T0_wall=T0_wall/4.
       Qswall = SUM(Qs_4)/4.
       Twall = SUM(Tw_4,2)/4.
    ENDIF

    
    bctype=.FALSE.
    kdz=2*kroof(nroof)/zroof(nroof)
    Pcoeff=(/em_ibld*SBConst,0.0d0,0.0d0,kdz+shc_airbld*CH_iroof,&
         -kdz*Troof(nroof)-shc_airbld*CH_iroof*Tievolve-Rs_iroof-Rl_iroof/)
    TN_roof=NewtonPolynomial(TN_roof,Pcoeff,conv,maxiter)
    bc(2)=TN_roof

    IF (radforce) THEN
       kdz=2*kroof(1)/zroof(1)
       Pcoeff=(/em_roof*SBConst,0.0d0,0.0d0,kdz+shc_air*chair*WS,&
            -kdz*Troof(1)-shc_air*chair*WS*Tair1-Rs_roof-Rl_roof/)
       T0_roof=NewtonPolynomial(T0_roof,Pcoeff,conv,maxiter)
       bc(1)=T0_roof
    ELSEIF (TsurfChoice==0) THEN
       bc(1)=Tsurf_all+C2K; T0_roof=bc(1)
    ELSE
       bc(1)=Troof_in+C2K; T0_roof=bc(1)
    ENDIF

    CALL heatcond1d(Troof,Qsroof,zroof(1:nroof),REAL(Tstep,KIND(1d0)),kroof(1:nroof),rroof(1:nroof),bc,bctype)


    
    bctype=.FALSE.
    kdz=2*kground(1)/zground(1)

    IF (radforce.OR.groundradforce) THEN
       Pcoeff=(/em_ground*SBConst,0.0d0,0.0d0,kdz+shc_air*chair_ground*WS,&
            -kdz*Tground(1)-shc_air*chair_ground*WS*Tair1-Rs_ground-Rl_ground/)
       T0_ground=NewtonPolynomial(T0_ground,Pcoeff,conv,maxiter)
       bc(1)=T0_ground
    ELSEIF (TsurfChoice==0) THEN
       bc(1)=Tsurf_all+C2K; T0_ground=bc(1)
    ELSE
       bc(1)=Troad+C2K; T0_ground=bc(1)
    ENDIF

    bc(2)=LBC_soil+C2K
    

    IF ( fground/=0. )   THEN   
       CALL heatcond1d(Tground,Qsground,zground(1:Nground),REAL(Tstep,KIND(1d0)),kground(1:Nground),rground(1:Nground),bc,bctype)
    ELSE
       Qsground=NAN
    END IF

    Qsair = fair*SHC_air*(Tair1-Tair2)/Tstep
    Qsibld = Qsibld*fibld
    Qswall = Qswall*fwall
    Qsroof = Qsroof*froof
    Qsground = Qsground*fground
    QS = Qsibld + Qswall + Qsroof + Qsground                              


    

    
    
    LUP_ground = SBConst*EM_ground*T0_ground**4
    LUP_WALL   = SBConst*EM_WALL*T0_WALL**4
    LUP_ROOF   = SBConst*EM_ROOF*T0_ROOF**4
    TVEG       = TAIR1
    LUP_VEG    = SBConst*EM_VEG*TVEG**4
    T0         = RVF_ground*T0_ground+RVF_WALL*T0_WALL+RVF_ROOF*T0_ROOF+RVF_VEG*TVEG
    LUP_net    = RVF_ground*LUP_ground+RVF_WALL*LUP_WALL+RVF_ROOF*LUP_ROOF+RVF_VEG*LUP_VEG
    EM_EQUIV   = LUP_net/(SBConst*T0**4) 
    RN_ground  = rs_ground+rl_ground-lup_ground
    RN_ROOF    = rs_roof+rl_roof-lup_roof
    RN_WALL    = rs_wall+rl_wall-lup_wall*(1-zvf_wall*em_wall)
    RN         = kdn_estm-kup_estm+ldown*EM_EQUIV-lup_net 
    QHestm     = (T0-Tair1)*CHair*SHC_air*WS
    sumemis    = sumemis+EM_EQUIV
    nemis      = nemis+1

    

    IF (Nwall<5)THEN
       Twallout  =(/Twall,(dum(ii),  ii=1,(5-Nwall))/)
    ELSE
       Twallout=Twall
    ENDIF

    IF (Nroof<5) THEN
       Troofout  =(/Troof,(dum(ii),  ii=1,(5-Nroof))/);
    ELSE
       Troofout=Troof
    ENDIF

    IF (Nground<5)THEN
       Tgroundout=(/Tground,(dum(ii),ii=1,(5-Nground))/)
    ELSE
       Tgroundout=Tground
    ENDIF

    IF (Nibld<5)THEN
       Tibldout  =(/Tibld,(dum(ii),  ii=1,(5-Nibld))/)
    ELSE
       Tibldout=Tibld
    ENDIF

    
    
    
    
    dataOutLineESTM=[&
         QS,Qsair,Qswall,Qsroof,Qsground,Qsibld,&
         Twallout,Troofout,Tgroundout,Tibldout,Tievolve]
    
    dataOutLineESTM=set_nan(dataOutLineESTM)



    Tair2=Tair1

    
    Tair2_grids(Gridiv)=Tair1
    lup_ground_grids(Gridiv) = lup_ground
    lup_wall_grids(Gridiv) = lup_wall
    lup_roof_grids(Gridiv) = lup_roof
    Tievolve_grids(Gridiv) = Tievolve
    T0_ibld_grids(Gridiv) = T0_ibld
    T0_ground_grids(Gridiv) = T0_ground
    T0_wall_grids(Gridiv) = T0_wall
    T0_roof_grids(Gridiv) = T0_roof
    TN_wall_grids(Gridiv) = TN_wall
    TN_roof_grids(Gridiv) = TN_roof
    Tground_grids(:,Gridiv) = Tground(:)
    Twall_grids(:,Gridiv) = Twall(:)
    Troof_grids(:,Gridiv) = Troof(:)
    Tibld_grids(:,Gridiv) = Tibld(:)
    Tw_4_grids(:,:,Gridiv) = Tw_4(:,:)

  END SUBROUTINE ESTM

  
  ELEMENTAL FUNCTION set_nan(x) RESULT(xx)
    IMPLICIT NONE
    REAL(KIND(1d0)),PARAMETER::pNAN=9999
    REAL(KIND(1d0)),PARAMETER::NAN=-999
    REAL(KIND(1d0)),INTENT(in)::x
    REAL(KIND(1d0))::xx

    IF(ABS(x)>pNAN) THEN
       xx=NAN
    ELSE
       xx=x
    ENDIF

  END FUNCTION set_nan
  

END MODULE ESTM_module
