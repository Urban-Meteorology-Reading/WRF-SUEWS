! HCW - No longer needed as error handling done by SUEWS_error.f95 instead
!module mod_error
!implicit none
!
!contains
!
! subroutine error(fname,ios,nostop)
! integer ::ios
! character (len=*)::fname
! integer,optional::nostop
!
! print*,"file error: iostat=",ios, trim(fname)
! if (.not.present(nostop)) stop
! return
! end subroutine error
!
!end module

!==============================================================================
MODULE mod_interp
  !     Created on Thu Jan 22 00:06:32 2004
  IMPLICIT NONE

CONTAINS
  ELEMENTAL FUNCTION interp1d(x1,x2,y1,y2,xi) RESULT(yi)
    REAL(8),INTENT(in) ::x1,x2,xi
    REAL(8),INTENT(in) ::y1,y2
    REAL (KIND(1D0))::b0,b1
    REAL(8)         ::yi
    !integer         ::ny                 !!!!!FO!!!!!
    b1=(y2-y1)/(x2-x1)
    b0=y1-b1*x1
    yi=b0+b1*xi
  END FUNCTION interp1d
END MODULE mod_interp

!=============================================================================
MODULE mod_solver
  !     Created on Thu Jan 22 07:50:01 2004
  !     Copyright (c) 2001 MyCompany. All rights reserved.

  IMPLICIT NONE

CONTAINS

  FUNCTION NewtonPolynomial(x0,Pcoeff,conv,maxiter) RESULT(x)
    !Solves Newton's Method for a polynomial of the form
    !f(x)=Pcoeff(1)*x^n+Pcoeff(2)*x^n-1+...+Pcoeff(n+1)
    !                f(x(i))
    ! x(i+1) = x(i)- -------
    !                f'(x(i))
    !
    !conv is the level required for convergence
    !maxiter is the maximum allowed iterations
    !----------------------------------------------------
    REAL(8) ::x0,x,conv
    REAL(8) ::Pcoeff(:)
    REAL(8) ::e, xprev
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
          fp = fp + Pcoeff(j)*(n-j)*x**(n-j-1)                              !!FO!! derivative
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
!==============================================================================

!==============================================================================
MODULE modSolarCalc
  USE MathConstants
  IMPLICIT NONE

CONTAINS
  !=======================================================
  FUNCTION min_zenith(lat,doy) RESULT(zmin)
    !returns max zenith
    !returns zenith in radians for lat, lng in degrees
    REAL(8) ::lat,dectime,zmin
    REAL(8) ::latr,decl
    INTEGER :: doy
    dectime=float(doy)
    latr=lat*dtr
    decl=0.409*COS(2*pi*(dectime-173)/365.25)
    zmin=pi/2.-ASIN(SIN(latr)*SIN(decl)-COS(latr)*COS(decl)*(-1))
  END FUNCTION min_zenith
  !=======================================================

  !=======================================================
  FUNCTION Local_apparent_time(lng,dectime) RESULT(la_time)
    !Oke, 1989, equation of time elsewhere
    REAL(8) ::lng,dectime,la_time
    REAL(8) ::gamma,eqtime,lmst

    lmst=dectime-4.*lng/60./1440.
    gamma=2.*pi/365.*(lmst-1.)
    eqtime=229.18*(7.5e-5+1.868e-3*COS(gamma)-0.032077*SIN(gamma)&
         &    -0.014615*COS(2.*gamma)-0.040849*SIN(2.*gamma))
    la_time=lmst+eqtime/1440.
  END FUNCTION Local_apparent_time
  !=======================================================

  SUBROUTINE Solar_angles(lat,lng,timezone,dectime,decl,zenith,azimuth)

    REAL, INTENT(in)  ::lat,lng,timezone,dectime
    INTEGER                 ::doy,hour,mn
    REAL(8), INTENT(out)  ::decl,zenith,azimuth
    REAL (KIND(1d0))  ::ha,latr,eqtime,tst,&
         time_offset,gamma           !!!!!FO!!!!! lngr, phi, theta

    latr=lat*pi/180.
    doy=FLOOR(dectime)
    hour=FLOOR((dectime-doy)*24.)
    mn=FLOOR((dectime-doy-hour/24.)*60.)   !!!Check this

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

  !=========================== SolarTimes ==================================
  SUBROUTINE Solar_Times(lat,lng,timezone,dectime,sunrise,sunset,snoon)
    !  for sunrise and sunset ha = ha(zenith=90)
    !  timezone is offset to GMT e.g. -5 for EST

    REAL(8), INTENT(in)  ::lat,lng,timezone,dectime
    INTEGER                 ::doy
    REAL(8), INTENT(out)  ::sunrise, sunset, snoon
    REAL(8)  :: ha, latr, eqtime, gamma, zenith, decl
    latr = lat*dtr
    zenith=90.833*dtr
    doy=FLOOR(dectime)
    gamma=2.*pi/365.*(float(doy)-0.5) !fractional year
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
  !=======================================================
  FUNCTION kdown_surface(doy,zenith) RESULT(Isurf)
    ! Calculates ground level solar irradiance clear sky
    ! assuming transmissivity = 1
    ! let it report zero if zenith >= 90
    REAL(8)    ::zenith,Isurf
    INTEGER    ::doy
    REAL (KIND(1d0))::Rmean, Rse, cosZ,Itoa

    Rmean = 149.6   !Stull 1998
    Rse=solar_ESdist(doy)
    IF(zenith<pi/2.) THEN
       cosZ = COS(zenith)
       Itoa = 1370*(Rmean/Rse)**2    !top of the atmosphere
       Isurf = Itoa*cosZ      !ground level solar irradiance in W/m2
    ELSE
       Isurf = 0.
    ENDIF

  END FUNCTION kdown_surface

  !=======================================================
  FUNCTION SmithLambda(lat) RESULT(G)
    !read kriged data based on Smith 1966 (JAM)
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
  !=======================================================
  FUNCTION transmissivity_CD(P,Td,G,zenith) RESULT(trans)           !!!!!FO!!!!! ,doy
    ! bulk atmospheric transmissivity (Crawford and Duchon, 1999)
    ! P = pressure (hPa)
    ! Td = dewpoint (C)
    ! G parameter is empirical value from Smith 1966 (JAM)
    ! zenith in radians
    ! if zenith > 80 use the value for 80?

    !        integer         ::doy           !!!!!FO!!!!!
    REAL(8)    ::P,Td,zenith,G,trans
    REAL (KIND(1d0))::m,TrTpg,u,Tw,Ta,cosZ
    REAL (KIND(1d0))::Tdf

    IF (zenith>80.*dtr) THEN
       cosZ=COS(80.*dtr)
    ELSE
       cosZ=COS(zenith)
    ENDIF
    Tdf = Td*1.8+32. !celsius to fahrenheit
    !  Transmission coefficients
    m = 35*cosZ/SQRT(1224.*cosZ*cosZ+1) !optical air mass at p=1013 mb
    TrTpg = 1.021-0.084*SQRT(m*(0.000949*P+0.051)) !first two trans coeff
    u = EXP(0.113-LOG(G+1)+0.0393*Tdf) !precipitable water
    Tw = 1-0.077*(u*m)**0.3    !vapor transmission coe3ff.
    Ta = 0.935**m        !4th trans coeff
    trans = TrTpg*Tw*Ta              !bulk atmospherics transmissivity
  END FUNCTION transmissivity_CD

  !=======================================================
  FUNCTION kdown_niemala(S0,vap_press,Tk) RESULT(kdown)           !!!!!FO!!!!! ,albedo
    !from Niemala et al. 2001. Atmospheric Research 58:141-154
    ! using generalized formula only, no empirical data from site
    !11 October 2001
    ! S0=base solar insolation at the surface
    ! albedo = surface albedo (required for multireflection diffuse)
    ! vap_press=screen level vapor pressure (Pa)
    ! Tk=screen level air temperature (K)
    ! ta=aerosol transmissivity
    ! tR=Rayleigh scattering transmissivity
    ! tg=uniformly mixed gas transmissivity
    ! tw=water vapor transmissivity
    ! toz=ozone transmissivity
!!!!!!!INCOMPLETE
    REAL  ::S0,vap_press,Tk,kdown           !!!!!FO!!!!! ,albedo
    REAL  ::Sdir,Diffuse,Diffuse_R,Diffuse_a,Diffuse_m
    REAL  ::tr,tg,tw,ta,toz, theta
    CALL transmissivity(vap_press,Tk,theta,tr,tg,tw,ta,toz)
    Sdir=0.9751*S0*tr*tg*tw*ta*toz
    Diffuse_R=0.
    Diffuse_a=0.
    Diffuse_m=0.
    Diffuse=Diffuse_R+Diffuse_a+Diffuse_m
    kdown=Sdir+Diffuse
  END FUNCTION kdown_niemala
  !=======================================================
  SUBROUTINE transmissivity(vap_press,Tk,theta,tr,tg,tw,ta,toz)
    !calculates atmospheric transmissivities for
    ! ta=aerosol transmissivity
    ! tR=Rayleigh scattering transmissivity
    ! tg=uniformly mixed gas transmissivity
    ! tw=water vapor transmissivity
    ! toz=ozone transmissivity
    ! from Iqbal (1983) and Niemala(2001)
    ! vap_press (Pa)
    ! Tk air temp (K)
    ! w=precipitable water content (cm)
!!!!!!!INCOMPLETE
    REAL  ::Tk, vap_press,theta
    REAL  ::tr,tg,tw,ta,toz,w
    w=0.493*vap_press/Tk
    ta=0.59+0.012*theta-1.336e-4*theta*theta

  END SUBROUTINE transmissivity
  !=======================================================
  FUNCTION solar_ESdist(doy) RESULT(Rse)
    !from Stull, 1998
    INTEGER  ::doy
    REAL(8)  ::Rse
    REAL (KIND(1d0)) ::MA,nu,e,a

    e = 0.0167
    a = 146.457

    MA = 2.*pi*(doy-3)/365.25463 !Mean anomaly
    nu=MA+0.0333988*SIN(MA)+.0003486*SIN(2.*MA)+5e-6*SIN(3.*MA) !true anomaly
    Rse = a*(1-e*e)/(1+e*COS(nu))

  END FUNCTION solar_ESdist

END MODULE modSolarCalc

!=====================================================================================
!=====================================================================================
MODULE heatflux
  IMPLICIT NONE
CONTAINS

  SUBROUTINE heatcond1d(T,Qs,dx,dt,k,rhocp,bc,bctype)
    REAL(8),INTENT(inout)::T(:)
    REAL(8),INTENT(in)::dx(:),dt,k(:),rhocp(:),bc(2)
    REAL(8),INTENT(out)::Qs
    LOGICAL,INTENT(in)::bctype(2)
    INTEGER         ::i,n!,j       !!!!!FO!!!!!
    REAL(8),ALLOCATABLE::w(:),a(:),T1(:)
    n=SIZE(T)
    ALLOCATE(w(0:n),a(n),T1(n))
    !w = interface tempea
    w(1:n)=T
    w(0)=bc(1); w(n)=bc(2)
    !convert from flux to equivalent temperature, not exact
    ! F = k dT/dX => dx*F/k + Ti = Ti
    IF (bctype(1)) w(0)=bc(1)*0.5*dx(1)/k(1)+w(1)
    IF (bctype(2)) w(n)=bc(2)*0.5*dx(n)/k(n)+w(n)

    a=k/dx
    DO i=1,n-1
       w(i)=(T(i+1)*a(i+1)+T(i)*a(i))/(a(i)+a(i+1))
    ENDDO
    !!FO!! print*, 'w: ', w
    DO i=1,n
       T1(i) = (dt/rhocp(i))*(w(i-1)-2*T(i) + w(i))*2*a(i)/dx(i) + T(i)
    ENDDO
    !!FO!! print*, 'T1: ', T1
    !for storage the internal distribution of heat should not be important
    Qs = (w(0)-T(1))*2*a(1) + (w(n)-T(n))*2*a(n)                           !!FO!! k*d(dT/dx)/dx = rhoCp*(dT/dt) -- rhoCp*(dT/dt)*dx = dQs -- dQs = k*d(dT/dx)
    ! Qs=sum((T1-T)*rhocp*dx)/dt!
    T=T1
  END SUBROUTINE heatcond1d
END MODULE heatflux

!==================================================================================
!===================================================================================
MODULE METEO

  USE MathConstants
  IMPLICIT NONE

  ! REAL (KIND(1d0)),PARAMETER ::  PI=3.141592654
  REAL (KIND(1d0)),PARAMETER ::  RAD2DEG=57.29577951
  REAL (KIND(1d0)),PARAMETER ::  DEG2RAD=0.017453292

  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_AIR=0.028965             ! kg for 1 mol dry air
  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_CO2=0.04401              ! kg for 1 mol CO2
  REAL (KIND(1d0)),PARAMETER ::  MOLMASS_H2O=0.0180153            ! kg for 1 mol water vapor
  REAL (KIND(1d0)),PARAMETER ::  MU_H2O=MOLMASS_AIR/MOLMASS_H2O   ! mol air/mol H2O
  REAL (KIND(1d0)),PARAMETER ::  MU_CO2=MOLMASS_AIR/MOLMASS_CO2   ! mol air/mol CO2
  REAL (KIND(1d0)),PARAMETER ::  R_DRY_MOL=8.31451                ! J/K/mol gas constant
  REAL (KIND(1D0)),PARAMETER ::  R_DRY_MASS=R_DRY_MOL/MOLMASS_AIR ! J/K/kg GAS CONSTANT
  !REAL (KIND(1d0)),PARAMETER ::  SIGMA_SB=5.67051e-8              ! Stefan-Boltzmann constant
  REAL (KIND(1d0)),PARAMETER ::  EPSIL=0.62197
  REAL (KIND(1d0)),PARAMETER ::  KB=1.3807E-25                    ! BOLTZMANN'S CONSTANT (m^3 MB K^-1)=R/A
  REAL (KIND(1d0)),PARAMETER ::  AVOGADRO=6.02252E23              ! AVOGADRO'S NUMBER (molecules/mol)

CONTAINS

  !============================================================================
  FUNCTION sat_vap_press(TK,P) RESULT(es)
    !c sg sept 99 f90
    !c     This uses eqns from Buck (1981) JAM 20, 1527-1532
    !c     units T (K) e (mb) P (mb)
    !c     f corrects for the fact that we are not dealing with pure water
    REAL(8)    :: TK,P,TC,es,e,f
    TC=TK-273.15
    IF(TC.EQ.0)THEN
       TC=0.001
    ENDIF
    !Valid for 50>T>-40
    e=6.1121*EXP(((18.729-TC/227.3)*TC)/(TC+257.87))
    f=1.00072+P*(3.2E-6+5.9E-10*TC**2)
    es=e*f
  END FUNCTION sat_vap_press

  REAL(8) FUNCTION SOS_DRYAIR(TK)
    !SPEED OF SOUND IN DRY AIR, BEER (1991)
    REAL(8) ::TK
    SOS_DRYAIR=SQRT(1.4*R_DRY_MOL*TK/(MOLMASS_AIR*1000.))
  END FUNCTION SOS_DRYAIR
  !============================================================================
  REAL(8) FUNCTION POTENTIAL_TEMP(TK,P)
    !TK = ABSOLUTE TEMPERATURE
    !P  = PRESS (hPa)
    REAL(8)    ::TK,P
    POTENTIAL_TEMP=TK*(1000./P)**0.286
  END FUNCTION POTENTIAL_TEMP

  REAL(8) FUNCTION LATENTHEAT_V(TK)
    !LATENT HEAT OF VAPORIZATION (J/kg) BOLTON(1980)
    !TK = ABSOLUTE TEMPERATURE
    REAL(8) ::TK
    LATENTHEAT_V=2.501E6-2370.*(TK-273.15)
  END FUNCTION LATENTHEAT_V

  REAL(8) FUNCTION LATENTHEAT_M(TK)
    !LATENT HEAT OF MELTING (J/kg) VALID BELOW 0C BOLTON(1980)
    !TK = ABSOLUTE TEMPERATURE
    REAL(8) ::TK,TC
    TC=TK-273.15
    LATENTHEAT_M=3.3358E5+TC*(2030.-10.46*TC)
  END FUNCTION LATENTHEAT_M

  REAL(8) FUNCTION SPEC_HEAT_DRYAIR(TK)
    ! BEER (1991) APPLIED ENVIRONMETRICS METEOROLOGICAL TABLES
    REAL(8) ::TK,TC
    TC=TK-273.15
    SPEC_HEAT_DRYAIR=1005.+((TC+23.15)**2)/3364.
  END FUNCTION SPEC_HEAT_DRYAIR

  REAL(8) FUNCTION SPEC_HEAT_VAPOR(TK,RH)
    ! BEER (1991) APPLIED ENVIRONMETRICS METEOROLOGICAL TABLES
    REAL(8) ::TK,TC_100,RH
    TC_100=(TK-273.15)/100.
    SPEC_HEAT_VAPOR=1859.+0.13*RH+(19.3+0.569*RH)*TC_100+(10.+0.5*RH)*TC_100**2
  END FUNCTION SPEC_HEAT_VAPOR

  REAL(8) FUNCTION HEATCAPACITY_AIR(TK,RH,P)
    REAL(8) ::TK,RH,P
    REAL(8) ::RHO_D,RHO_V
    REAL(8) ::CPD,CPV
    RHO_D=DENSITY_DRYAIR(TK,P)
    RHO_V=DENSITY_VAPOR(TK,RH,P)
    CPD=SPEC_HEAT_DRYAIR(TK)
    CPV=SPEC_HEAT_VAPOR(TK,RH)
    HEATCAPACITY_AIR=RHO_D*CPD+RHO_V*CPV
  END FUNCTION HEATCAPACITY_AIR

  REAL(8) FUNCTION DENSITY_MOIST(TVK,P)
    ! density of moist air FROM VIRTUAL TEMPERATURE
    !TVK = VIRTUAL TEMPERATURE (K)
    != = PRESSURE (hPa)
    REAL(8) ::TVK,P
    DENSITY_MOIST=P*100./(R_DRY_MASS*TVK)
  END FUNCTION DENSITY_MOIST

  REAL(8) FUNCTION DENSITY_VAPOR(TK,RH,P)
    !WATER VAPOR DENSITY
    REAL(8)    ::TK,P,RH,EA
    EA=SAT_VAP_PRESS(TK,P)*RH/100.
    DENSITY_VAPOR=(EA*100.*EPSIL)/(R_DRY_MASS*TK)
  END FUNCTION DENSITY_VAPOR

  REAL(8) FUNCTION DENSITY_DRYAIR(TK,P)
    REAL(8) ::TK,P
    DENSITY_DRYAIR=P*100./(R_DRY_MASS*TK)
  END FUNCTION DENSITY_DRYAIR

  REAL(8) FUNCTION DENSITY_GAS(TK,PP,MOLMASS)
    !DENSITY FOR IDEAL GAS SPECIES GIVEN ITS PARTIAL PRESSURE (hPa) AND MOLAR MASS (kg)
    REAL(8) ::TK,PP,MOLMASS
    DENSITY_GAS=PP*MOLMASS/(R_DRY_MOL*TK)
  END FUNCTION DENSITY_GAS

  REAL(8) FUNCTION PARTIAL_PRESSURE(TK,N)
    !PARTIAL PRESSURE OF IDEAL GAS (hPa)
    REAL(8) ::TK,N !N IS THE NUMBER DENSITY IN mol/m3
    PARTIAL_PRESSURE=N*KB*TK
  END FUNCTION PARTIAL_PRESSURE

  REAL(8) FUNCTION SCALE_HEIGHT(TK)
    REAL(8) ::TK
    !SCALE HEIGHT FOR DRY ATMOSPHERE IN km BEER (1991)
    SCALE_HEIGHT=R_DRY_MOL*TK/(MOLMASS_AIR*9.81)
  END FUNCTION SCALE_HEIGHT

  REAL(8) FUNCTION VAISALA_BRUNT_F(TK)
    !BEER (1991)
    REAL(8) ::TK
    VAISALA_BRUNT_F=SQRT(0.4/1.4*9.81/SCALE_HEIGHT(TK))
  END FUNCTION VAISALA_BRUNT_F

END MODULE METEO
