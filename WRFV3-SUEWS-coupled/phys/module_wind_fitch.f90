

MODULE module_wind_fitch









































  USE module_driver_constants, ONLY : max_domains
  USE module_model_constants, ONLY :  piconst

  USE module_llxy
  USE module_dm, ONLY : wrf_dm_min_real
  USE module_configure, ONLY : grid_config_rec_type

  IMPLICIT NONE

  INTEGER, PARAMETER :: MAXVALS  = 100   
  INTEGER, PARAMETER :: MAXVALS2 = 100     

  INTEGER           :: nt
  INTEGER, DIMENSION(:), ALLOCATABLE :: NKIND
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ival,jval
  REAL, DIMENSION(:), ALLOCATABLE :: hubheight,diameter,stc,stc2,cutin,cutout,npower

  REAL :: turbws(maxvals,maxvals2),turbtc(maxvals,maxvals2),turbpw(maxvals,maxvals2)

CONTAINS

  SUBROUTINE  dragforce(                      &
       & id                                      &
       &,z_at_w,u,v                 &
       &,dx,dz,dt,qke                            &
       &,du,dv                                   &
       &,windfarm_opt,power                      &
       &,ids,ide,jds,jde,kds,kde                 &
       &,ims,ime,jms,jme,kms,kme                 &
       &,its,ite,jts,jte,kts,kte                 &
       &)  



  INTEGER, INTENT(IN) :: id,windfarm_opt 
  INTEGER, INTENT(IN) :: its,ite,jts,jte,kts,kte
  INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme
  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde
  REAL, INTENT(IN) :: dx,dt
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz,u,v,z_at_w
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: du,dv,qke
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: power



  REAL     blade_l_point,blade_u_point,zheightl,zheightu,z1,z2,tarea
  REAL     speed,tkecof,powcof,thrcof,wfdensity
  INTEGER  itf,jtf,ktf
  INTEGER  i,j,k,n
  INTEGER  k_turbine_bot, k_turbine_top

  LOGICAL :: kfound



  REAL :: speedhub,speed1,speed2
  real :: power1,power2,area,ec
  INTEGER :: kbot,ktop,kt

  itf=MIN0(ite,ide-1)
  jtf=MIN0(jte,jde-1)
  ktf=MIN0(kte,kde-1)

    wfdensity = 1.0/(dx*dx)   
    power=0.

    DO kt = 1,nt  
      IF ( windfarm_opt .eq. 1 ) THEN



        k_turbine_bot=0      
        k_turbine_top=-1     
        i = ival(kt,id)
        j = jval(kt,id)

         if (i.ne.-9999.and.j.ne.-9999) then
        IF (( its .LE. i .AND. i .LE. itf ) .AND. &
            ( jts .LE. j .AND. j .LE. jtf )  ) THEN

          blade_l_point=hubheight(kt)-diameter(kt)/2. 
          blade_u_point=hubheight(kt)+diameter(kt)/2. 

          kfound = .false.
          zheightl=0.0
          
          DO k=kts,ktf
            IF(.NOT. kfound) THEN
              zheightu = zheightl + dz(i,k,j) 

              IF(blade_l_point .GE. zheightl .AND. blade_l_point .LE. zheightu) THEN
                k_turbine_bot=k 
              ENDIF

              IF(blade_u_point .GE. zheightl .AND. blade_u_point .LE. zheightu) THEN
                k_turbine_top=k 
                kfound = .TRUE.
              ENDIF

              zheightl = zheightu
            ENDIF
          ENDDO
          IF ( kfound ) THEN




          kfound = .false.
          zheightl=0.
          
          DO k=kts,ktf
            IF(.NOT. kfound) THEN
              z2 = zheightl + 0.5*dz(i,k,j) 

              IF(hubheight(kt) .GE. z2 ) THEN
                kbot=k 
              ELSE
                ktop=k
                kfound = .TRUE.
              ENDIF

              if (.NOT. kfound) z1=z2
              zheightl = z2 + 0.5*dz(i,k,j)
            ENDIF
          ENDDO

          speed1=0.
          speed2=0.
          if (ktop.eq.1) then
           speedhub=sqrt(u(i,1,j)**2.+v(i,1,j)**2.)*hubheight(kt)/z1
          else
           speed1=sqrt(u(i,kbot,j)**2.+v(i,kbot,j)**2.)
           speed2=sqrt(u(i,ktop,j)**2.+v(i,ktop,j)**2.)
           speedhub=speed1+((speed2-speed1)/(z2-z1))*(hubheight(kt)-z1)
          endif



              CALL dragcof(tkecof,powcof,thrcof,               &
                           speedhub,cutin(kt),cutout(kt),   &
                           npower(kt),diameter(kt),stc(kt),stc2(kt),nkind(kt))



          area=piconst/4.*diameter(kt)**2.          
          power1=0.5*1.23*speedhub**3.*area*powcof
          power(i,j)=power1+power(i,j)
          power2=0.

            DO k=k_turbine_bot,k_turbine_top 
              z1=z_at_w(i,k,j)-blade_l_point-z_at_w(i,1,j)  
              z2=z_at_w(i,k+1,j)-blade_l_point-z_at_w(i,1,j) 
              IF(z1 .LT. 0.) z1=0.0 
              IF(z2 .GT. diameter(kt)) z2=diameter(kt) 
              CALL turbine_area(z1,z2,diameter(kt),wfdensity,tarea)

              speed=sqrt(u(i,k,j)**2.+v(i,k,j)**2.)
              power2=power2+0.5*powcof*1.23*(speed**3.)*tarea/wfdensity
            ENDDO



            DO k=k_turbine_bot,k_turbine_top 
              z1=z_at_w(i,k,j)-blade_l_point-z_at_w(i,1,j)  
              z2=z_at_w(i,k+1,j)-blade_l_point-z_at_w(i,1,j) 
              IF(z1 .LT. 0.) z1=0.0 
              IF(z2 .GT. diameter(kt)) z2=diameter(kt) 

              CALL turbine_area(z1,z2,diameter(kt),wfdensity,tarea)

              speed=sqrt(u(i,k,j)**2.+v(i,k,j)**2.)



              if (power1.eq.0.or.power2.eq.0) then
              ec=1.
              else
              ec=power1/power2
              endif

              
              qke(i,k,j) = qke(i,k,j)+speed**3.*tarea*tkecof*dt/dz(i,k,j)*ec
              
              du(i,k,j) = du(i,k,j)-.5*u(i,k,j)*thrcof*speed*tarea/dz(i,k,j)*ec
              
              dv(i,k,j) = dv(i,k,j)-.5*v(i,k,j)*thrcof*speed*tarea/dz(i,k,j)*ec
            ENDDO
          ENDIF
        ENDIF
        endif
      ENDIF
    ENDDO

  END SUBROUTINE dragforce









  SUBROUTINE turbine_area(z1,z2,tdiameter,wfdensity,tarea)

  REAL, INTENT(IN) ::tdiameter,wfdensity
  REAL, INTENT(INOUT) ::z1,z2
  REAL, INTENT(OUT):: tarea
  REAL r,zc1,zc2

  r=tdiameter/2.              
  z1=r-z1                   
  z2=r-z2                   
  zc1=abs(z1)
  zc2=abs(z2)
  
  IF(z1 .GT. 0. .AND. z2 .GT. 0.) THEN
     tarea=zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)- &
     (zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r))
  ELSE IF(z1 .LT. 0. .AND. z2 .LT. 0.) THEN
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)- &
     (zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r))
  ELSE
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)+ &
     zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)
  ENDIF
  tarea=tarea*wfdensity      

  END SUBROUTINE turbine_area


  SUBROUTINE dragcof(tkecof,powcof,thrcof,speed,cispeed,cospeed, &
                     tpower,tdiameter,stdthrcoef,stdthrcoef2,nkind)


  REAL, INTENT(IN):: speed, cispeed, cospeed, tpower,tdiameter,stdthrcoef,stdthrcoef2
  REAL, INTENT(OUT):: tkecof,powcof,thrcof
  REAL :: power,area,mspeed,hspeed



   INTEGER :: nkind,k,nu,nb
   LOGICAL :: vfound
   REAL :: fac1,fac2

  area=piconst/4.*tdiameter**2.          

      vfound=.false.
      DO k=1,maxvals2
            IF(.NOT. vfound) THEN
              IF(turbws(nkind,k).GT.speed) THEN
                nu=k 
                nb=k-1
                vfound=.true.
              ENDIF
            ENDIF
      ENDDO

  IF (speed .LE. cispeed) THEN
     thrcof = stdthrcoef
  ELSE
    IF (speed .GE. cospeed) THEN
     thrcof = stdthrcoef2
     ELSE
     thrcof = turbtc(nkind,nb)+(turbtc(nkind,nu)-turbtc(nkind,nb))/(turbws(nkind,nu)-turbws(nkind,nb))*(speed-turbws(nkind,nb))
    ENDIF
  ENDIF



  IF(speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     power=0.
     powcof=0.
  ELSE
      fac1=1000./(0.5*1.23*turbws(nkind,nb)**3.*area)
      fac2=1000./(0.5*1.23*turbws(nkind,nu)**3.*area)
      power = turbpw(nkind,nb)+(turbpw(nkind,nu)-turbpw(nkind,nb))/(turbws(nkind,nu)-turbws(nkind,nb)) &
                               *(speed-turbws(nkind,nb))
      powcof = turbpw(nkind,nb)*fac1+(turbpw(nkind,nu)*fac2-turbpw(nkind,nb)*fac1)/(turbws(nkind,nu)-turbws(nkind,nb)) &
                                     *(speed-turbws(nkind,nb))
  ENDIF

  

  tkecof=thrcof-powcof
  IF(tkecof .LT. 0.) tkecof=0.

  END SUBROUTINE dragcof

  SUBROUTINE init_module_wind_fitch(id,config_flags,xlong,xlat,windfarm_initialized,&
                                            ims,ime,jms,jme,its,ite,jts,jte,ids,ide,jds,jde)

  IMPLICIT NONE

   integer ims,ime,jms,jme,ids,ide,jds,jde
   integer its,ite,jts,jte
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: xlong,xlat
   TYPE (grid_config_rec_type) :: config_flags
   TYPE (PROJ_INFO) :: ts_proj
   logical :: windfarm_initialized

   CHARACTER*256 num,input,message_wind
   real lat,lon,ts_rx,ts_ry
   REAL :: known_lat, known_lon
   INTEGER i,j,nval,k,id

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

      IF ( wrf_dm_on_monitor() ) THEN



        if ( config_flags%windfarm_ij .eq. 1 ) then
          open(70,file='windturbines-ij.txt',form='formatted',status='old')
        else
          open(70,file='windturbines.txt',form='formatted',status='old')
        end if



       nt=0
 10    read(70,*,end=100) 
       nt=nt+1
       goto 10

 100   continue
       rewind (70)
     END IF

     CALL wrf_dm_bcast_integer(nt,1)



     if (.not. windfarm_initialized) then
       allocate (nkind(nt),ival(nt,max_domains),jval(nt,max_domains))
       allocate (hubheight(nt),stc(nt),stc2(nt),cutin(nt),cutout(nt),diameter(nt),npower(nt))
       ival=-9999
       jval=-9999
       windfarm_initialized=.true.
     endif

     IF ( wrf_dm_on_monitor() ) THEN
     do k=1,nt
       if ( config_flags%windfarm_ij .eq. 1 ) then
         read(70,*) ival(k,id), jval(k,id), nkind(k)
         write(message_wind,*)'WINDFARM Turbine #',k,': I, J = ',ival(k,id), jval(k,id),'; Type = ',nkind(k)
         CALL wrf_message(message_wind)

       else

         read(70,*)lat,lon,nkind(k)
         write(message_wind,*)'WINDFARM Turbine #',k,': Lat, lon = ',lat,lon,'; Type = ',nkind(k)
         CALL wrf_message(message_wind)

         CALL map_init(ts_proj)

         known_lat = xlat(its,jts)
         known_lon = xlong(its,jts)

      
      IF (config_flags%map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, ts_proj,               &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = REAL(its),             &
                      knownj   = REAL(jts),             &
                      dx       = config_flags%dx)

      
      ELSE IF (config_flags%map_proj == PROJ_LC) THEN
         CALL map_set(PROJ_LC, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = REAL(its),              &
                      knownj   = REAL(jts),              &
                      dx       = config_flags%dx)

      ELSE IF (config_flags%map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = REAL(its),              &
                      knownj   = REAL(jts),              &
                      dx       = config_flags%dx)





























      END IF

         CALL latlon_to_ij(ts_proj, lat, lon, ts_rx, ts_ry)

          ival(k,id)=nint(ts_rx)
          jval(k,id)=nint(ts_ry)
          if (ival(k,id).lt.ids.and.ival(k,id).gt.ide) then
            ival(k,id)=-9999
            jval(k,id)=-9999
          endif


          write(message_wind,*)'WINDFARM Turbine #',k,': Lat, lon = ',lat,lon, &
                               ', (i,j) = (',ival(k,id),',',jval(k,id),'); Type = ',nkind(k)
          CALL wrf_debug(0,message_wind)

     end if

     enddo
      close(70)



         turbws=0.
         turbtc=0.
         turbpw=0.
         DO i=1,nt
          write(num,*) nkind(i)
          num=adjustl(num)
          input="wind-turbine-"//trim(num)//".tbl"
          OPEN(file=TRIM(input),unit=19,FORM='FORMATTED',STATUS='OLD')
          READ (19,*,ERR=132)nval
          READ(19,*,ERR=132)hubheight(i),diameter(i),stc(i),npower(i)
            DO k=1,nval
              READ(19,*,ERR=132)turbws(nkind(i),k),turbtc(nkind(i),k),turbpw(nkind(i),k)
            ENDDO
          cutin(i)  = turbws(nkind(i),1)
          cutout(i) = turbws(nkind(i),nval)
          stc2(i) = turbtc(nkind(i),nval)
          close (19)
         ENDDO

 132   continue



      endif

        CALL wrf_dm_bcast_integer(ival,nt*max_domains)
        CALL wrf_dm_bcast_integer(jval,nt*max_domains)
        CALL wrf_dm_bcast_real(hubheight,nt)
        CALL wrf_dm_bcast_real(diameter,nt)
        CALL wrf_dm_bcast_real(stc,nt)
        CALL wrf_dm_bcast_real(npower,nt)
        CALL wrf_dm_bcast_real(cutin,nt)
        CALL wrf_dm_bcast_real(cutout,nt)
        CALL wrf_dm_bcast_integer(nkind,nt) 
        CALL wrf_dm_bcast_real(turbws,maxvals*maxvals2) 
        CALL wrf_dm_bcast_real(turbtc,maxvals*maxvals2) 
        CALL wrf_dm_bcast_real(turbpw,maxvals*maxvals2) 

  END SUBROUTINE init_module_wind_fitch
  
END MODULE module_wind_fitch
