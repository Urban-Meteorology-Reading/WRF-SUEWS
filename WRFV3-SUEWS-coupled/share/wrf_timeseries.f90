






SUBROUTINE calc_ts_locations( grid )

   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure, ONLY : model_config_rec, grid_config_rec_type, model_to_grid_config_rec
   USE module_dm, ONLY : wrf_dm_min_real
   USE module_llxy
   USE module_state_description

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: ntsloc_temp
   INTEGER :: i, j, k, iunit
   REAL :: ts_rx, ts_ry, ts_xlat, ts_xlong, ts_hgt
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   CHARACTER (LEN=24) :: ts_profile_filename
   CHARACTER (LEN=2), DIMENSION(5) :: ts_file_endings = (/ 'UU', 'VV', 'PH', 'TH', 'QV' /)
   TYPE (PROJ_INFO) :: ts_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey


   IF ( grid%ntsloc .LE. 0 ) RETURN

   IF ( grid%dfi_stage == DFI_FST ) THEN
      CALL get_ijk_from_grid ( grid ,                               &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               ips, ipe, jps, jpe, kps, kpe,        &
                               imsx, imex, jmsx, jmex, kmsx, kmex,  &
                               ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                               imsy, imey, jmsy, jmey, kmsy, kmey,  &
                               ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
      
      CALL map_init(ts_proj)
   
      IF (ips <= 1 .AND. 1 <= ipe .AND. &
          jps <= 1 .AND. 1 <= jpe) THEN
         known_lat = grid%xlat(1,1)
         known_lon = grid%xlong(1,1)
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      known_lat = wrf_dm_min_real(known_lat)
      known_lon = wrf_dm_min_real(known_lon)
   
      
      IF (config_flags%map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, ts_proj,               &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_LC) THEN
         CALL map_set(PROJ_LC, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, ts_proj,                            &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &
                      lat0     = config_flags%pole_lat,                 &
                      lon0     = config_flags%pole_lon,                 &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)

      
      ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, ts_proj,                      &

                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
      END IF
   
      
      IF (.NOT. grid%have_calculated_tslocs) THEN
         grid%have_calculated_tslocs = .TRUE.
         WRITE(message, '(A43,I3)') 'Computing time series locations for domain ', grid%id
         CALL wrf_message(message)
   
         ntsloc_temp = 0
         DO k=1,grid%ntsloc
         
            IF (config_flags%map_proj == 0) THEN  
               ts_rx = grid%lattsloc(k)           
               ts_ry = grid%lontsloc(k)
            ELSE
               CALL latlon_to_ij(ts_proj, grid%lattsloc(k), grid%lontsloc(k), ts_rx, ts_ry)
            END IF
            

            ntsloc_temp = ntsloc_temp + 1
            grid%itsloc(ntsloc_temp) = NINT(ts_rx)
            grid%jtsloc(ntsloc_temp) = NINT(ts_ry)
            grid%id_tsloc(ntsloc_temp) = k
   
            
            IF (grid%itsloc(ntsloc_temp) < ids .OR. grid%itsloc(ntsloc_temp) > ide .OR. &
                grid%jtsloc(ntsloc_temp) < jds .OR. grid%jtsloc(ntsloc_temp) > jde) THEN
               ntsloc_temp = ntsloc_temp - 1
   
            END IF
   
         END DO
   
         grid%next_ts_time = 1
   
         grid%ntsloc_domain = ntsloc_temp
   
         DO k=1,grid%ntsloc_domain
   
            
            IF (grid%itsloc(k) < ips .OR. grid%itsloc(k) > ipe .OR. &
                grid%jtsloc(k) < jps .OR. grid%jtsloc(k) > jpe) THEN
               ts_xlat  = 1.E30
               ts_xlong = 1.E30
               ts_hgt   = 1.E30
            ELSE
               ts_xlat  = grid%xlat(grid%itsloc(k),grid%jtsloc(k))
               ts_xlong = grid%xlong(grid%itsloc(k),grid%jtsloc(k))
               ts_hgt   = grid%ht(grid%itsloc(k),grid%jtsloc(k))
            END IF
   
            IF ( wrf_dm_on_monitor() ) THEN
               iunit = get_unused_unit()
               IF ( iunit <= 0 ) THEN
                  CALL wrf_error_fatal3("<stdin>",187,&
'Error in calc_ts_locations: could not find a free Fortran unit.')
               END IF
               WRITE(grid%ts_filename(k),'(A)') TRIM(grid%nametsloc(grid%id_tsloc(k)))//'.d00.TS'
               i = LEN_TRIM(grid%ts_filename(k))
               WRITE(grid%ts_filename(k)(i-4:i-3),'(I2.2)') grid%id
               OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(k)), FORM='FORMATTED', STATUS='REPLACE')
               WRITE(UNIT=iunit, &
                     FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7)') &
                     grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                     ' '//grid%nametsloc(grid%id_tsloc(k)), &
                     ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
                     grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                     ts_xlat, ',', ts_xlong, ') ', &
                     ts_hgt,' meters'
               CLOSE(UNIT=iunit)

               ts_profile_filename = grid%ts_filename(k)
               DO j=1,5
                  
                  iunit = get_unused_unit()
                  IF ( iunit <= 0 ) THEN
                     CALL wrf_error_fatal3("<stdin>",209,&
'Error in calc_ts_locations: could not find a free Fortran unit.')
                  END IF
                  i = LEN_TRIM(ts_profile_filename)
                  WRITE(ts_profile_filename(i-1:i),'(A2)') ts_file_endings(j)
                  OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), FORM='FORMATTED', STATUS='REPLACE')
                  WRITE(UNIT=iunit, &
                        FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7)') &
                        grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                        ' '//grid%nametsloc(grid%id_tsloc(k)), &
                        ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
                        grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                        ts_xlat, ',', ts_xlong, ') ', &
                        ts_hgt,' meters'
                  CLOSE(UNIT=iunit)
               END DO
            END IF
         END DO
   
      END IF
   END IF

END SUBROUTINE calc_ts_locations


SUBROUTINE calc_ts( grid )

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   
   INTEGER :: i, k, mm, n, ix, iy, rc
   REAL :: earth_u, earth_v,                       &
           output_t, output_q, clw, xtime_minutes
   REAL, ALLOCATABLE, DIMENSION(:) :: p8w
   REAL, ALLOCATABLE, DIMENSION(:) :: earth_u_profile, earth_v_profile

   
       
       
   LOGICAL, PARAMETER :: ts_model_level = .FALSE. 

   
   ALLOCATE ( earth_u_profile(grid%max_ts_level), earth_v_profile(grid%max_ts_level) )

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN

   n = grid%next_ts_time

   ALLOCATE(p8w(grid%sm32:grid%em32))

   DO i=1,grid%ntsloc_domain

      ix = grid%itsloc(i)
      iy = grid%jtsloc(i)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN
       
         IF (ts_model_level) THEN
   
            
            
            
            earth_u = grid%u_2(ix,1,iy)*grid%cosa(ix,iy)-grid%v_2(ix,1,iy)*grid%sina(ix,iy)
            earth_v = grid%v_2(ix,1,iy)*grid%cosa(ix,iy)+grid%u_2(ix,1,iy)*grid%sina(ix,iy)
            output_t = grid%t_2(ix,1,iy)
            output_q = grid%moist(ix,1,iy,P_QV)
   
         ELSE
   
            
            
            
            DO k=1,grid%max_ts_level
            earth_u_profile(k) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)-grid%v_2(ix,k,iy)*grid%sina(ix,iy)
            earth_v_profile(k) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)+grid%u_2(ix,k,iy)*grid%sina(ix,iy)
            END DO
            earth_u = grid%u10(ix,iy)*grid%cosa(ix,iy)-grid%v10(ix,iy)*grid%sina(ix,iy)
            earth_v = grid%v10(ix,iy)*grid%cosa(ix,iy)+grid%u10(ix,iy)*grid%sina(ix,iy)
            output_q = grid%q2(ix,iy)
            output_t = grid%t2(ix,iy)
   
         END IF
   
         
         CALL calc_p8w(grid, ix, iy, p8w, grid%sm32, grid%em32)
         clw=0.
         DO mm = 1, num_moist
            IF ( (mm == P_QC) .OR. (mm == P_QR) .OR. (mm == P_QI) .OR. &
                 (mm == P_QS) .OR. (mm == P_QG) ) THEN
               DO k=grid%sm32,grid%em32-1
                  clw=clw+grid%moist(ix,k,iy,mm)*(p8w(k)-p8w(k+1))
               END DO
           END IF
         END DO
         clw = clw / g
   
         CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )
         grid%ts_hour(n,i) = xtime_minutes / 60.
            
            DO k=1,grid%max_ts_level
            grid%ts_u_profile(n,i,k)   = earth_u_profile(k)
            grid%ts_v_profile(n,i,k)   = earth_v_profile(k)
            grid%ts_gph_profile(n,i,k) = (grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy))/9.81 
            grid%ts_th_profile(n,i,k)  = grid%t_2(ix,k,iy) + 300
            grid%ts_qv_profile(n,i,k)  = grid%moist(ix,k,iy,P_QV)
            END DO
         grid%ts_u(n,i)    = earth_u
         grid%ts_v(n,i)    = earth_v
         grid%ts_t(n,i)    = output_t
         grid%ts_q(n,i)    = output_q
         grid%ts_psfc(n,i) = grid%psfc(ix,iy)
         grid%ts_glw(n,i)  = grid%glw(ix,iy)
         grid%ts_gsw(n,i)  = grid%gsw(ix,iy)
         grid%ts_hfx(n,i)  = grid%hfx(ix,iy)
         grid%ts_lh(n,i)   = grid%lh(ix,iy)
         grid%ts_clw(n,i)  = clw
         grid%ts_rainc(n,i)  = grid%rainc(ix,iy)
         grid%ts_rainnc(n,i) = grid%rainnc(ix,iy)
         grid%ts_tsk(n,i)  = grid%tsk(ix,iy)
         grid%ts_tslb(n,i) = grid%tslb(ix,1,iy)
   
      ELSE
         DO k=1,grid%max_ts_level
         grid%ts_u_profile(n,i,k)     = 1.E30
         grid%ts_v_profile(n,i,k)     = 1.E30
         grid%ts_gph_profile(n,i,k)   = 1.E30
         grid%ts_th_profile(n,i,k)    = 1.E30
         grid%ts_qv_profile(n,i,k)    = 1.E30
         END DO
         grid%ts_hour(n,i) = 1.E30
         grid%ts_u(n,i)    = 1.E30
         grid%ts_v(n,i)    = 1.E30
         grid%ts_t(n,i)    = 1.E30
         grid%ts_q(n,i)    = 1.E30
         grid%ts_psfc(n,i) = 1.E30
         grid%ts_glw(n,i)  = 1.E30
         grid%ts_gsw(n,i)  = 1.E30
         grid%ts_hfx(n,i)  = 1.E30
         grid%ts_lh(n,i)   = 1.E30
         grid%ts_clw(n,i)  = 1.E30
         grid%ts_rainc(n,i)  = 1.E30
         grid%ts_rainnc(n,i) = 1.E30
         grid%ts_tsk(n,i)  = 1.E30
         grid%ts_tslb(n,i) = 1.E30
   
      END IF
   END DO

   DEALLOCATE(p8w, earth_u_profile, earth_v_profile)
 
   grid%next_ts_time = grid%next_ts_time + 1

   IF ( grid%next_ts_time > grid%ts_buf_size ) CALL write_ts(grid)

END SUBROUTINE calc_ts


SUBROUTINE write_ts( grid )

   USE module_domain, ONLY : domain
   USE module_dm, ONLY : wrf_dm_min_reals
   USE module_state_description

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: i, n, ix, iy, iunit, k
   REAL, ALLOCATABLE, DIMENSION(:,:) :: ts_buf
   CHARACTER (LEN=24) :: ts_profile_filename
   CHARACTER (LEN=26) :: profile_format

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN


   IF ( wrf_dm_on_monitor() ) THEN

      iunit = get_unused_unit()
      IF ( iunit <= 0 ) THEN
         CALL wrf_error_fatal3("<stdin>",406,&
'Error in write_ts: could not find a free Fortran unit.')
      END IF

      DO i=1,grid%ntsloc_domain

         ix = grid%itsloc(i)
         iy = grid%jtsloc(i)

         OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(i)), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT='(i2,f13.6,i5,i5,i5,1x,14(f13.5,1x))')  &
                              grid%id, grid%ts_hour(n,i),                &
                              grid%id_tsloc(i), ix, iy,                  &
                              grid%ts_t(n,i),                            &
                              grid%ts_q(n,i),                            &
                              grid%ts_u(n,i),                            &
                              grid%ts_v(n,i),                            &
                              grid%ts_psfc(n,i),                         &
                              grid%ts_glw(n,i),                          &
                              grid%ts_gsw(n,i),                          &
                              grid%ts_hfx(n,i),                          &
                              grid%ts_lh(n,i),                           &
                              grid%ts_tsk(n,i),                          &
                              grid%ts_tslb(n,i),                         &
                              grid%ts_rainc(n,i),                        &
                              grid%ts_rainnc(n,i),                       &
                              grid%ts_clw(n,i)
         END DO
         CLOSE(UNIT=iunit)
         
         
         profile_format = '(f13.6,1x,000(f13.5,1x))'
         k= LEN_TRIM(profile_format)
         WRITE(profile_format(12:14),'(I3.3)') grid%max_ts_level

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",447,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         WRITE(ts_profile_filename,'(A)') TRIM(grid%nametsloc(grid%id_tsloc(i)))//'.d00.TS'
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-4:k-3),'(I2.2)') grid%id
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'UU'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)           &
                              grid%ts_hour(n,i),                      &
                              grid%ts_u_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",469,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'VV'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             & 
                              grid%ts_v_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",489,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'PH'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             &
                              grid%ts_gph_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)
         
         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",509,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'TH'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)   &
                              grid%ts_hour(n,i),              &
                              grid%ts_th_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit) 
       
         
         iunit = get_unused_unit()
            IF ( iunit <= 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",529,&
'Error in write_ts: could not find a free Fortran unit.')
            END IF
         
         k = LEN_TRIM(ts_profile_filename)
         WRITE(ts_profile_filename(k-1:k),'(A2)') 'QV'
         
         OPEN(UNIT=iunit, FILE=TRIM(ts_profile_filename), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT=profile_format)  &
                              grid%ts_hour(n,i),             &
                              grid%ts_qv_profile(n,i,1:grid%max_ts_level)                                       
         END DO
         CLOSE(UNIT=iunit)

      END DO

   END IF

   grid%next_ts_time = 1

END SUBROUTINE write_ts


SUBROUTINE calc_p8w(grid, ix, iy, p8w, k_start, k_end)

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: ix, iy, k_start, k_end
   REAL, DIMENSION(k_start:k_end), INTENT(OUT) :: p8w

   
   INTEGER :: k
   REAL    :: z0, z1, z2, w1, w2 
   REAL, DIMENSION(k_start:k_end)   :: z_at_w
   REAL, DIMENSION(k_start:k_end-1) :: z


   DO k = k_start, k_end
      z_at_w(k) = (grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy))/g
   END DO

   DO k = k_start, k_end-1
      z(k) = 0.5*(z_at_w(k) + z_at_w(k+1))
   END DO

   DO k = k_start+1, k_end-1
      p8w(k) = grid%fnm(k)*(grid%p(ix,k,iy)+grid%pb(ix,k,iy)) + &
               grid%fnp(k)*(grid%p(ix,k-1,iy)+grid%pb(ix,k-1,iy))
   END DO

   z0 = z_at_w(k_start)
   z1 = z(k_start)
   z2 = z(k_start+1)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_start) = w1*(grid%p(ix,k_start,iy)+grid%pb(ix,k_start,iy)) + &
                  w2*(grid%p(ix,k_start+1,iy)+grid%pb(ix,k_start+1,iy))

   z0 = z_at_w(k_end)
   z1 = z(k_end-1)
   z2 = z(k_end-2)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_end) = exp(w1*log(grid%p(ix,k_end-1,iy)+grid%pb(ix,k_end-1,iy)) + &
                    w2*log(grid%p(ix,k_end-2,iy)+grid%pb(ix,k_end-2,iy)))

END SUBROUTINE calc_p8w
