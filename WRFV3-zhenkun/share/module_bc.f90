



MODULE module_bc

   USE module_configure
   USE module_wrf_error
   USE module_model_constants
   IMPLICIT NONE






















   INTEGER, PARAMETER            :: bdyzone = 4
   INTEGER, PARAMETER            :: bdyzone_x = bdyzone
   INTEGER, PARAMETER            :: bdyzone_y = bdyzone

   INTERFACE stuff_bdy
     MODULE PROCEDURE stuff_bdy_new , stuff_bdy_old
   END INTERFACE

   INTERFACE stuff_bdytend
     MODULE PROCEDURE stuff_bdytend_new , stuff_bdytend_old
   END INTERFACE

CONTAINS

  SUBROUTINE boundary_condition_check ( config_flags, bzone, error, gn )







  IMPLICIT NONE

  TYPE( grid_config_rec_type ) config_flags

  INTEGER, INTENT(IN   ) :: bzone, gn
  INTEGER, INTENT(INOUT) :: error



  INTEGER :: xs_bc, xe_bc, ys_bc, ye_bc, bzone_min
  INTEGER :: nprocx, nprocy

  CALL wrf_debug( 100 , ' checking boundary conditions for grid ' )

  error = 0
  xs_bc = 0
  xe_bc = 0
  ys_bc = 0
  ye_bc = 0




  IF( config_flags%periodic_x ) THEN
    xs_bc = xs_bc+1
    xe_bc = xe_bc+1
  ENDIF

  IF( config_flags%periodic_y ) THEN
    ys_bc = ys_bc+1
    ye_bc = ye_bc+1
  ENDIF

  IF( config_flags%symmetric_xs ) xs_bc = xs_bc + 1
  IF( config_flags%symmetric_xe ) xe_bc = xe_bc + 1
  IF( config_flags%open_xs )      xs_bc = xs_bc + 1
  IF( config_flags%open_xe )      xe_bc = xe_bc + 1


  IF( config_flags%symmetric_ys ) ys_bc = ys_bc + 1
  IF( config_flags%symmetric_ye ) ye_bc = ye_bc + 1
  IF( config_flags%open_ys )      ys_bc = ys_bc + 1
  IF( config_flags%open_ye )      ye_bc = ye_bc + 1

  IF( config_flags%nested ) THEN
     xs_bc = xs_bc + 1
     xe_bc = xe_bc + 1
     ys_bc = ys_bc + 1
     ye_bc = ye_bc + 1
   ENDIF

  IF( config_flags%specified ) THEN
     IF( .NOT. config_flags%periodic_x)xs_bc = xs_bc + 1
     IF( .NOT. config_flags%periodic_x)xe_bc = xe_bc + 1
     ys_bc = ys_bc + 1
     ye_bc = ye_bc + 1
   ENDIF

  IF( config_flags%polar ) THEN
     ys_bc = ys_bc + 1
     ye_bc = ye_bc + 1
   ENDIF



   IF( (xs_bc /= 1) .or. &
       (xe_bc /= 1) .or. &
       (ys_bc /= 1) .or. &
       (ye_bc /= 1)         ) THEN

     error = 1

     write( wrf_err_message ,*) ' *** Error in boundary condition specification '
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at xs ', xs_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at xe ', xe_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at ys ', ys_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions at ye ', ye_bc
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' boundary conditions logicals are '
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' periodic_x   ',config_flags%periodic_x
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' periodic_y   ',config_flags%periodic_y
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_xs ',config_flags%symmetric_xs
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_xe ',config_flags%symmetric_xe
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_ys ',config_flags%symmetric_ys
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' symmetric_ye ',config_flags%symmetric_ye
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_xs      ',config_flags%open_xs
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_xe      ',config_flags%open_xe
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_ys      ',config_flags%open_ys
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' open_ye      ',config_flags%open_ye
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' polar        ',config_flags%polar
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' nested       ',config_flags%nested
     CALL wrf_message ( wrf_err_message )
     write( wrf_err_message ,*) ' specified    ',config_flags%specified
     CALL wrf_message ( wrf_err_message )
     CALL wrf_error_fatal3("<stdin>",165,&
' *** Error in boundary condition specification ' )

   ENDIF





   IF( config_flags%periodic_x   .or. &
       config_flags%periodic_y   .or. &
       config_flags%symmetric_xs .or. &
       config_flags%symmetric_xe .or. &
       config_flags%symmetric_ys .or. &
       config_flags%symmetric_ye        )  THEN

       bzone_min = MAX( 1,                                  &
                        (config_flags%h_mom_adv_order+1)/2, &
                        (config_flags%h_sca_adv_order+1)/2 )

       IF( bzone < bzone_min) THEN  

         error = 2
         WRITE ( wrf_err_message , * ) ' boundary zone not large enough '
         CALL wrf_message ( wrf_err_message )
         WRITE ( wrf_err_message , * ) ' boundary zone specified      ',bzone
         CALL wrf_message ( wrf_err_message )
         WRITE ( wrf_err_message , * ) ' minimum boundary zone needed ',bzone_min
         CALL wrf_error_fatal3("<stdin>",193,&
wrf_err_message )

       ENDIF
   ENDIF

   CALL wrf_debug ( 100 , ' boundary conditions OK for grid ' )

   END subroutine boundary_condition_check


   SUBROUTINE set_physical_bc2d( dat, variable_in,  &
                                 config_flags,           & 
                                 ids,ide, jds,jde,   & 
                                 ims,ime, jms,jme,   & 
                                 ips,ipe, jps,jpe,   & 
                                 its,ite, jts,jte   )      













      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , jms:jme ) :: dat
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, istag, jstag, itime, istart, iend

      LOGICAL  :: debug, open_bc_copy



      debug = .false.

      open_bc_copy = .false.

      variable = variable_in
      IF ( variable_in .ge. 'A' .and. variable_in .le. 'Z' ) THEN
        variable = CHAR( ICHAR(variable_in) - ICHAR('A') + ICHAR('a') )
      ENDIF
      IF ((variable == 'u') .or. (variable == 'v') .or.  &
          (variable == 'w') .or. (variable == 't') .or.  &
          (variable == 'x') .or. (variable == 'y') .or.  &
          (variable == 'r') .or. (variable == 'p') ) open_bc_copy = .true.



      istag = -1
      jstag = -1

      IF ((variable == 'u') .or. (variable == 'x')) istag = 0
      IF ((variable == 'v') .or. (variable == 'y')) jstag = 0

      if(debug) then
        write(6,*) ' in bc2d, var is ',variable, istag, jstag
        write(6,*) ' b.cs are ',  &
      config_flags%periodic_x,  &
      config_flags%periodic_y
      end if
      
      IF ( variable == 'd' ) then  
         istag = 0
         jstag = 0
      ENDIF
      IF ( variable == 'e' ) then  
         istag = 0
      ENDIF
      IF ( variable == 'f' ) then  
         jstag = 0
      ENDIF







      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN 
        IF ( ( ids == ips ) .and.  ( ide == ipe ) ) THEN  
          IF ( its == ids ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 0,-(bdyzone-1),-1
              dat(ids+i-1,j) = dat(ide+i-1,j)
            ENDDO
            ENDDO

          ENDIF

          IF ( ite == ide ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)

            DO i = -istag , bdyzone
              dat(ide+i+istag,j) = dat(ids+i+istag,j)
            ENDDO
            ENDDO

          ENDIF
        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 1, bdyzone
              dat(ids-i,j) = dat(ids+i-1,j) 
            ENDDO                             
            ENDDO

          ELSE

            IF( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                dat(ids-i,j) = - dat(ids+i,j) 
              ENDDO                             
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                dat(ids-i,j) =   dat(ids+i,j) 
              ENDDO                             
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs




        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO i = 1, bdyzone
              dat(ide+i-1,j) = dat(ide-i,j)  
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                dat(ide+i,j) = - dat(ide-i,j)  
              ENDDO
              ENDDO


            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO i = 0, bdyzone-1
                dat(ide+i,j) = dat(ide-i,j)  
              ENDDO
              ENDDO

            END IF

          END IF 

        END IF symmetry_xe




        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              dat(ids-1,j) = dat(ids,j) 
              dat(ids-2,j) = dat(ids,j)
              dat(ids-3,j) = dat(ids,j)
            ENDDO

        ENDIF open_xs




        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                          ( ite == ide ) .and. open_bc_copy  )  THEN

          IF ( variable /= 'u' .and. variable /= 'x') THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              dat(ide  ,j) = dat(ide-1,j) 
              dat(ide+1,j) = dat(ide-1,j) 
              dat(ide+2,j) = dat(ide-1,j) 
            ENDDO

          ELSE

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              dat(ide+1,j) = dat(ide,j)
              dat(ide+2,j) = dat(ide,j)
              dat(ide+3,j) = dat(ide,j)
            ENDDO

          END IF 

        END IF open_xe



      END IF periodicity_x








        istart = MAX(ids, its-1)
        iend = MIN(ite+1, ide+istag)
        IF ( its .eq. ids) THEN
          istart = ims
        END IF
        IF ( ite .eq. ide) THEN
          iend = ime
        END IF

      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN
        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN    

          IF( jts == jds ) then

            DO j = 0, -(bdyzone-1), -1
              
              DO i = istart, iend
                dat(i,jds+j-1) = dat(i,jde+j-1)
              ENDDO
            ENDDO

          END IF

          IF( jte == jde ) then

            DO j = -jstag, bdyzone
              
              DO i = istart, iend
                dat(i,jde+j+jstag) = dat(i,jds+j+jstag)
              ENDDO
            ENDDO

          END IF

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
              
              DO i = istart, iend
                dat(i,jds-j) = dat(i,jds+j-1) 
              ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
                
                DO i = istart, iend
                  dat(i,jds-j) = - dat(i,jds+j) 
                ENDDO              
              ENDDO

            ELSE

              DO j = 1, bdyzone
                
                DO i = istart, iend
                  dat(i,jds-j) = dat(i,jds+j) 
                ENDDO              
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys



        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
              
              DO i = istart, iend
                dat(i,jde+j-1) = dat(i,jde-j) 
              ENDDO                               
            ENDDO

          ELSE

            IF (variable == 'v' ) THEN

              DO j = 1, bdyzone
                
                DO i = istart, iend
                  dat(i,jde+j) = - dat(i,jde-j)    
                ENDDO                               
              ENDDO

            ELSE

              DO j = 1, bdyzone
                
                DO i = istart, iend
                  dat(i,jde+j) = dat(i,jde-j)
                ENDDO                               
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye



        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            
            DO i = istart, iend
              dat(i,jds-1) = dat(i,jds) 
              dat(i,jds-2) = dat(i,jds) 
              dat(i,jds-3) = dat(i,jds) 
            ENDDO

        ENDIF open_ys



        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF  (variable /= 'v' .and. variable /= 'y' ) THEN

            
            DO i = istart, iend
              dat(i,jde  ) = dat(i,jde-1) 
              dat(i,jde+1) = dat(i,jde-1) 
              dat(i,jde+2) = dat(i,jde-1) 
            ENDDO                               

          ELSE

            
            DO i = istart, iend
              dat(i,jde+1) = dat(i,jde) 
              dat(i,jde+2) = dat(i,jde) 
              dat(i,jde+3) = dat(i,jde) 
            ENDDO                               

          ENDIF

        END IF open_ye
      


      END IF periodicity_y



      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jts == jds) ) THEN  
           DO j = 0, -(bdyzone-1), -1
           DO i = 0, -(bdyzone-1), -1
             dat(ids+i-1,jds+j-1) = dat(ide+i-1,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  
           DO j = 0, -(bdyzone-1), -1
           DO i = 1, bdyzone
             dat(ide+i+istag,jds+j-1) = dat(ids+i+istag,jde+j-1)
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  
           DO j = 1, bdyzone
           DO i = 1, bdyzone
             dat(ide+i+istag,jde+j+jstag) = dat(ids+i+istag,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  
           DO j = 1, bdyzone
           DO i = 0, -(bdyzone-1), -1
             dat(ids+i-1,jde+j+jstag) = dat(ide+i-1,jds+j+jstag)
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE set_physical_bc2d



   SUBROUTINE set_physical_bc3d( dat, variable_in,        &
                               config_flags,                   & 
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )













      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      CHARACTER,    INTENT(IN   )    :: variable_in

      CHARACTER                      :: variable

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dat
      TYPE( grid_config_rec_type ) config_flags

      INTEGER  :: i, j, k, istag, jstag, itime, k_end

      LOGICAL  :: debug, open_bc_copy



      debug = .false.

      open_bc_copy = .false.

      variable = variable_in
      IF ( variable_in .ge. 'A' .and. variable_in .le. 'Z' ) THEN
        variable = CHAR( ICHAR(variable_in) - ICHAR('A') + ICHAR('a') )
      ENDIF

      IF ((variable == 'u') .or. (variable == 'v') .or.     &
          (variable == 'w') .or. (variable == 't') .or.     &
          (variable == 'd') .or. (variable == 'e') .or. &
          (variable == 'x') .or. (variable == 'y') .or. &
          (variable == 'f') .or. (variable == 'r') .or. &
          (variable == 'p')                        ) open_bc_copy = .true.



      istag = -1
      jstag = -1
      k_end = max(1,min(kde-1,kte))


      IF ((variable == 'u') .or. (variable == 'x')) istag = 0
      IF ((variable == 'v') .or. (variable == 'y')) jstag = 0
      IF ((variable == 'd') .or. (variable == 'xy')) then
         istag = 0
         jstag = 0
      ENDIF
      IF ((variable == 'e') ) then
         istag = 0
         k_end = min(kde,kte)
      ENDIF

      IF ((variable == 'f') ) then
         jstag = 0
         k_end = min(kde,kte)
      ENDIF

      IF ( variable == 'w')  k_end = min(kde,kte)



      if(debug) then
        write(6,*) ' in bc, var is ',variable, istag, jstag, kte, k_end
        write(6,*) ' b.cs are ',  &
      config_flags%periodic_x,  &
      config_flags%periodic_y
      end if
      








      periodicity_x:  IF( ( config_flags%periodic_x ) ) THEN

        IF ( ( ids == ips ) .and. ( ide == ipe ) ) THEN  
          IF ( its == ids ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 0,-(bdyzone-1),-1
              dat(ids+i-1,k,j) = dat(ide+i-1,k,j)
            ENDDO
            ENDDO
            ENDDO

          ENDIF


          IF ( ite == ide ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = -istag , bdyzone
              dat(ide+i+istag,k,j) = dat(ids+i+istag,k,j)
            ENDDO
            ENDDO
            ENDDO

          ENDIF

        ENDIF

      ELSE 

        symmetry_xs: IF( ( config_flags%symmetric_xs ) .and.  &
                         ( its == ids )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 1, bdyzone
              dat(ids-i,k,j) = dat(ids+i-1,k,j) 
            ENDDO                                 
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'u' ) THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                dat(ids-i,k,j) = - dat(ids+i,k,j) 
              ENDDO                                 
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                dat(ids-i,k,j) = dat(ids+i,k,j) 
              ENDDO                               
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_xs




        symmetry_xe: IF( ( config_flags%symmetric_xe ) .and.  &
                         ( ite == ide )                  )  THEN

          IF ( (variable /= 'u') .and. (variable /= 'x') ) THEN

            DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
            DO k = kts, k_end
            DO i = 1, bdyzone
              dat(ide+i-1,k,j) = dat(ide-i,k,j)  
            ENDDO
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'u') THEN

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                dat(ide+i,k,j) = - dat(ide-i,k,j)  
              ENDDO
              ENDDO
              ENDDO

            ELSE

              DO j = MAX(jds,jts-1), MIN(jte+1,jde+jstag)
              DO k = kts, k_end
              DO i = 1, bdyzone
                dat(ide+i,k,j) = dat(ide-i,k,j)  
              ENDDO
              ENDDO
              ENDDO

             END IF

          END IF 

        END IF symmetry_xe



        open_xs: IF( ( config_flags%open_xs   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( its == ids ) .and. open_bc_copy  )  THEN

            DO j = jts-bdyzone, MIN(jte,jde+jstag)+bdyzone
            DO k = kts, k_end
              dat(ids-1,k,j) = dat(ids,k,j) 
              dat(ids-2,k,j) = dat(ids,k,j)
              dat(ids-3,k,j) = dat(ids,k,j)
            ENDDO
            ENDDO

        ENDIF open_xs




        open_xe: IF( ( config_flags%open_xe   .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( ite == ide ) .and. open_bc_copy )  THEN

          IF (variable /= 'u' .and. variable /= 'x' ) THEN

            DO j = jts-bdyzone, MIN(jte,jde+jstag)+bdyzone
            DO k = kts, k_end
              dat(ide  ,k,j) = dat(ide-1,k,j)
              dat(ide+1,k,j) = dat(ide-1,k,j)
              dat(ide+2,k,j) = dat(ide-1,k,j)
            ENDDO
            ENDDO

          ELSE


            DO j = MAX(jds,jts-1)-bdyzone, MIN(jte+1,jde+jstag)+bdyzone
            DO k = kts, k_end
              dat(ide+1,k,j) = dat(ide,k,j)
              dat(ide+2,k,j) = dat(ide,k,j)
              dat(ide+3,k,j) = dat(ide,k,j)
            ENDDO
            ENDDO

          END IF 

        END IF open_xe



      END IF periodicity_x



      periodicity_y:  IF( ( config_flags%periodic_y ) ) THEN
        IF ( ( jds == jps ) .and. ( jde == jpe ) )  THEN      
          IF( jts == jds ) then

            DO j = 0, -(bdyzone-1), -1
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jds+j-1) = dat(i,k,jde+j-1)
            ENDDO
            ENDDO
            ENDDO

          END IF

          IF( jte == jde ) then

            DO j = -jstag, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jde+j+jstag) = dat(i,k,jds+j+jstag)
            ENDDO
            ENDDO
            ENDDO

          END IF

        END IF

      ELSE

        symmetry_ys: IF( ( config_flags%symmetric_ys ) .and.  &
                         ( jts == jds)                   )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jds-j) = dat(i,k,jds+j-1) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF (variable == 'v') THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                dat(i,k,jds-j) = - dat(i,k,jds+j) 
              ENDDO              
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                dat(i,k,jds-j) = dat(i,k,jds+j) 
              ENDDO              
              ENDDO
              ENDDO

            END IF

          ENDIF

        ENDIF symmetry_ys



        symmetry_ye: IF( ( config_flags%symmetric_ye ) .and.  &
                         ( jte == jde )                  )  THEN

          IF ( (variable /= 'v') .and. (variable /= 'y') ) THEN

            DO j = 1, bdyzone
            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jde+j-1) = dat(i,k,jde-j) 
            ENDDO                               
            ENDDO
            ENDDO

          ELSE

            IF ( variable == 'v' ) THEN

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                dat(i,k,jde+j) = - dat(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            ELSE

              DO j = 1, bdyzone
              DO k = kts, k_end
              DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
                dat(i,k,jde+j) = dat(i,k,jde-j) 
              ENDDO                               
              ENDDO
              ENDDO

            END IF

          ENDIF

        END IF symmetry_ye
      


        open_ys: IF( ( config_flags%open_ys   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jts == jds) .and. open_bc_copy )  THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jds-1) = dat(i,k,jds) 
              dat(i,k,jds-2) = dat(i,k,jds) 
              dat(i,k,jds-3) = dat(i,k,jds) 
            ENDDO
            ENDDO

        ENDIF open_ys



        open_ye: IF( ( config_flags%open_ye   .or. &
                       config_flags%polar     .or. &
                       config_flags%specified .or. &
                       config_flags%nested            ) .and.  &
                         ( jte == jde ) .and. open_bc_copy )  THEN

          IF (variable /= 'v' .and. variable /= 'y' ) THEN

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jde  ) = dat(i,k,jde-1) 
              dat(i,k,jde+1) = dat(i,k,jde-1) 
              dat(i,k,jde+2) = dat(i,k,jde-1) 
            ENDDO                               
            ENDDO

          ELSE

            DO k = kts, k_end
            DO i = MAX(ids,its-1), MIN(ite+1,ide+istag)
              dat(i,k,jde+1) = dat(i,k,jde) 
              dat(i,k,jde+2) = dat(i,k,jde) 
              dat(i,k,jde+3) = dat(i,k,jde) 
            ENDDO                               
            ENDDO

          ENDIF

      END IF open_ye



      END IF periodicity_y



      IF ( config_flags%periodic_x .and. config_flags%periodic_y &
           .and. (ids == ips) .and. (ide == ipe)                 &
           .and. (jds == jps) .and. (jde == jpe)                   ) THEN

         IF ( (its == ids) .and. (jts == jds) ) THEN  
           DO j = 0, -(bdyzone-1), -1
           DO k = kts, k_end
           DO i = 0, -(bdyzone-1), -1
             dat(ids+i-1,k,jds+j-1) = dat(ide+i-1,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jts == jds) ) THEN  
           DO j = 0, -(bdyzone-1), -1
           DO k = kts, k_end
           DO i = 1, bdyzone
             dat(ide+i+istag,k,jds+j-1) = dat(ids+i+istag,k,jde+j-1)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (ite == ide) .and. (jte == jde) ) THEN  
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 1, bdyzone
             dat(ide+i+istag,k,jde+j+jstag) = dat(ids+i+istag,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

         IF ( (its == ids) .and. (jte == jde) ) THEN  
           DO j = 1, bdyzone
           DO k = kts, k_end
           DO i = 0, -(bdyzone-1), -1
             dat(ids+i-1,k,jde+j+jstag) = dat(ide+i-1,k,jds+j+jstag)
           ENDDO
           ENDDO
           ENDDO
         END IF

       END IF

   END SUBROUTINE set_physical_bc3d

   SUBROUTINE init_module_bc
   END SUBROUTINE init_module_bc





   SUBROUTINE relax_bdytend   ( field, field_tend,                     &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte   &
                                )

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone, relax_zone
      REAL,         INTENT(IN   )    :: dtbc
      CHARACTER,    INTENT(IN   )    :: variable_in

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field_tend
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_ys, field_bdy_tend_ye
      REAL,  DIMENSION( spec_bdy_width ), INTENT(IN   ) :: fcx, gcx
      TYPE( grid_config_rec_type ) config_flags

      CALL relax_bdytend_core   ( field, field_tend,                     &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte,  & 
                                ims,ime, jms,jme, kms,kme )  
   END SUBROUTINE relax_bdytend




   SUBROUTINE relax_bdytend_tile   ( field, field_tend,                     &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte,  &
                                iXs,iXe, jXs,jXe, kXs,kXe   &  
                                )

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: iXs,iXe, jXs,jXe, kXs,kXe
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone, relax_zone
      REAL,         INTENT(IN   )    :: dtbc
      CHARACTER,    INTENT(IN   )    :: variable_in

      REAL,  DIMENSION( iXs:iXe , kXs:kXe , jXs:jXe ), INTENT(IN   ) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field_tend
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_ys, field_bdy_tend_ye
      REAL,  DIMENSION( spec_bdy_width ), INTENT(IN   ) :: fcx, gcx
      TYPE( grid_config_rec_type ) config_flags

      CALL relax_bdytend_core   ( field, field_tend,                     &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte,  &
                                iXs,iXe, jXs,jXe, kXs,kXe )  
   END SUBROUTINE relax_bdytend_tile

   SUBROUTINE relax_bdytend_core   ( field, field_tend,                     &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone, relax_zone, &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte,  & 
                                iXs,iXe, jXs,jXe, kXs,kXe   & 
                                )








      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: iXs,iXe, jXs,jXe, kXs,kXe
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone, relax_zone
      REAL,         INTENT(IN   )    :: dtbc
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( iXs:iXe , kXs:kXe , jXs:jXe ), INTENT(IN   ) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field_tend
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_ys, field_bdy_tend_ye
      REAL,  DIMENSION( spec_bdy_width ), INTENT(IN   ) :: fcx, gcx
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
      INTEGER    :: b_dist, b_limit
      REAL       :: fls0, fls1, fls2, fls3, fls4
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x
      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte


      IF (jts - jbs .lt. relax_zone) THEN

        DO j = max(jts,jbs+spec_zone), min(jtf,jbs+relax_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              im1 = max(i-1,ibs)
              ip1 = min(i+1,ibe)
              fls0 = field_bdy_ys(i, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ys(i, k, b_dist+1) &
                   - field(i,k,j)
              fls1 = field_bdy_ys(im1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ys(im1, k, b_dist+1) &
                   - field(im1,k,j)
              fls2 = field_bdy_ys(ip1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ys(ip1, k, b_dist+1) &
                   - field(ip1,k,j)
              fls3 = field_bdy_ys(i, k, b_dist) &
                   + dtbc * field_bdy_tend_ys(i, k, b_dist) &
                   - field(i,k,j-1)
              fls4 = field_bdy_ys(i, k, b_dist+2) &
                   + dtbc * field_bdy_tend_ys(i, k, b_dist+2) &
                   - field(i,k,j+1)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (jbe - jtf .lt. relax_zone) THEN



        DO j = max(jts,jbe-relax_zone+1), min(jtf,jbe-spec_zone)
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0


          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              im1 = max(i-1,ibs)
              ip1 = min(i+1,ibe)
              fls0 = field_bdy_ye(i, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ye(i, k, b_dist+1) &
                   - field(i,k,j)
              fls1 = field_bdy_ye(im1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ye(im1, k, b_dist+1) &
                   - field(im1,k,j)
              fls2 = field_bdy_ye(ip1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ye(ip1, k, b_dist+1) &
                   - field(ip1,k,j)
              fls3 = field_bdy_ye(i, k, b_dist) &
                   + dtbc * field_bdy_tend_ye(i, k, b_dist) &
                   - field(i,k,j+1)
              fls4 = field_bdy_ye(i, k, b_dist+2) &
                   + dtbc * field_bdy_tend_ye(i, k, b_dist+2) &
                   - field(i,k,j-1)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)

            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. relax_zone) THEN

        DO i = max(its,ibs+spec_zone), min(itf,ibs+relax_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              fls0 = field_bdy_xs(j, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xs(j, k, b_dist+1) &
                   - field(i,k,j)
              fls1 = field_bdy_xs(j-1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xs(j-1, k, b_dist+1) &
                   - field(i,k,j-1)
              fls2 = field_bdy_xs(j+1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xs(j+1, k, b_dist+1) &
                   - field(i,k,j+1)
              fls3 = field_bdy_xs(j, k, b_dist) &
                   + dtbc * field_bdy_tend_xs(j, k, b_dist) &
                   - field(i-1,k,j)
              fls4 = field_bdy_xs(j, k, b_dist+2) &
                   + dtbc * field_bdy_tend_xs(j, k, b_dist+2) &
                   - field(i+1,k,j)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)

            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. relax_zone) THEN

        DO i = max(its,ibe-relax_zone+1), min(itf,ibe-spec_zone)
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              fls0 = field_bdy_xe(j, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xe(j, k, b_dist+1) &
                   - field(i,k,j)
              fls1 = field_bdy_xe(j-1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xe(j-1, k, b_dist+1) &
                   - field(i,k,j-1)
              fls2 = field_bdy_xe(j+1, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xe(j+1, k, b_dist+1) &
                   - field(i,k,j+1)
              fls3 = field_bdy_xe(j, k, b_dist) &
                   + dtbc * field_bdy_tend_xe(j, k, b_dist) &
                   - field(i+1,k,j)
              fls4 = field_bdy_xe(j, k, b_dist+2) &
                   + dtbc * field_bdy_tend_xe(j, k, b_dist+2) &
                   - field(i-1,k,j)
              field_tend(i,k,j) = field_tend(i,k,j) &
                                + fcx(b_dist+1)*fls0 &
                                - gcx(b_dist+1)*(fls1+fls2+fls3+fls4-4.*fls0)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF



   END SUBROUTINE relax_bdytend_core


   SUBROUTINE spec_bdytend   ( field_tend,                           &
                               field_bdy_xs, field_bdy_xe,           &
                               field_bdy_ys, field_bdy_ye,           &
                               field_bdy_tend_xs, field_bdy_tend_xe, &
                               field_bdy_tend_ys, field_bdy_tend_ye, &
                               variable_in, config_flags, & 
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT  ) :: field_tend
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye 
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_ys, field_bdy_tend_ye 
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field_tend(i,k,j) = field_bdy_tend_ys(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 



        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0


          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field_tend(i,k,j) = field_bdy_tend_ye(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field_tend(i,k,j) = field_bdy_tend_xs(j, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field_tend(i,k,j) = field_bdy_tend_xe(j, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE spec_bdytend
   









   SUBROUTINE spec_bdytend_perturb   ( field_tend,                   &
                                       field_tend_perturb,           &
                                       mu_2, mub, c1, c2,            &
                                       variable_in,                  &
                                       msf, config_flags,            & 
                                       spec_bdy_width, spec_zone,    &
                                       kme_stoch,                    & 
                                       ids,ide, jds,jde, kds,kde,    & 
                                       ims,ime, jms,jme, kms,kme,    & 
                                       ips,ipe, jps,jpe, kps,kpe,    & 
                                       its,ite, jts,jte, kts,kte )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme, kme_stoch
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme       , jms:jme ), INTENT(INOUT  ) :: field_tend
      REAL,  DIMENSION( ims:ime , kms:kme_stoch , jms:kme ), INTENT(IN   )   :: field_tend_perturb
      REAL,  DIMENSION( ims:ime ,                 jms:jme ), INTENT(IN   )   :: mu_2	
      REAL,  DIMENSION( ims:ime ,                 jms:jme ), INTENT(IN   )   :: mub
      REAL,  DIMENSION( ims:ime ,                 jms:jme ), INTENT(IN   )   :: msf
      REAL,  DIMENSION(           kms:kme                 ), INTENT(IN   )   :: c1, c2
	  
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'T') variable = 't'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN


        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
            IF (variable == 't') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * & 
                                  (mu_2(i,j)+mub(i,j)) 
            ENDIF
            IF (variable == 'u') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * & 
                                  0.5*(mu_2(i,j)+mu_2(i-1,j) + mub(i,j)+mub(i-1,j)) / msf(i,j)
            ENDIF
            IF (variable == 'v') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * & 
                                  0.5*(mu_2(i,j)+mu_2(i,j-1) + mub(i,j)+mub(i,j-1)) / msf(i,j)
            ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 



        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
            IF (variable == 't') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * (mu_2(i,j)+mub(i,j))
            ENDIF
            IF (variable == 'u') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i-1,j) + mub(i,j)+mub(i-1,j)) / msf(i,j)
            ENDIF
            IF (variable == 'v') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i,j-1) + mub(i,j)+mub(i,j-1)) / msf(i,j)
            ENDIF
	    ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
            IF (variable == 't') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * (mu_2(i,j)+mub(i,j))
            ENDIF
            IF (variable == 'u') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i-1,j) + mub(i,j)+mub(i-1,j)) / msf(i,j)
            ENDIF
            IF (variable == 'v') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i,j-1) + mub(i,j)+mub(i,j-1)) / msf(i,j)
            ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
            IF (variable == 't') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * (mu_2(i,j)+mub(i,j))
            ENDIF
            IF (variable == 'u') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i-1,j) + mub(i,j)+mub(i-1,j)) / msf(i,j)
            ENDIF
            IF (variable == 'v') THEN
              field_tend(i,k,j) = field_tend(i,k,j) + &
                                  field_tend_perturb(i,min(k,kme_stoch),j) * 0.5*(mu_2(i,j)+mu_2(i,j-1) + mub(i,j)+mub(i,j-1)) / msf(i,j)
            ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE spec_bdytend_perturb




   SUBROUTINE spec_bdytend_perturb_chem   ( field_bdy_tend_xs, field_bdy_tend_xe, &
                                            field_bdy_tend_ys, field_bdy_tend_ye, &
                                            field_scalar_perturb,         &
                                            variable_in,                &
                                            periodic_x,                 &
                                            spec_bdy_width, spec_zone,  &
                                            kme_stoch,                  & 
                                            ids,ide, jds,jde, kds,kde,  &
                                            ims,ime, jms,jme, kms,kme,  &
                                            ips,ipe, jps,jpe, kps,kpe,  &
                                            its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme, kme_stoch
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      LOGICAL,      INTENT(IN   )    :: periodic_x

      REAL,  DIMENSION( ims:ime , kms:kme_stoch , jms:kme ) , INTENT(IN   ) :: field_scalar_perturb
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(INOUT) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(INOUT) :: field_bdy_tend_ys, field_bdy_tend_ye

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit

      variable = variable_in
      IF (variable == 'C') variable = 'c'

      IF (variable /= 'c') THEN
          write( wrf_err_message ,*) ' *** Error in spec_bdytend_perturb_chem'
          CALL wrf_message ( wrf_err_message )
      ENDIF

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
           b_dist = j - jbs
           b_limit = b_dist
           IF(periodic_x)b_limit = 0
           DO k = kts, ktf
              DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
                 field_bdy_tend_ys(i,k,b_dist+1) = field_bdy_tend_ys(i,k,b_dist+1) * & 
                                                   (1.0 + field_scalar_perturb(i,min(k,kme_stoch),j))
              ENDDO
           ENDDO
        ENDDO
      ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN

        DO j = max(jts,jbe-spec_zone+1), jtf
           b_dist = jbe - j
           b_limit = b_dist
           IF(periodic_x)b_limit = 0
           DO k = kts, ktf
              DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
                 field_bdy_tend_ye(i,k,b_dist+1) = field_bdy_tend_ye(i,k,b_dist+1) * & 
                                                   (1.0 + field_scalar_perturb(i,min(k,kme_stoch),j))
              ENDDO
           ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN

      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
           b_dist = i - ibs
           DO k = kts, ktf
              DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
                 field_bdy_tend_xs(j,k,b_dist+1) = field_bdy_tend_xs(j,k,b_dist+1) * & 
                                                   (1.0 + field_scalar_perturb(i,min(k,kme_stoch),j))
              ENDDO
           ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
           b_dist = ibe - i
           DO k = kts, ktf
             DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
                 field_bdy_tend_xe(j,k,b_dist+1) = field_bdy_tend_xe(j,k,b_dist+1) * (1.0 + field_scalar_perturb(i,k,j))
              ENDDO
           ENDDO
        ENDDO
      ENDIF

    ENDIF  

   END SUBROUTINE spec_bdytend_perturb_chem



   SUBROUTINE spec_bdyfield   ( field,                     &
                               field_bdy_xs, field_bdy_xe,           &
                               field_bdy_ys, field_bdy_ye,           &
                               variable_in, config_flags,  & 
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )






      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT  ) :: field
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field(i,k,j) = field_bdy_ys(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      IF (jbe - jtf .lt. spec_zone) THEN

        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field(i,k,j) = field_bdy_ye(i, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field_bdy_xs(j, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field_bdy_xe(j, k, b_dist+1)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

   END SUBROUTINE spec_bdyfield


   SUBROUTINE spec_bdyupdate(  field,      &
                               field_tend, dt,            &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )





      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 

        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              field(i,k,j) = field(i,k,j) + dt*field_tend(i,k,j) 
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE spec_bdyupdate

   SUBROUTINE spec_bdy_final   ( field, mu, c1, c2, msf,               &
                                field_bdy_xs, field_bdy_xe,            &
                                field_bdy_ys, field_bdy_ye,            &
                                field_bdy_tend_xs, field_bdy_tend_xe,  &
                                field_bdy_tend_ys, field_bdy_tend_ye,  &
                                variable_in, config_flags,             &
                                spec_bdy_width, spec_zone,             &
                                dtbc,                       &
                                ids,ide, jds,jde, kds,kde,  & 
                                ims,ime, jms,jme, kms,kme,  & 
                                ips,ipe, jps,jpe, kps,kpe,  & 
                                its,ite, jts,jte, kts,kte   )










      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_bdy_width, spec_zone
      REAL,         INTENT(IN   )    :: dtbc
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , jms:jme), INTENT(IN   ) :: mu, msf
      REAL,  DIMENSION( kms:kme ), INTENT(IN   ) :: c1, c2
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_xs, field_bdy_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_ys, field_bdy_ye
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_xs, field_bdy_tend_xe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ), INTENT(IN   ) :: field_bdy_tend_ys, field_bdy_tend_ye
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, im1, ip1
      INTEGER    :: b_dist, b_limit
      REAL       :: bfield, xmsf, xmu
      LOGICAL    :: periodic_x, msfcouple, mucouple

      periodic_x = config_flags%periodic_x
      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'W') variable = 'w'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'T') variable = 't'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kde
      IF (variable == 'h') ktf = kde
      IF (variable == 'w') ktf = kde

      msfcouple = .false.
      mucouple = .true.
      IF (variable == 'u' .OR. variable == 'v' .OR. variable == 'w')msfcouple = .true.
      IF (variable == 'm' )mucouple = .false.
      xmsf = 1.
      xmu = 1.

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              bfield = field_bdy_ys(i, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ys(i, k, b_dist+1) 
              if(msfcouple)xmsf = msf(i,j)
              if(mucouple)xmu = mu(i,j)
              field(i,k,j) = xmsf*bfield/xmu
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN

        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              bfield = field_bdy_ye(i, k, b_dist+1) &
                   + dtbc * field_bdy_tend_ye(i, k, b_dist+1) 
              if(msfcouple)xmsf = msf(i,j)
              if(mucouple)xmu = mu(i,j)
              field(i,k,j) = xmsf*bfield/xmu
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              bfield = field_bdy_xs(j, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xs(j, k, b_dist+1) 
              if(msfcouple)xmsf = msf(i,j)
              if(mucouple)xmu = mu(i,j)
              field(i,k,j) = xmsf*bfield/xmu
            ENDDO
          ENDDO
        ENDDO
      ENDIF

     IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              bfield = field_bdy_xe(j, k, b_dist+1) &
                   + dtbc * field_bdy_tend_xe(j, k, b_dist+1) 
              if(msfcouple)xmsf = msf(i,j)
              if(mucouple)xmu = mu(i,j)
              field(i,k,j) = xmsf*bfield/xmu
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

   END SUBROUTINE spec_bdy_final


   SUBROUTINE zero_grad_bdy (  field,                     &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )





      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'w') ktf = kde

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              field(i,k,j) = field(i_inner,k,jbs+spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 

        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              field(i,k,j) = field(i_inner,k,jbe-spec_zone)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              field(i,k,j) = field(ibs+spec_zone,k,j_inner)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              field(i,k,j) = field(ibe-spec_zone,k,j_inner)
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE zero_grad_bdy


   SUBROUTINE flow_dep_bdy  (  field,                     &
                               u, v, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )







      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 

        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j+1) .gt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE flow_dep_bdy



   SUBROUTINE flow_dep_bdy_qnn  (  field,          &
                               u, v, config_flags, &
                               spec_zone,                  &
                               ccn_conc,                   & 
                               ids,ide, jds,jde, kds,kde,  & 
                               ims,ime, jms,jme, kms,kme,  & 
                               ips,ipe, jps,jpe, kps,kpe,  & 
                               its,ite, jts,jte, kts,kte )







      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      REAL,         INTENT(IN   )    :: ccn_conc 


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf, i_inner, j_inner
      INTEGER    :: b_dist, b_limit
      LOGICAL    :: periodic_x

      periodic_x = config_flags%periodic_x

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN

        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbs+spec_zone)
              ELSE
                field(i,k,j) = ccn_conc 
              ENDIF
            ENDDO
          ENDDO
        ENDDO 
      ENDIF   
      IF (jbe - jtf .lt. spec_zone) THEN

        DO j = max(jts,jbe-spec_zone+1), jtf
          b_dist = jbe - j
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(periodic_x)i_inner = i
              IF(v(i,k,j+1) .gt. 0.)THEN
                field(i,k,j) = field(i_inner,k,jbe-spec_zone)
              ELSE
                field(i,k,j) = ccn_conc 
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN

        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                field(i,k,j) = field(ibs+spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = ccn_conc 
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (ibe - itf .lt. spec_zone) THEN

        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                field(i,k,j) = field(ibe-spec_zone,k,j_inner)
              ELSE
                field(i,k,j) = ccn_conc 
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

   END SUBROUTINE flow_dep_bdy_qnn



 SUBROUTINE stuff_bdy_new ( data3d , space_bdy_xs, space_bdy_xe, space_bdy_ys, space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3d
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_ys, space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy_xe(j,k,ii) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy_ys(i,k,j) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy_ye(i,k,jj) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdy_new
 
 SUBROUTINE stuff_bdytend_new ( data3dnew , data3dold , time_diff , &
                             space_bdy_xs, space_bdy_xe, space_bdy_ys, space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3dnew , data3dold
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_ys, space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff 
 
    INTEGER :: i , ii , j , jj , k

    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy_xe(j,k,ii) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy_ye(i,k,jj) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdytend_new



 SUBROUTINE stuff_bdy_old ( data3d , space_bdy , char_stagger , &
                             ijds , ijde , spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: ijds , ijde , spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3d
    REAL , DIMENSION(ijds:ijde,kds:kde,spec_bdy_width,4) , INTENT(OUT) :: space_bdy
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy(j,k,ii,P_XEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy(i,k,j,P_YSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy(i,k,jj,P_YEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = data3d(i,k,j)
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdy_old
 
 SUBROUTINE stuff_bdytend_old ( data3dnew , data3dold , time_diff , space_bdy , char_stagger , &
                             ijds , ijde , spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: ijds , ijde , spec_bdy_width
    REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: data3dnew , data3dold

    REAL , DIMENSION(ijds:ijde,kds:kde,spec_bdy_width,4) , INTENT(OUT) :: space_bdy
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff 
 
    INTEGER :: i , ii , j , jj , k
 
    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy(j,k,i,P_XSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy(j,k,ii,P_XEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO k = kds , kde - 1
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy(j,k,ii,P_XEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy(i,k,j,P_YSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy(i,k,j,P_YSB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy(i,k,jj,P_YEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    ELSE
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO k = kds , kde - 1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy(i,k,jj,P_YEB) = ( data3dnew(i,k,j) - data3dold(i,k,j) ) / time_diff

       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdytend_old

 SUBROUTINE stuff_bdy_ijk ( data3d , space_bdy_xs, space_bdy_xe, &
                             space_bdy_ys, space_bdy_ye, &
                             char_stagger , spec_bdy_width, &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN) :: data3d


    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_ys, space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)




       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdy_ijk
 
 SUBROUTINE stuff_bdytend_ijk ( data3dnew , data3dold , time_diff , &
                             space_bdy_xs, space_bdy_xe, space_bdy_ys, space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
 
 
 
    USE module_state_description
    
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width

    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN) :: data3dnew , data3dold
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: space_bdy_ys, space_bdy_ye

    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff 
 
    INTEGER :: i , ii , j , jj , k
 
    
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy_xe(j,k,ii) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          space_bdy_ys(i,k,j) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF
 
    
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          space_bdy_ye(i,k,jj) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          space_bdy_ye(i,k,jj) = ( data3dnew(i,j,k) - data3dold(i,j,k) ) / time_diff



       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE stuff_bdytend_ijk

END MODULE module_bc

SUBROUTINE get_bdyzone_x ( bzx )
  USE module_bc
  IMPLICIT NONE
  INTEGER bzx
  bzx = bdyzone_x
END SUBROUTINE get_bdyzone_x

SUBROUTINE get_bdyzone_y ( bzy)
  USE module_bc
  IMPLICIT NONE
  INTEGER bzy
  bzy = bdyzone_y
END SUBROUTINE get_bdyzone_y

SUBROUTINE get_bdyzone ( bz)
  USE module_bc
  IMPLICIT NONE
  INTEGER bz
  bz = bdyzone
END SUBROUTINE get_bdyzone
