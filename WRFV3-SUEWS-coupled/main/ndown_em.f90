


PROGRAM ndown_em

   USE module_machine
   USE module_domain, ONLY : domain, head_grid, alloc_and_configure_domain, &
                             domain_clock_set, domain_clock_get, get_ijk_from_grid
   USE module_domain_type, ONLY : program_name
   USE module_initialize_real, ONLY : wrfu_initialize, rebalance_driver
   USE module_integrate
   USE module_driver_constants
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec
   USE module_io_domain
   USE module_utility
   USE module_check_a_mundo

   USE module_timing
   USE module_wrf_error



   USE module_bc
   USE module_big_step_utilities_em
   USE module_get_file_names

   IMPLICIT NONE
 
   INTERFACE
     
     SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags)
       USE module_domain
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_read_wrf_chem_bioemiss

     SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
       USE module_domain
       USE module_configure
       TYPE(domain), POINTER  :: parent , nest
     END SUBROUTINE init_domain_constants_em_ptr

     SUBROUTINE vertical_interp (nested_grid,znw_c,znu_c,cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c,k_dim_c,c3h,c4h,c3f,c4f)
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER ::  nested_grid
         INTEGER , INTENT (IN) :: k_dim_c
         REAL , INTENT (IN) :: cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c
         REAL , DIMENSION(k_dim_c) , INTENT (IN) ::  znw_c,znu_c
         REAL , DIMENSION(k_dim_c) , INTENT (IN) ::  c3h,c4h,c3f,c4f
      END SUBROUTINE vertical_interp


   END INTERFACE
   INTEGER :: ids , ide , jds , jde , kds , kde
   INTEGER :: ims , ime , jms , jme , kms , kme
   INTEGER :: ips , ipe , jps , jpe , kps , kpe
   INTEGER :: its , ite , jts , jte , kts , kte
   INTEGER :: nids, nide, njds, njde, nkds, nkde,    &
              nims, nime, njms, njme, nkms, nkme,    &
              nips, nipe, njps, njpe, nkps, nkpe
   INTEGER :: spec_bdy_width
   INTEGER :: i , j , k , nvchem, nvmoist, nvscalar
   INTEGER :: time_loop_max , time_loop
   INTEGER :: total_time_sec , file_counter
   INTEGER :: julyr , julday , iswater , map_proj
   INTEGER :: icnt

   REAL    :: dt , new_bdy_frq
   REAL    :: gmt , cen_lat , cen_lon , dx , dy , truelat1 , truelat2 , moad_cen_lat , stand_lon

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp1 , vbdy3dtemp1 , tbdy3dtemp1 , pbdy3dtemp1 , qbdy3dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp1
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: ubdy3dtemp2 , vbdy3dtemp2 , tbdy3dtemp2 , pbdy3dtemp2 , qbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: mbdy2dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: cbdy3dtemp1 , cbdy3dtemp2
   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: sbdy3dtemp1 , sbdy3dtemp2
   REAL , DIMENSION(:,:,:,:) , ALLOCATABLE :: cbdy3dtemp0
   REAL , DIMENSION(:,:,:,:) , ALLOCATABLE :: qbdy3dtemp1_coupled, qbdy3dtemp2_coupled
   REAL , DIMENSION(:,:,:,:) , ALLOCATABLE :: sbdy3dtemp1_coupled, sbdy3dtemp2_coupled

   CHARACTER(LEN=19) :: start_date_char , current_date_char , end_date_char
   CHARACTER(LEN=19) :: stopTimeStr



   INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

   REAL    :: time
   INTEGER :: rc

   INTEGER :: loop , levels_to_process
   INTEGER , PARAMETER :: max_sanity_file_loop = 100

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain, parent_grid , nested_grid
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , fido, fidb , oid , idum1 , idum2 , ierr
   INTEGER :: status_next_var
   INTEGER :: debug_level
   LOGICAL :: input_from_file , need_new_file
   CHARACTER (LEN=19) :: date_string


   INTEGER                 :: idsi
   CHARACTER (LEN=256)     :: inpname , outname , bdyname
   CHARACTER (LEN=256)     :: si_inpname
character *19 :: temp19
character *24 :: temp24 , temp24b
character(len=24) :: start_date_hold

   CHARACTER (LEN=256)     :: message
integer :: ii

   CHARACTER (LEN=10) :: release_version = 'V3.9.1.1  '


    integer ::  n_ref_m,k_dim_c,k_dim_n
real :: cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c
   REAL , DIMENSION(:) , ALLOCATABLE :: znw_c,znu_c
   REAL , DIMENSION(:) , ALLOCATABLE :: c3h,c4h,c3f,c4f


   
   

   INTERFACE

      SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: parent_grid , nested_grid
      END SUBROUTINE med_interp_domain

      SUBROUTINE Setup_Timekeeping( parent_grid )
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
      END SUBROUTINE Setup_Timekeeping

      SUBROUTINE vert_cor(parent_grid,znw_c,k_dim_c)
         USE module_domain
         TYPE(domain), POINTER :: parent_grid
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c
      END SUBROUTINE vert_cor
   END INTERFACE

   

   program_name = "NDOWN_EM " // TRIM(release_version) // " PREPROCESSOR"

   CALL wrf_error_fatal3("<stdin>",155,&
'NDOWN :  HAVE TO BUILD FOR NESTING' )

   
   
   
   

   CALL init_modules(1)   
   CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
   CALL init_modules(2)   

   
   
   
   

   CALL initial_config

   CALL check_nml_consistency
   CALL setup_physics_suite
   CALL set_physics_rconfigs

   
   

   IF ( model_config_rec%io_form_auxinput2 .EQ. 0 ) THEN
      CALL wrf_error_fatal3("<stdin>",182,&
'ndown: Please set io_form_auxinput2 in the time_control namelist record (probably to 2).')
   END IF


   n_ref_m = model_config_rec % vert_refine_fact
   k_dim_c = model_config_rec % e_vert(1)
   k_dim_n = k_dim_c
   if (n_ref_m .ge. 2) k_dim_n = (k_dim_c - 1) *  n_ref_m + 1
   model_config_rec % e_vert(1) = k_dim_n
   model_config_rec % e_vert(2) = k_dim_n
   ALLOCATE(znw_c(k_dim_c))
   ALLOCATE(znu_c(k_dim_c))
   ALLOCATE(c3h(k_dim_c))
   ALLOCATE(c4h(k_dim_c))
   ALLOCATE(c3f(k_dim_c))
   ALLOCATE(c4f(k_dim_c))
   WRITE ( message , FMT = '(A,3I5)' ) 'KDIM_C', k_dim_c , model_config_rec % e_vert(1) , model_config_rec % e_vert(2)
   CALL       wrf_debug (  99,message )


   

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   
   

   NULLIFY( null_domain )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'ndown_em: calling alloc_and_configure_domain coarse ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   parent_grid => head_grid

   

   CALL Setup_Timekeeping ( parent_grid )

   CALL domain_clock_set( head_grid, &
                          time_step_seconds=model_config_rec%interval_seconds )
   CALL       wrf_debug ( 100 , 'ndown_em: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'ndown_em: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( parent_grid%id , idum1, idum2 )

   

   CALL       wrf_debug ( 100 , 'ndown_em: calling init_wrfio' )
   CALL init_wrfio

   
   


   
   

   WRITE ( start_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%start_year  (parent_grid%id) , &
           model_config_rec%start_month (parent_grid%id) , &
           model_config_rec%start_day   (parent_grid%id) , &
           model_config_rec%start_hour  (parent_grid%id) , &
           model_config_rec%start_minute(parent_grid%id) , &
           model_config_rec%start_second(parent_grid%id)

   WRITE (   end_date_char , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%  end_year  (parent_grid%id) , &
           model_config_rec%  end_month (parent_grid%id) , &
           model_config_rec%  end_day   (parent_grid%id) , &
           model_config_rec%  end_hour  (parent_grid%id) , &
           model_config_rec%  end_minute(parent_grid%id) , &
           model_config_rec%  end_second(parent_grid%id)

   
   CALL domain_clock_set( parent_grid, stop_timestr=end_date_char )

   CALL geth_idts ( end_date_char , start_date_char , total_time_sec )

   new_bdy_frq = model_config_rec%interval_seconds
   time_loop_max = total_time_sec / model_config_rec%interval_seconds + 1

   start_date        = start_date_char // '.0000'
   current_date      = start_date_char // '.0000'
   start_date_hold   = start_date_char // '.0000'
   current_date_char = start_date_char

   
   
   

   file_counter = 1
   need_new_file = .FALSE.
   CALL unix_ls ( 'wrfout' , parent_grid%id )

   

   CALL wrf_debug          ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
   CALL open_r_dataset     ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=AUXINPUT1", ierr )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                  ' for reading ierr=',ierr
      CALL wrf_error_fatal3("<stdin>",289,&
wrf_err_message )
   ENDIF

   

   big_time_loop_thingy : DO time_loop = 1 , time_loop_max

      

      CALL geth_newdate ( date_string , start_date_char , ( time_loop - 1 ) * NINT ( new_bdy_frq) )
      WRITE ( message , FMT = '(A,I5," ",A,A)' ) '-------->>>  Processing data: loop=',time_loop,'  date/time = ',date_string
      CALL       wrf_debug (  99,message )
      current_date_char = date_string
      current_date      = date_string // '.0000'
      start_date        = date_string // '.0000'
      WRITE ( message , FMT = '(A,I5," ",A,A)' ) 'loopmax = ', time_loop_max, '   ending date = ',end_date_char
      CALL       wrf_debug (  99,message )
      CALL domain_clock_set( parent_grid, &
                             current_timestr=current_date(1:19) )

      
      
      

      get_the_right_time : DO

         CALL wrf_get_next_time ( fid , date_string , status_next_var )
         WRITE ( message , FMT = '(A,A,A,A,A,I5)' ) 'file date/time = ',date_string,'     desired date = ',&
         current_date_char,'     status = ', status_next_var
         CALL       wrf_debug (  99,message )

         IF      (  status_next_var .NE. 0 ) THEN
            CALL wrf_debug          ( 100 , 'ndown_em main: calling close_dataset  for ' // TRIM(eligible_file_name(file_counter)) )
            CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )
            file_counter = file_counter + 1
            IF ( file_counter .GT. number_of_eligible_files ) THEN
               WRITE( wrf_err_message , FMT='(A)' ) 'program ndown: opening too many files'
               CALL wrf_message ( TRIM(wrf_err_message) )
               WRITE( wrf_err_message , FMT='(A)' ) 'Usually, this is caused by trying to run ndown past the last time available d01 model output'
               CALL wrf_message ( TRIM(wrf_err_message) )
               WRITE( wrf_err_message , FMT='(A)' ) 'The CG model output is used to supply lateral boundary conditions'
               CALL wrf_message ( TRIM(wrf_err_message) )
               WRITE( wrf_err_message , FMT='(A)' ) 'The ndown program uses the start and end times, the WRF model for d01 likely used the run times option.'
               CALL wrf_message ( TRIM(wrf_err_message) )
               WRITE( wrf_err_message , FMT='(A)' ) 'Check the namelist.input time_control section for inconsistencies.'
               CALL wrf_error_fatal3("<stdin>",335,&
wrf_err_message )
            END IF
            CALL wrf_debug      ( 100 , 'ndown_em main: calling open_r_dataset for ' // TRIM(eligible_file_name(file_counter)) )
            CALL open_r_dataset ( fid, TRIM(eligible_file_name(file_counter)) , head_grid , config_flags , "DATASET=INPUT", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(eligible_file_name(file_counter)), &
                                                           ' for reading ierr=',ierr
               CALL wrf_error_fatal3("<stdin>",343,&
wrf_err_message )
            ENDIF
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .LT. TRIM(current_date_char) ) THEN
            CYCLE get_the_right_time
         ELSE IF ( TRIM(date_string) .EQ. TRIM(current_date_char) ) THEN
            EXIT get_the_right_time
         ELSE IF ( TRIM(date_string) .GT. TRIM(current_date_char) ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,A,A)' ) 'Found ',TRIM(date_string),' before I found ',TRIM(current_date_char),'.'
            CALL wrf_error_fatal3("<stdin>",353,&
wrf_err_message )
         END IF
      END DO get_the_right_time

      CALL wrf_debug          ( 100 , 'wrf: calling input_history' )
      CALL wrf_get_previous_time ( fid , date_string , status_next_var )
      WRITE ( message , * ) 'CFB' ,cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c,znw_c(1),znu_c(1)
      CALL       wrf_debug (  99,message )
      CALL input_history ( fid , head_grid , config_flags, ierr)

      cf1_c = head_grid%cf1
      cf2_c = head_grid%cf2
      cf3_c = head_grid%cf3

      cfn_c = head_grid%cfn
      cfn1_c = head_grid%cfn1

      do k = 1,k_dim_c
      znw_c(k) = head_grid%znw(k)
      znu_c(k) = head_grid%znu(k)
      enddo
      do k = 1,k_dim_c
      c3h(k) = head_grid%c3h(k)
      c4h(k) = head_grid%c4h(k)
      c3f(k) = head_grid%c3f(k)
      c4f(k) = head_grid%c4f(k)
      enddo
      call vert_cor(head_grid,znw_c,k_dim_c)
      WRITE ( message , * ) 'CFA' ,cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c,znw_c(1),znu_c(1)
      CALL       wrf_debug (  99,message )
      WRITE ( message , * ) 'CFV' ,head_grid%cf1,head_grid%cf2,head_grid%cf3,head_grid%cfn,head_grid%cfn1,&
      head_grid%znw(1),head_grid%znu(1) , head_grid%e_vert , parent_grid%cf1 , parent_grid%znw(1) , parent_grid%znu(1)
      CALL       wrf_debug (  99,message )

      CALL wrf_debug          ( 100 , 'wrf: back from input_history' )

      

      CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' , map_proj , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'DX'  , dx  , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'DY'  , dy  , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LAT' , cen_lat , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'CEN_LON' , cen_lon , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT1' , truelat1 , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'TRUELAT2' , truelat2 , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , icnt , ierr )
      CALL wrf_get_dom_ti_real    ( fid , 'STAND_LON' , stand_lon , 1 , icnt , ierr )



      CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' , iswater , 1 , icnt , ierr )

      
      
      

      IF ( time_loop .EQ. 1 ) THEN

         CALL       wrf_message ( program_name )
         CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain fine ' )
         CALL alloc_and_configure_domain ( domain_id  = 2 ,                  &
                                           grid       = nested_grid ,        &
                                           parent     = parent_grid ,        &
                                           kid        = 1                   )

         CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
         CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
         CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
         CALL set_scalar_indices_from_config ( nested_grid%id , idum1, idum2 )

         

         CALL Setup_Timekeeping ( nested_grid )
         
         CALL domain_clock_get( parent_grid, stop_timestr=stopTimeStr )
         CALL domain_clock_set( nested_grid,                        &
                                current_timestr=current_date(1:19), &
                                stop_timestr=stopTimeStr ,          &
                                time_step_seconds=                  &
                                  model_config_rec%interval_seconds )

         

         CALL nl_set_bdyfrq ( nested_grid%id , new_bdy_frq )
         config_flags%bdyfrq = new_bdy_frq


         

         CALL init_domain_constants_em_ptr ( parent_grid , nested_grid )



         CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfinput' )
         CALL construct_filename1( outname , 'wrfinput' , nested_grid%id , 2 )
         CALL open_w_dataset     ( fido, TRIM(outname) , nested_grid , config_flags , output_input , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(outname),' for reading ierr=',ierr
            CALL wrf_error_fatal3("<stdin>",452,&
wrf_err_message )
         ENDIF

         

         ids = nested_grid%sd31
         ide = nested_grid%ed31
         kds = nested_grid%sd32
         kde = nested_grid%ed32
         jds = nested_grid%sd33
         jde = nested_grid%ed33

         ims = nested_grid%sm31
         ime = nested_grid%em31
         kms = nested_grid%sm32
         kme = nested_grid%em32
         jms = nested_grid%sm33
         jme = nested_grid%em33

         ips = nested_grid%sp31
         ipe = nested_grid%ep31
         kps = nested_grid%sp32
         kpe = nested_grid%ep32
         jps = nested_grid%sp33
         jpe = nested_grid%ep33






         spec_bdy_width = model_config_rec%spec_bdy_width

         
         

         ALLOCATE ( ubdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( mbdy2dtemp1(ims:ime,1:1,    jms:jme) )
         ALLOCATE ( ubdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( vbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( tbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( pbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( qbdy3dtemp1_coupled(ims:ime,kms:kme,jms:jme,1:num_moist) )
         ALLOCATE ( qbdy3dtemp2_coupled(ims:ime,kms:kme,jms:jme,1:num_moist) )
         ALLOCATE ( mbdy2dtemp2(ims:ime,1:1,    jms:jme) )
         ALLOCATE ( cbdy3dtemp0(ims:ime,kms:kme,jms:jme,1:num_chem) )
         ALLOCATE ( cbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( cbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( sbdy3dtemp1(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( sbdy3dtemp2(ims:ime,kms:kme,jms:jme) )
         ALLOCATE ( sbdy3dtemp1_coupled(ims:ime,kms:kme,jms:jme,1:num_scalar) )
         ALLOCATE ( sbdy3dtemp2_coupled(ims:ime,kms:kme,jms:jme,1:num_scalar) )


      END IF

      CALL domain_clock_set( nested_grid,                        &
                             current_timestr=current_date(1:19), &
                             time_step_seconds=                  &
                               model_config_rec%interval_seconds )

      

      nested_grid%imask_nostag = 1
      nested_grid%imask_xstag = 1
      nested_grid%imask_ystag = 1
      nested_grid%imask_xystag = 1

      CALL med_interp_domain ( head_grid , nested_grid )
      WRITE ( message , * ) 'MOUSTA_L', nested_grid%mu_2(ips,jps) , nested_grid%u_2(ips,kde-2,jps)
      CALL       wrf_debug (  99,message )
      CALL vertical_interp (nested_grid,znw_c,znu_c,cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c,k_dim_c,c3h,c4h,c3f,c4f)
      WRITE ( message , * ) 'MOUSTA_V', nested_grid%mu_2(ips,jps) , nested_grid%u_2(ips,kde-2,jps)
      CALL       wrf_debug (  99,message )
      nested_grid%ht_int = nested_grid%ht



      IF ( time_loop .EQ. 1 ) THEN

         

         CALL get_ijk_from_grid (  nested_grid ,                   &
                                   nids, nide, njds, njde, nkds, nkde,    &
                                   nims, nime, njms, njme, nkms, nkme,    &
                                   nips, nipe, njps, njpe, nkps, nkpe    )

         

         CALL  copy_3d_field ( nested_grid%ht_fine , nested_grid%ht , &
                               nids , nide , njds , njde , 1   , 1   , &
                               nims , nime , njms , njme , 1   , 1   , &
                               nips , nipe , njps , njpe , 1   , 1   )

         

         CALL construct_filename1( si_inpname , 'wrfndi' , nested_grid%id , 2 )
         CALL       wrf_debug ( 100 , 'med_sidata_input: calling open_r_dataset for ' // TRIM(si_inpname) )
         CALL open_r_dataset ( idsi, TRIM(si_inpname) , nested_grid , config_flags , "DATASET=INPUT", ierr )
         IF ( ierr .NE. 0 ) THEN
            CALL wrf_error_fatal3("<stdin>",558,&
'real: error opening FG input for reading: ' // TRIM (si_inpname) )
         END IF

         

         CALL       wrf_debug ( 100 , 'ndown_em: calling input_auxinput2' )
         CALL input_auxinput2 ( idsi , nested_grid , config_flags , ierr )
         nested_grid%ht_input = nested_grid%ht

         

         CALL       wrf_debug ( 100 , 'ndown_em: closing fine grid static input' )
         CALL close_dataset ( idsi , config_flags , "DATASET=INPUT" )

         

         CALL blend_terrain ( nested_grid%ht_fine , nested_grid%ht , &
                               nids , nide , njds , njde , 1   , 1   , &
                               nims , nime , njms , njme , 1   , 1   , &
                               nips , nipe , njps , njpe , 1   , 1   )

         nested_grid%ht_input = nested_grid%ht
         nested_grid%ht_int   = nested_grid%ht_fine

         
         

         IF      ( ( nested_grid%ivgtyp(ips,jps) .GT. 0 ) .AND. &
                   ( nested_grid%isltyp(ips,jps) .GT. 0 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  nested_grid% vegcat(i,j) = nested_grid%ivgtyp(i,j)
                  nested_grid%soilcat(i,j) = nested_grid%isltyp(i,j)
               END DO
            END DO

         ELSE IF ( ( nested_grid% vegcat(ips,jps) .GT. 0.5 ) .AND. &
                   ( nested_grid%soilcat(ips,jps) .GT. 0.5 ) ) THEN
            DO j = jps, MIN(jde-1,jpe)
               DO i = ips, MIN(ide-1,ipe)
                  nested_grid%ivgtyp(i,j) = NINT(nested_grid% vegcat(i,j))
                  nested_grid%isltyp(i,j) = NINT(nested_grid%soilcat(i,j))
               END DO
            END DO

         ELSE
            num_veg_cat      = SIZE ( nested_grid%landusef , DIM=2 )
            num_soil_top_cat = SIZE ( nested_grid%soilctop , DIM=2 )
            num_soil_bot_cat = SIZE ( nested_grid%soilcbot , DIM=2 )

            CALL land_percentages (  nested_grid%xland , &
                                     nested_grid%landusef , nested_grid%soilctop , nested_grid%soilcbot , &
                                     nested_grid%isltyp , nested_grid%ivgtyp , &
                                     num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     ips , ipe , jps , jpe , kps , kpe , &
                                     model_config_rec%iswater(nested_grid%id) )

          END IF

          DO j = jps, MIN(jde-1,jpe)
            DO i = ips, MIN(ide-1,ipe)
               nested_grid%lu_index(i,j) = nested_grid%ivgtyp(i,j)
            END DO
         END DO

         CALL check_consistency ( nested_grid%ivgtyp , nested_grid%isltyp , nested_grid%landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(nested_grid%id) )

         CALL check_consistency2( nested_grid%ivgtyp , nested_grid%isltyp , nested_grid%landmask , &
                                  nested_grid%tmn , nested_grid%tsk , nested_grid%sst , nested_grid%xland , &
                                  nested_grid%tslb , nested_grid%smois , nested_grid%sh2o , &
                                  config_flags%num_soil_layers , nested_grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  ips , ipe , jps , jpe , kps , kpe , &
                                  model_config_rec%iswater(nested_grid%id) )

      END IF



      
      

      nested_grid%ht_fine = nested_grid%ht_input
      nested_grid%ht      = nested_grid%ht_int

      
      
      

      CALL rebalance_driver ( nested_grid )

      
      
      
      

      IF ( time_loop .EQ. 1 ) THEN

         


         CALL domain_clock_set( nested_grid, &
                                current_timestr=current_date(1:19) )

         

         CALL output_input ( fido , nested_grid , config_flags , ierr )

         CALL wrf_put_dom_ti_integer ( fido , 'MAP_PROJ' , map_proj , 1 , ierr )


         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LAT' , cen_lat , 1 , ierr )
         CALL wrf_put_dom_ti_real    ( fido , 'CEN_LON' , cen_lon , 1 , ierr )
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT1' , truelat1 , 1 , ierr )
         CALL wrf_put_dom_ti_real    ( fido , 'TRUELAT2' , truelat2 , 1 , ierr )
         CALL wrf_put_dom_ti_real    ( fido , 'MOAD_CEN_LAT' , moad_cen_lat , 1 , ierr )
         CALL wrf_put_dom_ti_real    ( fido , 'STAND_LON' , stand_lon , 1 , ierr )
         CALL wrf_put_dom_ti_integer ( fido , 'ISWATER' , iswater , 1 , ierr )

         
         
         
         
         

         CALL geth_julgmt ( julyr , julday , gmt)
         CALL nl_set_julyr  ( nested_grid%id , julyr  )
         CALL nl_set_julday ( nested_grid%id , julday )
         CALL nl_set_gmt    ( nested_grid%id , gmt    )
         CALL wrf_put_dom_ti_real    ( fido , 'GMT' , gmt , 1 , ierr )
         CALL wrf_put_dom_ti_integer ( fido , 'JULYR' , julyr , 1 , ierr )
         CALL wrf_put_dom_ti_integer ( fido , 'JULDAY' , julday , 1 , ierr )








         CALL close_dataset      ( fido , config_flags , "DATASET=INPUT" )

         
         

         
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , ubdy3dtemp1 , nested_grid%u_2                 , &
                       'u' , nested_grid%msfuy , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , vbdy3dtemp1 , nested_grid%v_2                 , &
                       'v' , nested_grid%msfvx , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , tbdy3dtemp1 , nested_grid%t_2                 , &
                       't' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , pbdy3dtemp1 , nested_grid%ph_2                , &
                       'h' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , qbdy3dtemp1 , nested_grid%moist(ims:ime,kms:kme,jms:jme,nvmoist)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           qbdy3dtemp1_coupled(:,:,:,nvmoist) =  qbdy3dtemp1
         END DO
         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , sbdy3dtemp1 , nested_grid%scalar(ims:ime,kms:kme,jms:jme,nvscalar)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           sbdy3dtemp1_coupled(:,:,:,nvscalar) =  sbdy3dtemp1
         END DO

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp1(i,1,j) = nested_grid%mu_2(i,j)
             END DO
          END DO

         
         

         CALL stuff_bdy     ( ubdy3dtemp1 , nested_grid%u_bxs, nested_grid%u_bxe,                        &
                                            nested_grid%u_bys, nested_grid%u_bye,                        &
                                                                     'U' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , nested_grid%v_bxs, nested_grid%v_bxe,                        &
                                            nested_grid%v_bys, nested_grid%v_bye,                        &
                                                                     'V' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , nested_grid%t_bxs, nested_grid%t_bxe,                        &
                                            nested_grid%t_bys, nested_grid%t_bye,                        &
                                                                     'T' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , nested_grid%ph_bxs, nested_grid%ph_bxe,                      &
                                            nested_grid%ph_bys, nested_grid%ph_bye,                      &
                                                                     'W' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           qbdy3dtemp1 = qbdy3dtemp1_coupled(:,:,:,nvmoist)
           CALL stuff_bdy     ( qbdy3dtemp1 , nested_grid%moist_bxs(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_bxe(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_bys(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_bye(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                                                    'T' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         END DO

         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           sbdy3dtemp1 = sbdy3dtemp1_coupled(:,:,:,nvscalar)
           CALL stuff_bdy     ( sbdy3dtemp1 , nested_grid%scalar_bxs(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_bxe(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_bys(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_bye(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                                                     'T' ,                 spec_bdy_width      , &
                                                                             ids , ide , jds , jde , kds , kde , &
                                                                             ims , ime , jms , jme , kms , kme , &
                                                                             ips , ipe , jps , jpe , kps , kpe )
         END DO

         CALL stuff_bdy     ( mbdy2dtemp1 , nested_grid%mu_bxs, nested_grid%mu_bxe,                      &
                                            nested_grid%mu_bys, nested_grid%mu_bye,                      &
                                                                     'M' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )
      ELSE IF ( ( time_loop .GT. 1 ) .AND. ( time_loop .LT. time_loop_max ) ) THEN

         
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , ubdy3dtemp2 , nested_grid%u_2                 , &
                       'u' , nested_grid%msfuy , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , vbdy3dtemp2 , nested_grid%v_2                 , &
                       'v' , nested_grid%msfvx , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , tbdy3dtemp2 , nested_grid%t_2                 , &
                       't' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , pbdy3dtemp2 , nested_grid%ph_2                , &
                       'h' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , qbdy3dtemp2 , nested_grid%moist(ims:ime,kms:kme,jms:jme,nvmoist)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           qbdy3dtemp2_coupled(:,:,:,nvmoist) =  qbdy3dtemp2
         END DO
         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , sbdy3dtemp2 , nested_grid%scalar(ims:ime,kms:kme,jms:jme,nvscalar)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           sbdy3dtemp2_coupled(:,:,:,nvscalar) =  sbdy3dtemp2
         END DO

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp2(i,1,j) = nested_grid%mu_2(i,j)
             END DO
          END DO

         
         
         

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq ,                               &
                                            nested_grid%u_btxs, nested_grid%u_btxe   ,          &
                                            nested_grid%u_btys, nested_grid%u_btye   ,          &
                                                                  'U'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq ,                               &
                                            nested_grid%v_btxs, nested_grid%v_btxe   ,          &
                                            nested_grid%v_btys, nested_grid%v_btye   ,          &
                                                                  'V'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq ,                               &
                                            nested_grid%t_btxs, nested_grid%t_btxe   ,          &
                                            nested_grid%t_btys, nested_grid%t_btye   ,          &
                                                                  'T'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq ,                               &
                                            nested_grid%ph_btxs, nested_grid%ph_btxe   ,        &
                                            nested_grid%ph_btys, nested_grid%ph_btye   ,        &
                                                                  'W' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           qbdy3dtemp1 = qbdy3dtemp1_coupled(:,:,:,nvmoist)
           qbdy3dtemp2 = qbdy3dtemp2_coupled(:,:,:,nvmoist)
           CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq ,                               &
                                              nested_grid%moist_btxs(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_btxe(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_btys(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                              nested_grid%moist_btye(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                                                  'T' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         END DO

         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           sbdy3dtemp1 = sbdy3dtemp1_coupled(:,:,:,nvscalar)
           sbdy3dtemp2 = sbdy3dtemp2_coupled(:,:,:,nvscalar)
           CALL stuff_bdytend ( sbdy3dtemp2 , sbdy3dtemp1 , new_bdy_frq ,  &
                                              nested_grid%scalar_btxs(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_btxe(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_btys(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                              nested_grid%scalar_btye(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                                                 'T' , &
                                                                                spec_bdy_width,       &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         END DO

         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq ,                               &
                                            nested_grid%mu_btxs, nested_grid%mu_btxe   ,        &
                                            nested_grid%mu_btys, nested_grid%mu_btye   ,        &
                                                                  'M' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )
         IF ( time_loop .EQ. 2 ) THEN

            

            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL wrf_error_fatal3("<stdin>",930,&
wrf_err_message )
            ENDIF

         END IF

         

      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL nl_set_julyr  ( nested_grid%id , julyr  )
      CALL nl_set_julday ( nested_grid%id , julday )
      CALL nl_set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr )
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
print *,'bdy time = ',time_loop-1,'  bdy date = ',current_date,' ',start_date
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr )
         END IF

         
         
         

          DO j = jps , jpe
             DO k = kps , kpe
                DO i = ips , ipe
                   ubdy3dtemp1(i,k,j) = ubdy3dtemp2(i,k,j)
                   vbdy3dtemp1(i,k,j) = vbdy3dtemp2(i,k,j)
                   tbdy3dtemp1(i,k,j) = tbdy3dtemp2(i,k,j)
                   pbdy3dtemp1(i,k,j) = pbdy3dtemp2(i,k,j)
                   qbdy3dtemp1_coupled(i,k,j,:) = qbdy3dtemp2_coupled(i,k,j,:)
                   sbdy3dtemp1_coupled(i,k,j,:) = sbdy3dtemp2_coupled(i,k,j,:)
                END DO
             END DO
          END DO

          DO j = jps , jpe
             DO i = ips , ipe
                mbdy2dtemp1(i,1,j) = mbdy2dtemp2(i,1,j)
             END DO
          END DO

         
         

         CALL stuff_bdy     ( ubdy3dtemp1 , &
                              nested_grid%u_bxs, nested_grid%u_bxe     ,                   &
                              nested_grid%u_bys, nested_grid%u_bye     ,                   &
                                                       'U' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( vbdy3dtemp1 , &
                              nested_grid%v_bxs, nested_grid%v_bxe     ,                   &
                              nested_grid%v_bys, nested_grid%v_bye     ,                   &
                                                       'V' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( tbdy3dtemp1 , &
                              nested_grid%t_bxs, nested_grid%t_bxe     ,                   &
                              nested_grid%t_bys, nested_grid%t_bye     ,                   &
                                                       'T' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdy     ( pbdy3dtemp1 , &
                              nested_grid%ph_bxs, nested_grid%ph_bxe     ,                   &
                              nested_grid%ph_bys, nested_grid%ph_bye     ,                   &
                                                       'W' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           qbdy3dtemp1 = qbdy3dtemp1_coupled(:,:,:,nvmoist)
           CALL stuff_bdy     ( qbdy3dtemp1 , &
                                nested_grid%moist_bxs(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                nested_grid%moist_bxe(jms:jme,kms:kme,1:spec_bdy_width,nvmoist), &
                                nested_grid%moist_bys(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                nested_grid%moist_bye(ims:ime,kms:kme,1:spec_bdy_width,nvmoist), &
                                                       'T' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         END DO

         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           sbdy3dtemp1 = sbdy3dtemp1_coupled(:,:,:,nvscalar)
           CALL stuff_bdy     ( sbdy3dtemp1 , &
                                nested_grid%scalar_bxs(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                nested_grid%scalar_bxe(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                                nested_grid%scalar_bys(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                nested_grid%scalar_bye(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                                      'T' ,                  spec_bdy_width,   &
                                                                           ids , ide , jds , jde , kds , kde , &
                                                                           ims , ime , jms , jme , kms , kme , &
                                                                           ips , ipe , jps , jpe , kps , kpe )
         END DO


         CALL stuff_bdy     ( mbdy2dtemp1 , &
                              nested_grid%mu_bxs, nested_grid%mu_bxe    ,  &
                              nested_grid%mu_bys, nested_grid%mu_bye    ,  &
                                                                     'M' ,               spec_bdy_width      , &
                                                                           ids , ide , jds , jde , 1 , 1 , &
                                                                           ims , ime , jms , jme , 1 , 1 , &
                                                                           ips , ipe , jps , jpe , 1 , 1 )

      ELSE IF ( time_loop .EQ. time_loop_max ) THEN

         
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , ubdy3dtemp2 , nested_grid%u_2                 , &
                       'u' , nested_grid%msfuy , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , vbdy3dtemp2 , nested_grid%v_2                 , &
                       'v' , nested_grid%msfvx , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , tbdy3dtemp2 , nested_grid%t_2                 , &
                       't' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         CALL couple ( nested_grid%mu_2 , nested_grid%mub , pbdy3dtemp2 , nested_grid%ph_2                , &
                       'h' , nested_grid%msfty , &
                       nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                       ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , qbdy3dtemp2 , nested_grid%moist(ims:ime,kms:kme,jms:jme,nvmoist)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           qbdy3dtemp2_coupled(:,:,:,nvmoist) = qbdy3dtemp2
         END DO
         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           CALL couple ( nested_grid%mu_2 , nested_grid%mub , sbdy3dtemp2 , nested_grid%scalar(ims:ime,kms:kme,jms:jme,nvscalar)    , &
                         't' , nested_grid%msfty , &
                         nested_grid%c1h, nested_grid%c2h, nested_grid%c1f, nested_grid%c2f, &
                         ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe )
           sbdy3dtemp2_coupled(:,:,:,nvscalar) = sbdy3dtemp2
         END DO
         mbdy2dtemp2(:,1,:) = nested_grid%mu_2(:,:)

         
         
         

         CALL stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , new_bdy_frq , &
                              nested_grid%u_btxs  , nested_grid%u_btxe , &
                              nested_grid%u_btys  , nested_grid%u_btye , &
                                                             'U'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , new_bdy_frq , &
                              nested_grid%v_btxs  , nested_grid%v_btxe , &
                              nested_grid%v_btys  , nested_grid%v_btye , &
                                                             'V'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , new_bdy_frq , &
                              nested_grid%t_btxs  , nested_grid%t_btxe , &
                              nested_grid%t_btys  , nested_grid%t_btye , &
                                                             'T'  , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         CALL stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , new_bdy_frq , &
                              nested_grid%ph_btxs  , nested_grid%ph_btxe , &
                              nested_grid%ph_btys  , nested_grid%ph_btye , &
                                                             'W' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         DO nvmoist=PARAM_FIRST_SCALAR, num_moist
           qbdy3dtemp1 = qbdy3dtemp1_coupled(:,:,:,nvmoist)
           qbdy3dtemp2 = qbdy3dtemp2_coupled(:,:,:,nvmoist)
           CALL stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , new_bdy_frq , &
                                nested_grid%moist_btxs(jms:jme,kms:kme,1:spec_bdy_width,nvmoist) , &
                                nested_grid%moist_btxe(jms:jme,kms:kme,1:spec_bdy_width,nvmoist) , &
                                nested_grid%moist_btys(ims:ime,kms:kme,1:spec_bdy_width,nvmoist) , &
                                nested_grid%moist_btye(ims:ime,kms:kme,1:spec_bdy_width,nvmoist) , &
                                                             'T' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         END DO

         DO nvscalar=PARAM_FIRST_SCALAR, num_scalar
           sbdy3dtemp1 = sbdy3dtemp1_coupled(:,:,:,nvscalar)
           sbdy3dtemp2 = sbdy3dtemp2_coupled(:,:,:,nvscalar)
           CALL stuff_bdytend ( sbdy3dtemp2 , sbdy3dtemp1 , new_bdy_frq ,  &
                              nested_grid%scalar_btxs(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                              nested_grid%scalar_btxe(jms:jme,kms:kme,1:spec_bdy_width,nvscalar), &
                              nested_grid%scalar_btys(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                              nested_grid%scalar_btye(ims:ime,kms:kme,1:spec_bdy_width,nvscalar), &
                                                                  'T' , &
                                                                                spec_bdy_width ,  &
                                                                  ids , ide , jds , jde , kds , kde , &
                                                                  ims , ime , jms , jme , kms , kme , &
                                                                  ips , ipe , jps , jpe , kps , kpe )
         END DO

         CALL stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , new_bdy_frq , &
                              nested_grid%mu_btxs  , nested_grid%mu_btxe , &
                              nested_grid%mu_btys  , nested_grid%mu_btye , &
                                                             'M' , &
                                                                                spec_bdy_width      , &
                                                                  ids , ide , jds , jde , 1 , 1 , &
                                                                  ims , ime , jms , jme , 1 , 1 , &
                                                                  ips , ipe , jps , jpe , 1 , 1 )

         IF ( time_loop .EQ. 2 ) THEN

            

            CALL wrf_debug          ( 100 , 'ndown_em main: calling open_w_dataset for wrfbdy' )
            CALL construct_filename1( bdyname , 'wrfbdy' , nested_grid%id , 2 )
            CALL open_w_dataset     ( fidb, TRIM(bdyname) , nested_grid , config_flags , output_boundary , &
                                      "DATASET=BOUNDARY", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program ndown: error opening ',TRIM(bdyname),' for reading ierr=',ierr
               CALL wrf_error_fatal3("<stdin>",1174,&
wrf_err_message )
            ENDIF

         END IF

         

      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      temp24= current_date
      temp24b=start_date_hold
      start_date = start_date_hold
      CALL geth_newdate ( temp19 , temp24b(1:19) , (time_loop-2) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
      CALL geth_julgmt ( julyr , julday , gmt)
      CALL nl_set_julyr  ( nested_grid%id , julyr  )
      CALL nl_set_julday ( nested_grid%id , julday )
      CALL nl_set_gmt    ( nested_grid%id , gmt    )
      CALL wrf_put_dom_ti_real    ( fidb , 'GMT' , gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fidb , 'JULYR' , julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fidb , 'JULDAY' , julday , 1 , ierr )
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )
      CALL output_boundary ( fidb , nested_grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b
      CALL domain_clock_set( nested_grid, &
                             current_timestr=current_date(1:19) )

         IF ( time_loop .EQ. 2 ) THEN
            CALL wrf_put_dom_ti_real    ( fidb , 'BDYFRQ' , new_bdy_frq , 1 , ierr )
         END IF

         

         CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
         CALL close_dataset ( fidb , config_flags , "DATASET=BOUNDARY" )


      END IF

      

   END DO big_time_loop_thingy
   DEALLOCATE(znw_c)
   DEALLOCATE(znu_c)
   CALL model_to_grid_config_rec ( parent_grid%id , model_config_rec , config_flags )
   CALL med_shutdown_io ( parent_grid , config_flags )

   CALL wrf_debug ( 0 , 'ndown_em: SUCCESS COMPLETE NDOWN_EM INIT' )

   CALL wrf_shutdown

   CALL WRFU_Finalize( rc=rc )

END PROGRAM ndown_em

SUBROUTINE land_percentages ( xland , &
                              landuse_frac , soil_top_cat , soil_bot_cat , &
                              isltyp , ivgtyp , &
                              num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater )
   USE module_soil_pre

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater

   INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
   REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
   REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
   REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

   CALL process_percent_cat_new ( xland , &
                                  landuse_frac , soil_top_cat , soil_bot_cat , &
                                  isltyp , ivgtyp , &
                                  num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

END SUBROUTINE land_percentages

SUBROUTINE check_consistency ( ivgtyp , isltyp , landmask , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  iswater )

   IMPLICIT NONE

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte , &
                           iswater
   INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isltyp , ivgtyp
   REAL    , DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: landmask

   LOGICAL :: oops
   INTEGER :: oops_count , i , j

   oops = .FALSE.
   oops_count = 0

   DO j = jts, MIN(jde-1,jte)
      DO i = its, MIN(ide-1,ite)
         IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater ) ) .OR. &
              ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater ) ) ) THEN
            print *,'mismatch in landmask and veg type'
            print *,'i,j=',i,j, '  landmask =',NINT(landmask(i,j)),'  ivgtyp=',ivgtyp(i,j)
            oops = .TRUE.
            oops_count = oops_count + 1
landmask(i,j) = 0
ivgtyp(i,j)=16
isltyp(i,j)=14
         END IF
      END DO
   END DO

   IF ( oops ) THEN
      CALL wrf_debug( 0, 'mismatch in check_consistency, turned to water points, be careful' )
   END IF

END SUBROUTINE check_consistency

SUBROUTINE check_consistency2( ivgtyp , isltyp , landmask , &
                               tmn , tsk , sst , xland , &
                               tslb , smois , sh2o , &
                               num_soil_layers , id , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte , &
                               iswater )

   USE module_configure
   USE module_optional_input

   INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte
   INTEGER , INTENT(IN) :: num_soil_layers , id

   INTEGER , DIMENSION(ims:ime,jms:jme) :: ivgtyp , isltyp
   REAL    , DIMENSION(ims:ime,jms:jme) :: landmask , tmn , tsk , sst , xland
   REAL    , DIMENSION(ims:ime,num_soil_layers,jms:jme) :: tslb , smois , sh2o

   INTEGER :: oops1 , oops2
   INTEGER :: i , j , k

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(id) )

         CASE ( SLABSCHEME , SUEWSSCHEME , LSMSCHEME , RUCLSMSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( ( landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                     tmn(i,j) = sst(i,j)
                     tsk(i,j) = sst(i,j)
                  ELSE IF ( landmask(i,j) .LT. 0.5 ) THEN
                     tmn(i,j) = tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( tsk(i,j) .LT. 170 .or. tsk(i,j) .GT. 400. ) THEN
               print *,'error in the TSK'
               print *,'i,j=',i,j
               print *,'landmask=',landmask(i,j)
               print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                  tsk(i,j)=tmn(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tsk(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3("<stdin>",1362,&
'TSK unreasonable' )
               end if
            END IF
         END DO
      END DO

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tmn(i,j) .LT. 170. ) .OR. ( tmn(i,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TMN'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
               if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                  tmn(i,j)=tsk(i,j)
               else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                  tmn(i,j)=sst(i,j)
               else
                  CALL wrf_error_fatal3("<stdin>",1383,&
'TMN unreasonable' )
               endif
            END IF
         END DO
      END DO

      

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( tslb(i,1,j) .LT. 170. ) .OR. ( tslb(i,1,j) .GT. 400. ) ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
                  print *,'error in the TSLB'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'tsk, sst, tmn=',tsk(i,j),sst(i,j),tmn(i,j)
                  print *,'tslb = ',tslb(i,:,j)
                  print *,'old smois = ',smois(i,:,j)
                  DO l = 1 , num_soil_layers
                     sh2o(i,l,j) = 0.0
                  END DO
                  DO l = 1 , num_soil_layers
                     smois(i,l,j) = 0.3
                  END DO
                  if(tsk(i,j).gt.170. .and. tsk(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tsk(i,j)
                     END DO
                  else if(sst(i,j).gt.170. .and. sst(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=sst(i,j)
                     END DO
                  else if(tmn(i,j).gt.170. .and. tmn(i,j).lt.400.)then
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)=tmn(i,j)
                     END DO
                  else
                     CALL wrf_error_fatal3("<stdin>",1420,&
'TSLB unreasonable' )
                  endif
            END IF
         END DO
      END DO

      

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( ( ( landmask(i,j) .LT. 0.5 ) .AND. ( ivgtyp(i,j) .NE. iswater .OR. isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( landmask(i,j) .GT. 0.5 ) .AND. ( ivgtyp(i,j) .EQ. iswater .OR. isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  ivgtyp(i,j) = 5
                  isltyp(i,j) = 8
                  landmask(i,j) = 1
                  xland(i,j) = 1
               ELSE IF ( sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  ivgtyp(i,j) = iswater
                  isltyp(i,j) = 14
                  landmask(i,j) = 0
                  xland(i,j) = 2
               ELSE
                  print *,'the landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'landmask=',landmask(i,j)
                  print *,'ivgtyp=',ivgtyp(i,j)
                  print *,'isltyp=',isltyp(i,j)
                  print *,'iswater=', iswater
                  print *,'tslb=',tslb(i,:,j)
                  print *,'sst=',sst(i,j)
                  CALL wrf_error_fatal3("<stdin>",1456,&
'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif

END SUBROUTINE check_consistency2

      SUBROUTINE vert_cor(parent,znw_c,k_dim_c)
         USE module_domain
   IMPLICIT NONE
         TYPE(domain), POINTER :: parent
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c

       integer :: kde_c , kde_n ,n_refine,ii,kkk,k
       real :: dznw_m,cof1,cof2

        kde_c = k_dim_c
        kde_n = parent%e_vert
        n_refine = parent % vert_refine_fact


         kkk = 0
         do k = 1 , kde_c-1
         dznw_m = znw_c(k+1) - znw_c(k)
         do ii = 1,n_refine
         kkk = kkk + 1
         parent%znw(kkk) = znw_c(k) + float(ii-1)/float(n_refine)*dznw_m
         enddo
         enddo
         parent%znw(kde_n) = znw_c(kde_c)
         parent%znw(1) = znw_c(1)

      DO k=1, kde_n-1
         parent%dnw(k) = parent%znw(k+1) - parent%znw(k)
         parent%rdnw(k) = 1./parent%dnw(k)
         parent%znu(k) = 0.5*(parent%znw(k+1)+parent%znw(k))
      END DO

      DO k=2, kde_n-1
         parent%dn(k) = 0.5*(parent%dnw(k)+parent%dnw(k-1))
         parent%rdn(k) = 1./parent%dn(k)
         parent%fnp(k) = .5* parent%dnw(k  )/parent%dn(k)
         parent%fnm(k) = .5* parent%dnw(k-1)/parent%dn(k)
      END DO

  cof1 = (2.*parent%dn(2)+parent%dn(3))/(parent%dn(2)+parent%dn(3))*parent%dnw(1)/parent%dn(2)
  cof2 =     parent%dn(2)        /(parent%dn(2)+parent%dn(3))*parent%dnw(1)/parent%dn(3)

      parent%cf1  = parent%fnp(2) + cof1
      parent%cf2  = parent%fnm(2) - cof1 - cof2
      parent%cf3  = cof2

      parent%cfn  = (.5*parent%dnw(kde_n-1)+parent%dn(kde_n-1))/parent%dn(kde_n-1)
      parent%cfn1 = -.5*parent%dnw(kde_n-1)/parent%dn(kde_n-1)



      END SUBROUTINE vert_cor


SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER  :: parent , nest
   INTERFACE
   SUBROUTINE init_domain_constants_em ( parent , nest )
      USE module_domain
      USE module_configure
      TYPE(domain)  :: parent , nest
   END SUBROUTINE init_domain_constants_em
   END INTERFACE
   CALL init_domain_constants_em ( parent , nest )
END SUBROUTINE init_domain_constants_em_ptr


     SUBROUTINE vertical_interp (parent_grid,znw_c,znu_c,cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c,k_dim_c,c3h,c4h,c3f,c4f)
         USE module_domain
         USE module_configure
   IMPLICIT NONE
         TYPE(domain), POINTER ::  parent_grid
         INTEGER , INTENT (IN) :: k_dim_c
         REAL , INTENT (IN) :: cf1_c,cf2_c,cf3_c,cfn_c,cfn1_c
         REAL , DIMENSION(k_dim_c) , INTENT (IN) ::  znw_c,znu_c
         REAL , DIMENSION(k_dim_c) , INTENT (IN) ::  c3h,c4h,c3f,c4f

       integer :: kde_c , kde_n ,n_refine,ii,kkk
       integer :: i , j, k , itrace
       real :: p_top_m  , p_surf_m , mu_m , hsca_m , pre_c ,pre_n

       real, allocatable, dimension(:) ::  alt_w_c , alt_u_c ,pro_w_c , pro_u_c
       real, allocatable, dimension(:) ::  alt_w_n , alt_u_n ,pro_w_n , pro_u_n

   INTEGER :: nids, nide, njds, njde, nkds, nkde, &
              nims, nime, njms, njme, nkms, nkme, &
              nips, nipe, njps, njpe, nkps, nkpe


         hsca_m = 6.7
         n_refine = model_config_rec % vert_refine_fact
         kde_c = k_dim_c
         kde_n = parent_grid%e_vert

         CALL get_ijk_from_grid ( parent_grid , &
                                   nids, nide, njds, njde, nkds, nkde, &
                                   nims, nime, njms, njme, nkms, nkme, &
                                   nips, nipe, njps, njpe, nkps, nkpe )








         allocate (alt_w_c(kde_c))
         allocate (alt_u_c(kde_c+1))
         allocate (pro_w_c(kde_c))
         allocate (pro_u_c(kde_c+1))

         allocate (alt_w_n(kde_n))
         allocate (alt_u_n(kde_n+1))
         allocate (pro_w_n(kde_n))
         allocate (pro_u_n(kde_n+1))


         p_top_m = parent_grid%p_top
         p_surf_m = 1.e5
         mu_m = p_surf_m - p_top_m


         do  k = 1,kde_c
         pre_c = mu_m * znw_c(k) + p_top_m
         alt_w_c(k) =  -hsca_m * alog(pre_c/p_surf_m)
         enddo
         do  k = 1,kde_c-1
         pre_c = mu_m * znu_c(k) + p_top_m
         alt_u_c(k+1) =  -hsca_m * alog(pre_c/p_surf_m)
         enddo
         alt_u_c(1) =  alt_w_c(1)
         alt_u_c(kde_c+1) =  alt_w_c(kde_c)

         do  k = 1,kde_n
         pre_n = mu_m * parent_grid%znw(k) + p_top_m
         alt_w_n(k) =  -hsca_m * alog(pre_n/p_surf_m)
         enddo
         do  k = 1,kde_n-1
         pre_n = mu_m * parent_grid%znu(k) + p_top_m
         alt_u_n(k+1) =  -hsca_m * alog(pre_n/p_surf_m)
         enddo
         alt_u_n(1) =  alt_w_n(1)
         alt_u_n(kde_n+1) =  alt_w_n(kde_n)


IF ( SIZE( parent_grid%u_2, 1 ) * SIZE( parent_grid%u_2, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%u_2(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%u_2(i,1,j) &
                     + cf2_c*parent_grid%u_2(i,2,j) &
                     + cf3_c*parent_grid%u_2(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%u_2(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%u_2(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%u_2(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

IF ( SIZE( parent_grid%v_2, 1 ) * SIZE( parent_grid%v_2, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%v_2(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%v_2(i,1,j) &
                     + cf2_c*parent_grid%v_2(i,2,j) &
                     + cf3_c*parent_grid%v_2(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%v_2(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%v_2(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%v_2(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

IF ( SIZE( parent_grid%w_2, 1 ) * SIZE( parent_grid%w_2, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c
    pro_w_c(k) = parent_grid%w_2(i,k,j)
    enddo
    call inter(pro_w_c,alt_w_c,kde_c,pro_w_n,alt_w_n,kde_n)

    do k = 1,kde_n
    parent_grid%w_2(i,k,j) = pro_w_n(k)
    enddo
enddo
enddo
ENDIF

IF ( SIZE( parent_grid%t_2, 1 ) * SIZE( parent_grid%t_2, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%t_2(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%t_2(i,1,j) &
                     + cf2_c*parent_grid%t_2(i,2,j) &
                     + cf3_c*parent_grid%t_2(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%t_2(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%t_2(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%t_2(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( parent_grid%moist, 1 ) * SIZE( parent_grid%moist, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%moist(i,k,j,itrace)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%moist(i,1,j,itrace) &
                     + cf2_c*parent_grid%moist(i,2,j,itrace) &
                     + cf3_c*parent_grid%moist(i,3,j,itrace)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%moist(i,kde_c-1,j,itrace) &
                     + cfn1_c*parent_grid%moist(i,kde_c-2,j,itrace)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%moist(i,k,j,itrace) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF
ENDDO

DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE( parent_grid%dfi_moist, 1 ) * SIZE( parent_grid%dfi_moist, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%dfi_moist(i,k,j,itrace)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%dfi_moist(i,1,j,itrace) &
                     + cf2_c*parent_grid%dfi_moist(i,2,j,itrace) &
                     + cf3_c*parent_grid%dfi_moist(i,3,j,itrace)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%dfi_moist(i,kde_c-1,j,itrace) &
                     + cfn1_c*parent_grid%dfi_moist(i,kde_c-2,j,itrace)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%dfi_moist(i,k,j,itrace) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF
ENDDO

DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( parent_grid%scalar, 1 ) * SIZE( parent_grid%scalar, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%scalar(i,k,j,itrace)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%scalar(i,1,j,itrace) &
                     + cf2_c*parent_grid%scalar(i,2,j,itrace) &
                     + cf3_c*parent_grid%scalar(i,3,j,itrace)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%scalar(i,kde_c-1,j,itrace) &
                     + cfn1_c*parent_grid%scalar(i,kde_c-2,j,itrace)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%scalar(i,k,j,itrace) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF
ENDDO

DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( parent_grid%dfi_scalar, 1 ) * SIZE( parent_grid%dfi_scalar, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%dfi_scalar(i,k,j,itrace)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%dfi_scalar(i,1,j,itrace) &
                     + cf2_c*parent_grid%dfi_scalar(i,2,j,itrace) &
                     + cf3_c*parent_grid%dfi_scalar(i,3,j,itrace)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%dfi_scalar(i,kde_c-1,j,itrace) &
                     + cfn1_c*parent_grid%dfi_scalar(i,kde_c-2,j,itrace)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%dfi_scalar(i,k,j,itrace) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF
ENDDO



IF ( SIZE( parent_grid%f_ice_phy, 1 ) * SIZE( parent_grid%f_ice_phy, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%f_ice_phy(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%f_ice_phy(i,1,j) &
                     + cf2_c*parent_grid%f_ice_phy(i,2,j) &
                     + cf3_c*parent_grid%f_ice_phy(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%f_ice_phy(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%f_ice_phy(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%f_ice_phy(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

IF ( SIZE( parent_grid%f_rain_phy, 1 ) * SIZE( parent_grid%f_rain_phy, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%f_rain_phy(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%f_rain_phy(i,1,j) &
                     + cf2_c*parent_grid%f_rain_phy(i,2,j) &
                     + cf3_c*parent_grid%f_rain_phy(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%f_rain_phy(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%f_rain_phy(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%f_rain_phy(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF


IF ( SIZE( parent_grid%f_rimef_phy, 1 ) * SIZE( parent_grid%f_rimef_phy, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%f_rimef_phy(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%f_rimef_phy(i,1,j) &
                     + cf2_c*parent_grid%f_rimef_phy(i,2,j) &
                     + cf3_c*parent_grid%f_rimef_phy(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%f_rimef_phy(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%f_rimef_phy(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%f_rimef_phy(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

IF ( SIZE( parent_grid%h_diabatic, 1 ) * SIZE( parent_grid%h_diabatic, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) = parent_grid%h_diabatic(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%h_diabatic(i,1,j) &
                     + cf2_c*parent_grid%h_diabatic(i,2,j) &
                     + cf3_c*parent_grid%h_diabatic(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%h_diabatic(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%h_diabatic(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%h_diabatic(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF

IF ( SIZE( parent_grid%rthraten, 1 ) * SIZE( parent_grid%rthraten, 3 ) .GT. 1 ) THEN
do j = njms , njme
do i = nims , nime

    do k = 1,kde_c-1
    pro_u_c(k+1) =  parent_grid%rthraten(i,k,j)
    enddo
    pro_u_c(1      ) = cf1_c*parent_grid%rthraten(i,1,j) &
                     + cf2_c*parent_grid%rthraten(i,2,j) &
                     + cf3_c*parent_grid%rthraten(i,3,j)

    pro_u_c(kde_c+1) = cfn_c *parent_grid%rthraten(i,kde_c-1,j) &
                     + cfn1_c*parent_grid%rthraten(i,kde_c-2,j)

    call inter(pro_u_c,alt_u_c,kde_c+1,pro_u_n,alt_u_n,kde_n+1)

    do k = 1,kde_n-1
    parent_grid%rthraten(i,k,j) = pro_u_n(k+1)
    enddo

enddo
enddo
ENDIF











         deallocate (alt_w_c)
         deallocate (alt_u_c)
         deallocate (pro_w_c)
         deallocate (pro_u_c)

         deallocate (alt_w_n)
         deallocate (alt_u_n)
         deallocate (pro_w_n)
         deallocate (pro_u_n)


      END SUBROUTINE vertical_interp


    SUBROUTINE inter(pro_c,alt_c,kde_c,pro_n,alt_n,kde_n)

  IMPLICIT NONE
   INTEGER , INTENT(IN) :: kde_c,kde_n
   REAL , DIMENSION(kde_c) , INTENT(IN ) :: pro_c,alt_c
   REAL , DIMENSION(kde_n) , INTENT(IN ) :: alt_n
   REAL , DIMENSION(kde_n) , INTENT(OUT) :: pro_n

      real ,dimension(kde_c) :: a,b,c,d
      real :: p
      integer :: i,j


      call coeff_mon(alt_c,pro_c,a,b,c,d,kde_c)

       do i = 1,kde_n-1

          do j=1,kde_c-1

               if ( (alt_n(i) .ge. alt_c(j)).and.(alt_n(i) .lt. alt_c(j+1)) ) then
                p =  alt_n(i)-alt_c(j)
                pro_n(i) = p*( p*(a(j)*p+b(j))+c(j)) + d(j)
                goto 20
               endif
           enddo
20             continue
       enddo

       pro_n(kde_n) = pro_c(kde_c)


   END SUBROUTINE inter



     subroutine  coeff_mon(x,y,a,b,c,d,n)

      implicit none

      integer :: n
      real ,dimension(n) :: x,y,a,b,c,d
      real ,dimension(n) :: h,s,p,yp

       integer :: i


      do i=1,n-1
      h(i) = (x(i+1)-x(i))
      s(i) = (y(i+1)-y(i)) / h(i)
      enddo

      do i=2,n-1
      p(i) = (s(i-1)*h(i)+s(i)*h(i-1)) / (h(i-1)+h(i))
      enddo

      p(1) = s(1)
      p(n) = s(n-1)

      do i=1,n
      yp(i) = p(i)
      enddo


      do i=2,n-1
      yp(i) = (sign(1.,s(i-1))+sign(1.,s(i)))* min( abs(s(i-1)),abs(s(i)),0.5*abs(p(i)))
      enddo

      do i = 1,n-1
      a(i) = (yp(i)+yp(i+1)-2.*s(i))/(h(i)*h(i))
      b(i) = (3.*s(i)-2.*yp(i)-yp(i+1))/h(i)
      c(i) = yp(i)
      d(i) = y(i)
      enddo

      end  subroutine  coeff_mon
