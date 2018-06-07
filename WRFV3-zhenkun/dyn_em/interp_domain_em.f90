




SUBROUTINE interp_domain_em_part1 ( grid, ngrid, config_flags   &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye &


                 )
         USE module_state_description
         USE module_domain, ONLY : domain, get_ijk_from_grid
         USE module_configure, ONLY : grid_config_rec_type
         TYPE(domain), POINTER :: grid , ngrid






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye


      INTEGER nlev
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,k
      TYPE (grid_config_rec_type)            :: config_flags
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      






IF ( SIZE( grid%xlat, 1 ) * SIZE( grid%xlat, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%xlat,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlat,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong, 1 ) * SIZE( grid%xlong, 2 ) .GT. 1 ) THEN 
CALL interp_fcn_blint_ll (  &         
                  grid%xlong,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlong,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%xlat,ngrid%xlat&
,grid%input_from_file,ngrid%input_from_file&
                  ) 
ENDIF
IF ( SIZE( grid%lu_index, 1 ) * SIZE( grid%lu_index, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm_lu (  &         
                  grid%lu_index,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lu_index,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%xlat,ngrid%xlat&
,grid%xlong,ngrid%xlong&
,grid%dx,ngrid%dx&
,grid%grid_id,ngrid%grid_id&
                  ) 
ENDIF
IF ( SIZE( grid%t_max_p, 1 ) * SIZE( grid%t_max_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%t_max_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t_max_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ght_max_p, 1 ) * SIZE( grid%ght_max_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ght_max_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ght_max_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%max_p, 1 ) * SIZE( grid%max_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%max_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%max_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_min_p, 1 ) * SIZE( grid%t_min_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%t_min_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t_min_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ght_min_p, 1 ) * SIZE( grid%ght_min_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ght_min_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ght_min_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%min_p, 1 ) * SIZE( grid%min_p, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%min_p,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%min_p,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%u_2, 1 ) * SIZE( grid%u_2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%u_2,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%u_2,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%v_2, 1 ) * SIZE( grid%v_2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%v_2,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%v_2,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%w_2, 1 ) * SIZE( grid%w_2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%w_2,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         
                  ngrid%w_2,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ph_2, 1 ) * SIZE( grid%ph_2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ph_2,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         
                  ngrid%ph_2,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%phb, 1 ) * SIZE( grid%phb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%phb,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,   &         
                  ngrid%phb,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_2, 1 ) * SIZE( grid%t_2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%t_2,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%t_2,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t_init, 1 ) * SIZE( grid%t_init, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%t_init,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%t_init,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%mu_2, 1 ) * SIZE( grid%mu_2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%mu_2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%mu_2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%mub, 1 ) * SIZE( grid%mub, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%mub,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%mub,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%alb, 1 ) * SIZE( grid%alb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%alb,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%alb,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pb, 1 ) * SIZE( grid%pb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pb,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%pb,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%q2, 1 ) * SIZE( grid%q2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%q2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%q2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%t2, 1 ) * SIZE( grid%t2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%t2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%th2, 1 ) * SIZE( grid%th2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%th2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%th2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%psfc, 1 ) * SIZE( grid%psfc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%psfc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%psfc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%u10, 1 ) * SIZE( grid%u10, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%u10,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%u10,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%v10, 1 ) * SIZE( grid%v10, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%v10,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%v10,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lpi, 1 ) * SIZE( grid%lpi, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lpi,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lpi,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( moist, 1 ) * SIZE( moist, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE( dfi_moist, 1 ) * SIZE( dfi_moist, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  dfi_moist(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%dfi_moist(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%qvold, 1 ) * SIZE( grid%qvold, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%qvold,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%qvold,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qnwfa2d, 1 ) * SIZE( grid%qnwfa2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%qnwfa2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%qnwfa2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%dfi_scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%toposlpx, 1 ) * SIZE( grid%toposlpx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%toposlpx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%toposlpx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%toposlpy, 1 ) * SIZE( grid%toposlpy, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%toposlpy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%toposlpy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%slope, 1 ) * SIZE( grid%slope, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%slope,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%slope,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%slp_azi, 1 ) * SIZE( grid%slp_azi, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%slp_azi,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%slp_azi,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%shdmax , 1 )*SIZE( grid%shdmax , 2 ) .GT. 1 ), & 
                  grid%shdmax,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%shdmax,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%shdmin , 1 )*SIZE( grid%shdmin , 2 ) .GT. 1 ), & 
                  grid%shdmin,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%shdmin,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%landusef, 1 ) * SIZE( grid%landusef, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%landusef,   &       
                 cids, cide, 1, config_flags%num_land_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_land_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_land_cat, cjps, cjpe,   &         
                  ngrid%landusef,  &   
                 nids, nide, 1, config_flags%num_land_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%num_land_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_land_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%soilctop, 1 ) * SIZE( grid%soilctop, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%soilctop,   &       
                 cids, cide, 1, config_flags%num_soil_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_cat, cjps, cjpe,   &         
                  ngrid%soilctop,  &   
                 nids, nide, 1, config_flags%num_soil_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%soilcbot, 1 ) * SIZE( grid%soilcbot, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%soilcbot,   &       
                 cids, cide, 1, config_flags%num_soil_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_cat, cjps, cjpe,   &         
                  ngrid%soilcbot,  &   
                 nids, nide, 1, config_flags%num_soil_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tslb , 1 )*SIZE( grid%tslb , 3 ) .GT. 1 ), & 
                  grid%tslb,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%tslb,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%smois , 1 )*SIZE( grid%smois , 3 ) .GT. 1 ), & 
                  grid%smois,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%smois,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sh2o , 1 )*SIZE( grid%sh2o , 3 ) .GT. 1 ), & 
                  grid%sh2o,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%sh2o,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%smcrel , 1 )*SIZE( grid%smcrel , 3 ) .GT. 1 ), & 
                  grid%smcrel,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%smcrel,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xice , 1 )*SIZE( grid%xice , 2 ) .GT. 1 ), & 
                  grid%xice,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xice,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%isice,ngrid%isice&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%icedepth , 1 )*SIZE( grid%icedepth , 2 ) .GT. 1 ), & 
                  grid%icedepth,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%icedepth,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%isice,ngrid%isice&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xicem , 1 )*SIZE( grid%xicem , 2 ) .GT. 1 ), & 
                  grid%xicem,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xicem,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%isice,ngrid%isice&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%albsi , 1 )*SIZE( grid%albsi , 2 ) .GT. 1 ), & 
                  grid%albsi,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%albsi,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%isice,ngrid%isice&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowsi , 1 )*SIZE( grid%snowsi , 2 ) .GT. 1 ), & 
                  grid%snowsi,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snowsi,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%isice,ngrid%isice&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%smstav , 1 )*SIZE( grid%smstav , 2 ) .GT. 1 ), & 
                  grid%smstav,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%smstav,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sfcrunoff , 1 )*SIZE( grid%sfcrunoff , 2 ) .GT. 1 ), & 
                  grid%sfcrunoff,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sfcrunoff,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%udrunoff , 1 )*SIZE( grid%udrunoff , 2 ) .GT. 1 ), & 
                  grid%udrunoff,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%udrunoff,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%ivgtyp, 1 ) * SIZE( grid%ivgtyp, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%ivgtyp,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ivgtyp,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_soil (  &         
  ( SIZE( grid%isltyp , 1 )*SIZE( grid%isltyp , 2 ) .GT. 1 ), & 
                  grid%isltyp,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%isltyp,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%vegfra , 1 )*SIZE( grid%vegfra , 2 ) .GT. 1 ), & 
                  grid%vegfra,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%vegfra,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%acgrdflx, 1 ) * SIZE( grid%acgrdflx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acgrdflx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acgrdflx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%acsnow , 1 )*SIZE( grid%acsnow , 2 ) .GT. 1 ), & 
                  grid%acsnow,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acsnow,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%acrunoff , 1 )*SIZE( grid%acrunoff , 2 ) .GT. 1 ), & 
                  grid%acrunoff,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acrunoff,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%acsnom , 1 )*SIZE( grid%acsnom , 2 ) .GT. 1 ), & 
                  grid%acsnom,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acsnom,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snow , 1 )*SIZE( grid%snow , 2 ) .GT. 1 ), & 
                  grid%snow,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snow,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowh , 1 )*SIZE( grid%snowh , 2 ) .GT. 1 ), & 
                  grid%snowh,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snowh,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%canwat , 1 )*SIZE( grid%canwat , 2 ) .GT. 1 ), & 
                  grid%canwat,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%canwat,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sstsk , 1 )*SIZE( grid%sstsk , 2 ) .GT. 1 ), & 
                  grid%sstsk,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sstsk,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%lake_depth , 1 )*SIZE( grid%lake_depth , 2 ) .GT. 1 ), & 
                  grid%lake_depth,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lake_depth,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%uoce , 1 )*SIZE( grid%uoce , 2 ) .GT. 1 ), & 
                  grid%uoce,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%uoce,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%voce , 1 )*SIZE( grid%voce , 2 ) .GT. 1 ), & 
                  grid%voce,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%voce,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tr_urb2d , 1 )*SIZE( grid%tr_urb2d , 2 ) .GT. 1 ), & 
                  grid%tr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tgr_urb2d , 1 )*SIZE( grid%tgr_urb2d , 2 ) .GT. 1 ), & 
                  grid%tgr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tgr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tb_urb2d , 1 )*SIZE( grid%tb_urb2d , 2 ) .GT. 1 ), & 
                  grid%tb_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tb_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tg_urb2d , 1 )*SIZE( grid%tg_urb2d , 2 ) .GT. 1 ), & 
                  grid%tg_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tg_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tc_urb2d , 1 )*SIZE( grid%tc_urb2d , 2 ) .GT. 1 ), & 
                  grid%tc_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tc_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%qc_urb2d , 1 )*SIZE( grid%qc_urb2d , 2 ) .GT. 1 ), & 
                  grid%qc_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%qc_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%uc_urb2d , 1 )*SIZE( grid%uc_urb2d , 2 ) .GT. 1 ), & 
                  grid%uc_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%uc_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xxxr_urb2d , 1 )*SIZE( grid%xxxr_urb2d , 2 ) .GT. 1 ), & 
                  grid%xxxr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xxxr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xxxb_urb2d , 1 )*SIZE( grid%xxxb_urb2d , 2 ) .GT. 1 ), & 
                  grid%xxxb_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xxxb_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xxxg_urb2d , 1 )*SIZE( grid%xxxg_urb2d , 2 ) .GT. 1 ), & 
                  grid%xxxg_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xxxg_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xxxc_urb2d , 1 )*SIZE( grid%xxxc_urb2d , 2 ) .GT. 1 ), & 
                  grid%xxxc_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xxxc_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%cmcr_urb2d , 1 )*SIZE( grid%cmcr_urb2d , 2 ) .GT. 1 ), & 
                  grid%cmcr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%cmcr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%drelr_urb2d , 1 )*SIZE( grid%drelr_urb2d , 2 ) .GT. 1 ), & 
                  grid%drelr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%drelr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%drelb_urb2d , 1 )*SIZE( grid%drelb_urb2d , 2 ) .GT. 1 ), & 
                  grid%drelb_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%drelb_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%drelg_urb2d , 1 )*SIZE( grid%drelg_urb2d , 2 ) .GT. 1 ), & 
                  grid%drelg_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%drelg_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%flxhumr_urb2d , 1 )*SIZE( grid%flxhumr_urb2d , 2 ) .GT. 1 ), & 
                  grid%flxhumr_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%flxhumr_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%flxhumb_urb2d , 1 )*SIZE( grid%flxhumb_urb2d , 2 ) .GT. 1 ), & 
                  grid%flxhumb_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%flxhumb_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%flxhumg_urb2d , 1 )*SIZE( grid%flxhumg_urb2d , 2 ) .GT. 1 ), & 
                  grid%flxhumg_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%flxhumg_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tgrl_urb3d , 1 )*SIZE( grid%tgrl_urb3d , 3 ) .GT. 1 ), & 
                  grid%tgrl_urb3d,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%tgrl_urb3d,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%smr_urb3d , 1 )*SIZE( grid%smr_urb3d , 3 ) .GT. 1 ), & 
                  grid%smr_urb3d,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%smr_urb3d,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%trl_urb3d , 1 )*SIZE( grid%trl_urb3d , 3 ) .GT. 1 ), & 
                  grid%trl_urb3d,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%trl_urb3d,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tbl_urb3d , 1 )*SIZE( grid%tbl_urb3d , 3 ) .GT. 1 ), & 
                  grid%tbl_urb3d,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%tbl_urb3d,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tgl_urb3d , 1 )*SIZE( grid%tgl_urb3d , 3 ) .GT. 1 ), & 
                  grid%tgl_urb3d,   &       
                 cids, cide, 1, config_flags%num_soil_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_soil_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_soil_layers, cjps, cjpe,   &         
                  ngrid%tgl_urb3d,  &   
                 nids, nide, 1, config_flags%num_soil_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_soil_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_soil_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%sh_urb2d, 1 ) * SIZE( grid%sh_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%sh_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sh_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lh_urb2d, 1 ) * SIZE( grid%lh_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%lh_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lh_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%g_urb2d, 1 ) * SIZE( grid%g_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%g_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%g_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rn_urb2d, 1 ) * SIZE( grid%rn_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%rn_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rn_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ts_urb2d, 1 ) * SIZE( grid%ts_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%ts_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ts_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%frc_urb2d, 1 ) * SIZE( grid%frc_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%frc_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%frc_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%utype_urb2d, 1 ) * SIZE( grid%utype_urb2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm (  &         
                  grid%utype_urb2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%utype_urb2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%imperv , 1 )*SIZE( grid%imperv , 2 ) .GT. 1 ), & 
                  grid%imperv,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%imperv,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%canfra , 1 )*SIZE( grid%canfra , 2 ) .GT. 1 ), & 
                  grid%canfra,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%canfra,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%var2d, 1 ) * SIZE( grid%var2d, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%var2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%var2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oc12d, 1 ) * SIZE( grid%oc12d, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%oc12d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%oc12d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa1, 1 ) * SIZE( grid%oa1, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%oa1,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%oa1,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa2, 1 ) * SIZE( grid%oa2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%oa2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%oa2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa3, 1 ) * SIZE( grid%oa3, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%oa3,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%oa3,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%oa4, 1 ) * SIZE( grid%oa4, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%oa4,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%oa4,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol1, 1 ) * SIZE( grid%ol1, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ol1,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ol1,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol2, 1 ) * SIZE( grid%ol2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ol2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ol2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol3, 1 ) * SIZE( grid%ol3, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ol3,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ol3,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ol4, 1 ) * SIZE( grid%ol4, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ol4,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ol4,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ctopo, 1 ) * SIZE( grid%ctopo, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ctopo,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ctopo,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ctopo2, 1 ) * SIZE( grid%ctopo2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ctopo2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ctopo2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_aerod
IF ( SIZE( aerod, 1 ) * SIZE( aerod, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  aerod(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%aerod(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO
IF ( SIZE( grid%f_ice_phy, 1 ) * SIZE( grid%f_ice_phy, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%f_ice_phy,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%f_ice_phy,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f_rain_phy, 1 ) * SIZE( grid%f_rain_phy, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%f_rain_phy,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%f_rain_phy,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f_rimef_phy, 1 ) * SIZE( grid%f_rimef_phy, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%f_rimef_phy,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%f_rimef_phy,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_tmp, 1 ) * SIZE( grid%om_tmp, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_tmp,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_tmp,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_s, 1 ) * SIZE( grid%om_s, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_s,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_s,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_depth, 1 ) * SIZE( grid%om_depth, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_depth,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_depth,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_u, 1 ) * SIZE( grid%om_u, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_u,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_u,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_v, 1 ) * SIZE( grid%om_v, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_v,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_v,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_lat, 1 ) * SIZE( grid%om_lat, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_lat,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%om_lat,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_lon, 1 ) * SIZE( grid%om_lon, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_lon,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%om_lon,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_ml, 1 ) * SIZE( grid%om_ml, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_ml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%om_ml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_tini, 1 ) * SIZE( grid%om_tini, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_tini,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_tini,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%om_sini, 1 ) * SIZE( grid%om_sini, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%om_sini,   &       
                 cids, cide, 1, config_flags%ocean_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%ocean_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%ocean_levels, cjps, cjpe,   &         
                  ngrid%om_sini,  &   
                 nids, nide, 1, config_flags%ocean_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%ocean_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%ocean_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%h_diabatic, 1 ) * SIZE( grid%h_diabatic, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%h_diabatic,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%h_diabatic,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qv_diabatic, 1 ) * SIZE( grid%qv_diabatic, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%qv_diabatic,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%qv_diabatic,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qc_diabatic, 1 ) * SIZE( grid%qc_diabatic, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%qc_diabatic,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%qc_diabatic,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msft, 1 ) * SIZE( grid%msft, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msft,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msft,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfu, 1 ) * SIZE( grid%msfu, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfu,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfu,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfv, 1 ) * SIZE( grid%msfv, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfv,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfv,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msftx, 1 ) * SIZE( grid%msftx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msftx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msftx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfty, 1 ) * SIZE( grid%msfty, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfty,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfty,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfux, 1 ) * SIZE( grid%msfux, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfux,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfux,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfuy, 1 ) * SIZE( grid%msfuy, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfuy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfuy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvx, 1 ) * SIZE( grid%msfvx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfvx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfvx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvx_inv, 1 ) * SIZE( grid%msfvx_inv, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfvx_inv,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfvx_inv,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%msfvy, 1 ) * SIZE( grid%msfvy, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%msfvy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%msfvy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%f, 1 ) * SIZE( grid%f, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%f,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%f,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%e, 1 ) * SIZE( grid%e, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%e,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%e,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%sina, 1 ) * SIZE( grid%sina, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%sina,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sina,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%cosa, 1 ) * SIZE( grid%cosa, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%cosa,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%cosa,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ht, 1 ) * SIZE( grid%ht, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ht,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ht,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ht_shad, 1 ) * SIZE( grid%ht_shad, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ht_shad,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ht_shad,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tsk , 1 )*SIZE( grid%tsk , 2 ) .GT. 1 ), & 
                  grid%tsk,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tsk,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%rainc, 1 ) * SIZE( grid%rainc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rainc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rainc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rainsh, 1 ) * SIZE( grid%rainsh, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rainsh,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rainsh,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rainnc, 1 ) * SIZE( grid%rainnc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rainnc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rainnc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_rainc, 1 ) * SIZE( grid%i_rainc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_rainc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_rainc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_rainnc, 1 ) * SIZE( grid%i_rainnc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_rainnc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_rainnc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snownc, 1 ) * SIZE( grid%snownc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%snownc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snownc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%graupelnc, 1 ) * SIZE( grid%graupelnc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%graupelnc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%graupelnc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%hailnc, 1 ) * SIZE( grid%hailnc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%hailnc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%hailnc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%refl_10cm, 1 ) * SIZE( grid%refl_10cm, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%refl_10cm,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%refl_10cm,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%th_old, 1 ) * SIZE( grid%th_old, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%th_old,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%th_old,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%qv_old, 1 ) * SIZE( grid%qv_old, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%qv_old,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%qv_old,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%vmi3d, 1 ) * SIZE( grid%vmi3d, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%vmi3d,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%vmi3d,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%di3d, 1 ) * SIZE( grid%di3d, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%di3d,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%di3d,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rhopo3d, 1 ) * SIZE( grid%rhopo3d, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rhopo3d,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%rhopo3d,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%w_up, 1 ) * SIZE( grid%w_up, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%w_up,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%w_up,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rthraten, 1 ) * SIZE( grid%rthraten, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rthraten,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%rthraten,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdown, 1 ) * SIZE( grid%swdown, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdown,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdown,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%gsw, 1 ) * SIZE( grid%gsw, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%gsw,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%gsw,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%glw, 1 ) * SIZE( grid%glw, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%glw,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%glw,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swnorm, 1 ) * SIZE( grid%swnorm, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swnorm,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swnorm,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%diffuse_frac, 1 ) * SIZE( grid%diffuse_frac, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%diffuse_frac,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%diffuse_frac,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swddir, 1 ) * SIZE( grid%swddir, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swddir,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swddir,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swddni, 1 ) * SIZE( grid%swddni, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swddni,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swddni,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swddif, 1 ) * SIZE( grid%swddif, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swddif,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swddif,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%gx, 1 ) * SIZE( grid%gx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%gx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%gx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%bx, 1 ) * SIZE( grid%bx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%bx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%bx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%gg, 1 ) * SIZE( grid%gg, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%gg,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%gg,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%bb, 1 ) * SIZE( grid%bb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%bb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%bb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%coszen_ref, 1 ) * SIZE( grid%coszen_ref, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%coszen_ref,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%coszen_ref,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdown_ref, 1 ) * SIZE( grid%swdown_ref, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdown_ref,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdown_ref,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swddir_ref, 1 ) * SIZE( grid%swddir_ref, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swddir_ref,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swddir_ref,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupt, 1 ) * SIZE( grid%acswupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswuptc, 1 ) * SIZE( grid%acswuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnt, 1 ) * SIZE( grid%acswdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdntc, 1 ) * SIZE( grid%acswdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupb, 1 ) * SIZE( grid%acswupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswupbc, 1 ) * SIZE( grid%acswupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnb, 1 ) * SIZE( grid%acswdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%acswdnbc, 1 ) * SIZE( grid%acswdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%acswdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%acswdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupt, 1 ) * SIZE( grid%aclwupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwuptc, 1 ) * SIZE( grid%aclwuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnt, 1 ) * SIZE( grid%aclwdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdntc, 1 ) * SIZE( grid%aclwdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupb, 1 ) * SIZE( grid%aclwupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwupbc, 1 ) * SIZE( grid%aclwupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnb, 1 ) * SIZE( grid%aclwdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclwdnbc, 1 ) * SIZE( grid%aclwdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclwdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclwdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupt, 1 ) * SIZE( grid%i_acswupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswuptc, 1 ) * SIZE( grid%i_acswuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnt, 1 ) * SIZE( grid%i_acswdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdntc, 1 ) * SIZE( grid%i_acswdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupb, 1 ) * SIZE( grid%i_acswupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswupbc, 1 ) * SIZE( grid%i_acswupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnb, 1 ) * SIZE( grid%i_acswdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_acswdnbc, 1 ) * SIZE( grid%i_acswdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_acswdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_acswdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupt, 1 ) * SIZE( grid%i_aclwupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwuptc, 1 ) * SIZE( grid%i_aclwuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnt, 1 ) * SIZE( grid%i_aclwdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdntc, 1 ) * SIZE( grid%i_aclwdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupb, 1 ) * SIZE( grid%i_aclwupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwupbc, 1 ) * SIZE( grid%i_aclwupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnb, 1 ) * SIZE( grid%i_aclwdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%i_aclwdnbc, 1 ) * SIZE( grid%i_aclwdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcni (  &         
                  grid%i_aclwdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%i_aclwdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupt, 1 ) * SIZE( grid%swupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swuptc, 1 ) * SIZE( grid%swuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnt, 1 ) * SIZE( grid%swdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdntc, 1 ) * SIZE( grid%swdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupb, 1 ) * SIZE( grid%swupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swupbc, 1 ) * SIZE( grid%swupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnb, 1 ) * SIZE( grid%swdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%swdnbc, 1 ) * SIZE( grid%swdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%swdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%swdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupt, 1 ) * SIZE( grid%lwupt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwupt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwupt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwuptc, 1 ) * SIZE( grid%lwuptc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwuptc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwuptc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnt, 1 ) * SIZE( grid%lwdnt, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwdnt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwdnt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdntc, 1 ) * SIZE( grid%lwdntc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwdntc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwdntc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupb, 1 ) * SIZE( grid%lwupb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwupb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwupb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwupbc, 1 ) * SIZE( grid%lwupbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwupbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwupbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnb, 1 ) * SIZE( grid%lwdnb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwdnb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwdnb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lwdnbc, 1 ) * SIZE( grid%lwdnbc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%lwdnbc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lwdnbc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlat_u, 1 ) * SIZE( grid%xlat_u, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%xlat_u,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlat_u,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong_u, 1 ) * SIZE( grid%xlong_u, 2 ) .GT. 1 ) THEN 
CALL interp_fcn_blint_ll (  &         
                  grid%xlong_u,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlong_u,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%xlat_u,ngrid%xlat_u&
,grid%input_from_file,ngrid%input_from_file&
                  ) 
ENDIF
IF ( SIZE( grid%xlat_v, 1 ) * SIZE( grid%xlat_v, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%xlat_v,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlat_v,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%xlong_v, 1 ) * SIZE( grid%xlong_v, 2 ) .GT. 1 ) THEN 
CALL interp_fcn_blint_ll (  &         
                  grid%xlong_v,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xlong_v,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%xlat_v,ngrid%xlat_v&
,grid%input_from_file,ngrid%input_from_file&
                  ) 
ENDIF
IF ( SIZE( grid%clat, 1 ) * SIZE( grid%clat, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%clat,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%clat,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%isnowxy , 1 )*SIZE( grid%isnowxy , 2 ) .GT. 1 ), & 
                  grid%isnowxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%isnowxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tvxy , 1 )*SIZE( grid%tvxy , 2 ) .GT. 1 ), & 
                  grid%tvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tgxy , 1 )*SIZE( grid%tgxy , 2 ) .GT. 1 ), & 
                  grid%tgxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tgxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%canicexy , 1 )*SIZE( grid%canicexy , 2 ) .GT. 1 ), & 
                  grid%canicexy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%canicexy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%canliqxy , 1 )*SIZE( grid%canliqxy , 2 ) .GT. 1 ), & 
                  grid%canliqxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%canliqxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%eahxy , 1 )*SIZE( grid%eahxy , 2 ) .GT. 1 ), & 
                  grid%eahxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%eahxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tahxy , 1 )*SIZE( grid%tahxy , 2 ) .GT. 1 ), & 
                  grid%tahxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tahxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%cmxy , 1 )*SIZE( grid%cmxy , 2 ) .GT. 1 ), & 
                  grid%cmxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%cmxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chxy , 1 )*SIZE( grid%chxy , 2 ) .GT. 1 ), & 
                  grid%chxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%fwetxy , 1 )*SIZE( grid%fwetxy , 2 ) .GT. 1 ), & 
                  grid%fwetxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%fwetxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sneqvoxy , 1 )*SIZE( grid%sneqvoxy , 2 ) .GT. 1 ), & 
                  grid%sneqvoxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sneqvoxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%alboldxy , 1 )*SIZE( grid%alboldxy , 2 ) .GT. 1 ), & 
                  grid%alboldxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%alboldxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%qsnowxy , 1 )*SIZE( grid%qsnowxy , 2 ) .GT. 1 ), & 
                  grid%qsnowxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%qsnowxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%wslakexy , 1 )*SIZE( grid%wslakexy , 2 ) .GT. 1 ), & 
                  grid%wslakexy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%wslakexy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%zwtxy , 1 )*SIZE( grid%zwtxy , 2 ) .GT. 1 ), & 
                  grid%zwtxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%zwtxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%waxy , 1 )*SIZE( grid%waxy , 2 ) .GT. 1 ), & 
                  grid%waxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%waxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%wtxy , 1 )*SIZE( grid%wtxy , 2 ) .GT. 1 ), & 
                  grid%wtxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%wtxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tsnoxy , 1 )*SIZE( grid%tsnoxy , 3 ) .GT. 1 ), & 
                  grid%tsnoxy,   &       
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         
                  ngrid%tsnoxy,  &   
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%zsnsoxy , 1 )*SIZE( grid%zsnsoxy , 3 ) .GT. 1 ), & 
                  grid%zsnsoxy,   &       
                 cids, cide, 1, config_flags%num_snso_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_snso_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_snso_layers, cjps, cjpe,   &         
                  ngrid%zsnsoxy,  &   
                 nids, nide, 1, config_flags%num_snso_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_snso_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_snso_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snicexy , 1 )*SIZE( grid%snicexy , 3 ) .GT. 1 ), & 
                  grid%snicexy,   &       
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         
                  ngrid%snicexy,  &   
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snliqxy , 1 )*SIZE( grid%snliqxy , 3 ) .GT. 1 ), & 
                  grid%snliqxy,   &       
                 cids, cide, 1, config_flags%num_snow_layers, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_snow_layers, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_snow_layers, cjps, cjpe,   &         
                  ngrid%snliqxy,  &   
                 nids, nide, 1, config_flags%num_snow_layers, njds, njde,   &         
                 nims, nime, 1, config_flags%num_snow_layers, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_snow_layers, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%lfmassxy , 1 )*SIZE( grid%lfmassxy , 2 ) .GT. 1 ), & 
                  grid%lfmassxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lfmassxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%rtmassxy , 1 )*SIZE( grid%rtmassxy , 2 ) .GT. 1 ), & 
                  grid%rtmassxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rtmassxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%stmassxy , 1 )*SIZE( grid%stmassxy , 2 ) .GT. 1 ), & 
                  grid%stmassxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%stmassxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%woodxy , 1 )*SIZE( grid%woodxy , 2 ) .GT. 1 ), & 
                  grid%woodxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%woodxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%stblcpxy , 1 )*SIZE( grid%stblcpxy , 2 ) .GT. 1 ), & 
                  grid%stblcpxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%stblcpxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%fastcpxy , 1 )*SIZE( grid%fastcpxy , 2 ) .GT. 1 ), & 
                  grid%fastcpxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%fastcpxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%xsaixy , 1 )*SIZE( grid%xsaixy , 2 ) .GT. 1 ), & 
                  grid%xsaixy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xsaixy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t2mvxy , 1 )*SIZE( grid%t2mvxy , 2 ) .GT. 1 ), & 
                  grid%t2mvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2mvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t2mbxy , 1 )*SIZE( grid%t2mbxy , 2 ) .GT. 1 ), & 
                  grid%t2mbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2mbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%q2mvxy , 1 )*SIZE( grid%q2mvxy , 2 ) .GT. 1 ), & 
                  grid%q2mvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%q2mvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%q2mbxy , 1 )*SIZE( grid%q2mbxy , 2 ) .GT. 1 ), & 
                  grid%q2mbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%q2mbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tradxy , 1 )*SIZE( grid%tradxy , 2 ) .GT. 1 ), & 
                  grid%tradxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tradxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%neexy , 1 )*SIZE( grid%neexy , 2 ) .GT. 1 ), & 
                  grid%neexy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%neexy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%gppxy , 1 )*SIZE( grid%gppxy , 2 ) .GT. 1 ), & 
                  grid%gppxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%gppxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%nppxy , 1 )*SIZE( grid%nppxy , 2 ) .GT. 1 ), & 
                  grid%nppxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%nppxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%fvegxy , 1 )*SIZE( grid%fvegxy , 2 ) .GT. 1 ), & 
                  grid%fvegxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%fvegxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%qinxy , 1 )*SIZE( grid%qinxy , 2 ) .GT. 1 ), & 
                  grid%qinxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%qinxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%runsfxy , 1 )*SIZE( grid%runsfxy , 2 ) .GT. 1 ), & 
                  grid%runsfxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%runsfxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%runsbxy , 1 )*SIZE( grid%runsbxy , 2 ) .GT. 1 ), & 
                  grid%runsbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%runsbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%ecanxy , 1 )*SIZE( grid%ecanxy , 2 ) .GT. 1 ), & 
                  grid%ecanxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ecanxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%edirxy , 1 )*SIZE( grid%edirxy , 2 ) .GT. 1 ), & 
                  grid%edirxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%edirxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%etranxy , 1 )*SIZE( grid%etranxy , 2 ) .GT. 1 ), & 
                  grid%etranxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%etranxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%fsaxy , 1 )*SIZE( grid%fsaxy , 2 ) .GT. 1 ), & 
                  grid%fsaxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%fsaxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%firaxy , 1 )*SIZE( grid%firaxy , 2 ) .GT. 1 ), & 
                  grid%firaxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%firaxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%aparxy , 1 )*SIZE( grid%aparxy , 2 ) .GT. 1 ), & 
                  grid%aparxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aparxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%psnxy , 1 )*SIZE( grid%psnxy , 2 ) .GT. 1 ), & 
                  grid%psnxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%psnxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%savxy , 1 )*SIZE( grid%savxy , 2 ) .GT. 1 ), & 
                  grid%savxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%savxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sagxy , 1 )*SIZE( grid%sagxy , 2 ) .GT. 1 ), & 
                  grid%sagxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sagxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%rssunxy , 1 )*SIZE( grid%rssunxy , 2 ) .GT. 1 ), & 
                  grid%rssunxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rssunxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%rsshaxy , 1 )*SIZE( grid%rsshaxy , 2 ) .GT. 1 ), & 
                  grid%rsshaxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rsshaxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%bgapxy , 1 )*SIZE( grid%bgapxy , 2 ) .GT. 1 ), & 
                  grid%bgapxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%bgapxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%wgapxy , 1 )*SIZE( grid%wgapxy , 2 ) .GT. 1 ), & 
                  grid%wgapxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%wgapxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tgvxy , 1 )*SIZE( grid%tgvxy , 2 ) .GT. 1 ), & 
                  grid%tgvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tgvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tgbxy , 1 )*SIZE( grid%tgbxy , 2 ) .GT. 1 ), & 
                  grid%tgbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tgbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chvxy , 1 )*SIZE( grid%chvxy , 2 ) .GT. 1 ), & 
                  grid%chvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chbxy , 1 )*SIZE( grid%chbxy , 2 ) .GT. 1 ), & 
                  grid%chbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%shgxy , 1 )*SIZE( grid%shgxy , 2 ) .GT. 1 ), & 
                  grid%shgxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%shgxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%shcxy , 1 )*SIZE( grid%shcxy , 2 ) .GT. 1 ), & 
                  grid%shcxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%shcxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%shbxy , 1 )*SIZE( grid%shbxy , 2 ) .GT. 1 ), & 
                  grid%shbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%shbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%evgxy , 1 )*SIZE( grid%evgxy , 2 ) .GT. 1 ), & 
                  grid%evgxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%evgxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%evbxy , 1 )*SIZE( grid%evbxy , 2 ) .GT. 1 ), & 
                  grid%evbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%evbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%ghvxy , 1 )*SIZE( grid%ghvxy , 2 ) .GT. 1 ), & 
                  grid%ghvxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ghvxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%ghbxy , 1 )*SIZE( grid%ghbxy , 2 ) .GT. 1 ), & 
                  grid%ghbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ghbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%irgxy , 1 )*SIZE( grid%irgxy , 2 ) .GT. 1 ), & 
                  grid%irgxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%irgxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%ircxy , 1 )*SIZE( grid%ircxy , 2 ) .GT. 1 ), & 
                  grid%ircxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%ircxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%irbxy , 1 )*SIZE( grid%irbxy , 2 ) .GT. 1 ), & 
                  grid%irbxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%irbxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%trxy , 1 )*SIZE( grid%trxy , 2 ) .GT. 1 ), & 
                  grid%trxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%trxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%evcxy , 1 )*SIZE( grid%evcxy , 2 ) .GT. 1 ), & 
                  grid%evcxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%evcxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chleafxy , 1 )*SIZE( grid%chleafxy , 2 ) .GT. 1 ), & 
                  grid%chleafxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chleafxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chucxy , 1 )*SIZE( grid%chucxy , 2 ) .GT. 1 ), & 
                  grid%chucxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chucxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chv2xy , 1 )*SIZE( grid%chv2xy , 2 ) .GT. 1 ), & 
                  grid%chv2xy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chv2xy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chb2xy , 1 )*SIZE( grid%chb2xy , 2 ) .GT. 1 ), & 
                  grid%chb2xy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chb2xy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%chstarxy , 1 )*SIZE( grid%chstarxy , 2 ) .GT. 1 ), & 
                  grid%chstarxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%chstarxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%fdepthxy , 1 )*SIZE( grid%fdepthxy , 2 ) .GT. 1 ), & 
                  grid%fdepthxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%fdepthxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%eqzwt , 1 )*SIZE( grid%eqzwt , 2 ) .GT. 1 ), & 
                  grid%eqzwt,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%eqzwt,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%rechclim , 1 )*SIZE( grid%rechclim , 2 ) .GT. 1 ), & 
                  grid%rechclim,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%rechclim,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%riverbedxy , 1 )*SIZE( grid%riverbedxy , 2 ) .GT. 1 ), & 
                  grid%riverbedxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%riverbedxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%grainxy , 1 )*SIZE( grid%grainxy , 2 ) .GT. 1 ), & 
                  grid%grainxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%grainxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%gddxy , 1 )*SIZE( grid%gddxy , 2 ) .GT. 1 ), & 
                  grid%gddxy,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%gddxy,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%croptype , 1 )*SIZE( grid%croptype , 3 ) .GT. 1 ), & 
                  grid%croptype,   &       
                 cids, cide, 1, 5, cjds, cjde,   &         
                 cims, cime, 1, 5, cjms, cjme,   &         
                 cips, cipe, 1, 5, cjps, cjpe,   &         
                  ngrid%croptype,  &   
                 nids, nide, 1, 5, njds, njde,   &         
                 nims, nime, 1, 5, njms, njme,   &         
                 nips, nipe, 1, 5, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%planting , 1 )*SIZE( grid%planting , 2 ) .GT. 1 ), & 
                  grid%planting,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%planting,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%harvest , 1 )*SIZE( grid%harvest , 2 ) .GT. 1 ), & 
                  grid%harvest,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%harvest,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%season_gdd , 1 )*SIZE( grid%season_gdd , 2 ) .GT. 1 ), & 
                  grid%season_gdd,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%season_gdd,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tsk_mosaic , 1 )*SIZE( grid%tsk_mosaic , 3 ) .GT. 1 ), & 
                  grid%tsk_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%tsk_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%qsfc_mosaic , 1 )*SIZE( grid%qsfc_mosaic , 3 ) .GT. 1 ), & 
                  grid%qsfc_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%qsfc_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tslb_mosaic , 1 )*SIZE( grid%tslb_mosaic , 3 ) .GT. 1 ), & 
                  grid%tslb_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%tslb_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%smois_mosaic , 1 )*SIZE( grid%smois_mosaic , 3 ) .GT. 1 ), & 
                  grid%smois_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%smois_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%sh2o_mosaic , 1 )*SIZE( grid%sh2o_mosaic , 3 ) .GT. 1 ), & 
                  grid%sh2o_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%sh2o_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%canwat_mosaic , 1 )*SIZE( grid%canwat_mosaic , 3 ) .GT. 1 ), & 
                  grid%canwat_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%canwat_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%snow_mosaic , 1 )*SIZE( grid%snow_mosaic , 3 ) .GT. 1 ), & 
                  grid%snow_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%snow_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%snowh_mosaic , 1 )*SIZE( grid%snowh_mosaic , 3 ) .GT. 1 ), & 
                  grid%snowh_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%snowh_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%snowc_mosaic , 1 )*SIZE( grid%snowc_mosaic , 3 ) .GT. 1 ), & 
                  grid%snowc_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%snowc_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tr_urb2d_mosaic , 1 )*SIZE( grid%tr_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tr_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%tr_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tb_urb2d_mosaic , 1 )*SIZE( grid%tb_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tb_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%tb_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tg_urb2d_mosaic , 1 )*SIZE( grid%tg_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tg_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%tg_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tc_urb2d_mosaic , 1 )*SIZE( grid%tc_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tc_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%tc_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%ts_urb2d_mosaic , 1 )*SIZE( grid%ts_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%ts_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%ts_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%ts_rul2d_mosaic , 1 )*SIZE( grid%ts_rul2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%ts_rul2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%ts_rul2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%qc_urb2d_mosaic , 1 )*SIZE( grid%qc_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%qc_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%qc_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%uc_urb2d_mosaic , 1 )*SIZE( grid%uc_urb2d_mosaic , 3 ) .GT. 1 ), & 
                  grid%uc_urb2d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat, cjps, cjpe,   &         
                  ngrid%uc_urb2d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%trl_urb3d_mosaic , 1 )*SIZE( grid%trl_urb3d_mosaic , 3 ) .GT. 1 ), & 
                  grid%trl_urb3d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%trl_urb3d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tbl_urb3d_mosaic , 1 )*SIZE( grid%tbl_urb3d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tbl_urb3d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%tbl_urb3d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%tgl_urb3d_mosaic , 1 )*SIZE( grid%tgl_urb3d_mosaic , 3 ) .GT. 1 ), & 
                  grid%tgl_urb3d_mosaic,   &       
                 cids, cide, 1, config_flags%mosaic_cat_soil, cjds, cjde,   &         
                 cims, cime, 1, config_flags%mosaic_cat_soil, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%mosaic_cat_soil, cjps, cjpe,   &         
                  ngrid%tgl_urb3d_mosaic,  &   
                 nids, nide, 1, config_flags%mosaic_cat_soil, njds, njde,   &         
                 nims, nime, 1, config_flags%mosaic_cat_soil, njms, njme,   &         
                 nips, nipe, 1, config_flags%mosaic_cat_soil, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_land_field (  &         
  ( SIZE( grid%mosaic_cat_index , 1 )*SIZE( grid%mosaic_cat_index , 3 ) .GT. 1 ), & 
                  grid%mosaic_cat_index,   &       
                 cids, cide, 1, config_flags%num_land_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_land_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_land_cat, cjps, cjpe,   &         
                  ngrid%mosaic_cat_index,  &   
                 nids, nide, 1, config_flags%num_land_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%num_land_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_land_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
                  ) 
ENDIF
IF ( SIZE( grid%landusef2, 1 ) * SIZE( grid%landusef2, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%landusef2,   &       
                 cids, cide, 1, config_flags%num_land_cat, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_land_cat, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_land_cat, cjps, cjpe,   &         
                  ngrid%landusef2,  &   
                 nids, nide, 1, config_flags%num_land_cat, njds, njde,   &         
                 nims, nime, 1, config_flags%num_land_cat, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_land_cat, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tmn , 1 )*SIZE( grid%tmn , 2 ) .GT. 1 ), & 
                  grid%tmn,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tmn,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tyr , 1 )*SIZE( grid%tyr , 2 ) .GT. 1 ), & 
                  grid%tyr,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tyr,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tyra , 1 )*SIZE( grid%tyra , 2 ) .GT. 1 ), & 
                  grid%tyra,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tyra,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tdly , 1 )*SIZE( grid%tdly , 2 ) .GT. 1 ), & 
                  grid%tdly,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tdly,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tlag , 1 )*SIZE( grid%tlag , 3 ) .GT. 1 ), & 
                  grid%tlag,   &       
                 cids, cide, 1, config_flags%lagday, cjds, cjde,   &         
                 cims, cime, 1, config_flags%lagday, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%lagday, cjps, cjpe,   &         
                  ngrid%tlag,  &   
                 nids, nide, 1, config_flags%lagday, njds, njde,   &         
                 nims, nime, 1, config_flags%lagday, njms, njme,   &         
                 nips, nipe, 1, config_flags%lagday, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%xland, 1 ) * SIZE( grid%xland, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm_imask (  &         
                  grid%xland,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%xland,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%achfx, 1 ) * SIZE( grid%achfx, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%achfx,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%achfx,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%aclhf, 1 ) * SIZE( grid%aclhf, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%aclhf,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%aclhf,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowc , 1 )*SIZE( grid%snowc , 2 ) .GT. 1 ), & 
                  grid%snowc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snowc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%prec_acc_c, 1 ) * SIZE( grid%prec_acc_c, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%prec_acc_c,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%prec_acc_c,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%prec_acc_nc, 1 ) * SIZE( grid%prec_acc_nc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%prec_acc_nc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%prec_acc_nc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%snow_acc_nc, 1 ) * SIZE( grid%snow_acc_nc, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%snow_acc_nc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snow_acc_nc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tml , 1 )*SIZE( grid%tml , 2 ) .GT. 1 ), & 
                  grid%tml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t0ml , 1 )*SIZE( grid%t0ml , 2 ) .GT. 1 ), & 
                  grid%t0ml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t0ml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%hml , 1 )*SIZE( grid%hml , 2 ) .GT. 1 ), & 
                  grid%hml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%hml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h0ml , 1 )*SIZE( grid%h0ml , 2 ) .GT. 1 ), & 
                  grid%h0ml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%h0ml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%huml , 1 )*SIZE( grid%huml , 2 ) .GT. 1 ), & 
                  grid%huml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%huml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%hvml , 1 )*SIZE( grid%hvml , 2 ) .GT. 1 ), & 
                  grid%hvml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%hvml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%tmoml , 1 )*SIZE( grid%tmoml , 2 ) .GT. 1 ), & 
                  grid%tmoml,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%tmoml,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( SIZE( grid%vertstrucc, 1 ) * SIZE( grid%vertstrucc, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%vertstrucc,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%vertstrucc,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%vertstrucs, 1 ) * SIZE( grid%vertstrucs, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%vertstrucs,   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%vertstrucs,  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_sf, 1 ) * SIZE( grid%field_sf, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_sf,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_sf,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_pbl, 1 ) * SIZE( grid%field_pbl, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_pbl,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_pbl,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_conv, 1 ) * SIZE( grid%field_conv, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_conv,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_conv,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%ru_tendf_stoch, 1 ) * SIZE( grid%ru_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%ru_tendf_stoch,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%ru_tendf_stoch,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rv_tendf_stoch, 1 ) * SIZE( grid%rv_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rv_tendf_stoch,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%rv_tendf_stoch,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rt_tendf_stoch, 1 ) * SIZE( grid%rt_tendf_stoch, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rt_tendf_stoch,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%rt_tendf_stoch,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rand_pert, 1 ) * SIZE( grid%rand_pert, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rand_pert,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%rand_pert,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_conv, 1 ) * SIZE( grid%pattern_spp_conv, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pattern_spp_conv,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%pattern_spp_conv,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_pbl, 1 ) * SIZE( grid%pattern_spp_pbl, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pattern_spp_pbl,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%pattern_spp_pbl,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pattern_spp_lsm, 1 ) * SIZE( grid%pattern_spp_lsm, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pattern_spp_lsm,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%pattern_spp_lsm,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%rstoch, 1 ) * SIZE( grid%rstoch, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%rstoch,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%rstoch,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%numc , 1 )*SIZE( grid%numc , 2 ) .GT. 1 ), & 
                  grid%numc,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%numc,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%nump , 1 )*SIZE( grid%nump , 2 ) .GT. 1 ), & 
                  grid%nump,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%nump,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snl , 1 )*SIZE( grid%snl , 3 ) .GT. 1 ), & 
                  grid%snl,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snl,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowdp , 1 )*SIZE( grid%snowdp , 3 ) .GT. 1 ), & 
                  grid%snowdp,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowdp,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%wtc , 1 )*SIZE( grid%wtc , 3 ) .GT. 1 ), & 
                  grid%wtc,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%wtc,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%wtp , 1 )*SIZE( grid%wtp , 3 ) .GT. 1 ), & 
                  grid%wtp,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%wtp,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osno , 1 )*SIZE( grid%h2osno , 3 ) .GT. 1 ), & 
                  grid%h2osno,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osno,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_grnd , 1 )*SIZE( grid%t_grnd , 3 ) .GT. 1 ), & 
                  grid%t_grnd,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_grnd,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_veg , 1 )*SIZE( grid%t_veg , 3 ) .GT. 1 ), & 
                  grid%t_veg,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_veg,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2ocan , 1 )*SIZE( grid%h2ocan , 3 ) .GT. 1 ), & 
                  grid%h2ocan,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2ocan,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2ocan_col , 1 )*SIZE( grid%h2ocan_col , 3 ) .GT. 1 ), & 
                  grid%h2ocan_col,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2ocan_col,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t2m_max , 1 )*SIZE( grid%t2m_max , 2 ) .GT. 1 ), & 
                  grid%t2m_max,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2m_max,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t2m_min , 1 )*SIZE( grid%t2m_min , 2 ) .GT. 1 ), & 
                  grid%t2m_min,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2m_min,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t2clm , 1 )*SIZE( grid%t2clm , 2 ) .GT. 1 ), & 
                  grid%t2clm,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t2clm,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_ref2m , 1 )*SIZE( grid%t_ref2m , 3 ) .GT. 1 ), & 
                  grid%t_ref2m,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_ref2m,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq_s1 , 1 )*SIZE( grid%h2osoi_liq_s1 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq_s1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq_s1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq_s2 , 1 )*SIZE( grid%h2osoi_liq_s2 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq_s2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq_s2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq_s3 , 1 )*SIZE( grid%h2osoi_liq_s3 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq_s3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq_s3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq_s4 , 1 )*SIZE( grid%h2osoi_liq_s4 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq_s4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq_s4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq_s5 , 1 )*SIZE( grid%h2osoi_liq_s5 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq_s5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq_s5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq1 , 1 )*SIZE( grid%h2osoi_liq1 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq2 , 1 )*SIZE( grid%h2osoi_liq2 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq3 , 1 )*SIZE( grid%h2osoi_liq3 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq4 , 1 )*SIZE( grid%h2osoi_liq4 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq5 , 1 )*SIZE( grid%h2osoi_liq5 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq6 , 1 )*SIZE( grid%h2osoi_liq6 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq6,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq6,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq7 , 1 )*SIZE( grid%h2osoi_liq7 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq7,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq7,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq8 , 1 )*SIZE( grid%h2osoi_liq8 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq8,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq8,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq9 , 1 )*SIZE( grid%h2osoi_liq9 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq9,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq9,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_liq10 , 1 )*SIZE( grid%h2osoi_liq10 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq10,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_liq10,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice_s1 , 1 )*SIZE( grid%h2osoi_ice_s1 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice_s1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice_s1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice_s2 , 1 )*SIZE( grid%h2osoi_ice_s2 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice_s2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice_s2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice_s3 , 1 )*SIZE( grid%h2osoi_ice_s3 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice_s3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice_s3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice_s4 , 1 )*SIZE( grid%h2osoi_ice_s4 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice_s4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice_s4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice_s5 , 1 )*SIZE( grid%h2osoi_ice_s5 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice_s5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice_s5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice1 , 1 )*SIZE( grid%h2osoi_ice1 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice2 , 1 )*SIZE( grid%h2osoi_ice2 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice3 , 1 )*SIZE( grid%h2osoi_ice3 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice4 , 1 )*SIZE( grid%h2osoi_ice4 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice5 , 1 )*SIZE( grid%h2osoi_ice5 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice6 , 1 )*SIZE( grid%h2osoi_ice6 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice6,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice6,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice7 , 1 )*SIZE( grid%h2osoi_ice7 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice7,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice7,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice8 , 1 )*SIZE( grid%h2osoi_ice8 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice8,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice8,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice9 , 1 )*SIZE( grid%h2osoi_ice9 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice9,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice9,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_ice10 , 1 )*SIZE( grid%h2osoi_ice10 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice10,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_ice10,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno_s1 , 1 )*SIZE( grid%t_soisno_s1 , 3 ) .GT. 1 ), & 
                  grid%t_soisno_s1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno_s1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno_s2 , 1 )*SIZE( grid%t_soisno_s2 , 3 ) .GT. 1 ), & 
                  grid%t_soisno_s2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno_s2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno_s3 , 1 )*SIZE( grid%t_soisno_s3 , 3 ) .GT. 1 ), & 
                  grid%t_soisno_s3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno_s3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno_s4 , 1 )*SIZE( grid%t_soisno_s4 , 3 ) .GT. 1 ), & 
                  grid%t_soisno_s4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno_s4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno_s5 , 1 )*SIZE( grid%t_soisno_s5 , 3 ) .GT. 1 ), & 
                  grid%t_soisno_s5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno_s5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno1 , 1 )*SIZE( grid%t_soisno1 , 3 ) .GT. 1 ), & 
                  grid%t_soisno1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno2 , 1 )*SIZE( grid%t_soisno2 , 3 ) .GT. 1 ), & 
                  grid%t_soisno2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno3 , 1 )*SIZE( grid%t_soisno3 , 3 ) .GT. 1 ), & 
                  grid%t_soisno3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno4 , 1 )*SIZE( grid%t_soisno4 , 3 ) .GT. 1 ), & 
                  grid%t_soisno4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno5 , 1 )*SIZE( grid%t_soisno5 , 3 ) .GT. 1 ), & 
                  grid%t_soisno5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno6 , 1 )*SIZE( grid%t_soisno6 , 3 ) .GT. 1 ), & 
                  grid%t_soisno6,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno6,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno7 , 1 )*SIZE( grid%t_soisno7 , 3 ) .GT. 1 ), & 
                  grid%t_soisno7,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno7,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno8 , 1 )*SIZE( grid%t_soisno8 , 3 ) .GT. 1 ), & 
                  grid%t_soisno8,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno8,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno9 , 1 )*SIZE( grid%t_soisno9 , 3 ) .GT. 1 ), & 
                  grid%t_soisno9,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno9,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_soisno10 , 1 )*SIZE( grid%t_soisno10 , 3 ) .GT. 1 ), & 
                  grid%t_soisno10,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_soisno10,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%dzsnow1 , 1 )*SIZE( grid%dzsnow1 , 3 ) .GT. 1 ), & 
                  grid%dzsnow1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%dzsnow1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%dzsnow2 , 1 )*SIZE( grid%dzsnow2 , 3 ) .GT. 1 ), & 
                  grid%dzsnow2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%dzsnow2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%dzsnow3 , 1 )*SIZE( grid%dzsnow3 , 3 ) .GT. 1 ), & 
                  grid%dzsnow3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%dzsnow3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%dzsnow4 , 1 )*SIZE( grid%dzsnow4 , 3 ) .GT. 1 ), & 
                  grid%dzsnow4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%dzsnow4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%dzsnow5 , 1 )*SIZE( grid%dzsnow5 , 3 ) .GT. 1 ), & 
                  grid%dzsnow5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%dzsnow5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowrds1 , 1 )*SIZE( grid%snowrds1 , 3 ) .GT. 1 ), & 
                  grid%snowrds1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowrds1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowrds2 , 1 )*SIZE( grid%snowrds2 , 3 ) .GT. 1 ), & 
                  grid%snowrds2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowrds2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowrds3 , 1 )*SIZE( grid%snowrds3 , 3 ) .GT. 1 ), & 
                  grid%snowrds3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowrds3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowrds4 , 1 )*SIZE( grid%snowrds4 , 3 ) .GT. 1 ), & 
                  grid%snowrds4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowrds4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%snowrds5 , 1 )*SIZE( grid%snowrds5 , 3 ) .GT. 1 ), & 
                  grid%snowrds5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%snowrds5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake1 , 1 )*SIZE( grid%t_lake1 , 3 ) .GT. 1 ), & 
                  grid%t_lake1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake2 , 1 )*SIZE( grid%t_lake2 , 3 ) .GT. 1 ), & 
                  grid%t_lake2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake3 , 1 )*SIZE( grid%t_lake3 , 3 ) .GT. 1 ), & 
                  grid%t_lake3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake4 , 1 )*SIZE( grid%t_lake4 , 3 ) .GT. 1 ), & 
                  grid%t_lake4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake5 , 1 )*SIZE( grid%t_lake5 , 3 ) .GT. 1 ), & 
                  grid%t_lake5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake6 , 1 )*SIZE( grid%t_lake6 , 3 ) .GT. 1 ), & 
                  grid%t_lake6,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake6,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake7 , 1 )*SIZE( grid%t_lake7 , 3 ) .GT. 1 ), & 
                  grid%t_lake7,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake7,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake8 , 1 )*SIZE( grid%t_lake8 , 3 ) .GT. 1 ), & 
                  grid%t_lake8,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake8,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake9 , 1 )*SIZE( grid%t_lake9 , 3 ) .GT. 1 ), & 
                  grid%t_lake9,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake9,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%t_lake10 , 1 )*SIZE( grid%t_lake10 , 3 ) .GT. 1 ), & 
                  grid%t_lake10,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%t_lake10,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol1 , 1 )*SIZE( grid%h2osoi_vol1 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol1,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol1,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol2 , 1 )*SIZE( grid%h2osoi_vol2 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol2,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol2,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol3 , 1 )*SIZE( grid%h2osoi_vol3 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol3,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol3,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol4 , 1 )*SIZE( grid%h2osoi_vol4 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol4,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol4,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol5 , 1 )*SIZE( grid%h2osoi_vol5 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol5,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol5,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol6 , 1 )*SIZE( grid%h2osoi_vol6 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol6,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol6,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol7 , 1 )*SIZE( grid%h2osoi_vol7 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol7,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol7,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol8 , 1 )*SIZE( grid%h2osoi_vol8 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol8,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol8,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol9 , 1 )*SIZE( grid%h2osoi_vol9 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol9,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol9,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%h2osoi_vol10 , 1 )*SIZE( grid%h2osoi_vol10 , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol10,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%h2osoi_vol10,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%albedosubgrid , 1 )*SIZE( grid%albedosubgrid , 3 ) .GT. 1 ), & 
                  grid%albedosubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%albedosubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%lhsubgrid , 1 )*SIZE( grid%lhsubgrid , 3 ) .GT. 1 ), & 
                  grid%lhsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%lhsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%hfxsubgrid , 1 )*SIZE( grid%hfxsubgrid , 3 ) .GT. 1 ), & 
                  grid%hfxsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%hfxsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%lwupsubgrid , 1 )*SIZE( grid%lwupsubgrid , 3 ) .GT. 1 ), & 
                  grid%lwupsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%lwupsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%q2subgrid , 1 )*SIZE( grid%q2subgrid , 3 ) .GT. 1 ), & 
                  grid%q2subgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%q2subgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sabvsubgrid , 1 )*SIZE( grid%sabvsubgrid , 3 ) .GT. 1 ), & 
                  grid%sabvsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%sabvsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sabgsubgrid , 1 )*SIZE( grid%sabgsubgrid , 3 ) .GT. 1 ), & 
                  grid%sabgsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%sabgsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%nrasubgrid , 1 )*SIZE( grid%nrasubgrid , 3 ) .GT. 1 ), & 
                  grid%nrasubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%nrasubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%swupsubgrid , 1 )*SIZE( grid%swupsubgrid , 3 ) .GT. 1 ), & 
                  grid%swupsubgrid,   &       
                 cids, cide, 1, config_flags%maxpatch, cjds, cjde,   &         
                 cims, cime, 1, config_flags%maxpatch, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%maxpatch, cjps, cjpe,   &         
                  ngrid%swupsubgrid,  &   
                 nids, nide, 1, config_flags%maxpatch, njds, njde,   &         
                 nims, nime, 1, config_flags%maxpatch, njms, njme,   &         
                 nips, nipe, 1, config_flags%maxpatch, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%lakedepth2d , 1 )*SIZE( grid%lakedepth2d , 2 ) .GT. 1 ), & 
                  grid%lakedepth2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lakedepth2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%savedtke12d , 1 )*SIZE( grid%savedtke12d , 2 ) .GT. 1 ), & 
                  grid%savedtke12d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%savedtke12d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%snowdp2d , 1 )*SIZE( grid%snowdp2d , 2 ) .GT. 1 ), & 
                  grid%snowdp2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snowdp2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%h2osno2d , 1 )*SIZE( grid%h2osno2d , 2 ) .GT. 1 ), & 
                  grid%h2osno2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%h2osno2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%snl2d , 1 )*SIZE( grid%snl2d , 2 ) .GT. 1 ), & 
                  grid%snl2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%snl2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%t_grnd2d , 1 )*SIZE( grid%t_grnd2d , 2 ) .GT. 1 ), & 
                  grid%t_grnd2d,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%t_grnd2d,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%t_lake3d , 1 )*SIZE( grid%t_lake3d , 3 ) .GT. 1 ), & 
                  grid%t_lake3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%t_lake3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%lake_icefrac3d , 1 )*SIZE( grid%lake_icefrac3d , 3 ) .GT. 1 ), & 
                  grid%lake_icefrac3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%lake_icefrac3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%z_lake3d , 1 )*SIZE( grid%z_lake3d , 3 ) .GT. 1 ), & 
                  grid%z_lake3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%z_lake3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%dz_lake3d , 1 )*SIZE( grid%dz_lake3d , 3 ) .GT. 1 ), & 
                  grid%dz_lake3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%dz_lake3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%t_soisno3d , 1 )*SIZE( grid%t_soisno3d , 3 ) .GT. 1 ), & 
                  grid%t_soisno3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%t_soisno3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%h2osoi_ice3d , 1 )*SIZE( grid%h2osoi_ice3d , 3 ) .GT. 1 ), & 
                  grid%h2osoi_ice3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%h2osoi_ice3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%h2osoi_liq3d , 1 )*SIZE( grid%h2osoi_liq3d , 3 ) .GT. 1 ), & 
                  grid%h2osoi_liq3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%h2osoi_liq3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%h2osoi_vol3d , 1 )*SIZE( grid%h2osoi_vol3d , 3 ) .GT. 1 ), & 
                  grid%h2osoi_vol3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%h2osoi_vol3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%z3d , 1 )*SIZE( grid%z3d , 3 ) .GT. 1 ), & 
                  grid%z3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%z3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%dz3d , 1 )*SIZE( grid%dz3d , 3 ) .GT. 1 ), & 
                  grid%dz3d,   &       
                 cids, cide, 1, 15, cjds, cjde,   &         
                 cims, cime, 1, 15, cjms, cjme,   &         
                 cips, cipe, 1, 15, cjps, cjpe,   &         
                  ngrid%dz3d,  &   
                 nids, nide, 1, 15, njds, njde,   &         
                 nims, nime, 1, 15, njms, njme,   &         
                 nips, nipe, 1, 15, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%zi3d , 1 )*SIZE( grid%zi3d , 3 ) .GT. 1 ), & 
                  grid%zi3d,   &       
                 cids, cide, 1, 16, cjds, cjde,   &         
                 cims, cime, 1, 16, cjms, cjme,   &         
                 cips, cipe, 1, 16, cjps, cjpe,   &         
                  ngrid%zi3d,  &   
                 nids, nide, 1, 16, njds, njde,   &         
                 nims, nime, 1, 16, njms, njme,   &         
                 nips, nipe, 1, 16, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%watsat3d , 1 )*SIZE( grid%watsat3d , 3 ) .GT. 1 ), & 
                  grid%watsat3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%watsat3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%csol3d , 1 )*SIZE( grid%csol3d , 3 ) .GT. 1 ), & 
                  grid%csol3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%csol3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%tkmg3d , 1 )*SIZE( grid%tkmg3d , 3 ) .GT. 1 ), & 
                  grid%tkmg3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%tkmg3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%tkdry3d , 1 )*SIZE( grid%tkdry3d , 3 ) .GT. 1 ), & 
                  grid%tkdry3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%tkdry3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_water_field (  &         
  ( SIZE( grid%tksatu3d , 1 )*SIZE( grid%tksatu3d , 3 ) .GT. 1 ), & 
                  grid%tksatu3d,   &       
                 cids, cide, 1, 10, cjds, cjde,   &         
                 cims, cime, 1, 10, cjms, cjme,   &         
                 cips, cipe, 1, 10, cjps, cjpe,   &         
                  ngrid%tksatu3d,  &   
                 nids, nide, 1, 10, njds, njde,   &         
                 nims, nime, 1, 10, njms, njme,   &         
                 nips, nipe, 1, 10, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%islake,ngrid%islake&
                  ) 
ENDIF
IF ( SIZE( grid%field_u_tend_perturb, 1 ) * SIZE( grid%field_u_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_u_tend_perturb,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_u_tend_perturb,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_xstag,         &         
                  .TRUE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_v_tend_perturb, 1 ) * SIZE( grid%field_v_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_v_tend_perturb,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_v_tend_perturb,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_ystag,         &         
                  .FALSE., .TRUE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%field_t_tend_perturb, 1 ) * SIZE( grid%field_t_tend_perturb, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%field_t_tend_perturb,   &       
                 cids, cide, 1, config_flags%num_stoch_levels, cjds, cjde,   &         
                 cims, cime, 1, config_flags%num_stoch_levels, cjms, cjme,   &         
                 cips, cipe, 1, config_flags%num_stoch_levels, cjps, cjpe,   &         
                  ngrid%field_t_tend_perturb,  &   
                 nids, nide, 1, config_flags%num_stoch_levels, njds, njde,   &         
                 nims, nime, 1, config_flags%num_stoch_levels, njms, njme,   &         
                 nips, nipe, 1, config_flags%num_stoch_levels, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pcb, 1 ) * SIZE( grid%pcb, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pcb,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%pcb,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%pc_2, 1 ) * SIZE( grid%pc_2, 2 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  grid%pc_2,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%pc_2,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%landmask, 1 ) * SIZE( grid%landmask, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm_imask (  &         
                  grid%landmask,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%landmask,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( SIZE( grid%lakemask, 1 ) * SIZE( grid%lakemask, 2 ) .GT. 1 ) THEN 
CALL interp_fcnm_imask (  &         
                  grid%lakemask,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%lakemask,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
IF ( .TRUE. ) THEN 
CALL interp_mask_field (  &         
  ( SIZE( grid%sst , 1 )*SIZE( grid%sst , 2 ) .GT. 1 ), & 
                  grid%sst,   &       
                 cids, cide, 1, 1, cjds, cjde,   &         
                 cims, cime, 1, 1, cjms, cjme,   &         
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  ngrid%sst,  &   
                 nids, nide, 1, 1, njds, njde,   &         
                 nims, nime, 1, 1, njms, njme,   &         
                 nips, nipe, 1, 1, njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,grid%lu_index,ngrid%lu_index&
,grid%iswater,ngrid%iswater&
                  ) 
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_tracer
IF ( SIZE( tracer, 1 ) * SIZE( tracer, 3 ) .GT. 1 ) THEN 
CALL interp_fcn (  &         
                  tracer(grid%sm31,grid%sm32,grid%sm33,itrace),   &       
                 cids, cide, ckds, ckde, cjds, cjde,   &         
                 cims, cime, ckms, ckme, cjms, cjme,   &         
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  ngrid%tracer(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   
                 nids, nide, nkds, nkde, njds, njde,   &         
                 nims, nime, nkms, nkme, njms, njme,   &         
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  config_flags%shw, ngrid%imask_nostag,         &         
                  .FALSE., .FALSE.,                                                &         
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDIF
ENDDO


      RETURN

END SUBROUTINE interp_domain_em_part1


SUBROUTINE interp_domain_em_part2
END SUBROUTINE interp_domain_em_part2



