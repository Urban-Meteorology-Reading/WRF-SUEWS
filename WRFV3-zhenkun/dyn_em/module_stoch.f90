
module module_stoch

































































      implicit none
      public ::  SETUP_RAND_PERTURB, UPDATE_STOCH,& 
                         do_fftback_along_x,do_fftback_along_y,& 
                         rand_pert_update

      INTEGER :: LMINFORC, LMAXFORC, KMINFORC, KMAXFORC, & 
      &          LMINFORCT, LMAXFORCT, KMINFORCT, KMAXFORCT
      REAL    :: ALPH, ALPH_PSI, ALPH_T, TOT_BACKSCAT_PSI, TOT_BACKSCAT_T,  REXPONENT_PSI,REXPONENT_T



      INTEGER :: LENSAV
      INTEGER,ALLOCATABLE:: wavenumber_k(:), wavenumber_l(:)
      REAL, ALLOCATABLE :: WSAVE1(:),WSAVE2(:)


      REAL, PARAMETER:: RPI= 3.141592653589793 
      REAL, PARAMETER:: CP= 1006.0 
      REAL, PARAMETER:: T0= 300.0 

      save



contains






      SUBROUTINE INITIALIZE_STOCH  (grid, config_flags,                       &
                          first_trip_for_this_domain,                         & 
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          its, ite, jts, jte, kts, kte,                       & 
                          imsx, imex, jmsx, jmex, kmsx, kmex,  &
                          ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                          imsy, imey, jmsy, jmey, kmsy, kmey,  &
                          ipsy, ipey, jpsy, jpey, kpsy, kpey )


    USE module_configure
    USE module_domain, ONLY : domain

      IMPLICIT NONE

      TYPE (grid_config_rec_type)            :: config_flags
      TYPE ( domain ), INTENT(INOUT)         :: grid

      INTEGER , INTENT(IN)     ::               ids, ide, jds, jde, kds, kde,   &
                                                ims, ime, jms, jme, kms, kme,   &
                                                ips, ipe, jps, jpe, kps, kpe,   & 
                                                its, ite, jts, jte, kts, kte
      INTEGER , INTENT(IN)     ::               imsx,imex,jmsx,jmex,kmsx,kmex, &
                                                ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                                                imsy,imey,jmsy,jmey,kmsy,kmey, &
                                                ipsy,ipey,jpsy,jpey,kpsy,kpey

      LOGICAL                  ::               first_trip_for_this_domain
      INTEGER                  ::               K 


   IF ( first_trip_for_this_domain ) THEN
     grid%did_stoch = .FALSE.
   END IF

   IF ((( grid%id == 1) .AND. (.NOT. grid%did_stoch)) .AND. &
       (( grid%skebs_on== 1) .OR.( grid%sppt_on== 1) .OR. ( grid%rand_perturb_on== 1) .OR. &
         ( grid%spp_conv== 1) .OR. ( grid%spp_pbl== 1) .OR. ( grid%spp_lsm== 1)) ) THEN

     grid%did_stoch = .TRUE.

     IF (grid%skebs_on==1) then



     if ((.not.config_flags%restart) .or. (.not.config_flags%hrrr_cycling)) then 
         call rand_seed (config_flags, grid%ISEED_SKEBS, grid%iseedarr_skebs , kms, kme)
     endif
     call SETUP_RAND_PERTURB('W',                                         &
                       grid%skebs_vertstruc,config_flags%restart,         &
                       grid%SPSTREAM_AMP,                                 &
                       grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,grid%ALPH_PSI,&
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_sppt,                           &
                       grid%lengthscale_sppt,                             &
                       grid%timescale_sppt,                               &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )

     call SETUP_RAND_PERTURB('T',                                         &
                       grid%skebs_vertstruc,config_flags%restart,     &
                       grid%SPT_AMP,                                      &
                       grid%SPTFORCS,grid%SPTFORCC,grid%ALPH_T,           &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_sppt,                           &
                       grid%lengthscale_sppt,                             &
                       grid%timescale_sppt,                               &
                       grid%TOT_BACKSCAT_T,grid%ZTAU_T,                   &
                       grid%REXPONENT_T,                                  &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF

IF (grid%sppt_on==1) then

     if ((.not.config_flags%restart) .or. (.not.config_flags%hrrr_cycling)) then 
         call rand_seed (config_flags, grid%ISEED_SPPT, grid%iseedarr_sppt  , kms, kme)
     endif
     call SETUP_RAND_PERTURB('P',                                         &
                       grid%sppt_vertstruc,config_flags%restart,          &
                       grid%SPPT_AMP,                                     &
                       grid%SPPTFORCC,grid%SPPTFORCS,grid%ALPH_SPPT,      &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_sppt,                           &
                       grid%lengthscale_sppt,                             &
                       grid%timescale_sppt,                               &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF


     IF (grid%rand_perturb_on==1) then
     if ((.not.config_flags%restart) .or. (.not.config_flags%hrrr_cycling)) then 
         call rand_seed (config_flags, grid%ISEED_RAND_PERT, grid%iseedarr_rand_pert  , kms, kme)
     endif
     call SETUP_RAND_PERTURB('R',                                         &
                       grid%rand_pert_vertstruc,config_flags%restart,     &
                       grid%SP_AMP,                                       &
                       grid%SPFORCC,grid%SPFORCS,grid%ALPH_RAND,          &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_rand_pert,                      &
                       grid%lengthscale_rand_pert,                        &
                       grid%timescale_rand_pert,                          &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )

     if (.not.config_flags%restart) then 
        do k = 1,10
           CALL RAND_PERT_UPDATE(grid,'R',                                     &
                           grid%SPFORCS,grid%SPFORCC,                          &
                           grid%SP_AMP,grid%ALPH_RAND,                         &
                           ips, ipe, jps, jpe, kps, kpe,                       &
                           ids, ide, jds, jde, kds, kde,                       &
                           ims, ime, jms, jme, kms, kme,                       &
                           kts, kte,                                           &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           config_flags%restart, grid%iseedarr_rand_pert,      &
                           grid%DX,grid%DY,grid%rand_pert_vertstruc,           &
                           grid%RAND_PERT,                                     &
                           grid%gridpt_stddev_rand_pert,                      &
                           grid%lengthscale_rand_pert,                        &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT       )
         enddo
       ENDIF 
     ENDIF


     IF (grid%spp_conv==1) then
     if (.not.config_flags%restart) then 
         call rand_seed (config_flags, grid%iseed_spp_conv, grid%iseedarr_spp_conv , kms, kme)
     endif
     call SETUP_RAND_PERTURB('S',                                        &
                       grid%vertstruc_spp_conv,config_flags%restart,    &
                       grid%SP_AMP2,                                      &
                       grid%SPFORCC2,grid%SPFORCS2,grid%ALPH_RAND2,       &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_spp_conv,                     &
                       grid%lengthscale_spp_conv,                       &
                       grid%timescale_spp_conv,                         &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF

     IF (grid%spp_pbl==1) then
     if (.not.config_flags%restart) then 
         call rand_seed (config_flags, grid%iseed_spp_pbl, grid%iseedarr_spp_pbl , kms, kme)
     endif
     call SETUP_RAND_PERTURB('Q',                                        &
                       grid%vertstruc_spp_pbl,config_flags%restart,    &
                       grid%SP_AMP3,                                      &
                       grid%SPFORCC3,grid%SPFORCS3,grid%ALPH_RAND3,       &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_spp_pbl,                     &
                       grid%lengthscale_spp_pbl,                       &
                       grid%timescale_spp_pbl,                         &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF

     IF (grid%spp_lsm==1) then
     if (.not.config_flags%restart) then 
         call rand_seed (config_flags, grid%iseed_spp_lsm, grid%iseedarr_spp_lsm , kms, kme)
     endif
     call SETUP_RAND_PERTURB('O',                                        &
                       grid%vertstruc_spp_lsm,config_flags%restart,    &
                       grid%SP_AMP4,                                      &
                       grid%SPFORCC4,grid%SPFORCS4,grid%ALPH_RAND4,       &
                       grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT,     &
                       grid%KMINFORCT,grid%KMAXFORCT,                     &
                       grid%LMINFORCT,grid%LMAXFORCT,                     &
                       grid%KMAXFORCTH,grid%LMAXFORCTH,                   &
                       grid%time_step,grid%DX,grid%DY,                    &
                       grid%gridpt_stddev_spp_lsm,                     &
                       grid%lengthscale_spp_lsm,                       &
                       grid%timescale_spp_lsm,                         &
                       grid%TOT_BACKSCAT_PSI,grid%ZTAU_PSI,               &
                       grid%REXPONENT_PSI,                                &
                       ids, ide, jds, jde, kds, kde,                      &
                       ims, ime, jms, jme, kms, kme,                      &
                       its, ite, jts, jte, kts, kte                       )
     ENDIF

     ENDIF 
 
      END SUBROUTINE INITIALIZE_STOCH       

      


      subroutine SETUP_RAND_PERTURB( variable_in,& 
                       skebs_vertstruc,restart,                  & 
                       SP_AMP,SPFORCC,SPFORCS,ALPH,                  & 
                       VERTSTRUCC,VERTSTRUCS,VERTAMP,                &
                       KMINFORCT,KMAXFORCTH,LMINFORCT,LMAXFORCTH,    & 
                       KMAXFORCT,LMAXFORCT,                          &
                       itime_step,DX,DY,                             &
                       gridpt_stddev_rand_perturb, l_rand_perturb,   & 
                       tau_rand_perturb,                             &
                       TOT_BACKSCAT,ZTAU,REXPONENT,                  & 
                       ids, ide, jds, jde, kds, kde,                 &
                       ims, ime, jms, jme, kms, kme,                 &
                       its, ite, jts, jte, kts, kte                  )



      IMPLICIT NONE


      LOGICAL                                   :: restart
      REAL, PARAMETER                           :: RPI= 3.141592653589793 
      CHARACTER, INTENT(IN)                     :: variable_in 
      CHARACTER                                 :: variable


      INTEGER , INTENT(IN)                      :: ids, ide, jds, jde, kds, kde,   &
                                                   ims, ime, jms, jme, kms, kme,   &
                                                   its, ite, jts, jte, kts, kte
      INTEGER                                   :: IER,IK,IL,I,J,itime_step,skebs_vertstruc, & 
                                                   KMINFORCT,LMINFORCT,KMAXFORCT,LMAXFORCT,KMAXFORCTH,LMAXFORCTH, & 
                                                   KMAX,LMAX,LENSAV,ILEV
      REAL                                      :: DX,DY,RY,RX,ALPH,RHOKLMAX,ZREF,RHOKL,EPS
      REAL, DIMENSION (ims:ime,jms:jme)         :: SPFORCS,SPFORCC,SP_AMP
      REAL, DIMENSION (ims:ime,kms:kme,jms:jme) :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION (kms:kme)                 :: VERTAMP
      REAL, DIMENSION (ids:ide,jds:jde)         :: ZCHI


      REAL                                      :: gridpt_stddev_rand_perturb,kappat,tau_rand_perturb,l_rand_perturb
      REAL, DIMENSION (ims:ime,jms:jme)         :: var_sigma1



      REAL                                      :: z,phi,ZGAMMAN,ZCONSTF0,TOT_BACKSCAT,ZTAU,REXPONENT,ZSIGMA2
      LOGICAL                                   :: is_print = .true.


      variable = variable_in

      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 
      RY=  KMAX*DY
      RX=  LMAX*DX
      LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8



      IF ( ALLOCATED(WSAVE1)      ) DEALLOCATE(WSAVE1)
      IF ( ALLOCATED(WSAVE2)      ) DEALLOCATE(WSAVE2)
      ALLOCATE(WSAVE1(LENSAV),WSAVE2(LENSAV))

      IF ( ALLOCATED(WAVENUMBER_K)) DEALLOCATE(WAVENUMBER_K)
      IF ( ALLOCATED(WAVENUMBER_L)) DEALLOCATE(WAVENUMBER_L)
      ALLOCATE (wavenumber_k(jds:jde),wavenumber_l(ids:ide))


      call CFFT1I (LMAX, WSAVE1, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier

      call CFFT1I (KMAX, WSAVE2, LENSAV, IER)
      if(ier.ne. 0) write(*,95) ier

      95 format('error in cFFT2I=  ',i5)

      call findindex( wavenumber_k, wavenumber_l,             &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )


     KMAXFORCT=min0(((ide-ids)+1)/2,((jde-jds)+1 )/2)-5
     LMAXFORCT=KMAXFORCT
     if (KMAXFORCT > KMAXFORCTH) then
        KMAXFORCT=KMAXFORCTH
     endif
     if (LMAXFORCT > LMAXFORCTH) then
        LMAXFORCT=LMAXFORCTH
     endif





      ALPH   =  float(itime_step)/ZTAU 
      ZSIGMA2=1./(12.0*ALPH)

      if (is_print) then
      IF (variable == 'W') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing STREAMFUNCTION forcing pattern of  << '')')
      WRITE(*,'('' >> stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_PSI '',E12.5)') TOT_BACKSCAT
      WRITE(*,'('' Exponent for energy spectra, REXPONENT_PSI ='',E12.5)') REXPONENT
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, LMINFORC ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, LMAXFORC ='',I10)') LMAXFORCT
      WRITE(*,'('' Minimal wavenumber of streamfunction forcing, KMINFORC ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of streamfunction forcing, KMAXFORC ='',I10)') KMAXFORCT
      WRITE(*,'('' skebs_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Time step: itime_step='',I10)') itime_step
      WRITE(*,'('' Decorrelation time of noise, ZTAU_PSI ='',E12.5)') ZTAU
      WRITE(*,'('' Variance of noise, ZSIGMA2_EPS  ='',E12.5)') ZSIGMA2
      WRITE(*,'('' Autoregressive parameter 1-ALPH_PSI ='',E12.5)') 1.-ALPH
      WRITE(*,'('' =============================================='')')


      ELSEIF (variable == 'T') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing TEMPERATURE forcing pattern of  << '')')
      WRITE(*,'('' >> stochastic kinetic-energy backscatter scheme << '')')
      WRITE(*,'('' Total backscattered energy, TOT_BACKSCAT_T   '',E12.5)') TOT_BACKSCAT
      WRITE(*,'('' Exponent for energy spectra, REXPONENT_T   ='',E12.5)') REXPONENT
      WRITE(*,'('' Minimal wavenumber of tempearature forcing, LMINFORC ='',I10)') LMINFORCT
      WRITE(*,'('' Maximal wavenumber of tempearature forcing, LMAXFORC ='',I10)') LMAXFORCT
      WRITE(*,'('' Minimal wavenumber of tempearature forcing, KMINFORC ='',I10)') KMINFORCT
      WRITE(*,'('' Maximal wavenumber of tempearature forcing, KMAXFORC ='',I10)') KMAXFORCT
      WRITE(*,'('' skebs_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, ZTAU_T ='',E12.5)') ZTAU
      WRITE(*,'('' Variance of noise, ZSIGMA2_ETA  ='',E12.5)') ZSIGMA2
      WRITE(*,'('' Autoregressive parameter 1-ALPH_T ='',E12.5)') 1.-ALPH
      WRITE(*,'('' =============================================='')')
      endif

     IF ((variable == 'P') .or. (variable == 'R') .or. (variable == 'S') .or. (variable == 'Q') .or. (variable == 'O')) then
      kappat= L_rand_perturb**2 
      phi = exp (-float(itime_step)/tau_rand_perturb)
      alph = 1.-phi
     endif 




     if (variable == 'P') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing Stochastically Perturbed Physics Tendency scheme << '')')
      WRITE(*,'('' sppt_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
      endif 




     if (variable == 'R') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing random perturbations << '')')
      WRITE(*,'('' rand_pert_vertstruc                             '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
     endif 

     if (variable == 'S') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastic parameter perturbations for convection<< '')')
      WRITE(*,'('' rand_pert_vertstruc2                            '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
     endif 

     if (variable == 'Q') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastic parameter perturbations for PBL<< '')')
      WRITE(*,'('' rand_pert_vertstruc3                            '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
     endif 

     if (variable == 'O') then
      WRITE(*,'(''                                               '')')
      WRITE(*,'('' =============================================='')')
      WRITE(*,'('' >> Initializing stochastic parameter perturbations for LSM<< '')')
      WRITE(*,'('' rand_pert_vertstruc4                            '',I10)') skebs_vertstruc
      WRITE(*,'('' Decorrelation time of noise, Tau ='',E12.5)') tau_rand_perturb
      WRITE(*,'('' Autoregressive parameter Phi ='',E12.5)') phi
      WRITE(*,'('' Length Scale L'',E12.5)') l_rand_perturb
      WRITE(*,'('' Variance in gridpoint space'',E12.5)') gridpt_stddev_rand_perturb
      WRITE(*,'('' =============================================='')')
     endif 

     endif 




     ZCHI    =  0.0
     ZGAMMAN  =  0.0
      
      DO IK=jds-1,jde   
      DO IL=ids-1,ide
      if (((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((KMAXFORCT+0.5)/RX)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((KMINFORCT-0.5)/RX))) .or. &
          ((sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).lt.((LMAXFORCT+0.5)/RY)).and.&
           (sqrt((IK/RY*IK/RY)+(IL/RX*IL/RX)).ge.((LMINFORCT-0.5)/RY))))then
        if ((IK>0).or.(IL>0)) then
          if (variable == 'W') then
            ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)  
            ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT+1)        
          else if (variable == 'T') then
            ZCHI(IL+1,IK+1)=((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT/2.)  
            ZGAMMAN= ZGAMMAN + ((IK/RY*IK/RY)+(IL/RX*IL/RX))**(REXPONENT)          
          else if ((variable == 'P') .or. (variable == 'R') .or.  (variable == 'S') .or. (variable == 'O') .or.  (variable == 'Q     ')) then
            ZCHI(IL+1,IK+1)=exp(  -2*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) ) 
            ZGAMMAN= ZGAMMAN + exp(  -4*RPI**2*kappat*((IK/RY*IK/RY)+(IL/RX*IL/RX)) ) 
          endif
        endif
      endif
      enddo
      enddo
      ZGAMMAN=4.0*ZGAMMAN 
      if (variable == 'W') then
         ZCONSTF0=SQRT(ALPH*TOT_BACKSCAT/(float(itime_step)*ZSIGMA2*ZGAMMAN))/(2*RPI)
      elseif  (variable == 'T') then     
         ZCONSTF0=SQRT(T0*ALPH*TOT_BACKSCAT/(float(itime_step)*cp*ZSIGMA2*ZGAMMAN))
      elseif ((variable == 'P') .or. (variable == 'R') .or.  (variable == 'S') .or. (variable == 'O') .or.  (variable == 'Q     ')) then
         ZCONSTF0= gridpt_stddev_rand_perturb*sqrt((1.-phi**2)/(2.*ZGAMMAN))
      endif
  






      SP_AMP=0.0
      DO IK=jts,jte
      DO IL=its,ite
      if ((IL .le. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SP_AMP(IL,IK)      = ZCONSTF0*ZCHI(IL,IK)
      endif
      ENDDO
      ENDDO

      
      
      DO IK=jts,jte
      DO IL=its,ite
      if ( (IL .gt. (LMAX/2+1)) .and. (IK .le. (KMAX/2+1)) ) then
        SP_AMP(IL,IK)      =  ZCONSTF0*ZCHI(LMAX-IL+2,IK)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.le.LMAX/2) ) then
        SP_AMP(IL,IK)      = ZCONSTF0*ZCHI(IL,KMAX-IK+2)
      endif
      ENDDO
      ENDDO


      DO IK=jts,jte
      DO IL=its,ite
      if ((IK .gt. (KMAX/2+1)) .and. (IL.gt.LMAX/2) ) then
        SP_AMP(IL,IK)      = ZCONSTF0*ZCHI(LMAX-IL+2,KMAX-IK+2)
      endif
      ENDDO
      ENDDO



      VERTAMP=1.0 

      IF (skebs_vertstruc==1) then
        VERTSTRUCC=0.0
        VERTSTRUCS=0.0
        RHOKLMAX= sqrt(KMAX**2/DY**2 + LMAX**2/DX**2)
        ZREF=32.0
        DO ILEV=kts,kte
          DO IK=jts,jte
            DO IL=its,ite
            if (IL.le.(LMAX/2)) then
              RHOKL   = sqrt((IK+1)**2/DY**2 + (IL+1)**2/DX**2)
              EPS     = ((RHOKLMAX - RHOKL)/ RHOKLMAX)  * (ILEV/ZREF) * RPI
              VERTSTRUCC(IL,ILEV,IK) =  cos ( eps* (IL+1) )
              VERTSTRUCS(IL,ILEV,IK) =  sin ( eps* (IL+1) )
             else
              RHOKL   = sqrt((IK+1)**2/DY**2 + (LMAX-IL+2)**2/DX**2)
              EPS     = ((RHOKLMAX - RHOKL)/ RHOKLMAX)  * (ILEV/ZREF) * RPI
              VERTSTRUCC (IL,ILEV,IK) =   cos ( eps* (LMAX-IL+2) )
              VERTSTRUCS (IL,ILEV,IK) = - sin ( eps* (LMAX-IL+2) )
            endif
            ENDDO
          ENDDO
        ENDDO
      ENDIF

     END subroutine SETUP_RAND_PERTURB





     subroutine UPDATE_STOCH(                                         & 
                      SPFORCS,SPFORCC,SP_AMP,ALPH,                    &
                      restart,iseedarr,                             &
                      ids, ide, jds, jde, kds, kde,                   &
                      ims, ime, jms, jme, kms, kme,                   &
                      its, ite, jts, jte, kts, kte                    )
     IMPLICIT NONE

     REAL, DIMENSION( ids:ide,jds:jde)      :: ZRANDNOSS,ZRANDNOSC
     REAL, DIMENSION (ims:ime,jms:jme)      :: SPFORCS,SPFORCC,SP_AMP
     INTEGER , INTENT(IN)     ::               ids, ide, jds, jde, kds, kde,   &
                                               ims, ime, jms, jme, kms, kme,   &
                                               its, ite, jts, jte, kts, kte
   
     INTEGER, DIMENSION (kms:kme),             INTENT(INOUT) :: iseedarr
     INTEGER , ALLOCATABLE , DIMENSION(:) :: iseed
     REAL :: Z,ALPH
     REAL, PARAMETER :: thresh = 3.0
     INTEGER ::IL, IK,LMAX,KMAX
     INTEGER :: how_many
     LOGICAL :: LGAUSS,RESTART

     KMAX=(jde-jds)+1 
     LMAX=(ide-ids)+1 

           CALL random_seed(size=how_many)
           IF ( ALLOCATED(iseed)) DEALLOCATE(iseed)
           ALLOCATE(iseed(how_many))
           iseed=iseedarr(1:how_many)
           call random_seed(put=iseed(1:how_many))




     LGAUSS=.true.
     IF (LGAUSS) then
       DO IK=jds,jde
         DO IL=ids,ide
          do
           call gauss_noise(z)
           if (abs(z)<thresh) exit
          ENDDO
          ZRANDNOSS(IL,IK)=z
          do
           call gauss_noise(z)
           if (abs(z)<thresh) exit
          ENDDO
          ZRANDNOSC(IL,IK)=z
         ENDDO
       ENDDO
     ELSE
       DO IK=jds,jde
         DO IL=ids,ide
           CALL RANDOM_NUMBER(z)
           ZRANDNOSS(IL,IK)=z-0.5
           CALL RANDOM_NUMBER(z)
           ZRANDNOSC(IL,IK)=z-0.5
          ENDDO
        ENDDO
      ENDIF




      DO IK=jts,jte
      if ((IK.le.(KMAX/2+1)) .and. (IK>1)) then 
        DO IL=its,ite
          SPFORCC(IL,IK)       = (1.-ALPH)*SPFORCC(IL,IK)      + SP_AMP(IL,IK)     * ZRANDNOSC(IL,IK)  
          SPFORCS(IL,IK)       = (1.-ALPH)*SPFORCS(IL,IK)      + SP_AMP(IL,IK)     * ZRANDNOSS(IL,IK)  
        ENDDO
      ELSEIF (IK==1) then
        DO IL=its,ite
        if ((IL.le.(LMAX/2+1))) then
          SPFORCC(IL,IK)       = (1.-ALPH)*SPFORCC(IL,IK)      + SP_AMP(IL,IK)     * ZRANDNOSC(IL,IK)  
          SPFORCS(IL,IK)       = (1.-ALPH)*SPFORCS(IL,IK)      + SP_AMP(IL,IK)     * ZRANDNOSS(IL,IK)  
        elseif ((IL.gt.(LMAX/2+1))) then
          SPFORCC(IL,IK)       = (1.-ALPH)*SPFORCC(IL,IK)      + SP_AMP(IL,IK)     * ZRANDNOSC(LMAX-IL+2,IK)  
          SPFORCS(IL,IK)       = (1.-ALPH)*SPFORCS(IL,IK)      - SP_AMP(IL,IK)     * ZRANDNOSS(LMAX-IL+2,IK)  
        endif
        ENDDO
      ENDIF
      ENDDO

      DO IK=jts,jte
      if (IK.gt.(KMAX/2+1)) then 
        DO IL=its,ite
          if (IL.le.(LMAX/2+1).and.(IL.gt.1)) then 
           SPFORCC(IL,IK)      = (1.-ALPH)* SPFORCC(IL,IK)      + SP_AMP(IL,IK)      * ZRANDNOSC(LMAX-IL+2,KMAX-IK+2)
           SPFORCS(IL,IK)      = (1.-ALPH)* SPFORCS(IL,IK)      - SP_AMP(IL,IK)      * ZRANDNOSS(LMAX-IL+2,KMAX-IK+2)
          elseif (IL.eq.1) then 
           SPFORCC(IL,IK)      = (1.-ALPH)* SPFORCC(IL,IK)      + SP_AMP(IL,IK)      * ZRANDNOSC(        1,KMAX-IK+2)
           SPFORCS(IL,IK)      = (1.-ALPH)* SPFORCS(IL,IK)      - SP_AMP(IL,IK)      * ZRANDNOSS(        1,KMAX-IK+2)
          elseif (IL.gt.(LMAX/2+1)) then 
           SPFORCC(IL,IK)      = (1.-ALPH)* SPFORCC(IL,IK)      + SP_AMP(IL,IK)      * ZRANDNOSC(LMAX-IL+2,KMAX-IK+2)
           SPFORCS(IL,IK)      = (1.-ALPH)* SPFORCS(IL,IK)      - SP_AMP(IL,IK)      * ZRANDNOSS(LMAX-IL+2,KMAX-IK+2)
          endif
        ENDDO
      endif
      ENDDO

      call random_seed(get=iseed(1:how_many))
      iseedarr=0.0
      iseedarr(1:how_many)=iseed

     END subroutine UPDATE_STOCH

      SUBROUTINE UPDATE_STOCH_TEN(ru_tendf,rv_tendf,t_tendf, &
                       ru_tendf_stoch,rv_tendf_stoch,rt_tendf_stoch,& 
                       mu,mub,c1h,c2h,                              & 
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte,                &
                       kte_stoch,kme_stoch                          )

       IMPLICIT NONE
       INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte,   & 
                                       kte_stoch,kme_stoch

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) :: &
                                       ru_tendf, rv_tendf, t_tendf

       REAL , DIMENSION(ims:ime , kms:kme_stoch, jms:jme)           :: &
                      ru_tendf_stoch,rv_tendf_stoch,rt_tendf_stoch 

       REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub
       REAL , DIMENSION(kms:kme) , INTENT(IN) :: c1h,c2h

       INTEGER :: I,J,K,kh
       REAL  :: dt,xm

   
       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,ite 
             ru_tendf(i,k,j) = ru_tendf(i,k,j) +  ru_tendf_stoch(i,kh,j)  * (mu(i,j)+mub(i,j)) 
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,jte
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,MIN(ide-1,ite)
             rv_tendf(i,k,j) = rv_tendf(i,k,j) +  rv_tendf_stoch(i,kh,j) *  (mu(i,j)+mub(i,j)) 
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
           kh=min(k,kte_stoch)
           DO i = its,MIN(ide-1,ite)
             t_tendf(i,k,j) = t_tendf(i,k,j) + rt_tendf_stoch(i,kh,j) * (mu(i,j)+mub(i,j))
           ENDDO
         ENDDO
       ENDDO

       END SUBROUTINE UPDATE_STOCH_TEN



      subroutine perturb_physics_tend(gridpt_stddev_sppt,               & 
                       sppt_thresh_fact,rstoch,                         & 
                       ru_tendf,rv_tendf,t_tendf,moist_tend,            &
                       ids, ide, jds, jde, kds, kde,                    &
                       ims, ime, jms, jme, kms, kme,                    &
                       its, ite, jts, jte, kts, kte,                    & 
                       kte_stoch,kme_stoch                               )










       IMPLICIT NONE
       INTEGER , INTENT(IN)        ::  ids, ide, jds, jde, kds, kde,   &
                                       ims, ime, jms, jme, kms, kme,   &
                                       its, ite, jts, jte, kts, kte,   & 
                                       kte_stoch,kme_stoch

       REAL , DIMENSION(ims:ime , kms:kme, jms:jme),INTENT(INOUT) ::   &
                                        ru_tendf, rv_tendf, t_tendf,moist_tend
       REAL , DIMENSION(ims:ime,kms:kme_stoch, jms:jme),INTENT(INOUT) :: rstoch
       REAL :: gridpt_stddev_sppt ,thresh,sppt_thresh_fact

       INTEGER :: I,J,K,kh



       thresh=sppt_thresh_fact*gridpt_stddev_sppt
       DO j = jts,jte
         DO k = kts,min(kte-1,kte_stoch-1)
           DO i = its,ite 

             if (rstoch(i,k,j).lt.-thresh) then
                 rstoch(i,k,j)=-thresh
             endif
             if (rstoch(i,k,j).gt.thresh) then
                 rstoch(i,k,j)=thresh
             endif
           ENDDO
         ENDDO
       ENDDO


       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 ) 
           DO i = its,ite 
              ru_tendf(i,k,j)      = ru_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,jte
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 ) 
            DO i = its,MIN(ide-1,ite)
              rv_tendf(i,k,j)      = rv_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO

       DO j = jts,MIN(jde-1,jte)
         DO k = kts,kte-1
         kh = min( k, kte_stoch-1 ) 
           DO i = its,MIN(ide-1,ite)
              moist_tend(i,k,j)    = moist_tend(i,k,j)*(1.0 + rstoch(i,kh,j))
              t_tendf   (i,k,j)    =    t_tendf(i,k,j)*(1.0 + rstoch(i,kh,j))
           ENDDO
         ENDDO
       ENDDO

      end subroutine perturb_physics_tend







      SUBROUTINE RAND_PERT_UPDATE       (grid, variable_in,                   &
                          SPFORCS,SPFORCC,SP_AMP,ALPH_RAND,                   &
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          kts, kte,                                           &
                          imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                          imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                          kpe_stoch,kde_stoch,kme_stoch,kte_stoch,            &
                          restart,iseedarr,                                   &
                          DX,DY,skebs_vertstruc,                          &
                          RAND_PERT,thresh_fact,gridpt_stddev,                &
                          VERTSTRUCC,VERTSTRUCS,VERTAMP                       )



    USE module_domain, ONLY : domain


      IMPLICIT NONE

      TYPE ( domain ), INTENT(INOUT) :: grid

 
      INTEGER , INTENT(IN)     ::               ids, ide, jds, jde, kds, kde,   &
                                                ims, ime, jms, jme, kms, kme,   &
                                                ips, ipe, jps, jpe, kps, kpe,   & 
                                                kts, kte                       
      INTEGER , INTENT(IN)     ::               imsx,imex,jmsx,jmex,kmsx,kmex, &
                                                ipsx,ipex,jpsx,jpex,kpsx,kpex, &
                                                imsy,imey,jmsy,jmey,kmsy,kmey, &
                                                ipsy,ipey,jpsy,jpey,kpsy,kpey
      INTEGER                  ::               kpe_stoch,kde_stoch,kme_stoch,kte_stoch 

      REAL    , INTENT(IN)     ::               ALPH_RAND,dx,dy,thresh_fact,gridpt_stddev
      INTEGER , INTENT(IN)     ::               skebs_vertstruc
      CHARACTER, INTENT(IN)    ::               variable_in 
                                               
                                               
                                               

      INTEGER, DIMENSION (kms:kme),             INTENT(INOUT) :: iseedarr
      REAL, DIMENSION(ims:ime,kms:kme, jms:jme),INTENT(IN)    :: VERTSTRUCC,VERTSTRUCS
      REAL, DIMENSION(ims:ime,jms:jme)         ,INTENT(INOUT) :: SPFORCS,SPFORCC,SP_AMP
      REAL, DIMENSION(kms:kme )                ,INTENT(IN)    :: VERTAMP
      REAL, DIMENSION(ims:ime,kms:kme_stoch, jms:jme)         :: RAND_PERT  
      REAL                     ::               RY,RX



      INTEGER                  ::               IK,IL,ILEV,NLON,NLAT,IJ,I,J,K
      INTEGER                  ::               gridsp32y,gridsm32y,gridsp32x,gridsm32x,gridsp32 ,gridsm32
      INTEGER                  ::               gridep32y,gridem32y,gridep32x,gridem32x,gridep32 ,gridem32
      REAL                     ::               thresh

      REAL, DIMENSION(ims:ime,kms:kme_stoch, jms:jme)         :: RAND_REAL, RAND_IMAG
      LOGICAL :: RESTART
      CHARACTER  :: variable

      variable = variable_in

      NLAT=(jde-jds)+1 
      NLON=(ide-ids)+1 
      RY=  NLAT*DY
      RX=  NLON*DX



      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
             IF (variable .ne. 'V') THEN  
              CALL UPDATE_STOCH( &
                         SPFORCS,SPFORCC,SP_AMP,ALPH_RAND,                   &
                         restart,iseedarr,                         &
                         ids, ide, jds, jde, kds, kde,                       &
                         ims, ime, jms, jme, kms, kme,                       &
                         grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte                        )      
             endif
  


              IF (variable == 'T')   THEN  
              DO IK=grid%j_start(ij), grid%j_end(ij)
               DO ILEV=kts,kte_stoch
                DO IL=grid%i_start(ij),grid%i_end(ij)
                       grid%RAND_REAL(IL,ILEV,IK)  = SPFORCC(IL,IK)
                       grid%RAND_IMAG(IL,ILEV,IK)  = SPFORCS(IL,IK)
                  ENDDO
                ENDDO
              ENDDO

              ELSEIF (variable == 'U') THEN 
              DO IK=grid%j_start(ij), grid%j_end(ij)
               DO ILEV=kts,kte_stoch
                DO IL=grid%i_start(ij),grid%i_end(ij)
                       grid%RAND_REAL(IL,ILEV,IK)  =  2*RPI/RY*  wavenumber_k(IK) * SPFORCS(IL,IK)
                       grid%RAND_IMAG(IL,ILEV,IK)  = -2*RPI/RY*  wavenumber_k(IK) * SPFORCC(IL,IK)
                  ENDDO
                ENDDO
              ENDDO

              ELSEIF (variable == 'V') THEN 
              DO IK=grid%j_start(ij), grid%j_end(ij)
               DO ILEV=kts,kte_stoch
                DO IL=grid%i_start(ij),grid%i_end(ij)
                       grid%RAND_REAL(IL,ILEV,IK)  = -2*RPI/RX*  wavenumber_l(IL) * SPFORCS(IL,IK)
                       grid%RAND_IMAG(IL,ILEV,IK)  =  2*RPI/RX*  wavenumber_l(IL) * SPFORCC(IL,IK)
                  ENDDO
                ENDDO
              ENDDO
              endif




           IF (skebs_vertstruc.ne.0) then
             DO ILEV=kts,kte_stoch
              DO IL=grid%i_start(ij),grid%i_end(ij)
                DO IK=grid%j_start(ij), grid%j_end(ij)
                  grid%RAND_REAL(IL,ILEV,IK)  = VERTAMP(ILEV) * & 
                       (grid%RAND_REAL(IL,ILEV,IK) * VERTSTRUCC(IL,ILEV,IK) - grid%RAND_IMAG(IL,ILEV,IK) * VERTSTRUCS(IL,ILEV,IK))
                  grid%RAND_IMAG(IL,ILEV,IK)  = VERTAMP(ILEV) * & 
                       (grid%RAND_REAL(IL,ILEV,IK) * VERTSTRUCS(IL,ILEV,IK) + grid%RAND_IMAG(IL,ILEV,IK) * VERTSTRUCC(IL,ILEV,IK))
                ENDDO
             ENDDO
           ENDDO
           ENDIF
        ENDDO
       !$OMP END PARALLEL DO


          
        call do_fftback_along_x(grid%RAND_REAL,grid%RAND_IMAG,                          &
                              ids,ide,jds,jde,                                          & 
                              ims,ime,jms,jme,kms,min(kme,grid%num_stoch_levels),       & 
                              ips,ipe,jps,jpe,kps,min(kpe,grid%num_stoch_levels))   
        call do_fftback_along_y(grid%RAND_REAL,grid%RAND_IMAG,                          & 
                              ids,ide,jds,jde,                                          & 
                              ims,ime,jms,jme,kms,min(kme,grid%num_stoch_levels),       & 
                              ips,ipe,jps,jpe,kps,min(kpe,grid%num_stoch_levels))


      thresh=thresh_fact*gridpt_stddev
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles
               DO k=kts,min(kte,grid%num_stoch_levels)
                 DO I=grid%i_start(ij), grid%i_end(ij)
                   DO j=grid%j_start(ij), grid%j_end(ij)
                     RAND_PERT(I,K,J)=grid%RAND_REAL(I,K,J) 
                     RAND_PERT(I,K,J)=MAX(MIN(grid%RAND_REAL(I,K,J),thresh),-1.0*thresh)
                   ENDDO
                 ENDDO
                ENDDO
        ENDDO
       !$OMP END PARALLEL DO


       END SUBROUTINE RAND_PERT_UPDATE




       subroutine do_fftback_along_x(                            & 
                               fieldc,fields,                    &
                               ids,ide,jds,jde,                  & 
                               imsx,imex,jmsx,jmex,kmsx,kmex,    &
                               ipsx,ipex,jpsx,jpex,kpsx,kpex     ) 
       IMPLICIT NONE
 
       INTEGER, INTENT(IN):: imsx,imex,jmsx,jmex,kmsx,kmex,    &
                             ipsx,ipex,jpsx,jpex,kpsx,kpex,    & 
                             ids,ide,jds,jde

  
       REAL, DIMENSION    (imsx:imex, kmsx:kmex, jmsx:jmex) :: fieldc,fields

       COMPLEX, DIMENSION (ipsx:ipex)            :: dummy_complex
       INTEGER                                   :: IER,LENWRK,KMAX,LMAX,I,J,K
       REAL, ALLOCATABLE                         :: WORK(:)

       CHARACTER (LEN=160) :: mess


       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1

       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8

       DO k=kpsx,kpex 
         DO j = jpsx, jpex
           DO i = ipsx, ipex
             dummy_complex(i)=cmplx(fieldc(i,k,j),fields(i,k,j))
           ENDDO
           CALL cFFT1B (LMAX, 1 ,dummy_complex,LMAX, WSAVE1, LENSAV, WORK, LENWRK, IER)
           if (ier.ne.0) then 
              WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_x, field U'
              CALL wrf_debug(0,mess)
           end if
           DO i = ipsx, ipex
             fieldc(i,k,j)=real(dummy_complex(i))
             fields(i,k,j)=imag(dummy_complex(i))
           END DO
         END DO
       END DO

       DEALLOCATE(WORK)
       end subroutine do_fftback_along_x




       subroutine do_fftback_along_y(                            &
                               fieldc,fields,                    &
                               ids,ide,jds,jde,                  & 
                               imsy,imey,jmsy,jmey,kmsy,kmey,    &
                               ipsy,ipey,jpsy,jpey,kpsy,kpey     )
       IMPLICIT NONE
 
       INTEGER :: IER,LENWRK,KMAX,LMAX,I,J,K,skebs_vertstruc
 
       INTEGER, INTENT(IN) :: imsy,imey,jmsy,jmey,kmsy,kmey,    &
                              ipsy,ipey,jpsy,jpey,kpsy,kpey,    & 
                              ids,ide,jds,jde
  
       REAL, DIMENSION    (imsy:imey, kmsy:kmey, jmsy:jmey) :: fieldc,fields

       COMPLEX, DIMENSION (jpsy:jpey)            :: dummy_complex
       REAL, ALLOCATABLE :: WORK(:)

       CHARACTER (LEN=160) :: mess

       KMAX=(jde-jds)+1
       LMAX=(ide-ids)+1
       LENWRK=2*KMAX*LMAX
       ALLOCATE(WORK(LENWRK))
       LENSAV= 4*(KMAX+LMAX)+INT(LOG(REAL(KMAX))) + INT(LOG(REAL(LMAX))) + 8



        DO k=kpsy,kpey
          DO i = ipsy, ipey
            DO j = jpsy,jpey
            dummy_complex(j)=cmplx(fieldc(i,k,j),fields(i,k,j))
            ENDDO
            CALL cFFT1B (KMAX, 1 ,dummy_complex,KMAX, WSAVE2, LENSAV, WORK, LENWRK, IER)
            if (ier.ne.0) then 
               WRITE(mess,FMT='(A)') 'error in cFFT1B in do_fftback_along_y, field U'
               CALL wrf_debug(0,mess)
            end if
            DO j = jpsy, jpey
            fieldc(i,k,j)=real(dummy_complex(j))
            fields(i,k,j)=imag(dummy_complex(j))
            END DO
          END DO
        END DO 

     
       DEALLOCATE(WORK)
       end subroutine do_fftback_along_y



      subroutine findindex( wavenumber_k, wavenumber_L,             & 
                       ids, ide, jds, jde, kds, kde,                &
                       ims, ime, jms, jme, kms, kme,                &
                       its, ite, jts, jte, kts, kte                 )

      IMPLICIT NONE
      INTEGER :: IK,IL,KMAX,LMAX
      INTEGER, DIMENSION (jds:jde)::  wavenumber_k
      INTEGER, DIMENSION (ids:ide)::  wavenumber_l
      INTEGER , INTENT(IN)     ::  ids, ide, jds, jde, kds, kde,   &
                                   ims, ime, jms, jme, kms, kme,   &
                                   its, ite, jts, jte, kts, kte
      KMAX=(jde-jds)+1 
      LMAX=(ide-ids)+1 

      
      DO IK=1,KMAX/2+1 
        wavenumber_k(IK)=IK-1 
      ENDDO
      DO IK=KMAX,KMAX/2+2,-1
        wavenumber_k(IK)=IK-KMAX-1 
      ENDDO
      DO IL=1,LMAX/2+1 
        wavenumber_l(IL)=IL-1
      ENDDO
      DO IL=LMAX,LMAX/2+2,-1
        wavenumber_l(IL)=IL-LMAX-1 
      ENDDO

      END subroutine findindex
       

     subroutine gauss_noise(z)
      real :: z                    
      real :: x,y,r, coeff         



      do

      call random_number( x )
      call random_number( y )



      x = 2.0 * x - 1.0
      y = 2.0 * y - 1.0
      r = x * x + y * y

      if ( r > 0.0 .and. r < 1.0 ) exit

      end do



      coeff = sqrt( -2.0 * log(r) / r )
      z = coeff * x

     end subroutine gauss_noise

     SUBROUTINE rand_seed (config_flags, iseed1, iseedarr, kms, kme)
     USE module_configure
     IMPLICIT NONE


      TYPE (grid_config_rec_type)                       :: config_flags


     INTEGER             :: kms, kme, iseed1 
     INTEGER, DIMENSION (kms:kme), INTENT(OUT)           :: iseedarr


      integer*8          :: fctime, one_big
      integer            :: i 

      fctime = config_flags%start_year * ( config_flags%start_month*100+config_flags%start_day) + config_flags%start_hour

      one_big = 1
      iseedarr=0.0
      do i = kms,kme-3,4
         iseedarr(i  )= iseed1+config_flags%nens*1000000
         iseedarr(i+1)= mod(fctime+iseed1*config_flags%nens*1000000,19211*one_big)
         iseedarr(i+2)= mod(fctime+iseed1*config_flags%nens*1000000,71209*one_big)
         iseedarr(i+3)= mod(fctime+iseed1*config_flags%nens*1000000,11279*one_big)
      enddo

      end SUBROUTINE rand_seed

      end module module_stoch
