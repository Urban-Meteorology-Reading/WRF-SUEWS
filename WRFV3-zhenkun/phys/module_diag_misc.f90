









MODULE module_diag_misc
      PRIVATE :: WGAMMA
      PRIVATE :: GAMMLN
CONTAINS
   SUBROUTINE diagnostic_output_calc(                                 &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & 
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,p8w,pk1m,mu_2,mu_2m                             &
                     ,u,v, temp                                       &
                     ,raincv,rainncv,rainc,rainnc                     &
                     ,i_rainc,i_rainnc                                &
                     ,hfx,sfcevp,lh                                   &
                     ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC               & 
                     ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC               & 
                     ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC               & 
                     ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC               & 
                     ,I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC       & 
                     ,I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC       & 
                     ,I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC       & 
                     ,I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC       & 
                     ,dt,xtime,sbw,t2                                 &
                     ,diag_print                                      &
                     ,bucket_mm, bucket_J                             &
                     ,mphysics_opt                                    &
                     ,gsfcgce_hail, gsfcgce_2ice                      &
                     ,mpuse_hail                                      &
                     ,nssl_cnoh, nssl_cnohl                           &
                     ,nssl_rho_qh, nssl_rho_qhl                       &
                     ,nssl_alphah, nssl_alphahl                       &
                     ,prec_acc_c, prec_acc_nc, snow_acc_nc            &
                     ,snowncv, prec_acc_dt, curr_secs2                &
                     ,nwp_diagnostics, diagflag                       &
                     ,history_interval                                &
                     ,itimestep                                       &
                     ,u10,v10,w                                       &
                     ,wspd10max                                       &
                     ,up_heli_max                                     &
                     ,w_up_max,w_dn_max                               &
                     ,znw,w_colmean                                   &
                     ,numcolpts,w_mean                                &
                     ,grpl_max,grpl_colint,refd_max,refl_10cm         &
                     ,hail_maxk1,hail_max2d                           &
                     ,qg_curr                                         &
                     ,ng_curr,qh_curr,nh_curr,qr_curr,nr_curr         & 
                     ,rho,ph,phb,g                                    &
                                                                      )


  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_state_description, ONLY :                                  &
      KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME, &
      WSM6SCHEME, ETAMPNEW, THOMPSON, THOMPSONAERO,                     &
      MORR_TWO_MOMENT, GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME,           &
      NSSL_2MOM, NSSL_2MOMG, NSSL_2MOMCCN, NSSL_1MOM, NSSL_1MOMLFO,                 &
      MILBRANDT2MOM , CAMMGMPSCHEME, FAST_KHAIN_LYNN, FULL_KHAIN_LYNN    

   IMPLICIT NONE

































































   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                  &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   diag_print
   REAL,      INTENT(IN   )    ::   bucket_mm, bucket_J
   INTEGER,   INTENT(IN   )    ::   mphysics_opt
   INTEGER, INTENT(IN) :: gsfcgce_hail, gsfcgce_2ice, mpuse_hail
   REAL, INTENT(IN)    ::   nssl_cnoh, nssl_cnohl                &
                           ,nssl_rho_qh, nssl_rho_qhl            &
                           ,nssl_alphah, nssl_alphahl
   INTEGER,   INTENT(IN   )    ::   nwp_diagnostics
   LOGICAL,   INTENT(IN   )    ::   diagflag

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                       u  &
                                                    ,         v  &
                                                    ,       p8w

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                           MU_2  &
                                                    ,   RAINNCV  &
                                                    ,    RAINCV  &
                                                    ,   SNOWNCV  &
                                                    ,       HFX  &
                                                    ,        LH  &
                                                    ,    SFCEVP  &  
                                                    ,        T2     

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                DPSDT  &
                                                    ,     DMUDT  &
                                                    ,    RAINNC  &
                                                    ,     RAINC  &
                                                    ,     MU_2M  &
                                                    ,      PK1M
 
   REAL,  INTENT(IN   ) :: DT, XTIME
   INTEGER,  INTENT(IN   ) :: SBW
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::     &
                                                       I_RAINC,  &
                                                       I_RAINNC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC
   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC,  &
                      I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC,  &
                      I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC,  &
                      I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      PREC_ACC_C, PREC_ACC_NC, SNOW_ACC_NC

   REAL, OPTIONAL, INTENT(IN)::  PREC_ACC_DT, CURR_SECS2

   INTEGER :: i,j,k,its,ite,jts,jte,ij
   INTEGER :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh

   REAL              :: no_points
   REAL              :: dpsdt_sum, dmudt_sum, dardt_sum, drcdt_sum, drndt_sum
   REAL              :: hfx_sum, lh_sum, sfcevp_sum, rainc_sum, rainnc_sum, raint_sum
   REAL              :: dmumax, raincmax, rainncmax, snowhmax
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256     :: outstring
   CHARACTER*6       :: grid_str

   INTEGER, INTENT(IN) ::                                        &
                                     history_interval,itimestep

   REAL, DIMENSION( kms:kme ), INTENT(IN) ::                     &
                                                            znw

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                                              w  &
                                                          ,temp  &
                                                       ,qg_curr  &
                                                           ,rho  &
                                                     ,refl_10cm  &
                                                        ,ph,phb

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), OPTIONAL, INTENT(IN) ::    &
                                       ng_curr, qh_curr, nh_curr        &
                                               ,qr_curr, nr_curr

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) ::            &
                                                            u10  &
                                                           ,v10

   REAL, INTENT(IN) :: g

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
                                                      wspd10max  &
                                                   ,up_heli_max  &
                                             ,w_up_max,w_dn_max  &
                                    ,w_colmean,numcolpts,w_mean  &
                                          ,grpl_max,grpl_colint  &
                                         ,hail_maxk1,hail_max2d  &
                                                      ,refd_max

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme):: temp_qg, temp_ng, temp_qr, temp_nr

   INTEGER :: idump

   REAL :: wind_vel
   REAL :: depth

      DOUBLE PRECISION:: hail_max
      REAL:: hail_max_sp
      DOUBLE PRECISION, PARAMETER:: thresh_conc = 0.001d0                  
      LOGICAL:: scheme_has_graupel
      INTEGER, PARAMETER:: ngbins=50
      DOUBLE PRECISION, DIMENSION(ngbins+1):: xxDx
      DOUBLE PRECISION, DIMENSION(ngbins):: xxDg, xdtg
      REAL:: xrho_g, xam_g, xbm_g, xmu_g
      REAL, DIMENSION(3):: cge, cgg
      DOUBLE PRECISION:: f_d, sum_ng, lamg, ilamg, N0_g, lam_exp, N0exp
      DOUBLE PRECISION:: lamr, N0min
      REAL:: mvd_r, xslw1, ygra1, zans1
      INTEGER:: ng, n




   IF(bucket_mm .gt. 0. .AND. MOD(NINT(XTIME),360) .EQ. 0)THEN

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      IF (xtime .eq. 0.0)THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_rainnc(i,j) = 0
          i_rainc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(rainnc(i,j) .gt. bucket_mm)THEN
          rainnc(i,j) = rainnc(i,j) - bucket_mm
          i_rainnc(i,j) =  i_rainnc(i,j) + 1
        ENDIF
        IF(rainc(i,j) .gt. bucket_mm)THEN
          rainc(i,j) = rainc(i,j) - bucket_mm
          i_rainc(i,j) =  i_rainc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO

      IF (xtime .eq. 0.0 .and. bucket_J .gt. 0.0 .and. PRESENT(ACSWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_acswupt(i,j) = 0
          i_acswuptc(i,j) = 0
          i_acswdnt(i,j) = 0
          i_acswdntc(i,j) = 0
          i_acswupb(i,j) = 0
          i_acswupbc(i,j) = 0
          i_acswdnb(i,j) = 0
          i_acswdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (xtime .eq. 0.0  .and. bucket_J .gt. 0.0 .and. PRESENT(ACLWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_aclwupt(i,j) = 0
          i_aclwuptc(i,j) = 0
          i_aclwdnt(i,j) = 0
          i_aclwdntc(i,j) = 0
          i_aclwupb(i,j) = 0
          i_aclwupbc(i,j) = 0
          i_aclwdnb(i,j) = 0
          i_aclwdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (PRESENT(ACSWUPT) .and. bucket_J .gt. 0.0)THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(acswupt(i,j) .gt. bucket_J)THEN
          acswupt(i,j) = acswupt(i,j) - bucket_J
          i_acswupt(i,j) =  i_acswupt(i,j) + 1
        ENDIF
        IF(acswuptc(i,j) .gt. bucket_J)THEN
          acswuptc(i,j) = acswuptc(i,j) - bucket_J
          i_acswuptc(i,j) =  i_acswuptc(i,j) + 1
        ENDIF
        IF(acswdnt(i,j) .gt. bucket_J)THEN
          acswdnt(i,j) = acswdnt(i,j) - bucket_J
          i_acswdnt(i,j) =  i_acswdnt(i,j) + 1
        ENDIF
        IF(acswdntc(i,j) .gt. bucket_J)THEN
          acswdntc(i,j) = acswdntc(i,j) - bucket_J
          i_acswdntc(i,j) =  i_acswdntc(i,j) + 1
        ENDIF
        IF(acswupb(i,j) .gt. bucket_J)THEN
          acswupb(i,j) = acswupb(i,j) - bucket_J
          i_acswupb(i,j) =  i_acswupb(i,j) + 1
        ENDIF
        IF(acswupbc(i,j) .gt. bucket_J)THEN
          acswupbc(i,j) = acswupbc(i,j) - bucket_J
          i_acswupbc(i,j) =  i_acswupbc(i,j) + 1
        ENDIF
        IF(acswdnb(i,j) .gt. bucket_J)THEN
          acswdnb(i,j) = acswdnb(i,j) - bucket_J
          i_acswdnb(i,j) =  i_acswdnb(i,j) + 1
        ENDIF
        IF(acswdnbc(i,j) .gt. bucket_J)THEN
          acswdnbc(i,j) = acswdnbc(i,j) - bucket_J
          i_acswdnbc(i,j) =  i_acswdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
      IF (PRESENT(ACLWUPT) .and. bucket_J .gt. 0.0)THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(aclwupt(i,j) .gt. bucket_J)THEN
          aclwupt(i,j) = aclwupt(i,j) - bucket_J
          i_aclwupt(i,j) =  i_aclwupt(i,j) + 1
        ENDIF
        IF(aclwuptc(i,j) .gt. bucket_J)THEN
          aclwuptc(i,j) = aclwuptc(i,j) - bucket_J
          i_aclwuptc(i,j) =  i_aclwuptc(i,j) + 1
        ENDIF
        IF(aclwdnt(i,j) .gt. bucket_J)THEN
          aclwdnt(i,j) = aclwdnt(i,j) - bucket_J
          i_aclwdnt(i,j) =  i_aclwdnt(i,j) + 1
        ENDIF
        IF(aclwdntc(i,j) .gt. bucket_J)THEN
          aclwdntc(i,j) = aclwdntc(i,j) - bucket_J
          i_aclwdntc(i,j) =  i_aclwdntc(i,j) + 1
        ENDIF
        IF(aclwupb(i,j) .gt. bucket_J)THEN
          aclwupb(i,j) = aclwupb(i,j) - bucket_J
          i_aclwupb(i,j) =  i_aclwupb(i,j) + 1
        ENDIF
        IF(aclwupbc(i,j) .gt. bucket_J)THEN
          aclwupbc(i,j) = aclwupbc(i,j) - bucket_J
          i_aclwupbc(i,j) =  i_aclwupbc(i,j) + 1
        ENDIF
        IF(aclwdnb(i,j) .gt. bucket_J)THEN
          aclwdnb(i,j) = aclwdnb(i,j) - bucket_J
          i_aclwdnb(i,j) =  i_aclwdnb(i,j) + 1
        ENDIF
        IF(aclwdnbc(i,j) .gt. bucket_J)THEN
          aclwdnbc(i,j) = aclwdnbc(i,j) - bucket_J
          i_aclwdnbc(i,j) =  i_aclwdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF


   IF (prec_acc_dt .gt. 0.) THEN

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         IF (mod(curr_secs2, 60.* prec_acc_dt) == 0.) THEN
            prec_acc_c(i,j)  = 0.
            prec_acc_nc(i,j) = 0.
            snow_acc_nc(i,j)  = 0.
         ENDIF
         prec_acc_c(i,j)  = prec_acc_c(i,j)  +  RAINCV(i,j)
         prec_acc_nc(i,j) = prec_acc_nc(i,j) + RAINNCV(i,j)
         prec_acc_c(i,j)  = MAX (prec_acc_c(i,j), 0.0)
         prec_acc_nc(i,j) = MAX (prec_acc_nc(i,j), 0.0)
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) + SNOWNCV(I,J)

         IF ( t2(i,j) .lt. 273.15 ) THEN
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) +  RAINCV(i,j)
         snow_acc_nc(i,j)   = MAX (snow_acc_nc(i,j), 0.0)
         ENDIF
      ENDDO     
      ENDDO     

   ENDDO     

!  !$OMP END PARALLEL DO
   ENDIF





   IF ( nwp_diagnostics .EQ. 1 ) THEN

     idump = (history_interval * 60.) / dt











   IF ( MOD((itimestep - 1), idump) .eq. 0 ) THEN
     WRITE(outstring,*) 'NSSL Diagnostics: Resetting max arrays for domain with dt = ', dt
     CALL wrf_debug ( 10,TRIM(outstring) )

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
         wspd10max(i,j)   = 0.
         up_heli_max(i,j) = 0.
         w_up_max(i,j)    = 0.
         w_dn_max(i,j)    = 0.
         w_mean(i,j)      = 0.
         grpl_max(i,j)    = 0.
         refd_max(i,j)    = 0.
         hail_maxk1(i,j)  = 0.
         hail_max2d(i,j)  = 0.
       ENDDO
       ENDDO
     ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO i=i_start(ij),i_end(ij)



       w_colmean(i,j)   = 0.
       numcolpts(i,j)   = 0.
       grpl_colint(i,j) = 0.
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme
     DO i=i_start(ij),i_end(ij)



       IF ( p8w(i,k,j) .GT. 40000. .AND. w(i,k,j) .GT. w_up_max(i,j) ) THEN
         w_up_max(i,j) = w(i,k,j)
       ENDIF

       IF ( p8w(i,k,j) .GT. 40000. .AND. w(i,k,j) .LT. w_dn_max(i,j) ) THEN
         w_dn_max(i,j) = w(i,k,j)
       ENDIF




       IF ( znw(k) .GE. 0.5 .AND. znw(k) .LE. 0.8 ) THEN
         w_colmean(i,j) = w_colmean(i,j) + w(i,k,j)
         numcolpts(i,j) = numcolpts(i,j) + 1
       ENDIF
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)



       depth = ( ( ph(i,k+1,j) + phb(i,k+1,j) ) / g ) - &
               ( ( ph(i,k  ,j) + phb(i,k  ,j) ) / g )
       grpl_colint(i,j) = grpl_colint(i,j) + qg_curr(i,k,j) * depth * rho(i,k,j)
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO i=i_start(ij),i_end(ij)



       wind_vel = sqrt ( u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j) )
       IF ( wind_vel .GT. wspd10max(i,j) ) THEN
         wspd10max(i,j) = wind_vel
       ENDIF



       w_mean(i,j) = w_mean(i,j) + w_colmean(i,j) / numcolpts(i,j)

       IF ( MOD(itimestep, idump) .eq. 0 ) THEN
         w_mean(i,j) = w_mean(i,j) / idump
       ENDIF



       IF ( grpl_colint(i,j) .gt. grpl_max(i,j) ) THEN
          grpl_max(i,j) = grpl_colint(i,j)
       ENDIF

   

       IF ( refl_10cm(i,kms,j) .GT. refd_max(i,j) ) THEN
         refd_max(i,j) = refl_10cm(i,kms,j)
       ENDIF
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO














      WRITE(outstring,*) 'GT-Diagnostics, computing max-hail diameter'
      CALL wrf_debug (100, TRIM(outstring))


   IF (PRESENT(qh_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has hail mixing ratio'
   CALL wrf_debug (150, TRIM(outstring))
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_qg(i,k,j) = MAX(1.E-12, qh_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSE
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_qg(i,k,j) = MAX(1.E-12, qg_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

   IF (PRESENT(nh_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has hail number concentration'
   CALL wrf_debug (150, TRIM(outstring))
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = MAX(1.E-8, nh_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSEIF (PRESENT(ng_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has graupel number concentration'
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = MAX(1.E-8, ng_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSE
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = 1.E-8
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF


      
      
      
      

      xrho_g = 500.
      xam_g = 3.1415926536/6.0*xrho_g     
      xbm_g = 3.                          
      xmu_g = 0.
      scheme_has_graupel = .false.

      
      

      cge(1) = xbm_g + 1.
      cge(2) = xmu_g + 1.
      cge(3) = xbm_g + xmu_g + 1.
      do n = 1, 3
         cgg(n) = WGAMMA(cge(n))
      enddo

   mp_select: SELECT CASE(mphysics_opt)

     CASE (KESSLERSCHEME)


     CASE (LINSCHEME)
       scheme_has_graupel = .true.
       xrho_g = 917.
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e4
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (WSM3SCHEME)


     CASE (WSM5SCHEME)


     CASE (WSM6SCHEME)
       scheme_has_graupel = .true.
       xrho_g = 500.
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e6
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (WDM5SCHEME)


     CASE (WDM6SCHEME)
       scheme_has_graupel = .true.
       xrho_g = 500.
       N0exp = 4.e6
       if (mpuse_hail .eq. 1) then
         xrho_g = 700.
         N0exp = 4.e4
       endif
       xam_g = 3.1415926536/6.0*xrho_g
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (GSFCGCESCHEME)
      if (gsfcgce_2ice.eq.0 .OR. gsfcgce_2ice.eq.2) then
       scheme_has_graupel = .true.
       if (gsfcgce_hail .eq. 1) then
          xrho_g = 900.
       else
          xrho_g = 400.
       endif
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e4
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO
      endif

     CASE (SBU_YLINSCHEME)



     CASE (ETAMPNEW)



     CASE (THOMPSON, THOMPSONAERO)

       scheme_has_graupel = .true.

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO k=kms,kme-1
       DO i=i_start(ij),i_end(ij)
          temp_qr(i,k,j) = MAX(1.E-10, qr_curr(i,k,j)*rho(i,k,j))
          temp_nr(i,k,j) = MAX(1.E-8, nr_curr(i,k,j)*rho(i,k,j))
       ENDDO
       ENDDO
       ENDDO
      ENDDO
!  !$OMP END PARALLEL DO

       
       
       
       

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
        N0min = 1.E6
        DO k=kme-1, kms, -1
         if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
         lamr = (1000.0*3.1415926536/6.0*cgg(3)/cgg(2)*temp_nr(i,k,j)/temp_qr(i,k,j))**(1./3.)
         mvd_r = 3.672 / lamr   
         mvd_r = MAX(100.E-6, MIN(mvd_r, 2.5E-3))
         if (temp(i,k,j).lt.270.65 .and. mvd_r.gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r)
         else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, temp_qg(i,k,j)))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0exp = 10.**(zans1)
         N0exp = MAX(DBLE(1.E4), MIN(N0exp, DBLE(1.E6)))
         N0min = MIN(N0exp, N0min)
         N0exp = N0min
         lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
         lamg = lam_exp * (cgg(3)/cgg(2)/cgg(1))**(1./xbm_g)
         N0_g = N0exp/(cgg(2)*lam_exp) * lamg**cge(2)
         temp_ng(i,k,j) = N0_g*cgg(2)*lamg**(-cge(2))
         if (N0exp .ge. 1.E4 .AND. N0exp.le.1.E5) then
         WRITE(outstring,*) 'GT-Debug, ', N0exp, temp_qr(i,k,j)*1000., temp_ng(i,k,j)
         CALL wrf_debug (850, TRIM(outstring))
         endif
        ENDDO
       ENDDO
       ENDDO
      ENDDO
!  !$OMP END PARALLEL DO






     CASE (MORR_TWO_MOMENT)
       scheme_has_graupel = .true.
       xrho_g = 400.
       if (mpuse_hail .eq. 1) xrho_g = 900.
       xam_g = 3.1415926536/6.0*xrho_g

     CASE (MILBRANDT2MOM)
       WRITE(outstring,*) 'GT-Debug, using Milbrandt2mom, which has 2-moment hail'
       CALL wrf_debug (150, TRIM(outstring))
       scheme_has_graupel = .true.
       xrho_g = 900.
       xam_g = 3.1415926536/6.0*xrho_g




     CASE (NSSL_1MOMLFO, NSSL_1MOM, NSSL_2MOM, NSSL_2MOMG, NSSL_2MOMCCN)

       scheme_has_graupel = .true.
       xrho_g = nssl_rho_qh
       N0exp = nssl_cnoh
       if (PRESENT(qh_curr)) then
          xrho_g = nssl_rho_qhl
          N0exp = nssl_cnohl
       endif
       xam_g = 3.1415926536/6.0*xrho_g

       if (PRESENT(ng_curr)) xmu_g = nssl_alphah
       if (PRESENT(nh_curr)) xmu_g = nssl_alphahl

       if (xmu_g .NE. 0.) then
          cge(1) = xbm_g + 1.
          cge(2) = xmu_g + 1.
          cge(3) = xbm_g + xmu_g + 1.
          do n = 1, 3
             cgg(n) = WGAMMA(cge(n))
          enddo
       endif

       
       

       if (.NOT.(PRESENT(nh_curr).OR.PRESENT(ng_curr)) ) then
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO
       endif




     CASE (CAMMGMPSCHEME)


     CASE (FULL_KHAIN_LYNN)



     CASE (FAST_KHAIN_LYNN)



     CASE DEFAULT

   END SELECT mp_select


   if (scheme_has_graupel) then


      xxDx(1) = 500.D-6
      xxDx(ngbins+1) = 0.075d0
      do n = 2, ngbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(ngbins) &
                  *DLOG(xxDx(ngbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, ngbins
         xxDg(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdtg(n) = xxDx(n+1) - xxDx(n)
      enddo


!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
        DO j=j_start(ij),j_end(ij)
        DO k=kms,kme-1
        DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lamg = (xam_g*cgg(3)/cgg(2)*temp_ng(i,k,j)/temp_qg(i,k,j))**(1./xbm_g)
           N0_g = temp_ng(i,k,j)/cgg(2)*lamg**cge(2)
           sum_ng = 0.0d0
           do ng = ngbins, 1, -1
              f_d = N0_g*xxDg(ng)**xmu_g * DEXP(-lamg*xxDg(ng))*xdtg(ng)
              sum_ng = sum_ng + f_d
              if (sum_ng .gt. thresh_conc) then
                 exit
              endif
           enddo
           if (ng .ge. ngbins) then
              hail_max = xxDg(ngbins)
           elseif (xxDg(ng+1) .gt. 1.E-3) then
              hail_max = xxDg(ng+1)
           else
              hail_max = 1.E-4
           endif
           if (hail_max .gt. 1E-2) then
            WRITE(outstring,*) 'GT-Debug-Hail, ', hail_max*1000.
            CALL wrf_debug (350, TRIM(outstring))
           endif
           hail_max_sp = hail_max
           if (k.eq.kms) then
              hail_maxk1(i,j) = MAX(hail_maxk1(i,j), hail_max_sp)
           endif
           hail_max2d(i,j) = MAX(hail_max2d(i,j), hail_max_sp)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!  !$OMP END PARALLEL DO

   endif


   ENDIF


   if (diag_print .eq. 0 ) return

   IF ( xtime .ne. 0. ) THEN


   no_points = float((ide-ids)*(jde-jds))


!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   dmumax = 0.
   DO ij = 1 , num_tiles


      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=(p8w(i,kms,j)-pk1m(i,j))/dt
         dmudt(i,j)=(mu_2(i,j)-mu_2m(i,j))/dt
         if(abs(dmudt(i,j)*dt).gt.dmumax)then
           dmumax=abs(dmudt(i,j)*dt)
           idp=i
           jdp=j
         endif
      ENDDO      
      ENDDO

   ENDDO
!  !$OMP END PARALLEL DO


   dmumax = dmumax*1.e-5

   CALL wrf_dm_maxval ( dmumax,  idp, jdp )



   dpsdt_sum = 0.
   dmudt_sum = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       dpsdt_sum = dpsdt_sum + abs(dpsdt(i,j))
       dmudt_sum = dmudt_sum + abs(dmudt(i,j))
     ENDDO
   ENDDO


   dpsdt_sum = wrf_dm_sum_real ( dpsdt_sum )
   dmudt_sum = wrf_dm_sum_real ( dmudt_sum )



   IF ( diag_print .eq. 2 ) THEN
   dardt_sum = 0.
   drcdt_sum = 0.
   drndt_sum = 0.
   rainc_sum = 0.
   raint_sum = 0.
   rainnc_sum = 0.
   sfcevp_sum = 0.
   hfx_sum = 0.
   lh_sum = 0.
   raincmax = 0.
   rainncmax = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       drcdt_sum = drcdt_sum + abs(raincv(i,j))
       drndt_sum = drndt_sum + abs(rainncv(i,j))
       dardt_sum = dardt_sum + abs(raincv(i,j)) + abs(rainncv(i,j))
       rainc_sum = rainc_sum + abs(rainc(i,j))

       IF(rainc(i,j).gt.raincmax)then
          raincmax=rainc(i,j)
          irc=i
          jrc=j
       ENDIF
       rainnc_sum = rainnc_sum + abs(rainnc(i,j))

       IF(rainnc(i,j).gt.rainncmax)then
          rainncmax=rainnc(i,j)
          irnc=i
          jrnc=j
       ENDIF
       raint_sum = raint_sum + abs(rainc(i,j)) + abs(rainnc(i,j))
       sfcevp_sum = sfcevp_sum + abs(sfcevp(i,j))
       hfx_sum = hfx_sum + abs(hfx(i,j))
       lh_sum = lh_sum + abs(lh(i,j))
     ENDDO
   ENDDO


   CALL wrf_dm_maxval ( raincmax, irc, jrc )
   CALL wrf_dm_maxval ( rainncmax, irnc, jrnc )


   drcdt_sum = wrf_dm_sum_real ( drcdt_sum )
   drndt_sum = wrf_dm_sum_real ( drndt_sum )
   dardt_sum = wrf_dm_sum_real ( dardt_sum )
   rainc_sum = wrf_dm_sum_real ( rainc_sum )
   rainnc_sum = wrf_dm_sum_real ( rainnc_sum )
   raint_sum = wrf_dm_sum_real ( raint_sum )
   sfcevp_sum = wrf_dm_sum_real ( sfcevp_sum )
   hfx_sum = wrf_dm_sum_real ( hfx_sum )
   lh_sum = wrf_dm_sum_real ( lh_sum )

   ENDIF



   CALL get_current_grid_name( grid_str )




     WRITE(outstring,*) grid_str,'Domain average of dpsdt, dmudt (mb/3h): ', xtime, &
           dpsdt_sum/no_points*108., &
           dmudt_sum/no_points*108.
     CALL wrf_message ( TRIM(outstring) )

     WRITE(outstring,*) grid_str,'Max mu change time step: ', idp,jdp,dmumax
     CALL wrf_message ( TRIM(outstring) )

     IF ( diag_print .eq. 2) THEN
     WRITE(outstring,*) grid_str,'Domain average of dardt, drcdt, drndt (mm/sec): ', xtime, &
           dardt_sum/dt/no_points, &
           drcdt_sum/dt/no_points, &
           drndt_sum/dt/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of rt_sum, rc_sum, rnc_sum (mm): ', xtime, &
           raint_sum/no_points, &
           rainc_sum/no_points, &
           rainnc_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Resolved Precip,   I,J  (mm): '               ,&
           rainncmax,irnc,jrnc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Convective Precip,   I,J  (mm): '             ,&
           raincmax,irc,jrc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of sfcevp, hfx, lh: ', xtime, &
           sfcevp_sum/no_points, &
           hfx_sum/no_points, &
           lh_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     ENDIF




   ENDIF


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         pk1m(i,j)=p8w(i,kms,j)
         mu_2m(i,j)=mu_2(i,j)
      ENDDO
      ENDDO

      IF ( xtime .lt. 0.0001 ) THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=0.
         dmudt(i,j)=0.
      ENDDO
      ENDDO
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE diagnostic_output_calc



      REAL FUNCTION GAMMLN(XX)

      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN


      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA



END MODULE module_diag_misc


