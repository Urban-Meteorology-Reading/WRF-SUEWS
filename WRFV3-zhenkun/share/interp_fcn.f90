MODULE module_interp_info
     INTEGER , PARAMETER :: NOT_DEFINED_YET    = 0
     INTEGER , PARAMETER :: BILINEAR           = 1
     INTEGER , PARAMETER :: SINT               = 2
     INTEGER , PARAMETER :: NEAREST_NEIGHBOR   = 3
     INTEGER , PARAMETER :: QUADRATIC          = 4
     INTEGER , PARAMETER :: SPLINE             = 5
     INTEGER , PARAMETER :: SINT_NEW           = 12

     INTEGER             :: interp_method_type = 0
CONTAINS
   SUBROUTINE interp_info_init
     CALL nl_get_interp_method_type ( 1 , interp_method_type )
   END SUBROUTINE interp_info_init
END MODULE module_interp_info







   SUBROUTINE interp_init
      USE module_interp_info
      CALL interp_info_init
   END SUBROUTINE interp_init



   SUBROUTINE interp_fcn    ( cfld,                                 &  
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  
                              imask,                                &  
                              xstag, ystag,                         &  
                              ipos, jpos,                           &  
                              nri, nrj                              )  

     USE module_interp_info      

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     IF      ( interp_method_type .EQ. NOT_DEFINED_YET  ) THEN
        interp_method_type = SINT
     END IF

     IF      ( interp_method_type .EQ. BILINEAR         ) THEN
       CALL interp_fcn_blint ( cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj)                                
     ELSE IF ( MOD(interp_method_type,10) .EQ. SINT             ) THEN
       CALL interp_fcn_sint  ( cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj)                                
     ELSE IF ( interp_method_type .EQ. NEAREST_NEIGHBOR ) THEN
       CALL interp_fcn_nn    ( cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj)                                
     ELSE IF ( interp_method_type .EQ. QUADRATIC        ) THEN
       CALL interp_fcn_lagr  ( cfld,                                 &  
                               cids, cide, ckds, ckde, cjds, cjde,   &
                               cims, cime, ckms, ckme, cjms, cjme,   &
                               cits, cite, ckts, ckte, cjts, cjte,   &
                               nfld,                                 &  
                               nids, nide, nkds, nkde, njds, njde,   &
                               nims, nime, nkms, nkme, njms, njme,   &
                               nits, nite, nkts, nkte, njts, njte,   &
                               shw,                                  &  
                               imask,                                &  
                               xstag, ystag,                         &  
                               ipos, jpos,                           &  
                               nri, nrj)                                
     ELSE
        CALL wrf_error_fatal3("<stdin>",124,&
'Hold on there cowboy, we need to know which interpolation option you want')
     END IF

   END SUBROUTINE interp_fcn





   SUBROUTINE interp_fcn_blint    ( cfld,                                 &  
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  
                              imask,                                &  
                              xstag, ystag,                         &  
                              ipos, jpos,                           &  
                              nri, nrj)                                

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, ioff, joff, i, j, k
     REAL :: wx, wy, cfld_ll, cfld_lr, cfld_ul, cfld_ur
     REAL :: cxp0, cxp1, nx, cyp0, cyp1, ny

     

     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     
     
     
     
     
     

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        cj = compute_CGLL ( nj , jpos , nrj , jstag )
        ny = REAL(nj)
        cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 
        cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

        

        wy = ( cyp1 - ny ) / ( cyp1 - cyp0 )

        

        k_loop : DO nk = nkts, nkte

          

           i_loop : DO ni = nits, MIN(nide-istag,nite)

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN
 
                 
   
                 ci = compute_CGLL ( ni , ipos , nri , istag )
                 nx = REAL(ni)
                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 
                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 
   
                 wx = ( cxp1 - nx ) / ( cxp1 - cxp0 )
   
                 
   
                 cfld_ll = cfld(ci  ,nk,cj  )
                 cfld_lr = cfld(ci+1,nk,cj  )
                 cfld_ul = cfld(ci  ,nk,cj+1)
                 cfld_ur = cfld(ci+1,nk,cj+1)

                 

                 nfld( ni , nk , nj ) =     wy  * ( cfld_ll * wx + cfld_lr * (1.-wx) ) + &
                                        (1.-wy) * ( cfld_ul * wx + cfld_ur * (1.-wx) )

              END IF
           END DO i_loop
        END DO    k_loop
     END DO       j_loop

   END SUBROUTINE interp_fcn_blint





   SUBROUTINE interp_fcn_blint_ll    ( cfld_inp,                                 &  
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  
                              imask,                                &  
                              xstag, ystag,                         &  
                              ipos, jpos,                           &  
                              nri, nrj,                             &  
                              clat_in, nlat_in,                     & 
                              cinput_from_file, ninput_from_file )    

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld_inp, cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: clat_in
     REAL, DIMENSION ( nims:nime,            njms:njme ) :: nlat_in
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     LOGICAL :: cinput_from_file, ninput_from_file

     

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, ioff, joff, i, j, k
     REAL :: wx, wy, cfld_ll, cfld_lr, cfld_ul, cfld_ur
     REAL :: cxp0, cxp1, nx, cyp0, cyp1, ny
     LOGICAL :: probably_by_dateline
     REAL :: max_lon, min_lon
     LOGICAL :: probably_by_pole
     REAL :: max_lat, min_lat

     

     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     
     
     
     
     
     

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     
     
     
     

     probably_by_pole = .FALSE.
     max_lat = -90
     min_lat = +90
     DO nj = njts, MIN(njde-jstag,njte)
        DO ni = nits, MIN(nide-istag,nite)
           max_lat = MAX ( nlat_in(ni,nj) , max_lat )       
           min_lat = MIN ( nlat_in(ni,nj) , min_lat )       
        END DO
     END DO

     IF ( ( max_lat .GT. 85 ) .OR. ( ABS(min_lat) .GT. 85 ) ) THEN
        probably_by_pole = .TRUE.
     END IF

     IF ( ( probably_by_pole ) .AND. ( .NOT. ninput_from_file ) ) THEN
        CALL wrf_error_fatal3("<stdin>",376,&
'Nest over the pole, single input domain, longitudes will be wrong' )
     END IF

     

     probably_by_dateline = .FALSE.
     max_lon = -180
     min_lon = +180
     DO nj = njts, MIN(njde-jstag,njte)
        cj = compute_CGLL ( nj , jpos , nrj , jstag )
        DO ni = nits, MIN(nide-istag,nite)
           ci = compute_CGLL ( ni , ipos , nri , istag )
           max_lon = MAX ( cfld_inp(ci,1,cj) , max_lon )       
           min_lon = MIN ( cfld_inp(ci,1,cj) , min_lon )       
        END DO
     END DO

     IF ( max_lon - min_lon .GT. 300 ) THEN
        probably_by_dateline = .TRUE.
     END IF

     

     DO cj = MIN(cjts-1,cjms), MAX(cjte+1,cjme)
       DO ci = MIN(cits-1,cims), MAX(cite+1,cime)
         IF ( ( cfld_inp(ci,ckts,cj) .LT. 0 ) .AND. ( probably_by_dateline ) ) THEN
           cfld(ci,ckts,cj) = 360 + cfld_inp(ci,ckts,cj)
         ELSE
           cfld(ci,ckts,cj) =       cfld_inp(ci,ckts,cj)
         END IF
       END DO
     END DO

     

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        cj = compute_CGLL ( nj , jpos , nrj , jstag )
        ny = REAL(nj)
        cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 
        cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

        

        wy = ( cyp1 - ny ) / ( cyp1 - cyp0 )

        

        k_loop : DO nk = nkts, nkte

          

           i_loop : DO ni = nits, MIN(nide-istag,nite)

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN
 
                 
   
                 ci = compute_CGLL ( ni , ipos , nri , istag )
                 nx = REAL(ni)
                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 
                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 
   
                 wx = ( cxp1 - nx ) / ( cxp1 - cxp0 )
   
                 
   
                 cfld_ll = cfld(ci  ,nk,cj  )
                 cfld_lr = cfld(ci+1,nk,cj  )
                 cfld_ul = cfld(ci  ,nk,cj+1)
                 cfld_ur = cfld(ci+1,nk,cj+1)

                 

                 nfld( ni , nk , nj ) =     wy  * ( cfld_ll * wx + cfld_lr * (1.-wx) ) + &
                                        (1.-wy) * ( cfld_ul * wx + cfld_ur * (1.-wx) )

              END IF
           END DO i_loop
        END DO    k_loop
     END DO       j_loop

     

     DO nj = njts, MIN(njde-jstag,njte)
        DO ni = nits, MIN(nide-istag,nite)
           IF ( nfld(ni,nkts,nj) .GT. 180 ) THEN
              nfld(ni,nkts,nj) = -360 + nfld(ni,nkts,nj)
           END IF
        END DO
    END DO

   END SUBROUTINE interp_fcn_blint_ll






   SUBROUTINE interp_fcn_lagr ( cfld,                                 &  
                                cids, cide, ckds, ckde, cjds, cjde,   &
                                cims, cime, ckms, ckme, cjms, cjme,   &
                                cits, cite, ckts, ckte, cjts, cjte,   &
                                nfld,                                 &  
                                nids, nide, nkds, nkde, njds, njde,   &
                                nims, nime, nkms, nkme, njms, njme,   &
                                nits, nite, nkts, nkte, njts, njte,   &
                                shw,                                  &  
                                imask,                                &  
                                xstag, ystag,                         &  
                                ipos, jpos,                           &  
                                nri, nrj)                                

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, i, j, k
     REAL :: nx, x0, x1, x2, x3, x
     REAL :: ny, y0, y1, y2, y3
     REAL :: cxm1, cxp0, cxp1, cxp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2
     REAL :: cym1, cyp0, cyp1, cyp2
     INTEGER :: ioff, joff

     

     REAL, EXTERNAL :: lagrange_quad_avg
     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     
     
     
     
     
     

     
     
     
     
     
     
     
     

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        

        
        
        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        
        
        

        
        
        
        
        
        
        
        
        

        cj = compute_CGLL ( nj , jpos , nrj , jstag )

        

        k_loop : DO nk = nkts, nkte

          

           i_loop : DO ni = nits, MIN(nide-istag,nite)
 
              

              ci = compute_CGLL ( ni , ipos , nri , istag )

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

              

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN

                 

                 nx = REAL(ni)

                 

                 cxm1 = nest_loc_of_cg ( ci-1 , ipos , nri , ioff ) 

                 

                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 

                 

                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 

                 

                 cxp2 = nest_loc_of_cg ( ci+2 , ipos , nri , ioff ) 

                 

                 nfld_m1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj-1), cfld(ci+0,nk,cj-1), cfld(ci+1,nk,cj-1), cfld(ci+2,nk,cj-1) )

                 

                 nfld_p0 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+0), cfld(ci+0,nk,cj+0), cfld(ci+1,nk,cj+0), cfld(ci+2,nk,cj+0) )

                 

                 nfld_p1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+1), cfld(ci+0,nk,cj+1), cfld(ci+1,nk,cj+1), cfld(ci+2,nk,cj+1) )

                 

                 nfld_p2 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, cfld(ci-1,nk,cj+2), cfld(ci+0,nk,cj+2), cfld(ci+1,nk,cj+2), cfld(ci+2,nk,cj+2) )

                 

                 ny = REAL(nj)

                 

                 cym1 = nest_loc_of_cg ( cj-1 , jpos , nrj , joff ) 

                 

                 cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 

                 

                 cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

                 

                 cyp2 = nest_loc_of_cg ( cj+2 , jpos , nrj , joff ) 

                 

                 nfld(ni,nk,nj) = lagrange_quad_avg ( ny, cym1, cyp0, cyp1,    &
                                                      cyp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2 )

              END IF

           END DO i_loop
        END DO    k_loop
     END DO       j_loop

   END SUBROUTINE interp_fcn_lagr



   REAL FUNCTION lagrange_quad ( x , x0, x1, x2, y0, y1, y2 )

     IMPLICIT NONE 

     REAL :: x , x0, x1, x2, y0, y1, y2

     
     
     
     
     
     

     lagrange_quad = &
            (x-x1)*(x-x2)*y0 / ( (x0-x1)*(x0-x2) ) + &
            (x-x0)*(x-x2)*y1 / ( (x1-x0)*(x1-x2) ) + &
            (x-x0)*(x-x1)*y2 / ( (x2-x0)*(x2-x1) ) 

   END FUNCTION lagrange_quad



   REAL FUNCTION lagrange_quad_avg ( x , x0, x1, x2, x3, y0, y1, y2, y3 )

     IMPLICIT NONE

     REAL, EXTERNAL :: lagrange_quad
     REAL :: x , x0, x1, x2, x3, y0, y1, y2, y3

     
     
     
    
     
     

     lagrange_quad_avg = & 



       ( lagrange_quad ( x , x0, x1, x2,     y0, y1, y2     ) * ( x2 - x  ) + &
         lagrange_quad ( x ,     x1, x2, x3,     y1, y2, y3 ) * ( x  - x1 ) ) / &
         ( x2 - x1 )

   END FUNCTION lagrange_quad_avg



   REAL FUNCTION nest_loc_of_cg ( ci , ipos , nri , ioff )

     
     
     
     
     
     
     
     

     IMPLICIT NONE 

     INTEGER :: ci , ipos , nri , ioff

     nest_loc_of_cg = & 
       ( ci - ipos ) * nri + ( 1 - ioff ) * REAL ( nri + 1 ) / 2. + ioff

   END FUNCTION nest_loc_of_cg



   FUNCTION compute_CGLL ( ni , ipos , nri , istag ) RESULT ( CGLL_loc )

      IMPLICIT NONE

      INTEGER , INTENT(IN ) :: ni , ipos , nri , istag
      INTEGER :: CGLL_loc

      

      INTEGER :: starting_position , increments_of_CG_cells
      INTEGER :: location_of_LL_wrt_this_CG
      INTEGER :: ioff
      INTEGER , PARAMETER :: MOMENTUM_STAG   = 0
      INTEGER , PARAMETER :: MASS_POINT_STAG = 1

      starting_position = ipos
      increments_of_CG_cells = ( ni - 1 ) / nri
      ioff = MOD ( nri , 2 )

      IF      ( istag .EQ. MOMENTUM_STAG   ) THEN
         location_of_LL_wrt_this_CG =   MOD ( ( ni - 1 ) , nri )          /   ( nri + ioff )       - istag 
      ELSE IF ( istag .EQ. MASS_POINT_STAG ) THEN
         location_of_LL_wrt_this_CG = ( MOD ( ( ni - 1 ) , nri ) + ioff ) / ( ( nri + ioff ) / 2 ) - istag
      ELSE
         CALL wrf_error_fatal3("<stdin>",858,&
'Hold on there pard, there are only two staggerings I accept.' )
      END IF

      CGLL_loc = starting_position + increments_of_CG_cells + location_of_LL_wrt_this_CG




   END FUNCTION compute_CGLL





   SUBROUTINE interp_fcn_sint ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, nioff, njoff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER nrio2, nrjo2

     
     

     ioff  = 0 ; joff  = 0
     nioff = 0 ; njoff = 0
     IF ( xstag ) THEN 
       ioff = (nri-1)/2
       nioff = nri 
     ENDIF
     IF ( ystag ) THEN
       joff = (nrj-1)/2
       njoff = nrj
     ENDIF

     nrio2 = nri/2
     nrjo2 = nrj/2

     nfx = nri * nrj
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ci,cj,ip,jp,nk,ck,nf,icmask,psca )
     DO k = ckts, ckte
        icmask = .FALSE.
        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrjo2 + 1 )  
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nrio2 + 1 )    
                if ( ni .ge. nits-nioff-nrio2 .and. &
                     ni .le. nite+nioff+nrio2 .and. &
                     nj .ge. njts-njoff-nrjo2 .and. &
                     nj .le. njte+njoff+nrjo2 ) then
                  if ( ni.ge.nims.and.ni.le.nime.and.nj.ge.njms.and.nj.le.njme) then
                    if ( imask(ni,nj) .eq. 1 ) then
                      icmask( i, j ) = .TRUE.
                    endif
                  endif
                  if ( ni-nioff.ge.nims.and.ni.le.nime.and.nj-njoff.ge.njms.and.nj.le.njme) then
                    if (ni .ge. nits-nioff .and. nj .ge. njts-njoff ) then
                      if ( imask(ni-nioff,nj-njoff) .eq. 1) then
                        icmask( i, j ) = .TRUE.
                      endif
                    endif
                  endif
                endif
                psca(i,j,nf) = cfld(i,k,j)
              ENDDO           
           ENDDO              
        ENDDO                 






        CALL sint( psca,                     &
                   cims, cime, cjms, cjme, icmask,   &
                   cits-1, cite+1, cjts-1, cjte+1, nrj*nri, xstag, ystag )

        DO nj = njts, njte+joff
           cj = jpos + (nj-1) / nrj 
           jp = mod ( nj-1 , nrj )  
           nk = k
           ck = nk
           DO ni = nits, nite+ioff
               ci = ipos + (ni-1) / nri      
               ip = mod ( ni-1 , nri )  
               if ( ( ni-ioff .ge. nits ) .and. ( nj-joff .ge. njts ) ) then
                  if ( imask ( ni, nj ) .eq. 1 .or. imask ( ni-ioff, nj-joff ) .eq. 1  ) then
                    nfld( ni-ioff, nk, nj-joff ) = psca( ci , cj, ip+1 + (jp)*nri )
                  endif
               endif
           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE interp_fcn_sint





   SUBROUTINE interp_fcn_nn ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     INTEGER ci, cj, ck, ni, nj, nk

     
     
     

     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              if ( imask ( ni, nj ) .eq. 1 ) then
                ci = ipos + (ni-1) / nri      
                nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO

   END SUBROUTINE interp_fcn_nn



   SUBROUTINE interp_fcn_bl ( cfld,                                 &  
                              cids, cide, ckds, ckde, cjds, cjde,   &
                              cims, cime, ckms, ckme, cjms, cjme,   &
                              cits, cite, ckts, ckte, cjts, cjte,   &
                              nfld,                                 &  
                              nids, nide, nkds, nkde, njds, njde,   &
                              nims, nime, nkms, nkme, njms, njme,   &
                              nits, nite, nkts, nkte, njts, njte,   &
                              shw,                                  &  
                              imask,                                &  
                              xstag, ystag,                         &  
                              ipos, jpos,                           &  
                              nri, nrj,                             &  
                              cht, nht,                             &  
                              ct_max_p,nt_max_p,                    &  
                              cght_max_p,nght_max_p,                &  
                              cmax_p,nmax_p,                        &  
                              ct_min_p,nt_min_p,                    &  
                              cght_min_p,nght_min_p,                &  
                              cmin_p,nmin_p,                        &  
                              zn, p_top                             )  
     USE module_timing

     USE module_model_constants , ONLY : g , r_d, cp, p1000mb, t0

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( cims:cime, cjms:cjme ) :: cht, ct_max_p, cght_max_p, cmax_p, ct_min_p, cght_min_p, cmin_p
     REAL, DIMENSION ( nims:nime, njms:njme ) :: nht, nt_max_p, nght_max_p, nmax_p, nt_min_p, nght_min_p, nmin_p
     REAL, DIMENSION ( ckms:ckme ) :: zn
     REAL :: p_top
     REAL, EXTERNAL :: v_interp_col

     

     INTEGER ci, cj, ni, nj, nk, istag, jstag, i, j, k
     REAL :: wx, wy, nprs, cfld_ll, cfld_lr, cfld_ul, cfld_ur
     REAL , DIMENSION(ckms:ckme) :: cprs
     REAL    :: p00 , t00 , a , tiso , p_surf

     
     
     

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cpb
  
     

     REAL, DIMENSION ( cits-2:cite+2, cjts-2:cjte+2 ) :: cfld_max_p, cfld_min_p

     

     REAL, DIMENSION ( nits:nite, nkts:nkte, njts:njte ) :: npb

     

     CALL nl_get_base_pres  ( 1 , p00 )
     CALL nl_get_base_temp  ( 1 , t00 )
     CALL nl_get_base_lapse ( 1 , a   )
     CALL nl_get_iso_temp   ( 1 , tiso )

     
     
     
     
     
     

     IF ( xstag ) THEN
        istag = 0 
     ELSE
        istag = 1
     END IF

     IF ( ystag ) THEN
        jstag = 0 
     ELSE
        jstag = 1
     END IF

     
     
     
     
     
     
     
     
     

     DO j = cjts-2 , cjte+2
        DO i = cits-2 , cite+2
           p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*cht(i,j)/a/r_d ) **0.5 )
           DO k = ckts , ckte
              cpb(i,k,j) = zn(k)*(p_surf - p_top) + p_top  
           END DO
           IF ( ckte .EQ. ckme ) THEN
              cfld_max_p(i,j) = cght_max_p(i,j) * g
              cfld_min_p(i,j) = cght_min_p(i,j) * g
           ELSE
              cfld_max_p(i,j) = ct_max_p(i,j) * (p1000mb/cmax_p(i,j))**(r_d/cp) - t0
              cfld_min_p(i,j) = ct_min_p(i,j) * (p1000mb/cmin_p(i,j))**(r_d/cp) - t0
           END IF
        END DO
     END DO

     
     
     
     

     DO j = njts , MIN(njde-1,njte)
        DO i = nits , MIN(nide-1,nite)
           p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*nht(i,j)/a/r_d ) **0.5 )
           DO k = nkts , nkte
              npb(i,k,j) = zn(k)*(p_surf - p_top) + p_top  
           END DO
        END DO
     END DO

     

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        cj = jpos + (nj-1)/nrj - jstag + (MOD(nj-1,nrj)+1 + nrj/2)/nrj


        

        IF ( ystag ) THEN
           wy = 1. - ( REAL(MOD(nj+(nrj-1)/2,nrj)) / REAL(nrj) +  1. / REAL (2 * nrj) )
        ELSE
           wy = 1. - ( REAL(MOD(nj+(nrj-1)/2,nrj)) / REAL(nrj)                        )
        END IF

        

        k_loop : DO nk = nkts, nkte

          

           i_loop : DO ni = nits, MIN(nide-istag,nite)
 
              

              ci = ipos + (ni-1)/nri - istag + (MOD(ni-1,nri)+1 + nri/2)/nri

              

              IF ( xstag ) THEN
                 wx = 1. - ( REAL(MOD(ni+(nri-1)/2,nri)) / REAL(nri) +  1. / REAL (2 * nri) )
              ELSE
                 wx = 1. - ( REAL(MOD(ni+(nri-1)/2,nri)) / REAL(nri)                        )
              END IF

              

              IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
                 nprs =   npb(  ni  , nk , nj  ) 
              ELSE IF ( xstag ) THEN
                 nprs = ( npb(  ni-1, nk , nj  ) + npb(  ni  , nk , nj  ) ) * 0.5
              ELSE IF ( ystag ) THEN
                 nprs = ( npb(  ni  , nk , nj-1) + npb(  ni  , nk , nj  ) ) * 0.5
              END IF

              

              IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
                 cprs(:) =   cpb(ci  ,:,cj  )
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) =   cpb(ci+1,:,cj  )
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) =   cpb(ci  ,:,cj+1)
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) =   cpb(ci+1,:,cj+1)
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )

              ELSE IF ( xstag ) THEN
                 cprs(:) = ( cpb(ci  ,:,cj  ) + cpb(ci-1,:,cj  ) )*0.5 
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) = ( cpb(ci+1,:,cj  ) + cpb(ci  ,:,cj  ) )*0.5
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) = ( cpb(ci  ,:,cj+1) + cpb(ci-1,:,cj+1) )*0.5
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) = ( cpb(ci+1,:,cj+1) + cpb(ci  ,:,cj+1) )*0.5
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )
              ELSE IF ( ystag ) THEN
                 cprs(:) = ( cpb(ci  ,:,cj  ) + cpb(ci  ,:,cj-1) )*0.5
                 cfld_ll = v_interp_col ( cfld(ci  ,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj  , &
                                          cfld_max_p(ci  ,cj  ) , cmax_p(ci  ,cj  ) , cfld_min_p(ci  ,cj  ) , cmin_p(ci  ,cj  ) )
                 cprs(:) = ( cpb(ci+1,:,cj  ) + cpb(ci+1,:,cj-1) )*0.5
                 cfld_lr = v_interp_col ( cfld(ci+1,:,cj  ) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj  , &
                                          cfld_max_p(ci+1,cj  ) , cmax_p(ci+1,cj  ) , cfld_min_p(ci+1,cj  ) , cmin_p(ci+1,cj  ) )
                 cprs(:) = ( cpb(ci  ,:,cj+1) + cpb(ci  ,:,cj  ) )*0.5
                 cfld_ul = v_interp_col ( cfld(ci  ,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci  , cj+1, &
                                          cfld_max_p(ci  ,cj+1) , cmax_p(ci  ,cj+1) , cfld_min_p(ci  ,cj+1) , cmin_p(ci  ,cj+1) )
                 cprs(:) = ( cpb(ci+1,:,cj+1) + cpb(ci+1,:,cj  ) )*0.5
                 cfld_ur = v_interp_col ( cfld(ci+1,:,cj+1) , cprs(:) , ckms , ckme , ckte, nprs, ni, nj, nk, ci+1, cj+1, &
                                          cfld_max_p(ci+1,cj+1) , cmax_p(ci+1,cj+1) , cfld_min_p(ci+1,cj+1) , cmin_p(ci+1,cj+1) )
              END IF

              

              nfld( ni , nk , nj ) =     wy  * ( cfld_ll * wx + cfld_lr * (1.-wx) ) + &
                                     (1.-wy) * ( cfld_ul * wx + cfld_ur * (1.-wx) )

           END DO i_loop
        END DO    k_loop
     END DO       j_loop

     

     IF ( ckme .EQ. ckte ) THEN
        DO nj = njts,njte
           DO ni = nits, nite
              nfld(ni,nkts,nj) = 0.0
           END DO
        END DO
     END IF

   END SUBROUTINE interp_fcn_bl



   FUNCTION v_interp_col ( cfld_orig , cprs_orig , ckms , ckme , ckte , nprs, ni, nj, nk, ci, cj, &
                           cfld_max_p , cmax_p , cfld_min_p , cmin_p ) RESULT ( cfld_interp )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ni, nj, nk, ci, cj
      INTEGER , INTENT(IN) :: ckms , ckme , ckte
      REAL , DIMENSION(ckms:ckme) , INTENT(IN) :: cfld_orig , cprs_orig
      REAL , INTENT(IN) :: cfld_max_p , cmax_p , cfld_min_p , cmin_p
      REAL , INTENT(IN) :: nprs
      REAL :: cfld_interp

      

      INTEGER :: ck
      LOGICAL :: found
      CHARACTER(LEN=256) :: joe_mess
      REAL , DIMENSION(ckms:ckme+1+1) :: cfld , cprs

      

      cfld(1) = cfld_max_p
      cprs(1) = cmax_p

      cfld(ckte+2) = cfld_min_p
      cprs(ckte+2) = cmin_p

      DO ck = ckms , ckte
         cfld(ck+1) = cfld_orig(ck)
         cprs(ck+1) = cprs_orig(ck)
      END DO

      found = .FALSE.

      IF      ( cprs(ckms) .LT. nprs ) THEN
         cfld_interp = cfld(ckms)
         RETURN
      ELSE IF ( cprs(ckte+2) .GE. nprs ) THEN
         cfld_interp = cfld(ckte+2)
         RETURN
      END IF

      DO ck = ckms , ckte+1
         IF ( ( cprs(ck  ) .GE. nprs ) .AND. &
              ( cprs(ck+1) .LT. nprs ) ) THEN
            cfld_interp = ( cfld(ck  ) * ( nprs     - cprs(ck+1) ) + &
                            cfld(ck+1) * ( cprs(ck) - nprs       ) ) / &
                                         ( cprs(ck) - cprs(ck+1) )
            RETURN
         END IF
      END DO

      CALL wrf_error_fatal3("<stdin>",1389,&
'ERROR -- vertical interpolation for nest interp cannot find trapping pressures' )
   
   END FUNCTION v_interp_col




   SUBROUTINE copy_fcn ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ),INTENT(IN)  :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ),INTENT(IN)  :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     
     
     
     
     

     
     
     

     
     
 
     
     
     

     
     
     

     CALL nl_get_spec_zone( 1 , spec_zone )
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        IF      ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO










                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = (nri+1)/2 , (nri+1)/2 + nri*(nri-1) , nri
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO




                 ENDDO
              ENDDO
           ENDDO

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = ( nrj*nrj +1 )/2 - nrj/2 , ( nrj*nrj +1 )/2 - nrj/2 + nrj-1
                       ipoints = MOD((ijpoints-1),nri) + 1 - nri/2 - 1
                       jpoints = (ijpoints-1)/nri + 1 - nrj/2 - 1
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(    nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO




                 ENDDO
              ENDDO
           ENDDO

        END IF

     

     ELSE IF ( MOD(nrj,2) .EQ. 0) THEN
        IF ( ( .NOT. xstag ) .AND. ( .NOT. ystag ) ) THEN

        
        
        
        
        
        
   
        
        
        
        
   
        
        
        
        












































           
           
           
           
   
           
           
           
           
           
           
           
           
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + jstag
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + istag
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri * nrj
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri*nrj) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO





                 END DO
              END DO
           END DO

        

        ELSE IF ( (       xstag ) .AND. ( .NOT. ystag ) ) THEN



























           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri*nrj , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO



                 ENDDO
              ENDDO
           ENDDO

        

        ELSE IF ( ( .NOT. xstag ) .AND. (       ystag ) ) THEN
           DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
              nj = (cj-jpos)*nrj + 1
              DO ck = ckts, ckte
                 nk = ck
                 DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                    ni = (ci-ipos)*nri + 1
                    cfld( ci, ck, cj ) = 0.
                    DO ijpoints = 1 , nri
                       ipoints = MOD((ijpoints-1),nri)
                       jpoints = (ijpoints-1)/nri
                       cfld( ci, ck, cj ) =  cfld( ci, ck, cj ) + &
                                             1./REAL(nri    ) * nfld( ni+ipoints , nk , nj+jpoints )
                    END DO



                 ENDDO
              ENDDO
           ENDDO
        END IF
     END IF

     RETURN

   END SUBROUTINE copy_fcn




   SUBROUTINE copy_fcnm (  cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcnm




   SUBROUTINE copy_fcni ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN)  :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints
     INTEGER , PARAMETER :: passes = 2
     INTEGER spec_zone

     CALL nl_get_spec_zone( 1, spec_zone ) 
     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     IF( MOD(nrj,2) .NE. 0) THEN  

        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + jstag + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + istag + 1
                 cfld( ci, ck, cj ) =  nfld( ni  , nk , nj  )
              ENDDO
           ENDDO
        ENDDO

     ELSE  
        DO cj = MAX(jpos+spec_zone,cjts),MIN(jpos+(njde-njds)/nrj-jstag-spec_zone,cjte)
           nj = (cj-jpos)*nrj + 1
           DO ck = ckts, ckte
              nk = ck
              DO ci = MAX(ipos+spec_zone,cits),MIN(ipos+(nide-nids)/nri-istag-spec_zone,cite)
                 ni = (ci-ipos)*nri + 1
                 ipoints = nri/2 -1
                 jpoints = nrj/2 -1
                 cfld( ci, ck, cj ) =  nfld( ni+ipoints , nk , nj+jpoints )
              END DO
           END DO
        END DO

     END IF

     RETURN

   END SUBROUTINE copy_fcni



   SUBROUTINE vert_interp_vert_nesting ( cfld,                                 &  
                                         ids, ide, kds, kde, jds, jde,         & 
                                         ims, ime, kms, kme, jms, jme,         &
                                         its, ite, kts, kte, jts, jte,         &   
                                         pgrid_s_vert, pgrid_e_vert,           &  
					 cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c,   &  
                                         alt_u_c, alt_u_n)



   IMPLICIT NONE
   REAL, DIMENSION ( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                
   REAL, INTENT(IN)    :: cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1), INTENT(IN) :: alt_u_c
   REAL, DIMENSION(kde+1), INTENT(IN) :: alt_u_n

   
   
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1) :: pro_u_c           
   REAL, DIMENSION(kde+1) :: pro_u_n
   
   DO j = jms,jme
   DO i = ims,ime
   
      
    
      do k = pgrid_s_vert,pgrid_e_vert-1
      pro_u_c(k+1) = cfld(i,k,j)
      enddo 
      
      

      pro_u_c(1      ) = cf1_c*cfld(i,1,j) &
                       + cf2_c*cfld(i,2,j) &
                       + cf3_c*cfld(i,3,j)
   
      pro_u_c(pgrid_e_vert+1) = cfn_c *cfld(i,pgrid_e_vert-1,j) &
                              + cfn1_c*cfld(i,pgrid_e_vert-2,j)  
		       
      call inter_wrf_copy(pro_u_c, alt_u_c, pgrid_e_vert+1, pro_u_n, alt_u_n, kde+1)
      
      do k = 1,kde-1
         cfld(i,k,j) = pro_u_n(k+1)
      enddo	
            
   ENDDO
   ENDDO
  
   
   END SUBROUTINE vert_interp_vert_nesting
   
 

   SUBROUTINE vert_interp_vert_nesting_w ( cfld,                                 &  
                                           ids, ide, kds, kde, jds, jde,         & 
                                           ims, ime, kms, kme, jms, jme,         &
                                           its, ite, kts, kte, jts, jte,         &   
                                           pgrid_s_vert, pgrid_e_vert,           &  
                                           alt_w_c, alt_w_n)



   IMPLICIT NONE
   REAL, DIMENSION ( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert), INTENT(IN) :: alt_w_c
   REAL, DIMENSION(kde), INTENT(IN) :: alt_w_n

   
   
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert) :: pro_w_c           
   REAL, DIMENSION(kde) :: pro_w_n
   
   DO j = jms,jme
   DO i = ims,ime
   
      
    
      do k = pgrid_s_vert,pgrid_e_vert
      pro_w_c(k) = cfld(i,k,j)
      enddo 
	       
      call inter_wrf_copy(pro_w_c, alt_w_c, pgrid_e_vert, pro_w_n, alt_w_n, kde)
      
      do k = 1,kde
         cfld(i,k,j) = pro_w_n(k)
      enddo	
            
   ENDDO
   ENDDO
  
   
   END SUBROUTINE vert_interp_vert_nesting_w



   SUBROUTINE vert_interp_vert_nesting_1d ( cfld,                                 &  
                                            ids, ide, kds, kde, jds, jde,         & 
                                            ims, ime, kms, kme, jms, jme,         &
                                            its, ite, kts, kte, jts, jte,         &   
                                            pgrid_s_vert, pgrid_e_vert,           &  
					    cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c,   &  
                                            alt_u_c, alt_u_n)



   IMPLICIT NONE
   REAL, DIMENSION (kms:kme),INTENT(INOUT) :: cfld   
   INTEGER, INTENT(IN) :: ids, ide, kds, kde, jds, jde,   &
                          ims, ime, kms, kme, jms, jme,   &
                          its, ite, kts, kte, jts, jte
   INTEGER, INTENT(IN) :: pgrid_s_vert, pgrid_e_vert                
   REAL, INTENT(IN)    :: cf1_c, cf2_c, cf3_c, cfn_c, cfn1_c
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1), INTENT(IN) :: alt_u_c
   REAL, DIMENSION(kde+1), INTENT(IN) :: alt_u_n

   
   
   
   INTEGER :: i,j,k
   REAL, DIMENSION(pgrid_s_vert:pgrid_e_vert+1) :: pro_u_c           
   REAL, DIMENSION(kde+1) :: pro_u_n
   

      
    
      do k = pgrid_s_vert,pgrid_e_vert-1
      pro_u_c(k+1) = cfld(k)
      enddo 
      
      

      pro_u_c(1      ) = cf1_c*cfld(1) &
                       + cf2_c*cfld(2) &
                       + cf3_c*cfld(3)
   
      pro_u_c(pgrid_e_vert+1) = cfn_c *cfld(pgrid_e_vert-1) &
                              + cfn1_c*cfld(pgrid_e_vert-2)  
		       
      call inter_wrf_copy(pro_u_c, alt_u_c, pgrid_e_vert+1, pro_u_n, alt_u_n, kde+1)
      
      do k = 1,kde-1
         cfld(k) = pro_u_n(k+1)
      enddo	
            
   
   END SUBROUTINE vert_interp_vert_nesting_1d   
   
     
 


  SUBROUTINE inter_wrf_copy(pro_c,alt_c,kde_c,pro_n,alt_n,kde_n)
  


   IMPLICIT NONE
   INTEGER , INTENT(IN) :: kde_c,kde_n
   REAL , DIMENSION(kde_c) , INTENT(IN ) :: pro_c,alt_c
   REAL , DIMENSION(kde_n) , INTENT(IN ) :: alt_n
   REAL , DIMENSION(kde_n) , INTENT(OUT) :: pro_n

      real ,dimension(kde_c) :: a,b,c,d
      real :: p
      integer :: i,j


      call coeff_mon_wrf_copy(alt_c,pro_c,a,b,c,d,kde_c)

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


   END SUBROUTINE inter_wrf_copy




     subroutine  coeff_mon_wrf_copy(x,y,a,b,c,d,n)
     


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

      end  subroutine  coeff_mon_wrf_copy
      
  
  



   SUBROUTINE p2c ( cfld,                                 &  
                    cids, cide, ckds, ckde, cjds, cjde,   &
                    cims, cime, ckms, ckme, cjms, cjme,   &
                    cits, cite, ckts, ckte, cjts, cjte,   &
                    nfld,                                 &  
                    nids, nide, nkds, nkde, njds, njde,   &
                    nims, nime, nkms, nkme, njms, njme,   &
                    nits, nite, nkts, nkte, njts, njte,   &
                    shw,                                  &  
                    imask,                                &  
                    xstag, ystag,                         &  
                    ipos, jpos,                           &  
                    nri, nrj                              &  
                    )   
     USE module_configure
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     CALL  interp_fcn  (cfld,                             &  
                        cids, cide, ckds, ckde, cjds, cjde,   &
                        cims, cime, ckms, ckme, cjms, cjme,   &
                        cits, cite, ckts, ckte, cjts, cjte,   &
                        nfld,                             &  
                        nids, nide, nkds, nkde, njds, njde,   &
                        nims, nime, nkms, nkme, njms, njme,   &
                        nits, nite, nkts, nkte, njts, njte,   &
                        shw,                                  &  
                        imask,                                &  
                        xstag, ystag,                         &  
                        ipos, jpos,                           &  
                        nri, nrj                             )   

   END SUBROUTINE p2c



   SUBROUTINE c2f_interp ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &  
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  








                           parent_id,nest_id                        &
                           )   
     USE module_configure
     IMPLICIT NONE







                            
     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj,parent_id,nest_id            

     LOGICAL, INTENT(IN) :: xstag, ystag
     
     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask




     REAL cdt, ndt
     
     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     






     If ( nest_id .eq. 3 ) then 
     DO nj = njts, njte
     
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO
     ENDIF  
     RETURN

   END SUBROUTINE c2f_interp



   SUBROUTINE bdy_interp ( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           cbdy_xs, nbdy_xs,                     &  
                           cbdy_xe, nbdy_xe,                     &  
                           cbdy_ys, nbdy_ys,                     &  
                           cbdy_ye, nbdy_ye,                     &  
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              )  

     USE module_interp_info

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj

     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs, nbdy_xs, nbdy_txs
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe, nbdy_xe, nbdy_txe
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys, nbdy_ys, nbdy_tys
     REAL,  DIMENSION( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye, nbdy_ye, nbdy_tye
     REAL cdt, ndt

     

     INTEGER nijds, nijde, spec_bdy_width

     nijds = min(nids, njds)
     nijde = max(nide, njde)
     CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

     IF      ( interp_method_type .EQ. NOT_DEFINED_YET  ) THEN
        interp_method_type = SINT
     END IF

     IF      ( interp_method_type .EQ. SINT      ) THEN
         CALL bdy_interp1( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde ,                        &  
                           spec_bdy_width ,                      &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                     &  
                           cbdy_xe, nbdy_xe,                     &  
                           cbdy_ys, nbdy_ys,                     &  
                           cbdy_ye, nbdy_ye,                     &  
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              &  
                                                                 )

     ELSE IF      ( ( interp_method_type .EQ. BILINEAR         ) .OR. &
                    ( interp_method_type .EQ. NEAREST_NEIGHBOR ) .OR. &
                    ( interp_method_type .EQ. QUADRATIC        ) .OR. &
                    ( interp_method_type .EQ. SPLINE           ) .OR. &
                    ( interp_method_type .EQ. SINT_NEW         ) ) THEN 
         CALL bdy_interp2( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde ,                        &  
                           spec_bdy_width ,                      &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw, imask,                           &
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, nbdy_xs,                     &  
                           cbdy_xe, nbdy_xe,                     &  
                           cbdy_ys, nbdy_ys,                     &  
                           cbdy_ye, nbdy_ye,                     &  
                           cbdy_txs, nbdy_txs,                   &
                           cbdy_txe, nbdy_txe,                   &  
                           cbdy_tys, nbdy_tys,                   &
                           cbdy_tye, nbdy_tye,                   &
                           cdt, ndt                              &  
                                                                 )

     ELSE
        CALL wrf_error_fatal3("<stdin>",2390,&
'Hold on there cowboy #2, we need to know which nested lateral boundary interpolation option you want')
     END IF

   END SUBROUTINE bdy_interp



   SUBROUTINE bdy_interp1( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde, spec_bdy_width ,          &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, bdy_xs,                           &
                           cbdy_xe, bdy_xe,                           &
                           cbdy_ys, bdy_ys,                           &
                           cbdy_ye, bdy_ye,                           &
                           cbdy_txs, bdy_txs,                       &
                           cbdy_txe, bdy_txe,                       &
                           cbdy_tys, bdy_tys,                       &
                           cbdy_tye, bdy_tye,                       &
                           cdt, ndt                              &
                                        )


     USE module_state_description

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye   
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xs, bdy_txs
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xe, bdy_txe
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ys, bdy_tys
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ye, bdy_tye

     

     REAL*8 rdt
     INTEGER ci, cj, ck, ni, nj, nk, ni1, nj1, nk1, ip, jp, ioff, joff
     INTEGER nfx, ior
     PARAMETER (ior=2)
     INTEGER nf
     REAL psca1(cims:cime,cjms:cjme,nri*nrj)
     REAL psca(cims:cime,cjms:cjme,nri*nrj)
     LOGICAL icmask( cims:cime, cjms:cjme )
     INTEGER i,j,k
     INTEGER shw
     INTEGER spec_zone 
     INTEGER relax_zone
     INTEGER sz
     INTEGER n2ci,n
     INTEGER n2cj


     n2ci(n) = (n+ipos*nri-1)/nri
     n2cj(n) = (n+jpos*nrj-1)/nrj

     rdt = 1.D0/cdt

     shw = 0

     ioff  = 0 ; joff  = 0
     IF ( xstag ) THEN 
       ioff = MAX((nri-1)/2,1)
     ENDIF
     IF ( ystag ) THEN
       joff = MAX((nrj-1)/2,1)
     ENDIF

     
     

     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )
     sz = MIN(MAX( spec_zone, relax_zone + 1 ),spec_bdy_width)

     nfx = nri * nrj

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,ni,nj,ni1,nj1,ci,cj,ip,jp,nk,ck,nf,icmask,psca,psca1 )
     DO k = ckts, ckte

        DO nf = 1,nfx
           DO j = cjms,cjme
              nj = (j-jpos) * nrj + ( nrj / 2 + 1 )  
              DO i = cims,cime
                ni = (i-ipos) * nri + ( nri / 2 + 1 )   
                psca1(i,j,nf) = cfld(i,k,j)
              ENDDO
           ENDDO
        ENDDO




               IF   ( njts .ge. njds .and. njts .le. njds + sz + joff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njds)), n2cj(MIN(njte,njds+sz+joff)), nrj*nri, xstag, ystag )
               ENDIF

               IF   ( njte .le. njde .and. njte .ge. njde - sz - joff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(nits)-1, n2ci(nite)+1, n2cj(MAX(njts,njde-sz-joff)), n2cj(MIN(njte,njde-1+joff)), nrj*nri, xstag, ystag )
               ENDIF

               IF   ( nits .ge. nids .and. nits .le. nids + sz + ioff  ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nids)), n2ci(MIN(nite,nids+sz+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF

               IF   ( nite .le. nide .and. nite .ge. nide - sz - ioff ) THEN
        CALL sintb( psca1, psca,                     &
          cims, cime, cjms, cjme, icmask,  &
          n2ci(MAX(nits,nide-sz-ioff)), n2ci(MIN(nite,nide-1+ioff)), n2cj(njts)-1, n2cj(njte)+1, nrj*nri, xstag, ystag )
               ENDIF

        DO nj1 = MAX(njds,njts-1), MIN(njde+joff,njte+joff+1) 
           cj = jpos + (nj1-1) / nrj     
           jp = mod ( nj1-1 , nrj )  
           nk = k
           ck = nk
           DO ni1 = MAX(nids,nits-1), MIN(nide+ioff,nite+ioff+1)
               ci = ipos + (ni1-1) / nri      
               ip = mod ( ni1-1 , nri )  

               ni = ni1-ioff
               nj = nj1-joff

               IF ( ( ni.LT.nids) .OR. (nj.LT.njds) ) THEN
                  CYCLE
               END IF




        
               IF   ( ni .ge. nids .and. ni .lt. nids + sz ) THEN
                 bdy_txs( nj,k,ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_xs( nj,k,ni ) = nfld(ni,k,nj)
               ENDIF

        
               IF   ( nj .ge. njds .and. nj .lt. njds + sz ) THEN
                 bdy_tys( ni,k,nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                 bdy_ys( ni,k,nj ) = nfld(ni,k,nj)
               ENDIF

        
               IF ( xstag ) THEN
                 IF   ( ni .ge. nide - sz + 1 .AND. ni .le. nide ) THEN
                   bdy_txe( nj,k,nide-ni+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni+1 ) = nfld(ni,k,nj)
                 ENDIF
               ELSE
                 IF   ( ni .ge. nide - sz .AND. ni .le. nide-1 ) THEN
                   bdy_txe( nj,k,nide-ni ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_xe( nj,k,nide-ni ) = nfld(ni,k,nj)
                 ENDIF
               ENDIF

        
               IF ( ystag ) THEN
                 IF   ( nj .ge. njde - sz + 1 .AND. nj .le. njde  ) THEN
                   bdy_tye( ni,k,njde-nj+1 ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj+1 ) = nfld(ni,k,nj)
                 ENDIF
               ELSE
                 IF   (  nj .ge. njde - sz .AND. nj .le. njde-1 ) THEN
                   bdy_tye(ni,k,njde-nj ) = rdt*(psca(ci+shw,cj+shw,ip+1+(jp)*nri)-nfld(ni,k,nj))
                   bdy_ye( ni,k,njde-nj ) = nfld(ni,k,nj)
                 ENDIF
               ENDIF

           ENDDO
        ENDDO
     ENDDO
   !$OMP END PARALLEL DO
                  
     RETURN

   END SUBROUTINE bdy_interp1



   SUBROUTINE bdy_interp2( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nijds, nijde, spec_bdy_width ,        &
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw1,                                 &
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &
                           cbdy_xs, bdy_xs,                      &
                           cbdy_xe, bdy_xe,                      &
                           cbdy_ys, bdy_ys,                      &
                           cbdy_ye, bdy_ye,                      &
                           cbdy_txs, bdy_txs,                    &
                           cbdy_txe, bdy_txe,                    &
                           cbdy_tys, bdy_tys,                    &
                           cbdy_tye, bdy_tye,                    &
                           cdt, ndt                              &
                                                                 )



     USE module_interp_info

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw1,                                 &  
                            ipos, jpos,                           &
                            nri, nrj
     INTEGER, INTENT(IN) :: nijds, nijde, spec_bdy_width
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(INOUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(INOUT) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xs, cbdy_txs   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_xe, cbdy_txe   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ys, cbdy_tys   
     REAL, DIMENSION ( * ), INTENT(INOUT) :: cbdy_ye, cbdy_tye   
     REAL                                 :: cdt, ndt
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xs, bdy_txs
     REAL, DIMENSION ( njms:njme, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_xe, bdy_txe
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ys, bdy_tys
     REAL, DIMENSION ( nims:nime, nkms:nkme, spec_bdy_width ), INTENT(INOUT) :: bdy_ye, bdy_tye

     

     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld_horiz_interp 
                                                                              
                                                                              
                                                                              
     INTEGER ni, nj, nk, istag, jstag
     INTEGER shw
     INTEGER spec_zone 
     INTEGER relax_zone
     INTEGER sz
     REAL*8 rdt

     shw = 0  

     

     CALL interp_fcn ( cfld,                                 &  
                       cids, cide, ckds, ckde, cjds, cjde,   &
                       cims, cime, ckms, ckme, cjms, cjme,   &
                       cits, cite, ckts, ckte, cjts, cjte,   &
                       nfld_horiz_interp,                    &  
                       nids, nide, nkds, nkde, njds, njde,   &
                       nims, nime, nkms, nkme, njms, njme,   &
                       MAX(nits-nri,nids),MIN(nite+nri,nide),&
                       nkts, nkte,                           &
                       MAX(njts-nrj,njds),MIN(njte+nrj,njde),&
                       shw,                                  &  
                       imask,                                &  
                       xstag, ystag,                         &  
                       ipos, jpos,                           &  

                       nri, nrj                              )  

     

     IF ( xstag ) THEN 
        istag = 0
     ELSE
        istag = 1
     END IF
     IF ( ystag ) THEN
        jstag = 0
     ELSE 
        jstag = 1
     END IF

     

     rdt = 1.D0/cdt

     CALL nl_get_spec_zone( 1, spec_zone )
     CALL nl_get_relax_zone( 1, relax_zone )

     

     sz = MIN(MAX( spec_zone, relax_zone + 1 ),spec_bdy_width)

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ni,nj,nk )
     
     DO nj = MAX ( njts-nrj, njds ) , MIN ( njte+nrj, njde-jstag )
        DO nk = nkts, nkte
           DO ni = MAX( nits-nri, nids ) , MIN ( nite+nri, nide-istag )

              

              IF ( ni .LT. nids + sz ) THEN
                  bdy_txs(nj,nk,ni) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                  bdy_xs (nj,nk,ni) =      nfld(ni,nk,nj)
              END IF

               

               IF ( nj .LT. njds + sz ) THEN
                  bdy_tys(ni,nk,nj) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                  bdy_ys (ni,nk,nj) =      nfld(ni,nk,nj)
               END IF

               

               IF ( xstag ) THEN
                  IF ( ( ni .GE. nide - sz + 1 ) .AND. ( ni .LE. nide ) ) THEN
                     bdy_txe(nj,nk,nide-ni+1) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_xe (nj,nk,nide-ni+1) =      nfld(ni,nk,nj)
                  END IF
               ELSE
                  IF ( ( ni .GE. nide - sz ) .AND. ( ni .LE. nide-1 ) ) THEN
                     bdy_txe(nj,nk,nide-ni  ) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_xe (nj,nk,nide-ni  ) =      nfld(ni,nk,nj)
                  END IF
               END IF

               

               IF ( ystag ) THEN
                  IF ( ( nj .GE. njde - sz + 1 ) .AND. ( nj .LE. njde  ) ) THEN
                     bdy_tye(ni,nk,njde-nj+1) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_ye (ni,nk,njde-nj+1) =      nfld(ni,nk,nj)
                  END IF
               ELSE
                  IF ( (  nj .GE. njde - sz ) .AND. ( nj .LE. njde-1 ) ) THEN
                     bdy_tye(ni,nk,njde-nj  ) = rdt*(nfld_horiz_interp(ni,nk,nj)-nfld(ni,nk,nj))
                     bdy_ye (ni,nk,njde-nj  ) =      nfld(ni,nk,nj)
                  END IF
               END IF

           END DO      
        END DO         
     END DO            

   !$OMP END PARALLEL DO

   END SUBROUTINE bdy_interp2



   SUBROUTINE interp_fcni( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     




     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              if ( imask(ni,nj) .NE. 1 ) cycle
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcni

   SUBROUTINE interp_fcnm( cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     




     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              nfld( ni, nk, nj ) = cfld( ci , ck , cj )
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm

   SUBROUTINE interp_fcnm_lu( cfld,                              &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           cxlat,    nxlat,                      &
                           cxlong,   nxlong,                     &
                           cdx, ndx,                             &
                           cid, nid                            ) 
     USE module_configure

     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj,                             &
                            cid, nid
     LOGICAL, INTENT(IN) :: xstag, ystag 

     REAL,    INTENT(IN) :: cdx, ndx

     REAL,    INTENT(IN),  DIMENSION ( cims:cime, cjms:cjme ) :: cxlat, cxlong
     REAL,    INTENT(IN),  DIMENSION ( nims:nime, njms:njme ) :: nxlat, nxlong


     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER i, ci, cj, ck, ni, nj, nk, ip, jp, ierr

   
   
     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              if ( imask(ni,nj) .eq. 1 ) then
               nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO
     RETURN

   END SUBROUTINE interp_fcnm_lu


   SUBROUTINE interp_fcnm_imask( cfld,                           &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     
     




     DO nj = njts, njte
        cj = jpos + (nj-1) / nrj     
        jp = mod ( nj , nrj )  
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = ipos + (ni-1) / nri      
              ip = mod ( ni , nri )  
              
              
              if ( imask(ni,nj) .eq. 1 ) then
               nfld( ni, nk, nj ) = cfld( ci , ck , cj )
              endif
           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcnm_imask





   SUBROUTINE interp_mask_land_field ( enable,                   &  
                                       cfld,                     &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu                              )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen , iswater, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER(LEN=255) :: message
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
   
      
   
      CALL nl_get_iswater(1,iswater)

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         

       IF ( enable ) THEN

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) =  cfld(ci  ,ck,cj  )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. iswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .NE. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) ) THEN
                     nfld(ni,nk,nj) = -1

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .NE. iswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO


         

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .NE. iswater ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -1) ) THEN

         
         

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite
                    IF ( imask(ni, nj) .NE. 1 ) cycle
                    IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .NE. iswater ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                          Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - island (', ni, ',', nj, '), using average ', avg_n(nk)
                          CALL wrf_message ( message )
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("<stdin>",3283,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_land_field

   SUBROUTINE interp_mask_water_field ( enable,                  &  
                                        cfld,                    &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu, cflag, nflag                )

      USE module_configure
      USE module_wrf_error
      USE module_dm, only : wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE
   
   
      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj, cflag, nflag
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu
   
      
   
      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount , ii , jj , ist , ien , jst , jen, ierr
      REAL :: avg , sum , dx , dy
      INTEGER , PARAMETER :: max_search = 5
      INTEGER :: icount_n(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_n(nkts:nkte), sum_n(nkts:nkte), dummy(nkts:nkte)
      CHARACTER(LEN=255) :: message

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

       IF ( enable ) THEN
         

         DO nj = njts, njte
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF
   



                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = cfld(ci  ,ck,cj  )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. nflag ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. nflag ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. nflag ) ) THEN
                     nfld(ni,nk,nj) = -4

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. nflag ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. nflag ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

         

         sum_n     = 0
         icount_n  = 0
         DO nj = njts, njte
            DO nk = nkts, nkte
               DO ni = nits, nite
                  IF ( nfld(ni,nk,nj) .NE. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .EQ. nflag ) THEN
                       icount_n(nk)  = icount_n(nk) + 1
                       sum_n(nk) = sum_n(nk) + nfld(ni,nk,nj)
                     END IF
                  END IF
               END DO
            END DO
         END DO

         CALL wrf_dm_sum_reals(      sum_n(nkts:nkte),  dummy(nkts:nkte))
         sum_n    = dummy
         CALL wrf_dm_sum_integers(icount_n(nkts:nkte), idummy(nkts:nkte))
         icount_n = idummy
         DO nk = nkts, nkte
            IF ( icount_n(nk) .GT. 0 )  &
              avg_n(nk)  = sum_n(nk) / icount_n(nk)
         END DO
       ENDIF

       IF ( enable ) THEN
         IF ( ANY(nfld .EQ. -4) ) THEN

           
           

           DO nj = njts, njte
              DO nk = nkts, nkte
                 DO ni = nits, nite

                    IF ( nfld(ni,nk,nj) .EQ. -4 ) THEN
                       IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                          cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
                       ELSE
                          cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
                       END IF
                       IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                          ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                       ELSE
                          ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                       END IF
                       ist = MAX (ci-max_search,cits)
                       ien = MIN (ci+max_search,cite,cide-1)
                       jst = MAX (cj-max_search,cjts)
                       jen = MIN (cj+max_search,cjte,cjde-1)
                       icount = 0 
                       sum = 0
                       DO jj = jst,jen
                          DO ii = ist,ien
                             IF ( NINT(clu(ii,jj)) .EQ. nflag ) THEN
                                icount = icount + 1
                                sum = sum + cfld(ii,nk,jj)
                             END IF
                          END DO
                       END DO
                       IF ( icount .GT. 0 ) THEN
                          nfld(ni,nk,nj) = sum / REAL ( icount ) 
                       ELSE
                         Write(message,fmt='(a,i4,a,i4,a,f10.4)') &
                            'horizontal interp error - lake (', ni, ',', nj, '), using average ', avg_n(nk)
                         CALL wrf_message ( message )                         
                          nfld(ni,nk,nj) = avg_n(nk)
                       END IF        
                    END IF
                 END DO
              END DO
           END DO
         ENDIF
       ENDIF
      ELSE
         CALL wrf_error_fatal3("<stdin>",3527,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_water_field


   SUBROUTINE p2c_mask (   cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu,                             &  
                           ctslb,ntslb,                          &  
                           cnum_soil_layers,nnum_soil_layers,    &  
                           ciswater, niswater                    )  

      USE module_configure
      USE module_wrf_error

      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &
                             ipos, jpos,                           &
                             nri, nrj,                             &
                             cnum_soil_layers, nnum_soil_layers,   &
                             ciswater, niswater 

      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
   
      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      REAL, DIMENSION ( cims:cime, 1:cnum_soil_layers, cjms:cjme ) :: ctslb
      REAL, DIMENSION ( nims:nime, 1:nnum_soil_layers, njms:njme ) :: ntslb

      
   
      INTEGER ci, cj, ck, ni, nj, nk
      INTEGER :: icount 
      REAL :: sum , dx , dy

      
   
      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

         

         DO nj = njts, MIN(njde-1,njte)
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1 
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1 
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, MIN(nide-1,nite)
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1 
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1 
                  END IF

                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  


                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri ) 
                  ELSE 
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri ) 
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj ) 
                  ELSE 
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj ) 
                  END IF
   
                  

                  IF      ( ( NINT(nlu(ni  ,nj  )) .NE. niswater ) ) THEN
                     nfld(ni,nk,nj) = 273.18

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .EQ. niswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                             dy   * cfld(ci  ,ck,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                             dy   * cfld(ci+1,ck,cj+1) )

                  
                  

                  ELSE IF ( ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj  )) .NE. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj  )) .NE. niswater ) .AND. &
                            ( NINT(clu(ci  ,cj+1)) .NE. niswater ) .AND. &
                            ( NINT(clu(ci+1,cj+1)) .NE. niswater ) ) THEN
                     nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * ctslb(ci  ,cnum_soil_layers,cj  )   + &
                                                             dy   * ctslb(ci  ,cnum_soil_layers,cj+1) ) + &
                                             dx   * ( ( 1. - dy ) * ctslb(ci+1,cnum_soil_layers,cj  )   + &
                                                             dy   * ctslb(ci+1,cnum_soil_layers,cj+1) )

                  
                  
                  ELSE IF ( NINT(nlu(ni  ,nj  )) .EQ. niswater ) THEN
                     icount = 0
                     sum = 0
                     IF ( NINT(clu(ci  ,cj  )) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci+1,cj  )) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj  )
                     END IF
                     IF ( NINT(clu(ci  ,cj+1)) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci  ,ck,cj+1)
                     END IF
                     IF ( NINT(clu(ci+1,cj+1)) .EQ. niswater ) THEN
                        icount = icount + 1
                        sum = sum + cfld(ci+1,ck,cj+1)
                     END IF
                     nfld(ni,nk,nj) = sum / REAL ( icount ) 
                  END IF
               END DO
            END DO
         END DO

      ELSE
         CALL wrf_error_fatal3("<stdin>",3695,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE p2c_mask

   SUBROUTINE none
   END SUBROUTINE none

   SUBROUTINE smoother ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      xstag, ystag,                         &  
                      ipos, jpos,                           &  
                      nri, nrj                              &
                      )
 
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
      INTEGER             :: smooth_option, feedback , spec_zone
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld

      

      CALL nl_get_feedback       ( 1, feedback  )
      IF ( feedback == 0 ) RETURN
      CALL nl_get_spec_zone ( 1, spec_zone )

      
      
      

      CALL nl_get_smooth_option  ( 1, smooth_option  )

      IF      ( smooth_option == 0 ) THEN

      ELSE IF ( smooth_option == 1 ) THEN
         CALL sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
      ELSE IF ( smooth_option == 2 ) THEN
         CALL smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
      END IF

   END SUBROUTINE smoother 

   SUBROUTINE sm121 ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      INTEGER                        :: i , j , k , loop
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 

      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      
   
      smoothing_passes : DO loop = 1 , smooth_passes
   
         DO k = ckts , ckte
   
            

            DO i = MAX(ipos,cits-3) , MIN(ipos+(nide-nids)/nri,cite+3)
               DO j = MAX(jpos,cjts-3) , MIN(jpos+(njde-njds)/nrj,cjte+3)
                  cfldnew(i,j) = cfld(i,k,j) 
               END DO
            END DO

            
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = 0.25 * ( cfld(i,k,j+1) + 2.*cfld(i,k,j) + cfld(i,k,j-1) )
               END DO
            END DO

            
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) =  0.25 * ( cfldnew(i+1,j) + 2.*cfldnew(i,j) + cfldnew(i-1,j) )
               END DO
            END DO
       
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE sm121

   SUBROUTINE smdsm ( cfld , &
                      cids, cide, ckds, ckde, cjds, cjde,   &
                      cims, cime, ckms, ckme, cjms, cjme,   &
                      cits, cite, ckts, ckte, cjts, cjte,   &
                      xstag, ystag,                         &  
                      nids, nide, nkds, nkde, njds, njde,   &
                      nims, nime, nkms, nkme, njms, njme,   &
                      nits, nite, nkts, nkte, njts, njte,   &
                      nri, nrj,                             &  
                      ipos, jpos                            &  
                      )
   
      USE module_configure
      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             nri, nrj,                             &  
                             ipos, jpos
      LOGICAL, INTENT(IN) :: xstag, ystag
   
      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: cfldnew
   
      REAL , DIMENSION ( 2 )         :: xnu
      INTEGER                        :: i , j , k , loop , n 
      INTEGER :: istag,jstag

      INTEGER, PARAMETER  :: smooth_passes = 1 

      xnu  =  (/ 0.50 , -0.52 /)
    
      istag = 1 ; jstag = 1
      IF ( xstag ) istag = 0
      IF ( ystag ) jstag = 0
   
      
      
   
      smoothing_passes : DO loop = 1 , smooth_passes * 2
   
         n  =  2 - MOD ( loop , 2 )
    
         DO k = ckts , ckte
   
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i,k,j+1) + cfld(i,k,j-1)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
               DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfldnew(i,j) = cfld(i,k,j) + xnu(n) * ((cfld(i+1,k,j) + cfld(i-1,k,j)) * 0.5-cfld(i,k,j))
               END DO
            END DO
       
            DO j = MAX(jpos+2,cjts-2) , MIN(jpos+(njde-njds)/nrj-2-jstag,cjte+2)
               DO i = MAX(ipos+2,cits-2) , MIN(ipos+(nide-nids)/nri-2-istag,cite+2)
                  cfld(i,k,j) = cfldnew(i,j)
               END DO
            END DO
   
         END DO
    
      END DO smoothing_passes
   
   END SUBROUTINE smdsm




   SUBROUTINE mark_domain (  cfld,                                 &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj                             )   
     USE module_configure
     USE module_wrf_error
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ), INTENT(OUT) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ), INTENT(IN) :: nfld
     INTEGER, DIMENSION ( nims:nime, njms:njme ), INTENT(IN) :: imask

     

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp, ioff, joff, ioffa, joffa
     INTEGER :: icmin,icmax,jcmin,jcmax
     INTEGER :: istag,jstag, ipoints,jpoints,ijpoints

     istag = 1 ; jstag = 1
     IF ( xstag ) istag = 0
     IF ( ystag ) jstag = 0

     DO cj = MAX(jpos+1,cjts),MIN(jpos+(njde-njds)/nrj-jstag-1,cjte)
        nj = (cj-jpos)*nrj + jstag + 1
        DO ck = ckts, ckte
           nk = ck
           DO ci = MAX(ipos+1,cits),MIN(ipos+(nide-nids)/nri-istag-1,cite)
              ni = (ci-ipos)*nri + istag + 1
              cfld( ci, ck, cj ) =  9021000.  
           ENDDO
        ENDDO
     ENDDO

   END SUBROUTINE mark_domain





   SUBROUTINE interp_mask_field ( enable,                  &  
                                       cfld,                     &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu, cflag, nflag ) 

      USE module_configure
      USE module_wrf_error
      USE module_dm , only :  wrf_dm_sum_reals, wrf_dm_sum_integers

      IMPLICIT NONE


      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &  
                             ipos, jpos,                           &
                             nri, nrj, cflag, nflag
      LOGICAL, INTENT(IN) :: xstag, ystag

      REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      

      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount, ii , jj , ist , ien , jst , jen , iswater, ierr
      REAL :: avg, sum, dx , dy
      INTEGER :: icount_water(nkts:nkte), icount_land(nkts:nkte), idummy(nkts:nkte)
      REAL :: avg_water(nkts:nkte), avg_land(nkts:nkte), sum_water(nkts:nkte), sum_land(nkts:nkte), dummy(nkts:nkte)
      CHARACTER (len=256) :: message
      CHARACTER (len=256) :: a_mess

      

      
      iswater = nflag

      

      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

        

        IF ( enable ) THEN
          DO nj = njts, njte
            
            IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
               cj = ( nj + (nrj/2)-1 ) / nrj + jpos -1
            ELSE
               cj = ( nj + (nrj-1)/2 ) / nrj + jpos -1
            END IF
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  
                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     ci = ( ni + (nri/2)-1 ) / nri + ipos -1
                  ELSE
                     ci = ( ni + (nri-1)/2 ) / nri + ipos -1
                  END IF

                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  

                  

                  IF ( MOD ( nri , 2 ) .EQ. 0 ) THEN
                     dx = ( REAL ( MOD ( ni+(nri-1)/2 , nri ) ) + 0.5 ) / REAL ( nri )
                  ELSE
                     dx =   REAL ( MOD ( ni+(nri-1)/2 , nri ) )         / REAL ( nri )
                  END IF
                  IF ( MOD ( nrj , 2 ) .EQ. 0 ) THEN
                     dy = ( REAL ( MOD ( nj+(nrj-1)/2 , nrj ) ) + 0.5 ) / REAL ( nrj )
                  ELSE
                     dy =   REAL ( MOD ( nj+(nrj-1)/2 , nrj ) )         / REAL ( nrj )
                  END IF

                  
                  IF ( ( NINT(nlu(ni, nj)) .EQ. iswater ) ) THEN

                     
                     
                     
                     
                     

                     IF ( ALL( clu(ci:ci+1,cj:cj+1) == iswater ) .OR. &
                          ALL( clu(ci:ci+1,cj:cj+1) /= iswater ) ) THEN

                       nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                               dy   * cfld(ci  ,ck,cj+1) ) + &
                                               dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                               dy   * cfld(ci+1,ck,cj+1) )

                     
                     ELSE
                       icount = 0
                       sum = 0
                       IF ( NINT(clu(ci  ,cj  )) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci  ,ck,cj  )
                       END IF
                       IF ( NINT(clu(ci+1,cj  )) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci+1,ck,cj  )
                       END IF
                       IF ( NINT(clu(ci  ,cj+1)) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci  ,ck,cj+1)
                       END IF
                       IF ( NINT(clu(ci+1,cj+1)) .EQ. iswater ) THEN
                          icount = icount + 1
                          sum = sum + cfld(ci+1,ck,cj+1)
                       END IF
                       nfld(ni,nk,nj) = sum / REAL ( icount )
                     END IF

                  
                   ELSE IF ( ( NINT(nlu(ni, nj)) .NE. iswater ) ) THEN

                     
                     
                     
                     
                     

                     IF ( ALL( clu(ci:ci+1,cj:cj+1) == iswater ) .OR. &
                          ALL( clu(ci:ci+1,cj:cj+1) /= iswater ) ) THEN

                       nfld(ni,nk,nj) = ( 1. - dx ) * ( ( 1. - dy ) * cfld(ci  ,ck,cj  )   + &
                                                               dy   * cfld(ci  ,ck,cj+1) ) + &
                                               dx   * ( ( 1. - dy ) * cfld(ci+1,ck,cj  )   + &
                                                               dy   * cfld(ci+1,ck,cj+1) )

                    
                    ELSE
                      icount = 0
                      sum = 0
                      IF ( NINT(clu(ci  ,cj  )) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci  ,ck,cj  )
                      END IF
                      IF ( NINT(clu(ci+1,cj  )) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci+1,ck,cj  )
                      END IF
                      IF ( NINT(clu(ci  ,cj+1)) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci  ,ck,cj+1)
                      END IF
                      IF ( NINT(clu(ci+1,cj+1)) .NE. iswater ) THEN
                         icount = icount + 1
                         sum = sum + cfld(ci+1,ck,cj+1)
                      END IF
                      nfld(ni,nk,nj) = sum / REAL ( icount )

                    END IF
                  END IF

               END DO
            END DO
          END DO

        END IF
      ELSE
         CALL wrf_error_fatal3("<stdin>",4186,&
"only unstaggered fields right now" )
      END IF

   END SUBROUTINE interp_mask_field


   SUBROUTINE interp_mask_soil ( enable,                  &  
                                       cfld,                     &  
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           shw,                                  &  
                           imask,                                &  
                           xstag, ystag,                         &  
                           ipos, jpos,                           &  
                           nri, nrj,                             &  
                           clu, nlu )

      USE module_configure
      USE module_wrf_error
      USE module_dm , only : wrf_dm_sum_real, wrf_dm_sum_integer

      IMPLICIT NONE


      LOGICAL, INTENT(IN) :: enable
      INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                             cims, cime, ckms, ckme, cjms, cjme,   &
                             cits, cite, ckts, ckte, cjts, cjte,   &
                             nids, nide, nkds, nkde, njds, njde,   &
                             nims, nime, nkms, nkme, njms, njme,   &
                             nits, nite, nkts, nkte, njts, njte,   &
                             shw,                                  &  
                             ipos, jpos,                           &
                             nri, nrj
      LOGICAL, INTENT(IN) :: xstag, ystag

      INTEGER, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
      INTEGER, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
      INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask

      REAL, DIMENSION ( cims:cime, cjms:cjme ) :: clu
      REAL, DIMENSION ( nims:nime, njms:njme ) :: nlu

      

      INTEGER ci, cj, ck, ni, nj, nk, ip, jp
      INTEGER :: icount, ii , jj , ist , ien , jst , jen , iswater, num_soil_cat, ierr
      REAL :: avg, sum, dx , dy
      INTEGER , ALLOCATABLE :: icount_water(:,: ), icount_land(:,:)
      INTEGER , PARAMETER :: max_search = 5
      CHARACTER*120 message
      INTEGER, PARAMETER :: isoilwater = 14

      CALL nl_get_iswater(1,iswater)
      CALL nl_get_num_soil_cat(1,num_soil_cat)

      allocate (icount_water(nkms:nkme,1:num_soil_cat))
      allocate ( icount_land(nkms:nkme,1:num_soil_cat))

      

      IF ( ( .NOT. xstag) .AND. ( .NOT. ystag ) ) THEN

        

        IF ( enable ) THEN

          DO nj = njts, njte
             cj = jpos + (nj-1) / nrj     
            DO nk = nkts, nkte
               ck = nk
               DO ni = nits, nite
                  ci = ipos + (ni-1) / nri      

                  IF ( imask(ni, nj) .NE. 1 ) cycle

                  IF ( ( NINT(nlu(ni, nj)) .EQ. iswater ) ) then

                     IF ( ( NINT(clu(ci  ,cj  )) .EQ. iswater ) ) then 
                       nfld(ni,nk,nj) = cfld(ci,ck,cj)
                     ELSE 
                       nfld(ni,nk,nj) = -1
                     ENDIF

                  ELSE IF ( ( NINT(nlu(ni, nj)) .NE. iswater ) ) THEN

                      IF ( ( NINT(clu(ci  ,cj  )) .NE. iswater ) ) THEN 
                         nfld(ni,nk,nj) = cfld(ci,ck,cj)
                      ELSE 
                         nfld(ni,nk,nj) = -1
                      ENDIF

                  END IF
               END DO
            END DO
          END DO

          DO nj = njts, njte
             DO nk = nkts, nkte
                DO ni = nits, nite
                  IF ( imask(ni, nj) .NE. 1 ) cycle
                  IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                     IF ( NINT(nlu(ni,nj)) .EQ. iswater ) THEN 
                        nfld(ni,nk,nj) = isoilwater
                     END IF
                  END IF
               END DO
             END DO
          END DO

          IF ( ANY(nfld .EQ. -1) ) THEN
            DO nj = njts, njte
               DO nk = nkts, nkte
                  DO ni = nits, nite
                     IF ( imask(ni, nj) .NE. 1 ) cycle
                     IF ( nfld(ni,nk,nj) .EQ. -1 ) THEN
                        nfld(ni,nk,nj) = 8
                     END IF
                  ENDDO
               ENDDO
            ENDDO
          END IF 

        END IF  
      ELSE
         CALL wrf_error_fatal3("<stdin>",4317,&
"only unstaggered fields right now" )
      END IF


      deallocate (icount_water)
      deallocate (icount_land)

   END SUBROUTINE interp_mask_soil
 





   SUBROUTINE interp_fcn_lagr_ll ( cfld_inp,                          &  
                                cids, cide, ckds, ckde, cjds, cjde,   &
                                cims, cime, ckms, ckme, cjms, cjme,   &
                                cits, cite, ckts, ckte, cjts, cjte,   &
                                nfld,                                 &  
                                nids, nide, nkds, nkde, njds, njde,   &
                                nims, nime, nkms, nkme, njms, njme,   &
                                nits, nite, nkts, nkte, njts, njte,   &
                                shw,                                  &  
                                imask,                                &  
                                xstag, ystag,                         &  
                                ipos, jpos,                           &  
                                nri, nrj,                             & 
                                clat_in, nlat_in,                     & 
                                cinput_from_file, ninput_from_file )    

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            shw,                                  &
                            ipos, jpos,                           &
                            nri, nrj
     LOGICAL, INTENT(IN) :: xstag, ystag

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld_inp, cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld
     REAL, DIMENSION ( cims:cime,            cjms:cjme ) :: clat_in
     REAL, DIMENSION ( nims:nime,            njms:njme ) :: nlat_in
     INTEGER, DIMENSION ( nims:nime, njms:njme ) :: imask
     LOGICAL :: cinput_from_file, ninput_from_file

     

     INTEGER ci, cj, ck, ni, nj, nk, istag, jstag, i, j, k
     REAL :: nx, x0, x1, x2, x3, x
     REAL :: ny, y0, y1, y2, y3
     REAL :: cxm1, cxp0, cxp1, cxp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2
     REAL :: cym1, cyp0, cyp1, cyp2
     INTEGER :: ioff, joff
     LOGICAL :: probably_by_dateline
     REAL :: max_lon, min_lon
     LOGICAL :: probably_by_pole
     REAL :: max_lat, min_lat

     

     REAL, EXTERNAL :: lagrange_quad_avg
     REAL, EXTERNAL :: nest_loc_of_cg
     INTEGER, EXTERNAL :: compute_CGLL

     
     
     
     
     
     

     
     
     
     
     
     
     
     

     IF ( xstag ) THEN
        istag = 0 
        ioff  = 1
     ELSE
        istag = 1
        ioff  = 0
     END IF

     IF ( ystag ) THEN
        jstag = 0 
        joff  = 1
     ELSE
        jstag = 1
        joff  = 0
     END IF

     
     
     
     

     probably_by_pole = .FALSE.
     max_lat = -90
     min_lat = +90
     DO nj = njts, MIN(njde-jstag,njte)
        DO ni = nits, MIN(nide-istag,nite)
           max_lat = MAX ( nlat_in(ni,nj) , max_lat )       
           min_lat = MIN ( nlat_in(ni,nj) , min_lat )       
        END DO
     END DO

     IF ( ( max_lat .GT. 85 ) .OR. ( ABS(min_lat) .GT. 85 ) ) THEN
        probably_by_pole = .TRUE.
     END IF

     IF ( ( probably_by_pole ) .AND. ( .NOT. ninput_from_file ) ) THEN
        CALL wrf_error_fatal3("<stdin>",4439,&
'Nest over the pole, single input domain, longitudes will be wrong' )
     END IF

     

     probably_by_dateline = .FALSE.
     max_lon = -180
     min_lon = +180
     DO nj = njts, MIN(njde-jstag,njte)
        cj = compute_CGLL ( nj , jpos , nrj , jstag )
        DO ni = nits, MIN(nide-istag,nite)
           ci = compute_CGLL ( ni , ipos , nri , istag )
           max_lon = MAX ( cfld_inp(ci,1,cj) , max_lon )       
           min_lon = MIN ( cfld_inp(ci,1,cj) , min_lon )       
        END DO
     END DO

     IF ( max_lon - min_lon .GT. 300 ) THEN
        probably_by_dateline = .TRUE.
     END IF

     

     DO cj = MIN(cjts-1,cjms), MAX(cjte+1,cjme)
       DO ci = MIN(cits-1,cims), MAX(cite+1,cime)
         IF ( ( cfld_inp(ci,ckts,cj) .LT. 0 ) .AND. ( probably_by_dateline ) ) THEN
           cfld(ci,ckts,cj) = 360 + cfld_inp(ci,ckts,cj)
         ELSE
           cfld(ci,ckts,cj) =       cfld_inp(ci,ckts,cj)
         END IF
       END DO
     END DO

     

     j_loop : DO nj = njts, MIN(njde-jstag,njte)

        

        
        
        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        
        
        

        
        
        
        
        
        
        
        
        

        cj = compute_CGLL ( nj , jpos , nrj , jstag )

        

        k_loop : DO nk = nkts, nkte

          

           i_loop : DO ni = nits, MIN(nide-istag,nite)
 
              

              ci = compute_CGLL ( ni , ipos , nri , istag )

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              

              

              IF ( imask ( ni, nj ) .EQ. 1 ) THEN

                 

                 nx = REAL(ni)

                 

                 cxm1 = nest_loc_of_cg ( ci-1 , ipos , nri , ioff ) 

                 

                 cxp0 = nest_loc_of_cg ( ci   , ipos , nri , ioff ) 

                 

                 cxp1 = nest_loc_of_cg ( ci+1 , ipos , nri , ioff ) 

                 

                 cxp2 = nest_loc_of_cg ( ci+2 , ipos , nri , ioff ) 

                 

                 nfld_m1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2, &
                                               cfld(ci-1,nk,cj-1), cfld(ci+0,nk,cj-1), &
                                               cfld(ci+1,nk,cj-1), cfld(ci+2,nk,cj-1) )

                 

                 nfld_p0 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2,  &
                                               cfld(ci-1,nk,cj+0), cfld(ci+0,nk,cj+0), &
                                               cfld(ci+1,nk,cj+0), cfld(ci+2,nk,cj+0) )

                 

                 nfld_p1 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2,  &
                                               cfld(ci-1,nk,cj+1), cfld(ci+0,nk,cj+1), &
                                               cfld(ci+1,nk,cj+1), cfld(ci+2,nk,cj+1) )

                 

                 nfld_p2 = lagrange_quad_avg ( nx, cxm1, cxp0, cxp1, cxp2,  &
                                               cfld(ci-1,nk,cj+2), cfld(ci+0,nk,cj+2), &
                                               cfld(ci+1,nk,cj+2), cfld(ci+2,nk,cj+2) )

                 

                ny = REAL(nj)

                 

                 cym1 = nest_loc_of_cg ( cj-1 , jpos , nrj , joff ) 

                 

                 cyp0 = nest_loc_of_cg ( cj   , jpos , nrj , joff ) 

                 

                cyp1 = nest_loc_of_cg ( cj+1 , jpos , nrj , joff ) 

                 

                 cyp2 = nest_loc_of_cg ( cj+2 , jpos , nrj , joff ) 

                 

                 nfld(ni,nk,nj) = lagrange_quad_avg ( ny, cym1, cyp0, cyp1,  &
                                                      cyp2, nfld_m1, nfld_p0, nfld_p1, nfld_p2 )

             END IF

           END DO i_loop
        END DO    k_loop
     END DO       j_loop

     

     DO nj = njts, MIN(njde-jstag,njte)
        DO ni = nits, MIN(nide-istag,nite)
           IF ( nfld(ni,nkts,nj) .GT. 180 ) THEN
              nfld(ni,nkts,nj) = -360 + nfld(ni,nkts,nj)
           END IF
        END DO
    END DO

   END SUBROUTINE interp_fcn_lagr_ll


 
