
























MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_configure
   USE module_wrf_error
   USE module_utility
   USE module_domain_type

   
   
   
   

   
   
   
   
   

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   
   
   
   

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

   
   TYPE(domain), POINTER :: current_grid
   LOGICAL, SAVE :: current_grid_set = .FALSE.

   
   PRIVATE domain_time_test_print
   PRIVATE test_adjust_io_timestr

   INTERFACE get_ijk_from_grid
     MODULE PROCEDURE get_ijk_from_grid1, get_ijk_from_grid2
   END INTERFACE

   INTEGER, PARAMETER :: max_hst_mods = 200

CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

    END SELECT data_ordering


    RETURN
   END SUBROUTINE adjust_domain_dims_for_move

   SUBROUTINE get_ijk_from_grid1 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid1

   SUBROUTINE get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
           ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
           ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ; 
       CASE  ( DATA_ORDER_YXZ )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ; 
       CASE  ( DATA_ORDER_ZXY )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_ZYX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_XZY )
           ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
       CASE  ( DATA_ORDER_YZX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid2




   SUBROUTINE get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid




   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
















































   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent



   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )


   bdy_mask = .true.     



   sp1 = sd1 ; sp2 = sd2 ; sp3 = sd3
   ep1 = ed1 ; ep2 = ed2 ; ep3 = ed3
   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
         sm1  = sp1 - bdx ; em1 = ep1 + bdx
         sm2  = sp2 - bdy ; em2 = ep2 + bdy
         sm3  = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_YXZ )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3       ; em3 = ep3
      CASE ( DATA_ORDER_ZXY )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdx ; em2 = ep2 + bdx
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_ZYX )
         sm1 = sp1       ; em1 = ep1
         sm2 = sp2 - bdy ; em2 = ep2 + bdy
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
      CASE ( DATA_ORDER_XZY )
         sm1 = sp1 - bdx ; em1 = ep1 + bdx
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdy ; em3 = ep3 + bdy
      CASE ( DATA_ORDER_YZX )
         sm1 = sp1 - bdy ; em1 = ep1 + bdy
         sm2 = sp2       ; em2 = ep2
         sm3 = sp3 - bdx ; em3 = ep3 + bdx
   END SELECT
   sm1x = sm1       ; em1x = em1    
   sm2x = sm2       ; em2x = em2
   sm3x = sm3       ; em3x = em3
   sm1y = sm1       ; em1y = em1    
   sm2y = sm2       ; em2y = em2
   sm3y = sm3       ; em3y = em3

   sp1x = sp1 ; ep1x = ep1 ; sp2x = sp2 ; ep2x = ep2 ; sp3x = sp3 ; ep3x = ep3
   sp1y = sp1 ; ep1y = ep1 ; sp2y = sp2 ; ep2y = ep2 ; sp3y = sp3 ; ep3y = ep3


   RETURN
   END SUBROUTINE wrf_patch_domain

   SUBROUTINE alloc_and_configure_domain ( domain_id , active_this_task, grid , parent, kid )









































      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: domain_id
      LOGICAL , OPTIONAL, INTENT(IN)  :: active_this_task 
      TYPE( domain ) , POINTER        :: grid
      TYPE( domain ) , POINTER        :: parent
      INTEGER , INTENT(IN)            :: kid    

      
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny
      LOGICAL :: active


      active = .TRUE.
      IF ( PRESENT( active_this_task ) ) THEN
         active = active_this_task
      ENDIF






      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: ', &
          'Incorrect value for num_time_levels ', num_time_levels
        CALL wrf_error_fatal3("<stdin>",598,&
TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF


      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE( new_grid%head_statevars )
      new_grid%head_statevars%Ndim = 0
      NULLIFY( new_grid%head_statevars%next)
      new_grid%tail_statevars => new_grid%head_statevars 

      ALLOCATE ( new_grid%parents( max_parents ) ) 
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      new_grid%domain_clock_created = .FALSE.
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO
      new_grid%time_set = .FALSE.
      new_grid%is_intermediate = .FALSE.
      new_grid%have_displayed_alloc_stats = .FALSE.

      new_grid%tiling_latch = .FALSE.  

      
      
      
      
      

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 
      new_grid%active_this_task = active

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         









        
      new_grid%active_this_task = active
      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. , active,     &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                               sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   
      )







      new_grid%stepping_to_time = .FALSE.
      new_grid%adaptation_domain = 1
      new_grid%last_step_updated = -1



      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           



      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)




      grid => new_grid

 

      IF ( grid%active_this_task ) THEN

        ALLOCATE( grid%lattsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%lontsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%nametsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%desctsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%itsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%jtsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%id_tsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%ts_filename( grid%max_ts_locs ) )
        grid%ntsloc        = 0
        grid%ntsloc_domain = 0


        ALLOCATE( grid%track_time_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_in( grid%track_loc_in ) )
  
        ALLOCATE( grid%track_time_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_i( grid%track_loc_in ) )
        ALLOCATE( grid%track_j( grid%track_loc_in ) )

      grid%track_loc        = 0
      grid%track_loc_domain = 0
      grid%track_have_calculated = .FALSE.
      grid%track_have_input      = .FALSE.
      ELSE
        WRITE (wrf_err_message,*)"Not allocating time series storage for domain ",domain_id," on this set of tasks"
        CALL wrf_message(TRIM(wrf_err_message))
      ENDIF


      grid%interp_mp = .true.

   END SUBROUTINE alloc_and_configure_domain

   SUBROUTINE get_fieldstr(ix,c,instr,outstr,noutstr,noerr)
     IMPLICIT NONE
     INTEGER, INTENT(IN)          :: ix
     CHARACTER*(*), INTENT(IN)    :: c
     CHARACTER*(*), INTENT(IN)    :: instr
     CHARACTER*(*), INTENT(OUT)   :: outstr
     INTEGER,       INTENT(IN)    :: noutstr  
     LOGICAL,       INTENT(INOUT) :: noerr     
     
     INTEGER, PARAMETER :: MAX_DEXES = 100
     INTEGER I, PREV, IDEX
     INTEGER DEXES(MAX_DEXES)
     outstr = ""
     prev = 1
     dexes(1) = 1
     DO i = 2,MAX_DEXES
       idex = INDEX(instr(prev:LEN(TRIM(instr))),c)
       IF ( idex .GT. 0 ) THEN
         dexes(i) = idex+prev
         prev = dexes(i)+1
       ELSE
         dexes(i) = LEN(TRIM(instr))+2
       ENDIF
     ENDDO

     IF     ( (dexes(ix+1)-2)-(dexes(ix)) .GT. noutstr ) THEN
       noerr = .FALSE.  
     ELSE IF( dexes(ix) .EQ. dexes(ix+1) ) THEN 
       noerr = .FALSE.  
     ELSE
       outstr = instr(dexes(ix):(dexes(ix+1)-2))
       noerr = noerr .AND. .TRUE.
     ENDIF
   END SUBROUTINE get_fieldstr

   SUBROUTINE change_to_lower_case(instr,outstr)
     CHARACTER*(*) ,INTENT(IN)  :: instr
     CHARACTER*(*) ,INTENT(OUT) :: outstr

     CHARACTER*1                :: c
     INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
     INTEGER                    :: i,n,n1

     outstr = ' '
     N = len(instr)
     N1 = len(outstr)
     N = MIN(N,N1)
     outstr(1:N) = instr(1:N)
     DO i=1,N
       c = instr(i:i)
       if('A'<=c .and. c <='Z') outstr(i:i)=achar(iachar(c)+upper_to_lower)
     ENDDO
     RETURN
   END SUBROUTINE change_to_lower_case


   SUBROUTINE modify_io_masks1 ( grid , id )
      IMPLICIT NONE

      INTEGER              , INTENT(IN  )  :: id
      TYPE(domain), POINTER                :: grid
      
      TYPE(fieldlist), POINTER :: p, q
      INTEGER, PARAMETER :: read_unit = 10
      LOGICAL, EXTERNAL  :: wrf_dm_on_monitor
      CHARACTER*256      :: fname, inln, mess, dname, t1, lookee
      CHARACTER*256      :: fieldlst
      CHARACTER*1        :: op, strmtyp
      CHARACTER*3        :: strmid
      CHARACTER*10       :: strmtyp_name
      INTEGER            :: io_status
      INTEGER            :: strmtyp_int, count_em
      INTEGER            :: lineno, fieldno, istrm, retval, itrace
      LOGICAL            :: keepgoing, noerr, gavewarning, ignorewarning, found
      LOGICAL, SAVE      :: you_warned_me = .FALSE.
      LOGICAL, SAVE      :: you_warned_me2(max_hst_mods,max_domains) = .FALSE.

      gavewarning = .FALSE.

      CALL nl_get_iofields_filename( id, fname )

      IF ( grid%is_intermediate ) RETURN                
      IF ( TRIM(fname) .EQ. "NONE_SPECIFIED" ) RETURN   

      IF ( wrf_dm_on_monitor() ) THEN
        OPEN ( UNIT   = read_unit    ,      &
               FILE   = TRIM(fname)      ,      &
               FORM   = "FORMATTED"      ,      &
               STATUS = "OLD"            ,      &
               IOSTAT = io_status         )
        IF ( io_status .EQ. 0 ) THEN   
          keepgoing = .TRUE.
          lineno = 0
          count_em = 0    
          DO WHILE ( keepgoing )
            READ(UNIT=read_unit,FMT='(A)',IOSTAT=io_status) inln
            keepgoing = (io_status .EQ. 0) .AND. (LEN(TRIM(inln)) .GT. 0)  
            IF ( keepgoing ) THEN
              lineno = lineno + 1
              IF ( .NOT. LEN(TRIM(inln)) .LT. LEN(inln) ) THEN
                WRITE(mess,*)'W A R N I N G : Line ',lineno,' of ',TRIM(fname),' is too long. Limit is ',LEN(inln),' characters.' 
                gavewarning = .TRUE.
              ENDIF
              IF ( INDEX(inln,'#') .EQ. 0 ) THEN   
                IF ( keepgoing ) THEN
                  noerr = .TRUE.
                  CALL get_fieldstr(1,':',inln,op,1,noerr)          
                  IF ( TRIM(op) .NE. '+' .AND. TRIM(op) .NE. '-' ) THEN
                    WRITE(mess,*)'W A R N I N G : unknown operation ',TRIM(op),' (should be + or -). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(2,':',inln,t1,1,noerr)          
                  CALL change_to_lower_case(t1,strmtyp) 

                  SELECT CASE (TRIM(strmtyp))
                  CASE ('h')
                     strmtyp_name = 'history'
                     strmtyp_int  = first_history
                  CASE ('i')
                     strmtyp_name = 'input'
                     strmtyp_int  = first_input
                  CASE DEFAULT
                     WRITE(mess,*)'W A R N I N G : unknown stream type ',TRIM(strmtyp),'. Line ',lineno
                     gavewarning = .TRUE.
                  END SELECT

                  CALL get_fieldstr(3,':',inln,strmid,3,noerr)      
                  READ(strmid,'(I3)') istrm
                  IF ( istrm .LT. 0 .OR. istrm .GT. last_history ) THEN
                    WRITE(mess,*)'W A R N I N G : invalid stream id ',istrm,' (should be 0 <= id <= ',last_history,'). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(4,':',inln,fieldlst,1024,noerr) 
                  IF ( noerr ) THEN
                    fieldno = 1
                    CALL get_fieldstr(fieldno,',',fieldlst,t1,256,noerr)
                    CALL change_to_lower_case(t1,lookee)
                    DO WHILE ( noerr )    
                      p => grid%head_statevars%next
                      found = .FALSE.
                      count_em = count_em + 1
                      DO WHILE ( ASSOCIATED( p ) )
  
                        IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
  
                          DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                            CALL change_to_lower_case( p%dname_table( grid%id, itrace ) , dname ) 

                            IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                            CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                      strmtyp_name, dname, fname, lookee,      &
                                                      p%streams_table(grid%id,itrace)%stream,  &
                                                      mess, found, you_warned_me2)
                          ENDDO
                        ELSE 
                          IF ( p%Ntl .GT. 0 ) THEN
                            CALL change_to_lower_case(p%DataName(1:LEN(TRIM(p%DataName))-2),dname)
                          ELSE
                            CALL change_to_lower_case(p%DataName,dname)
                          ENDIF
  
                          IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                          CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                    strmtyp_name, dname, fname, lookee,      &
                                                    p%streams, mess, found, you_warned_me2)
                        ENDIF
                        p => p%next
                      ENDDO
                      IF ( .NOT. found ) THEN
                        WRITE(mess,*)'W A R N I N G : Unable to modify mask for ',TRIM(lookee),&
                                     '.  Variable not found. File: ',TRIM(fname),' at line ',lineno
                        CALL wrf_message(mess)
                        gavewarning = .TRUE.
                      ENDIF
                      fieldno = fieldno + 1
                      CALL get_fieldstr(fieldno,',',fieldlst,t1,256,noerr)
                      CALL change_to_lower_case(t1,lookee)
                    ENDDO
                  ELSE
                    WRITE(mess,*)'W A R N I N G : Problem reading ',TRIM(fname),' at line ',lineno
                    CALL wrf_message(mess)
                    gavewarning = .TRUE.
                  ENDIF
                ENDIF  
              ENDIF    
            ENDIF      
          ENDDO
        ELSE
          WRITE(mess,*)'W A R N I N G : Problem opening ',TRIM(fname)
          CALL wrf_message(mess)
          gavewarning = .TRUE.
        ENDIF
        CLOSE( read_unit )
        IF ( gavewarning ) THEN
          CALL nl_get_ignore_iofields_warning(1,ignorewarning)
          IF ( .NOT. ignorewarning ) THEN
            CALL wrf_message(mess)
            WRITE(mess,*)'modify_io_masks: problems reading ',TRIM(fname) 
            CALL wrf_message(mess)
            CALL wrf_error_fatal3("<stdin>",1098,&
'Set ignore_iofields_warn to true in namelist to ignore')
          ELSE
            IF ( .NOT. you_warned_me ) THEN
              if ( .NOT. you_warned_me2(count_em,id) ) CALL wrf_message(mess)  
              WRITE(mess,*)'Ignoring problems reading ',TRIM(fname) 
              CALL wrf_message(mess)
              CALL wrf_message('Continuing.  To make this a fatal error, set ignore_iofields_warn to false in namelist' )
              CALL wrf_message(' ')
              you_warned_me = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF  

      
   END SUBROUTINE modify_io_masks1

   SUBROUTINE warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                   strmtyp_name, dname, fname, lookee,      &
                                   p_stream, mess, found, you_warned_me2)

      IMPLICIT NONE






     INTEGER,       INTENT(IN )   :: id, istrm, lineno, strmtyp_int
     INTEGER,       INTENT(IN )   :: p_stream(*), count_em
     CHARACTER*1,   INTENT(IN )   :: op
     CHARACTER*10,  INTENT(IN )   :: strmtyp_name
     CHARACTER*256, INTENT(IN )   :: dname, fname, lookee
     CHARACTER*256, INTENT(OUT)   :: mess
     LOGICAL,       INTENT(OUT)   :: found
     LOGICAL,       INTENT(INOUT) :: you_warned_me2(max_hst_mods,max_domains)
   
     INTEGER                      :: retval

     found = .TRUE.
     IF      ( TRIM(op) .EQ. '+' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .NE. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already on ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '.  File: ', TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Setting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                  TRIM(DNAME)  ; CALL wrf_debug(1,mess)
         CALL set_mask( p_stream, strmtyp_int + istrm - 1 )
       ENDIF
     ELSE IF ( TRIM(op) .EQ. '-' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .EQ. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already off ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '. File: ',TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Resetting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                    TRIM(DNAME)  ; CALL wrf_debug(1,mess) 
         CALL reset_mask( p_stream, strmtyp_int + istrm - 1)
       ENDIF
     ENDIF
     IF ( count_em > max_hst_mods ) THEN
       WRITE(mess,*)'ERROR module_domain:  Array size for you_warned_me2 is fixed at ',max_hst_mods
       CALL wrf_message(mess)
       CALL wrf_error_fatal3("<stdin>",1163,&
'Did you really type > max_hst_mods fields into ', TRIM(fname) ,' ?')
     ELSE
       IF ( .NOT. you_warned_me2(count_em,id) ) THEN
         CALL wrf_message(mess)     
         you_warned_me2(count_em,id) = .TRUE.
       ENDIF
     ENDIF

   END SUBROUTINE warn_me_or_set_mask 







   SUBROUTINE alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_alloc_space_0, ONLY : alloc_space_field_core_0
      USE module_alloc_space_1, ONLY : alloc_space_field_core_1
      USE module_alloc_space_2, ONLY : alloc_space_field_core_2
      USE module_alloc_space_3, ONLY : alloc_space_field_core_3
      USE module_alloc_space_4, ONLY : alloc_space_field_core_4
      USE module_alloc_space_5, ONLY : alloc_space_field_core_5
      USE module_alloc_space_6, ONLY : alloc_space_field_core_6
      USE module_alloc_space_7, ONLY : alloc_space_field_core_7
      USE module_alloc_space_8, ONLY : alloc_space_field_core_8
      USE module_alloc_space_9, ONLY : alloc_space_field_core_9

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in

      
      INTEGER(KIND=8)  num_bytes_allocated
      INTEGER  idum1, idum2

      IF ( grid%id .EQ. 1 ) CALL wrf_message ( &
          'DYNAMICS OPTION: Eulerian Mass Coordinate ')

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 

      
      CALL alloc_space_field_core_0 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated , &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_1 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_2 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_3 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_4 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_5 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_6 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_7 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_8 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_9 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IF ( .NOT. grid%have_displayed_alloc_stats ) THEN
        
        
        WRITE(wrf_err_message,*)&
            'alloc_space_field: domain ',id,', ',num_bytes_allocated,' bytes allocated'
        CALL  wrf_debug( 0, wrf_err_message )
        grid%have_displayed_alloc_stats = .TRUE.   
      ENDIF


      grid%alloced_sd31=sd31
      grid%alloced_ed31=ed31
      grid%alloced_sd32=sd32
      grid%alloced_ed32=ed32
      grid%alloced_sd33=sd33
      grid%alloced_ed33=ed33
      grid%alloced_sm31=sm31
      grid%alloced_em31=em31
      grid%alloced_sm32=sm32
      grid%alloced_em32=em32
      grid%alloced_sm33=sm33
      grid%alloced_em33=em33
      grid%alloced_sm31x=sm31x
      grid%alloced_em31x=em31x
      grid%alloced_sm32x=sm32x
      grid%alloced_em32x=em32x
      grid%alloced_sm33x=sm33x
      grid%alloced_em33x=em33x
      grid%alloced_sm31y=sm31y
      grid%alloced_em31y=em31y
      grid%alloced_sm32y=sm32y
      grid%alloced_em32y=em32y
      grid%alloced_sm33y=sm33y
      grid%alloced_em33y=em33y

      grid%allocated=.TRUE.

   END SUBROUTINE alloc_space_field

   
   
   
   
   

   SUBROUTINE ensure_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in
      LOGICAL                         :: size_changed

      size_changed=         .not. ( &
         grid%alloced_sd31 .eq. sd31 .and. grid%alloced_ed31 .eq. ed31 .and. &
         grid%alloced_sd32 .eq. sd32 .and. grid%alloced_ed32 .eq. ed32 .and. &
         grid%alloced_sd33 .eq. sd33 .and. grid%alloced_ed33 .eq. ed33 .and. &
         grid%alloced_sm31 .eq. sm31 .and. grid%alloced_em31 .eq. em31 .and. &
         grid%alloced_sm32 .eq. sm32 .and. grid%alloced_em32 .eq. em32 .and. &
         grid%alloced_sm33 .eq. sm33 .and. grid%alloced_em33 .eq. em33 .and. &
         grid%alloced_sm31x .eq. sm31x .and. grid%alloced_em31x .eq. em31x .and. &
         grid%alloced_sm32x .eq. sm32x .and. grid%alloced_em32x .eq. em32x .and. &
         grid%alloced_sm33x .eq. sm33x .and. grid%alloced_em33x .eq. em33x .and. &
         grid%alloced_sm31y .eq. sm31y .and. grid%alloced_em31y .eq. em31y .and. &
         grid%alloced_sm32y .eq. sm32y .and. grid%alloced_em32y .eq. em32y .and. &
         grid%alloced_sm33y .eq. sm33y .and. grid%alloced_em33y .eq. em33y &
      )
      if(.not. grid%allocated .or. size_changed) then
         if(.not. grid%allocated) then
            call wrf_debug(1,'ensure_space_field: calling alloc_space_field because a grid was not allocated.')
         else
            if(size_changed) &
                 call wrf_debug(1,'ensure_space_field: deallocating and reallocating a grid because grid size changed.')
         end if
         if(grid%allocated) &
              call dealloc_space_field( grid )
         call alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )
      end if

   END SUBROUTINE ensure_space_field






   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: id

      

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      
      
      

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL domain_destroy( grid )
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: ', &
           'dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3("<stdin>",1475,&
TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain








   SUBROUTINE domain_destroy ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain) , POINTER          :: grid

      CALL dealloc_space_field ( grid )
      CALL dealloc_linked_lists( grid )
      DEALLOCATE( grid%parents )
      DEALLOCATE( grid%nests )
      
      CALL domain_clock_destroy( grid )
      CALL domain_alarms_destroy( grid )
      IF ( ASSOCIATED( grid%i_start ) ) THEN
        DEALLOCATE( grid%i_start ) 
      ENDIF
      IF ( ASSOCIATED( grid%i_end ) ) THEN
        DEALLOCATE( grid%i_end )
      ENDIF
      IF ( ASSOCIATED( grid%j_start ) ) THEN
        DEALLOCATE( grid%j_start )
      ENDIF
      IF ( ASSOCIATED( grid%j_end ) ) THEN
        DEALLOCATE( grid%j_end )
      ENDIF
      IF ( ASSOCIATED( grid%itsloc ) ) THEN
        DEALLOCATE( grid%itsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%jtsloc ) ) THEN
        DEALLOCATE( grid%jtsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%id_tsloc ) ) THEN
        DEALLOCATE( grid%id_tsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lattsloc ) ) THEN
        DEALLOCATE( grid%lattsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lontsloc ) ) THEN
        DEALLOCATE( grid%lontsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%nametsloc ) ) THEN
        DEALLOCATE( grid%nametsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%desctsloc ) ) THEN
        DEALLOCATE( grid%desctsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%ts_filename ) ) THEN
        DEALLOCATE( grid%ts_filename )
      ENDIF 
      IF ( ASSOCIATED( grid%track_time_in ) ) THEN
        DEALLOCATE( grid%track_time_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_in ) ) THEN
        DEALLOCATE( grid%track_lat_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_in ) ) THEN
        DEALLOCATE( grid%track_lon_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_i ) ) THEN
        DEALLOCATE( grid%track_i )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_j ) ) THEN
        DEALLOCATE( grid%track_j )
      ENDIF

      IF ( ASSOCIATED( grid%track_time_domain ) ) THEN
        DEALLOCATE( grid%track_time_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_domain ) ) THEN
        DEALLOCATE( grid%track_lat_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_domain ) ) THEN
        DEALLOCATE( grid%track_lon_domain )
      ENDIF
      DEALLOCATE( grid )
      NULLIFY( grid )

   END SUBROUTINE domain_destroy

   SUBROUTINE dealloc_linked_lists ( grid )
      IMPLICIT NONE
      TYPE(domain), POINTER :: grid
      TYPE(fieldlist), POINTER :: p, q
      p => grid%head_statevars
      DO WHILE ( ASSOCIATED( p ) )
         q => p ; p => p%next ; DEALLOCATE(q)
      ENDDO
      NULLIFY(grid%head_statevars) ; NULLIFY( grid%tail_statevars)
      IF ( .NOT. grid%is_intermediate ) THEN
        ALLOCATE( grid%head_statevars )
        NULLIFY( grid%head_statevars%next)
        grid%tail_statevars => grid%head_statevars
      ENDIF
   END SUBROUTINE dealloc_linked_lists

   RECURSIVE SUBROUTINE show_nest_subtree ( grid )
      TYPE(domain), POINTER :: grid
      INTEGER myid
      INTEGER kid
      IF ( .NOT. ASSOCIATED( grid ) ) RETURN
      myid = grid%id
      DO kid = 1, max_nests
        IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          IF ( grid%nests(kid)%ptr%id .EQ. myid ) THEN
            CALL wrf_error_fatal3("<stdin>",1600,&
'show_nest_subtree: nest hierarchy corrupted' )
          ENDIF
          CALL show_nest_subtree( grid%nests(kid)%ptr )
        ENDIF
      ENDDO
   END SUBROUTINE show_nest_subtree
   







   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain)              , POINTER :: grid

      

      INTEGER                             ::  ierr







<<<<<<< HEAD
IF ( ASSOCIATED( grid%ohmcoef ) ) THEN 
  DEALLOCATE(grid%ohmcoef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1636,&
=======

IF ( ASSOCIATED( grid%ohmcoef ) ) THEN 
  DEALLOCATE(grid%ohmcoef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1685,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ohmcoef. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qn1_av_suews ) ) THEN 
  DEALLOCATE(grid%qn1_av_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1643,&
=======
 CALL wrf_error_fatal3("<stdin>",1692,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qn1_av_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai_suews ) ) THEN 
  DEALLOCATE(grid%lai_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1650,&
=======
 CALL wrf_error_fatal3("<stdin>",1699,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lai_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albdectr_suews ) ) THEN 
  DEALLOCATE(grid%albdectr_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1657,&
=======
 CALL wrf_error_fatal3("<stdin>",1706,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albdectr_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albevetr_suews ) ) THEN 
  DEALLOCATE(grid%albevetr_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1664,&
=======
 CALL wrf_error_fatal3("<stdin>",1713,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albevetr_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albgrass_suews ) ) THEN 
  DEALLOCATE(grid%albgrass_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1671,&
=======
 CALL wrf_error_fatal3("<stdin>",1720,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albgrass_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%decidcap_suews ) ) THEN 
  DEALLOCATE(grid%decidcap_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1678,&
=======
 CALL wrf_error_fatal3("<stdin>",1727,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%decidcap_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%porosity_suews ) ) THEN 
  DEALLOCATE(grid%porosity_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1685,&
=======
 CALL wrf_error_fatal3("<stdin>",1734,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%porosity_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gdd_suews ) ) THEN 
  DEALLOCATE(grid%gdd_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1692,&
=======
 CALL wrf_error_fatal3("<stdin>",1741,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gdd_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hdd_suews ) ) THEN 
  DEALLOCATE(grid%hdd_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1699,&
=======
 CALL wrf_error_fatal3("<stdin>",1748,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hdd_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hdd_prev_suews ) ) THEN 
  DEALLOCATE(grid%hdd_prev_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1706,&
=======
 CALL wrf_error_fatal3("<stdin>",1755,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hdd_prev_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%state_suews ) ) THEN 
  DEALLOCATE(grid%state_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1713,&
=======
 CALL wrf_error_fatal3("<stdin>",1762,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%state_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilmoist_suews ) ) THEN 
  DEALLOCATE(grid%soilmoist_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1720,&
=======
 CALL wrf_error_fatal3("<stdin>",1769,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilmoist_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%surf_var_suews ) ) THEN 
  DEALLOCATE(grid%surf_var_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1727,&
=======
 CALL wrf_error_fatal3("<stdin>",1776,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%surf_var_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dqndt_suews ) ) THEN 
  DEALLOCATE(grid%dqndt_suews,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1734,&
=======
 CALL wrf_error_fatal3("<stdin>",1783,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dqndt_suews. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1741,&
=======
 CALL wrf_error_fatal3("<stdin>",1790,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1748,&
=======
 CALL wrf_error_fatal3("<stdin>",1797,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlong. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1755,&
=======
 CALL wrf_error_fatal3("<stdin>",1804,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lu_index. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1762,&
=======
 CALL wrf_error_fatal3("<stdin>",1811,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lu_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znu ) ) THEN 
  DEALLOCATE(grid%znu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1769,&
=======
 CALL wrf_error_fatal3("<stdin>",1818,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%znu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znw ) ) THEN 
  DEALLOCATE(grid%znw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1776,&
=======
 CALL wrf_error_fatal3("<stdin>",1825,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%znw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zs ) ) THEN 
  DEALLOCATE(grid%zs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1783,&
=======
 CALL wrf_error_fatal3("<stdin>",1832,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzs ) ) THEN 
  DEALLOCATE(grid%dzs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1790,&
=======
 CALL wrf_error_fatal3("<stdin>",1839,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_i ) ) THEN 
  DEALLOCATE(grid%traj_i,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1797,&
=======
 CALL wrf_error_fatal3("<stdin>",1846,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traj_i. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_j ) ) THEN 
  DEALLOCATE(grid%traj_j,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1804,&
=======
 CALL wrf_error_fatal3("<stdin>",1853,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traj_j. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_k ) ) THEN 
  DEALLOCATE(grid%traj_k,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1811,&
=======
 CALL wrf_error_fatal3("<stdin>",1860,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traj_k. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_long ) ) THEN 
  DEALLOCATE(grid%traj_long,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1818,&
=======
 CALL wrf_error_fatal3("<stdin>",1867,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traj_long. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_lat ) ) THEN 
  DEALLOCATE(grid%traj_lat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1825,&
=======
 CALL wrf_error_fatal3("<stdin>",1874,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traj_lat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_gc ) ) THEN 
  DEALLOCATE(grid%u_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1832,&
=======
 CALL wrf_error_fatal3("<stdin>",1881,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_gc ) ) THEN 
  DEALLOCATE(grid%v_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1839,&
=======
 CALL wrf_error_fatal3("<stdin>",1888,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_gc ) ) THEN 
  DEALLOCATE(grid%t_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1846,&
=======
 CALL wrf_error_fatal3("<stdin>",1895,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_gc ) ) THEN 
  DEALLOCATE(grid%rh_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1853,&
=======
 CALL wrf_error_fatal3("<stdin>",1902,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_gc ) ) THEN 
  DEALLOCATE(grid%ght_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1860,&
=======
 CALL wrf_error_fatal3("<stdin>",1909,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ght_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_gc ) ) THEN 
  DEALLOCATE(grid%p_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1867,&
=======
 CALL wrf_error_fatal3("<stdin>",1916,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prho_gc ) ) THEN 
  DEALLOCATE(grid%prho_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1874,&
=======
 CALL wrf_error_fatal3("<stdin>",1923,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%prho_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_gc ) ) THEN 
  DEALLOCATE(grid%xlat_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1881,&
=======
 CALL wrf_error_fatal3("<stdin>",1930,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlat_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_gc ) ) THEN 
  DEALLOCATE(grid%xlong_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1888,&
=======
 CALL wrf_error_fatal3("<stdin>",1937,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlong_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_gc ) ) THEN 
  DEALLOCATE(grid%ht_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1895,&
=======
 CALL wrf_error_fatal3("<stdin>",1944,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%var_sso ) ) THEN 
  DEALLOCATE(grid%var_sso,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1902,&
=======
 CALL wrf_error_fatal3("<stdin>",1951,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%var_sso. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lap_hgt ) ) THEN 
  DEALLOCATE(grid%lap_hgt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1909,&
=======
 CALL wrf_error_fatal3("<stdin>",1958,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lap_hgt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_gc ) ) THEN 
  DEALLOCATE(grid%tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1916,&
=======
 CALL wrf_error_fatal3("<stdin>",1965,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tavgsfc ) ) THEN 
  DEALLOCATE(grid%tavgsfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1923,&
=======
 CALL wrf_error_fatal3("<stdin>",1972,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tavgsfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmn_gc ) ) THEN 
  DEALLOCATE(grid%tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1930,&
=======
 CALL wrf_error_fatal3("<stdin>",1979,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tmn_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pslv_gc ) ) THEN 
  DEALLOCATE(grid%pslv_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1937,&
=======
 CALL wrf_error_fatal3("<stdin>",1986,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pslv_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sct_dom_gc ) ) THEN 
  DEALLOCATE(grid%sct_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1944,&
=======
 CALL wrf_error_fatal3("<stdin>",1993,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sct_dom_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scb_dom_gc ) ) THEN 
  DEALLOCATE(grid%scb_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1951,&
=======
 CALL wrf_error_fatal3("<stdin>",2000,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scb_dom_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%greenfrac ) ) THEN 
  DEALLOCATE(grid%greenfrac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1958,&
=======
 CALL wrf_error_fatal3("<stdin>",2007,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%greenfrac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo12m ) ) THEN 
  DEALLOCATE(grid%albedo12m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1965,&
=======
 CALL wrf_error_fatal3("<stdin>",2014,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albedo12m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai12m ) ) THEN 
  DEALLOCATE(grid%lai12m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1972,&
=======
 CALL wrf_error_fatal3("<stdin>",2021,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lai12m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pd_gc ) ) THEN 
  DEALLOCATE(grid%pd_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1979,&
=======
 CALL wrf_error_fatal3("<stdin>",2028,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pd_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pdrho_gc ) ) THEN 
  DEALLOCATE(grid%pdrho_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1986,&
=======
 CALL wrf_error_fatal3("<stdin>",2035,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pdrho_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_gc ) ) THEN 
  DEALLOCATE(grid%psfc_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",1993,&
=======
 CALL wrf_error_fatal3("<stdin>",2042,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psfc_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%intq_gc ) ) THEN 
  DEALLOCATE(grid%intq_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2000,&
=======
 CALL wrf_error_fatal3("<stdin>",2049,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%intq_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pdhs ) ) THEN 
  DEALLOCATE(grid%pdhs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2007,&
=======
 CALL wrf_error_fatal3("<stdin>",2056,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pdhs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_gc ) ) THEN 
  DEALLOCATE(grid%qv_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2014,&
=======
 CALL wrf_error_fatal3("<stdin>",2063,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_gc ) ) THEN 
  DEALLOCATE(grid%sh_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2021,&
=======
 CALL wrf_error_fatal3("<stdin>",2070,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cl_gc ) ) THEN 
  DEALLOCATE(grid%cl_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2028,&
=======
 CALL wrf_error_fatal3("<stdin>",2077,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cl_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cf_gc ) ) THEN 
  DEALLOCATE(grid%cf_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2035,&
=======
 CALL wrf_error_fatal3("<stdin>",2084,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cf_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icefrac_gc ) ) THEN 
  DEALLOCATE(grid%icefrac_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2042,&
=======
 CALL wrf_error_fatal3("<stdin>",2091,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icefrac_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icepct ) ) THEN 
  DEALLOCATE(grid%icepct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2049,&
=======
 CALL wrf_error_fatal3("<stdin>",2098,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icepct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qr_gc ) ) THEN 
  DEALLOCATE(grid%qr_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2056,&
=======
 CALL wrf_error_fatal3("<stdin>",2105,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qr_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_gc ) ) THEN 
  DEALLOCATE(grid%qc_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2063,&
=======
 CALL wrf_error_fatal3("<stdin>",2112,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qs_gc ) ) THEN 
  DEALLOCATE(grid%qs_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2070,&
=======
 CALL wrf_error_fatal3("<stdin>",2119,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qs_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qi_gc ) ) THEN 
  DEALLOCATE(grid%qi_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2077,&
=======
 CALL wrf_error_fatal3("<stdin>",2126,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qi_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qg_gc ) ) THEN 
  DEALLOCATE(grid%qg_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2084,&
=======
 CALL wrf_error_fatal3("<stdin>",2133,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qg_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qh_gc ) ) THEN 
  DEALLOCATE(grid%qh_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2091,&
=======
 CALL wrf_error_fatal3("<stdin>",2140,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qni_gc ) ) THEN 
  DEALLOCATE(grid%qni_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2098,&
=======
 CALL wrf_error_fatal3("<stdin>",2147,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qni_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnr_gc ) ) THEN 
  DEALLOCATE(grid%qnr_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2105,&
=======
 CALL wrf_error_fatal3("<stdin>",2154,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnr_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_gc ) ) THEN 
  DEALLOCATE(grid%qnwfa_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2112,&
=======
 CALL wrf_error_fatal3("<stdin>",2161,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_gc ) ) THEN 
  DEALLOCATE(grid%qnifa_gc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2119,&
=======
 CALL wrf_error_fatal3("<stdin>",2168,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_now ) ) THEN 
  DEALLOCATE(grid%qnwfa_now,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2126,&
=======
 CALL wrf_error_fatal3("<stdin>",2175,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jan ) ) THEN 
  DEALLOCATE(grid%qnwfa_jan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2133,&
=======
 CALL wrf_error_fatal3("<stdin>",2182,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_feb ) ) THEN 
  DEALLOCATE(grid%qnwfa_feb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2140,&
=======
 CALL wrf_error_fatal3("<stdin>",2189,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_mar ) ) THEN 
  DEALLOCATE(grid%qnwfa_mar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2147,&
=======
 CALL wrf_error_fatal3("<stdin>",2196,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_apr ) ) THEN 
  DEALLOCATE(grid%qnwfa_apr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2154,&
=======
 CALL wrf_error_fatal3("<stdin>",2203,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_may ) ) THEN 
  DEALLOCATE(grid%qnwfa_may,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2161,&
=======
 CALL wrf_error_fatal3("<stdin>",2210,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jun ) ) THEN 
  DEALLOCATE(grid%qnwfa_jun,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2168,&
=======
 CALL wrf_error_fatal3("<stdin>",2217,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jul ) ) THEN 
  DEALLOCATE(grid%qnwfa_jul,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2175,&
=======
 CALL wrf_error_fatal3("<stdin>",2224,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_aug ) ) THEN 
  DEALLOCATE(grid%qnwfa_aug,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2182,&
=======
 CALL wrf_error_fatal3("<stdin>",2231,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_sep ) ) THEN 
  DEALLOCATE(grid%qnwfa_sep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2189,&
=======
 CALL wrf_error_fatal3("<stdin>",2238,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_oct ) ) THEN 
  DEALLOCATE(grid%qnwfa_oct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2196,&
=======
 CALL wrf_error_fatal3("<stdin>",2245,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_nov ) ) THEN 
  DEALLOCATE(grid%qnwfa_nov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2203,&
=======
 CALL wrf_error_fatal3("<stdin>",2252,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_dec ) ) THEN 
  DEALLOCATE(grid%qnwfa_dec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2210,&
=======
 CALL wrf_error_fatal3("<stdin>",2259,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_now ) ) THEN 
  DEALLOCATE(grid%qnifa_now,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2217,&
=======
 CALL wrf_error_fatal3("<stdin>",2266,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jan ) ) THEN 
  DEALLOCATE(grid%qnifa_jan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2224,&
=======
 CALL wrf_error_fatal3("<stdin>",2273,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_feb ) ) THEN 
  DEALLOCATE(grid%qnifa_feb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2231,&
=======
 CALL wrf_error_fatal3("<stdin>",2280,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_mar ) ) THEN 
  DEALLOCATE(grid%qnifa_mar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2238,&
=======
 CALL wrf_error_fatal3("<stdin>",2287,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_apr ) ) THEN 
  DEALLOCATE(grid%qnifa_apr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2245,&
=======
 CALL wrf_error_fatal3("<stdin>",2294,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_may ) ) THEN 
  DEALLOCATE(grid%qnifa_may,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2252,&
=======
 CALL wrf_error_fatal3("<stdin>",2301,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jun ) ) THEN 
  DEALLOCATE(grid%qnifa_jun,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2259,&
=======
 CALL wrf_error_fatal3("<stdin>",2308,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jul ) ) THEN 
  DEALLOCATE(grid%qnifa_jul,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2266,&
=======
 CALL wrf_error_fatal3("<stdin>",2315,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_aug ) ) THEN 
  DEALLOCATE(grid%qnifa_aug,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2273,&
=======
 CALL wrf_error_fatal3("<stdin>",2322,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_sep ) ) THEN 
  DEALLOCATE(grid%qnifa_sep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2280,&
=======
 CALL wrf_error_fatal3("<stdin>",2329,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_oct ) ) THEN 
  DEALLOCATE(grid%qnifa_oct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2287,&
=======
 CALL wrf_error_fatal3("<stdin>",2336,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_nov ) ) THEN 
  DEALLOCATE(grid%qnifa_nov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2294,&
=======
 CALL wrf_error_fatal3("<stdin>",2343,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_dec ) ) THEN 
  DEALLOCATE(grid%qnifa_dec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2301,&
=======
 CALL wrf_error_fatal3("<stdin>",2350,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnifa_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qntemp ) ) THEN 
  DEALLOCATE(grid%qntemp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2308,&
=======
 CALL wrf_error_fatal3("<stdin>",2357,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qntemp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qntemp2 ) ) THEN 
  DEALLOCATE(grid%qntemp2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2315,&
=======
 CALL wrf_error_fatal3("<stdin>",2364,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qntemp2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_max_p ) ) THEN 
  DEALLOCATE(grid%t_max_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2322,&
=======
 CALL wrf_error_fatal3("<stdin>",2371,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_max_p ) ) THEN 
  DEALLOCATE(grid%ght_max_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2329,&
=======
 CALL wrf_error_fatal3("<stdin>",2378,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ght_max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%max_p ) ) THEN 
  DEALLOCATE(grid%max_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2336,&
=======
 CALL wrf_error_fatal3("<stdin>",2385,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_min_p ) ) THEN 
  DEALLOCATE(grid%t_min_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2343,&
=======
 CALL wrf_error_fatal3("<stdin>",2392,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_min_p ) ) THEN 
  DEALLOCATE(grid%ght_min_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2350,&
=======
 CALL wrf_error_fatal3("<stdin>",2399,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ght_min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%min_p ) ) THEN 
  DEALLOCATE(grid%min_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2357,&
=======
 CALL wrf_error_fatal3("<stdin>",2406,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgtmaxw ) ) THEN 
  DEALLOCATE(grid%hgtmaxw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2364,&
=======
 CALL wrf_error_fatal3("<stdin>",2413,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hgtmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgttrop ) ) THEN 
  DEALLOCATE(grid%hgttrop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2371,&
=======
 CALL wrf_error_fatal3("<stdin>",2420,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hgttrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmaxw ) ) THEN 
  DEALLOCATE(grid%pmaxw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2378,&
=======
 CALL wrf_error_fatal3("<stdin>",2427,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmaxwnn ) ) THEN 
  DEALLOCATE(grid%pmaxwnn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2385,&
=======
 CALL wrf_error_fatal3("<stdin>",2434,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pmaxwnn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ptrop ) ) THEN 
  DEALLOCATE(grid%ptrop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2392,&
=======
 CALL wrf_error_fatal3("<stdin>",2441,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ptrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ptropnn ) ) THEN 
  DEALLOCATE(grid%ptropnn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2399,&
=======
 CALL wrf_error_fatal3("<stdin>",2448,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ptropnn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmaxw ) ) THEN 
  DEALLOCATE(grid%tmaxw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2406,&
=======
 CALL wrf_error_fatal3("<stdin>",2455,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ttrop ) ) THEN 
  DEALLOCATE(grid%ttrop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2413,&
=======
 CALL wrf_error_fatal3("<stdin>",2462,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ttrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%umaxw ) ) THEN 
  DEALLOCATE(grid%umaxw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2420,&
=======
 CALL wrf_error_fatal3("<stdin>",2469,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%umaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%utrop ) ) THEN 
  DEALLOCATE(grid%utrop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2427,&
=======
 CALL wrf_error_fatal3("<stdin>",2476,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%utrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmaxw ) ) THEN 
  DEALLOCATE(grid%vmaxw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2434,&
=======
 CALL wrf_error_fatal3("<stdin>",2483,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vtrop ) ) THEN 
  DEALLOCATE(grid%vtrop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2441,&
=======
 CALL wrf_error_fatal3("<stdin>",2490,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vtrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_1 ) ) THEN 
  DEALLOCATE(grid%u_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2448,&
=======
 CALL wrf_error_fatal3("<stdin>",2497,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_2 ) ) THEN 
  DEALLOCATE(grid%u_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2455,&
=======
 CALL wrf_error_fatal3("<stdin>",2504,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_bxs ) ) THEN 
  DEALLOCATE(grid%u_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2462,&
=======
 CALL wrf_error_fatal3("<stdin>",2511,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_bxs. ')
 endif
  NULLIFY(grid%u_bxs)
ENDIF
IF ( ASSOCIATED( grid%u_bxe ) ) THEN 
  DEALLOCATE(grid%u_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2470,&
=======
 CALL wrf_error_fatal3("<stdin>",2519,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_bxe. ')
 endif
  NULLIFY(grid%u_bxe)
ENDIF
IF ( ASSOCIATED( grid%u_bys ) ) THEN 
  DEALLOCATE(grid%u_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2478,&
=======
 CALL wrf_error_fatal3("<stdin>",2527,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_bys. ')
 endif
  NULLIFY(grid%u_bys)
ENDIF
IF ( ASSOCIATED( grid%u_bye ) ) THEN 
  DEALLOCATE(grid%u_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2486,&
=======
 CALL wrf_error_fatal3("<stdin>",2535,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_bye. ')
 endif
  NULLIFY(grid%u_bye)
ENDIF
IF ( ASSOCIATED( grid%u_btxs ) ) THEN 
  DEALLOCATE(grid%u_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2494,&
=======
 CALL wrf_error_fatal3("<stdin>",2543,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_btxs. ')
 endif
  NULLIFY(grid%u_btxs)
ENDIF
IF ( ASSOCIATED( grid%u_btxe ) ) THEN 
  DEALLOCATE(grid%u_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2502,&
=======
 CALL wrf_error_fatal3("<stdin>",2551,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_btxe. ')
 endif
  NULLIFY(grid%u_btxe)
ENDIF
IF ( ASSOCIATED( grid%u_btys ) ) THEN 
  DEALLOCATE(grid%u_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2510,&
=======
 CALL wrf_error_fatal3("<stdin>",2559,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_btys. ')
 endif
  NULLIFY(grid%u_btys)
ENDIF
IF ( ASSOCIATED( grid%u_btye ) ) THEN 
  DEALLOCATE(grid%u_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2518,&
=======
 CALL wrf_error_fatal3("<stdin>",2567,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_btye. ')
 endif
  NULLIFY(grid%u_btye)
ENDIF
IF ( ASSOCIATED( grid%ru ) ) THEN 
  DEALLOCATE(grid%ru,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2526,&
=======
 CALL wrf_error_fatal3("<stdin>",2575,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ru. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_m ) ) THEN 
  DEALLOCATE(grid%ru_m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2533,&
=======
 CALL wrf_error_fatal3("<stdin>",2582,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ru_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_tend ) ) THEN 
  DEALLOCATE(grid%ru_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2540,&
=======
 CALL wrf_error_fatal3("<stdin>",2589,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ru_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_save ) ) THEN 
  DEALLOCATE(grid%u_save,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2547,&
=======
 CALL wrf_error_fatal3("<stdin>",2596,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_force ) ) THEN 
  DEALLOCATE(grid%z_force,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2554,&
=======
 CALL wrf_error_fatal3("<stdin>",2603,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_force. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_force_tend ) ) THEN 
  DEALLOCATE(grid%z_force_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2561,&
=======
 CALL wrf_error_fatal3("<stdin>",2610,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_force_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_g ) ) THEN 
  DEALLOCATE(grid%u_g,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2568,&
=======
 CALL wrf_error_fatal3("<stdin>",2617,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_g_tend ) ) THEN 
  DEALLOCATE(grid%u_g_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2575,&
=======
 CALL wrf_error_fatal3("<stdin>",2624,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_g_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_1 ) ) THEN 
  DEALLOCATE(grid%v_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2582,&
=======
 CALL wrf_error_fatal3("<stdin>",2631,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_2 ) ) THEN 
  DEALLOCATE(grid%v_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2589,&
=======
 CALL wrf_error_fatal3("<stdin>",2638,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_bxs ) ) THEN 
  DEALLOCATE(grid%v_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2596,&
=======
 CALL wrf_error_fatal3("<stdin>",2645,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_bxs. ')
 endif
  NULLIFY(grid%v_bxs)
ENDIF
IF ( ASSOCIATED( grid%v_bxe ) ) THEN 
  DEALLOCATE(grid%v_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2604,&
=======
 CALL wrf_error_fatal3("<stdin>",2653,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_bxe. ')
 endif
  NULLIFY(grid%v_bxe)
ENDIF
IF ( ASSOCIATED( grid%v_bys ) ) THEN 
  DEALLOCATE(grid%v_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2612,&
=======
 CALL wrf_error_fatal3("<stdin>",2661,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_bys. ')
 endif
  NULLIFY(grid%v_bys)
ENDIF
IF ( ASSOCIATED( grid%v_bye ) ) THEN 
  DEALLOCATE(grid%v_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2620,&
=======
 CALL wrf_error_fatal3("<stdin>",2669,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_bye. ')
 endif
  NULLIFY(grid%v_bye)
ENDIF
IF ( ASSOCIATED( grid%v_btxs ) ) THEN 
  DEALLOCATE(grid%v_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2628,&
=======
 CALL wrf_error_fatal3("<stdin>",2677,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_btxs. ')
 endif
  NULLIFY(grid%v_btxs)
ENDIF
IF ( ASSOCIATED( grid%v_btxe ) ) THEN 
  DEALLOCATE(grid%v_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2636,&
=======
 CALL wrf_error_fatal3("<stdin>",2685,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_btxe. ')
 endif
  NULLIFY(grid%v_btxe)
ENDIF
IF ( ASSOCIATED( grid%v_btys ) ) THEN 
  DEALLOCATE(grid%v_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2644,&
=======
 CALL wrf_error_fatal3("<stdin>",2693,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_btys. ')
 endif
  NULLIFY(grid%v_btys)
ENDIF
IF ( ASSOCIATED( grid%v_btye ) ) THEN 
  DEALLOCATE(grid%v_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2652,&
=======
 CALL wrf_error_fatal3("<stdin>",2701,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_btye. ')
 endif
  NULLIFY(grid%v_btye)
ENDIF
IF ( ASSOCIATED( grid%rv ) ) THEN 
  DEALLOCATE(grid%rv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2660,&
=======
 CALL wrf_error_fatal3("<stdin>",2709,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_m ) ) THEN 
  DEALLOCATE(grid%rv_m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2667,&
=======
 CALL wrf_error_fatal3("<stdin>",2716,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_tend ) ) THEN 
  DEALLOCATE(grid%rv_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2674,&
=======
 CALL wrf_error_fatal3("<stdin>",2723,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_save ) ) THEN 
  DEALLOCATE(grid%v_save,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2681,&
=======
 CALL wrf_error_fatal3("<stdin>",2730,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_g ) ) THEN 
  DEALLOCATE(grid%v_g,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2688,&
=======
 CALL wrf_error_fatal3("<stdin>",2737,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_g_tend ) ) THEN 
  DEALLOCATE(grid%v_g_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2695,&
=======
 CALL wrf_error_fatal3("<stdin>",2744,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_g_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_1 ) ) THEN 
  DEALLOCATE(grid%w_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2702,&
=======
 CALL wrf_error_fatal3("<stdin>",2751,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_2 ) ) THEN 
  DEALLOCATE(grid%w_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2709,&
=======
 CALL wrf_error_fatal3("<stdin>",2758,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_bxs ) ) THEN 
  DEALLOCATE(grid%w_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2716,&
=======
 CALL wrf_error_fatal3("<stdin>",2765,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_bxs. ')
 endif
  NULLIFY(grid%w_bxs)
ENDIF
IF ( ASSOCIATED( grid%w_bxe ) ) THEN 
  DEALLOCATE(grid%w_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2724,&
=======
 CALL wrf_error_fatal3("<stdin>",2773,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_bxe. ')
 endif
  NULLIFY(grid%w_bxe)
ENDIF
IF ( ASSOCIATED( grid%w_bys ) ) THEN 
  DEALLOCATE(grid%w_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2732,&
=======
 CALL wrf_error_fatal3("<stdin>",2781,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_bys. ')
 endif
  NULLIFY(grid%w_bys)
ENDIF
IF ( ASSOCIATED( grid%w_bye ) ) THEN 
  DEALLOCATE(grid%w_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2740,&
=======
 CALL wrf_error_fatal3("<stdin>",2789,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_bye. ')
 endif
  NULLIFY(grid%w_bye)
ENDIF
IF ( ASSOCIATED( grid%w_btxs ) ) THEN 
  DEALLOCATE(grid%w_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2748,&
=======
 CALL wrf_error_fatal3("<stdin>",2797,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_btxs. ')
 endif
  NULLIFY(grid%w_btxs)
ENDIF
IF ( ASSOCIATED( grid%w_btxe ) ) THEN 
  DEALLOCATE(grid%w_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2756,&
=======
 CALL wrf_error_fatal3("<stdin>",2805,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_btxe. ')
 endif
  NULLIFY(grid%w_btxe)
ENDIF
IF ( ASSOCIATED( grid%w_btys ) ) THEN 
  DEALLOCATE(grid%w_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2764,&
=======
 CALL wrf_error_fatal3("<stdin>",2813,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_btys. ')
 endif
  NULLIFY(grid%w_btys)
ENDIF
IF ( ASSOCIATED( grid%w_btye ) ) THEN 
  DEALLOCATE(grid%w_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2772,&
=======
 CALL wrf_error_fatal3("<stdin>",2821,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_btye. ')
 endif
  NULLIFY(grid%w_btye)
ENDIF
IF ( ASSOCIATED( grid%ww ) ) THEN 
  DEALLOCATE(grid%ww,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2780,&
=======
 CALL wrf_error_fatal3("<stdin>",2829,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ww. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rw ) ) THEN 
  DEALLOCATE(grid%rw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2787,&
=======
 CALL wrf_error_fatal3("<stdin>",2836,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ww_m ) ) THEN 
  DEALLOCATE(grid%ww_m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2794,&
=======
 CALL wrf_error_fatal3("<stdin>",2843,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ww_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_subs ) ) THEN 
  DEALLOCATE(grid%w_subs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2801,&
=======
 CALL wrf_error_fatal3("<stdin>",2850,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_subs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_subs_tend ) ) THEN 
  DEALLOCATE(grid%w_subs_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2808,&
=======
 CALL wrf_error_fatal3("<stdin>",2857,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_subs_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_1 ) ) THEN 
  DEALLOCATE(grid%ph_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2815,&
=======
 CALL wrf_error_fatal3("<stdin>",2864,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_2 ) ) THEN 
  DEALLOCATE(grid%ph_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2822,&
=======
 CALL wrf_error_fatal3("<stdin>",2871,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_bxs ) ) THEN 
  DEALLOCATE(grid%ph_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2829,&
=======
 CALL wrf_error_fatal3("<stdin>",2878,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_bxs. ')
 endif
  NULLIFY(grid%ph_bxs)
ENDIF
IF ( ASSOCIATED( grid%ph_bxe ) ) THEN 
  DEALLOCATE(grid%ph_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2837,&
=======
 CALL wrf_error_fatal3("<stdin>",2886,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_bxe. ')
 endif
  NULLIFY(grid%ph_bxe)
ENDIF
IF ( ASSOCIATED( grid%ph_bys ) ) THEN 
  DEALLOCATE(grid%ph_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2845,&
=======
 CALL wrf_error_fatal3("<stdin>",2894,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_bys. ')
 endif
  NULLIFY(grid%ph_bys)
ENDIF
IF ( ASSOCIATED( grid%ph_bye ) ) THEN 
  DEALLOCATE(grid%ph_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2853,&
=======
 CALL wrf_error_fatal3("<stdin>",2902,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_bye. ')
 endif
  NULLIFY(grid%ph_bye)
ENDIF
IF ( ASSOCIATED( grid%ph_btxs ) ) THEN 
  DEALLOCATE(grid%ph_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2861,&
=======
 CALL wrf_error_fatal3("<stdin>",2910,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_btxs. ')
 endif
  NULLIFY(grid%ph_btxs)
ENDIF
IF ( ASSOCIATED( grid%ph_btxe ) ) THEN 
  DEALLOCATE(grid%ph_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2869,&
=======
 CALL wrf_error_fatal3("<stdin>",2918,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_btxe. ')
 endif
  NULLIFY(grid%ph_btxe)
ENDIF
IF ( ASSOCIATED( grid%ph_btys ) ) THEN 
  DEALLOCATE(grid%ph_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2877,&
=======
 CALL wrf_error_fatal3("<stdin>",2926,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_btys. ')
 endif
  NULLIFY(grid%ph_btys)
ENDIF
IF ( ASSOCIATED( grid%ph_btye ) ) THEN 
  DEALLOCATE(grid%ph_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2885,&
=======
 CALL wrf_error_fatal3("<stdin>",2934,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_btye. ')
 endif
  NULLIFY(grid%ph_btye)
ENDIF
IF ( ASSOCIATED( grid%phb ) ) THEN 
  DEALLOCATE(grid%phb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2893,&
=======
 CALL wrf_error_fatal3("<stdin>",2942,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%phb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%phb_fine ) ) THEN 
  DEALLOCATE(grid%phb_fine,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2900,&
=======
 CALL wrf_error_fatal3("<stdin>",2949,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%phb_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph0 ) ) THEN 
  DEALLOCATE(grid%ph0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2907,&
=======
 CALL wrf_error_fatal3("<stdin>",2956,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%php ) ) THEN 
  DEALLOCATE(grid%php,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2914,&
=======
 CALL wrf_error_fatal3("<stdin>",2963,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%php. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_1 ) ) THEN 
  DEALLOCATE(grid%t_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2921,&
=======
 CALL wrf_error_fatal3("<stdin>",2970,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_2 ) ) THEN 
  DEALLOCATE(grid%t_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2928,&
=======
 CALL wrf_error_fatal3("<stdin>",2977,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_bxs ) ) THEN 
  DEALLOCATE(grid%t_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2935,&
=======
 CALL wrf_error_fatal3("<stdin>",2984,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_bxs. ')
 endif
  NULLIFY(grid%t_bxs)
ENDIF
IF ( ASSOCIATED( grid%t_bxe ) ) THEN 
  DEALLOCATE(grid%t_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2943,&
=======
 CALL wrf_error_fatal3("<stdin>",2992,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_bxe. ')
 endif
  NULLIFY(grid%t_bxe)
ENDIF
IF ( ASSOCIATED( grid%t_bys ) ) THEN 
  DEALLOCATE(grid%t_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2951,&
=======
 CALL wrf_error_fatal3("<stdin>",3000,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_bys. ')
 endif
  NULLIFY(grid%t_bys)
ENDIF
IF ( ASSOCIATED( grid%t_bye ) ) THEN 
  DEALLOCATE(grid%t_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2959,&
=======
 CALL wrf_error_fatal3("<stdin>",3008,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_bye. ')
 endif
  NULLIFY(grid%t_bye)
ENDIF
IF ( ASSOCIATED( grid%t_btxs ) ) THEN 
  DEALLOCATE(grid%t_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2967,&
=======
 CALL wrf_error_fatal3("<stdin>",3016,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_btxs. ')
 endif
  NULLIFY(grid%t_btxs)
ENDIF
IF ( ASSOCIATED( grid%t_btxe ) ) THEN 
  DEALLOCATE(grid%t_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2975,&
=======
 CALL wrf_error_fatal3("<stdin>",3024,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_btxe. ')
 endif
  NULLIFY(grid%t_btxe)
ENDIF
IF ( ASSOCIATED( grid%t_btys ) ) THEN 
  DEALLOCATE(grid%t_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2983,&
=======
 CALL wrf_error_fatal3("<stdin>",3032,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_btys. ')
 endif
  NULLIFY(grid%t_btys)
ENDIF
IF ( ASSOCIATED( grid%t_btye ) ) THEN 
  DEALLOCATE(grid%t_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2991,&
=======
 CALL wrf_error_fatal3("<stdin>",3040,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_btye. ')
 endif
  NULLIFY(grid%t_btye)
ENDIF
IF ( ASSOCIATED( grid%t_init ) ) THEN 
  DEALLOCATE(grid%t_init,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",2999,&
=======
 CALL wrf_error_fatal3("<stdin>",3048,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_init. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_save ) ) THEN 
  DEALLOCATE(grid%t_save,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3006,&
=======
 CALL wrf_error_fatal3("<stdin>",3055,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x ) ) THEN 
  DEALLOCATE(grid%th_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3013,&
=======
 CALL wrf_error_fatal3("<stdin>",3062,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3020,&
=======
 CALL wrf_error_fatal3("<stdin>",3069,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y ) ) THEN 
  DEALLOCATE(grid%th_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3027,&
=======
 CALL wrf_error_fatal3("<stdin>",3076,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3034,&
=======
 CALL wrf_error_fatal3("<stdin>",3083,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3041,&
=======
 CALL wrf_error_fatal3("<stdin>",3090,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3048,&
=======
 CALL wrf_error_fatal3("<stdin>",3097,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3055,&
=======
 CALL wrf_error_fatal3("<stdin>",3104,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3062,&
=======
 CALL wrf_error_fatal3("<stdin>",3111,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3069,&
=======
 CALL wrf_error_fatal3("<stdin>",3118,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3076,&
=======
 CALL wrf_error_fatal3("<stdin>",3125,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3083,&
=======
 CALL wrf_error_fatal3("<stdin>",3132,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3090,&
=======
 CALL wrf_error_fatal3("<stdin>",3139,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x ) ) THEN 
  DEALLOCATE(grid%u_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3097,&
=======
 CALL wrf_error_fatal3("<stdin>",3146,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3104,&
=======
 CALL wrf_error_fatal3("<stdin>",3153,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y ) ) THEN 
  DEALLOCATE(grid%u_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3111,&
=======
 CALL wrf_error_fatal3("<stdin>",3160,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3118,&
=======
 CALL wrf_error_fatal3("<stdin>",3167,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x ) ) THEN 
  DEALLOCATE(grid%v_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3125,&
=======
 CALL wrf_error_fatal3("<stdin>",3174,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3132,&
=======
 CALL wrf_error_fatal3("<stdin>",3181,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y ) ) THEN 
  DEALLOCATE(grid%v_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3139,&
=======
 CALL wrf_error_fatal3("<stdin>",3188,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3146,&
=======
 CALL wrf_error_fatal3("<stdin>",3195,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_t_tend ) ) THEN 
  DEALLOCATE(grid%th_t_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3153,&
=======
 CALL wrf_error_fatal3("<stdin>",3202,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_t_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_t_tend ) ) THEN 
  DEALLOCATE(grid%qv_t_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3160,&
=======
 CALL wrf_error_fatal3("<stdin>",3209,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_t_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_largescale ) ) THEN 
  DEALLOCATE(grid%th_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3167,&
=======
 CALL wrf_error_fatal3("<stdin>",3216,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_largescale_tend ) ) THEN 
  DEALLOCATE(grid%th_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3174,&
=======
 CALL wrf_error_fatal3("<stdin>",3223,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_largescale ) ) THEN 
  DEALLOCATE(grid%qv_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3181,&
=======
 CALL wrf_error_fatal3("<stdin>",3230,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_largescale_tend ) ) THEN 
  DEALLOCATE(grid%qv_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3188,&
=======
 CALL wrf_error_fatal3("<stdin>",3237,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_largescale ) ) THEN 
  DEALLOCATE(grid%ql_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3195,&
=======
 CALL wrf_error_fatal3("<stdin>",3244,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_largescale_tend ) ) THEN 
  DEALLOCATE(grid%ql_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3202,&
=======
 CALL wrf_error_fatal3("<stdin>",3251,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ql_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_largescale ) ) THEN 
  DEALLOCATE(grid%u_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3209,&
=======
 CALL wrf_error_fatal3("<stdin>",3258,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_largescale_tend ) ) THEN 
  DEALLOCATE(grid%u_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3216,&
=======
 CALL wrf_error_fatal3("<stdin>",3265,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_largescale ) ) THEN 
  DEALLOCATE(grid%v_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3223,&
=======
 CALL wrf_error_fatal3("<stdin>",3272,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_largescale_tend ) ) THEN 
  DEALLOCATE(grid%v_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3230,&
=======
 CALL wrf_error_fatal3("<stdin>",3279,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_largescale ) ) THEN 
  DEALLOCATE(grid%tau_largescale,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3237,&
=======
 CALL wrf_error_fatal3("<stdin>",3286,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_largescale_tend ) ) THEN 
  DEALLOCATE(grid%tau_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3244,&
=======
 CALL wrf_error_fatal3("<stdin>",3293,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_x ) ) THEN 
  DEALLOCATE(grid%tau_x,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3251,&
=======
 CALL wrf_error_fatal3("<stdin>",3300,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_x_tend ) ) THEN 
  DEALLOCATE(grid%tau_x_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3258,&
=======
 CALL wrf_error_fatal3("<stdin>",3307,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_y ) ) THEN 
  DEALLOCATE(grid%tau_y,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3265,&
=======
 CALL wrf_error_fatal3("<stdin>",3314,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_y_tend ) ) THEN 
  DEALLOCATE(grid%tau_y_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3272,&
=======
 CALL wrf_error_fatal3("<stdin>",3321,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3279,&
=======
 CALL wrf_error_fatal3("<stdin>",3328,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_val. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3286,&
=======
 CALL wrf_error_fatal3("<stdin>",3335,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3293,&
=======
 CALL wrf_error_fatal3("<stdin>",3342,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_val. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3300,&
=======
 CALL wrf_error_fatal3("<stdin>",3349,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_soil ) ) THEN 
  DEALLOCATE(grid%tau_soil,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3307,&
=======
 CALL wrf_error_fatal3("<stdin>",3356,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tau_soil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_depth_force ) ) THEN 
  DEALLOCATE(grid%soil_depth_force,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3314,&
=======
 CALL wrf_error_fatal3("<stdin>",3363,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soil_depth_force. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_1 ) ) THEN 
  DEALLOCATE(grid%mu_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3321,&
=======
 CALL wrf_error_fatal3("<stdin>",3370,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_2 ) ) THEN 
  DEALLOCATE(grid%mu_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3328,&
=======
 CALL wrf_error_fatal3("<stdin>",3377,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_bxs ) ) THEN 
  DEALLOCATE(grid%mu_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3335,&
=======
 CALL wrf_error_fatal3("<stdin>",3384,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_bxs. ')
 endif
  NULLIFY(grid%mu_bxs)
ENDIF
IF ( ASSOCIATED( grid%mu_bxe ) ) THEN 
  DEALLOCATE(grid%mu_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3343,&
=======
 CALL wrf_error_fatal3("<stdin>",3392,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_bxe. ')
 endif
  NULLIFY(grid%mu_bxe)
ENDIF
IF ( ASSOCIATED( grid%mu_bys ) ) THEN 
  DEALLOCATE(grid%mu_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3351,&
=======
 CALL wrf_error_fatal3("<stdin>",3400,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_bys. ')
 endif
  NULLIFY(grid%mu_bys)
ENDIF
IF ( ASSOCIATED( grid%mu_bye ) ) THEN 
  DEALLOCATE(grid%mu_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3359,&
=======
 CALL wrf_error_fatal3("<stdin>",3408,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_bye. ')
 endif
  NULLIFY(grid%mu_bye)
ENDIF
IF ( ASSOCIATED( grid%mu_btxs ) ) THEN 
  DEALLOCATE(grid%mu_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3367,&
=======
 CALL wrf_error_fatal3("<stdin>",3416,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_btxs. ')
 endif
  NULLIFY(grid%mu_btxs)
ENDIF
IF ( ASSOCIATED( grid%mu_btxe ) ) THEN 
  DEALLOCATE(grid%mu_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3375,&
=======
 CALL wrf_error_fatal3("<stdin>",3424,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_btxe. ')
 endif
  NULLIFY(grid%mu_btxe)
ENDIF
IF ( ASSOCIATED( grid%mu_btys ) ) THEN 
  DEALLOCATE(grid%mu_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3383,&
=======
 CALL wrf_error_fatal3("<stdin>",3432,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_btys. ')
 endif
  NULLIFY(grid%mu_btys)
ENDIF
IF ( ASSOCIATED( grid%mu_btye ) ) THEN 
  DEALLOCATE(grid%mu_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3391,&
=======
 CALL wrf_error_fatal3("<stdin>",3440,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_btye. ')
 endif
  NULLIFY(grid%mu_btye)
ENDIF
IF ( ASSOCIATED( grid%mub ) ) THEN 
  DEALLOCATE(grid%mub,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3399,&
=======
 CALL wrf_error_fatal3("<stdin>",3448,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mub. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mub_fine ) ) THEN 
  DEALLOCATE(grid%mub_fine,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3406,&
=======
 CALL wrf_error_fatal3("<stdin>",3455,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mub_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mub_save ) ) THEN 
  DEALLOCATE(grid%mub_save,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3413,&
=======
 CALL wrf_error_fatal3("<stdin>",3462,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mub_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu0 ) ) THEN 
  DEALLOCATE(grid%mu0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3420,&
=======
 CALL wrf_error_fatal3("<stdin>",3469,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mudf ) ) THEN 
  DEALLOCATE(grid%mudf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3427,&
=======
 CALL wrf_error_fatal3("<stdin>",3476,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mudf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muu ) ) THEN 
  DEALLOCATE(grid%muu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3434,&
=======
 CALL wrf_error_fatal3("<stdin>",3483,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%muu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muus ) ) THEN 
  DEALLOCATE(grid%muus,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3441,&
=======
 CALL wrf_error_fatal3("<stdin>",3490,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%muus. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muv ) ) THEN 
  DEALLOCATE(grid%muv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3448,&
=======
 CALL wrf_error_fatal3("<stdin>",3497,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%muv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muvs ) ) THEN 
  DEALLOCATE(grid%muvs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3455,&
=======
 CALL wrf_error_fatal3("<stdin>",3504,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%muvs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mut ) ) THEN 
  DEALLOCATE(grid%mut,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3462,&
=======
 CALL wrf_error_fatal3("<stdin>",3511,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mut. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muts ) ) THEN 
  DEALLOCATE(grid%muts,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3469,&
=======
 CALL wrf_error_fatal3("<stdin>",3518,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%muts. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nest_pos ) ) THEN 
  DEALLOCATE(grid%nest_pos,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3476,&
=======
 CALL wrf_error_fatal3("<stdin>",3525,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nest_pos. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nest_mask ) ) THEN 
  DEALLOCATE(grid%nest_mask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3483,&
=======
 CALL wrf_error_fatal3("<stdin>",3532,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nest_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_coarse ) ) THEN 
  DEALLOCATE(grid%ht_coarse,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3490,&
=======
 CALL wrf_error_fatal3("<stdin>",3539,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_coarse. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_1 ) ) THEN 
  DEALLOCATE(grid%tke_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3497,&
=======
 CALL wrf_error_fatal3("<stdin>",3546,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tke_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_2 ) ) THEN 
  DEALLOCATE(grid%tke_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3504,&
=======
 CALL wrf_error_fatal3("<stdin>",3553,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tke_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p ) ) THEN 
  DEALLOCATE(grid%p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3511,&
=======
 CALL wrf_error_fatal3("<stdin>",3560,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%al ) ) THEN 
  DEALLOCATE(grid%al,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3518,&
=======
 CALL wrf_error_fatal3("<stdin>",3567,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%al. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alt ) ) THEN 
  DEALLOCATE(grid%alt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3525,&
=======
 CALL wrf_error_fatal3("<stdin>",3574,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alb ) ) THEN 
  DEALLOCATE(grid%alb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3532,&
=======
 CALL wrf_error_fatal3("<stdin>",3581,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zx ) ) THEN 
  DEALLOCATE(grid%zx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3539,&
=======
 CALL wrf_error_fatal3("<stdin>",3588,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zy ) ) THEN 
  DEALLOCATE(grid%zy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3546,&
=======
 CALL wrf_error_fatal3("<stdin>",3595,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdz ) ) THEN 
  DEALLOCATE(grid%rdz,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3553,&
=======
 CALL wrf_error_fatal3("<stdin>",3602,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rdz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdzw ) ) THEN 
  DEALLOCATE(grid%rdzw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3560,&
=======
 CALL wrf_error_fatal3("<stdin>",3609,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rdzw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pb ) ) THEN 
  DEALLOCATE(grid%pb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3567,&
=======
 CALL wrf_error_fatal3("<stdin>",3616,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rho ) ) THEN 
  DEALLOCATE(grid%rho,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3574,&
=======
 CALL wrf_error_fatal3("<stdin>",3623,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rho. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fnm ) ) THEN 
  DEALLOCATE(grid%fnm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3581,&
=======
 CALL wrf_error_fatal3("<stdin>",3630,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fnm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fnp ) ) THEN 
  DEALLOCATE(grid%fnp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3588,&
=======
 CALL wrf_error_fatal3("<stdin>",3637,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fnp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdnw ) ) THEN 
  DEALLOCATE(grid%rdnw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3595,&
=======
 CALL wrf_error_fatal3("<stdin>",3644,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rdnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdn ) ) THEN 
  DEALLOCATE(grid%rdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3602,&
=======
 CALL wrf_error_fatal3("<stdin>",3651,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dnw ) ) THEN 
  DEALLOCATE(grid%dnw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3609,&
=======
 CALL wrf_error_fatal3("<stdin>",3658,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dn ) ) THEN 
  DEALLOCATE(grid%dn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3616,&
=======
 CALL wrf_error_fatal3("<stdin>",3665,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_base ) ) THEN 
  DEALLOCATE(grid%t_base,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3623,&
=======
 CALL wrf_error_fatal3("<stdin>",3672,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z ) ) THEN 
  DEALLOCATE(grid%z,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3630,&
=======
 CALL wrf_error_fatal3("<stdin>",3679,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_at_w ) ) THEN 
  DEALLOCATE(grid%z_at_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3637,&
=======
 CALL wrf_error_fatal3("<stdin>",3686,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_at_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_hyd ) ) THEN 
  DEALLOCATE(grid%p_hyd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3644,&
=======
 CALL wrf_error_fatal3("<stdin>",3693,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_hyd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_hyd_w ) ) THEN 
  DEALLOCATE(grid%p_hyd_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3651,&
=======
 CALL wrf_error_fatal3("<stdin>",3700,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_hyd_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3658,&
=======
 CALL wrf_error_fatal3("<stdin>",3707,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3665,&
=======
 CALL wrf_error_fatal3("<stdin>",3714,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3672,&
=======
 CALL wrf_error_fatal3("<stdin>",3721,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3679,&
=======
 CALL wrf_error_fatal3("<stdin>",3728,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3686,&
=======
 CALL wrf_error_fatal3("<stdin>",3735,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3693,&
=======
 CALL wrf_error_fatal3("<stdin>",3742,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lpi ) ) THEN 
  DEALLOCATE(grid%lpi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3700,&
=======
 CALL wrf_error_fatal3("<stdin>",3749,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lpi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uratx ) ) THEN 
  DEALLOCATE(grid%uratx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3707,&
=======
 CALL wrf_error_fatal3("<stdin>",3756,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vratx ) ) THEN 
  DEALLOCATE(grid%vratx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3714,&
=======
 CALL wrf_error_fatal3("<stdin>",3763,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tratx ) ) THEN 
  DEALLOCATE(grid%tratx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3721,&
=======
 CALL wrf_error_fatal3("<stdin>",3770,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%obs_savwt ) ) THEN 
  DEALLOCATE(grid%obs_savwt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3728,&
=======
 CALL wrf_error_fatal3("<stdin>",3777,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%obs_savwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%power ) ) THEN 
  DEALLOCATE(grid%power,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3735,&
=======
 CALL wrf_error_fatal3("<stdin>",3784,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%power. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3742,&
=======
 CALL wrf_error_fatal3("<stdin>",3791,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%imask_nostag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3749,&
=======
 CALL wrf_error_fatal3("<stdin>",3798,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%imask_xstag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3756,&
=======
 CALL wrf_error_fatal3("<stdin>",3805,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%imask_ystag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3763,&
=======
 CALL wrf_error_fatal3("<stdin>",3812,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%imask_xystag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3770,&
=======
 CALL wrf_error_fatal3("<stdin>",3819,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%moist_bxs ) ) THEN 
  DEALLOCATE(grid%moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3777,&
=======
 CALL wrf_error_fatal3("<stdin>",3826,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_bxs. ')
 endif
  NULLIFY(grid%moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%moist_bxe ) ) THEN 
  DEALLOCATE(grid%moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3785,&
=======
 CALL wrf_error_fatal3("<stdin>",3834,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_bxe. ')
 endif
  NULLIFY(grid%moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%moist_bys ) ) THEN 
  DEALLOCATE(grid%moist_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3793,&
=======
 CALL wrf_error_fatal3("<stdin>",3842,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_bys. ')
 endif
  NULLIFY(grid%moist_bys)
ENDIF
IF ( ASSOCIATED( grid%moist_bye ) ) THEN 
  DEALLOCATE(grid%moist_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3801,&
=======
 CALL wrf_error_fatal3("<stdin>",3850,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_bye. ')
 endif
  NULLIFY(grid%moist_bye)
ENDIF
IF ( ASSOCIATED( grid%moist_btxs ) ) THEN 
  DEALLOCATE(grid%moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3809,&
=======
 CALL wrf_error_fatal3("<stdin>",3858,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_btxs. ')
 endif
  NULLIFY(grid%moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%moist_btxe ) ) THEN 
  DEALLOCATE(grid%moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3817,&
=======
 CALL wrf_error_fatal3("<stdin>",3866,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_btxe. ')
 endif
  NULLIFY(grid%moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%moist_btys ) ) THEN 
  DEALLOCATE(grid%moist_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3825,&
=======
 CALL wrf_error_fatal3("<stdin>",3874,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_btys. ')
 endif
  NULLIFY(grid%moist_btys)
ENDIF
IF ( ASSOCIATED( grid%moist_btye ) ) THEN 
  DEALLOCATE(grid%moist_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3833,&
=======
 CALL wrf_error_fatal3("<stdin>",3882,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%moist_btye. ')
 endif
  NULLIFY(grid%moist_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist ) ) THEN 
  DEALLOCATE(grid%dfi_moist,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3841,&
=======
 CALL wrf_error_fatal3("<stdin>",3890,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3848,&
=======
 CALL wrf_error_fatal3("<stdin>",3897,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxs. ')
 endif
  NULLIFY(grid%dfi_moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3856,&
=======
 CALL wrf_error_fatal3("<stdin>",3905,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxe. ')
 endif
  NULLIFY(grid%dfi_moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3864,&
=======
 CALL wrf_error_fatal3("<stdin>",3913,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bys. ')
 endif
  NULLIFY(grid%dfi_moist_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3872,&
=======
 CALL wrf_error_fatal3("<stdin>",3921,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bye. ')
 endif
  NULLIFY(grid%dfi_moist_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3880,&
=======
 CALL wrf_error_fatal3("<stdin>",3929,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxs. ')
 endif
  NULLIFY(grid%dfi_moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3888,&
=======
 CALL wrf_error_fatal3("<stdin>",3937,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxe. ')
 endif
  NULLIFY(grid%dfi_moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3896,&
=======
 CALL wrf_error_fatal3("<stdin>",3945,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btys. ')
 endif
  NULLIFY(grid%dfi_moist_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3904,&
=======
 CALL wrf_error_fatal3("<stdin>",3953,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btye. ')
 endif
  NULLIFY(grid%dfi_moist_btye)
ENDIF
IF ( ASSOCIATED( grid%qvold ) ) THEN 
  DEALLOCATE(grid%qvold,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3912,&
=======
 CALL wrf_error_fatal3("<stdin>",3961,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qvold. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rimi ) ) THEN 
  DEALLOCATE(grid%rimi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3919,&
=======
 CALL wrf_error_fatal3("<stdin>",3968,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rimi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa2d ) ) THEN 
  DEALLOCATE(grid%qnwfa2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3926,&
=======
 CALL wrf_error_fatal3("<stdin>",3975,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnwfa2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_cloud ) ) THEN 
  DEALLOCATE(grid%re_cloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3933,&
=======
 CALL wrf_error_fatal3("<stdin>",3982,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%re_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_ice ) ) THEN 
  DEALLOCATE(grid%re_ice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3940,&
=======
 CALL wrf_error_fatal3("<stdin>",3989,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%re_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_snow ) ) THEN 
  DEALLOCATE(grid%re_snow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3947,&
=======
 CALL wrf_error_fatal3("<stdin>",3996,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%re_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_cloud ) ) THEN 
  DEALLOCATE(grid%dfi_re_cloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3954,&
=======
 CALL wrf_error_fatal3("<stdin>",4003,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_re_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_ice ) ) THEN 
  DEALLOCATE(grid%dfi_re_ice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3961,&
=======
 CALL wrf_error_fatal3("<stdin>",4010,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_re_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_snow ) ) THEN 
  DEALLOCATE(grid%dfi_re_snow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3968,&
=======
 CALL wrf_error_fatal3("<stdin>",4017,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_re_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3975,&
=======
 CALL wrf_error_fatal3("<stdin>",4024,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scalar_bxs ) ) THEN 
  DEALLOCATE(grid%scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3982,&
=======
 CALL wrf_error_fatal3("<stdin>",4031,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_bxs. ')
 endif
  NULLIFY(grid%scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxe ) ) THEN 
  DEALLOCATE(grid%scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3990,&
=======
 CALL wrf_error_fatal3("<stdin>",4039,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_bxe. ')
 endif
  NULLIFY(grid%scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_bys ) ) THEN 
  DEALLOCATE(grid%scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",3998,&
=======
 CALL wrf_error_fatal3("<stdin>",4047,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_bys. ')
 endif
  NULLIFY(grid%scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%scalar_bye ) ) THEN 
  DEALLOCATE(grid%scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4006,&
=======
 CALL wrf_error_fatal3("<stdin>",4055,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_bye. ')
 endif
  NULLIFY(grid%scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxs ) ) THEN 
  DEALLOCATE(grid%scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4014,&
=======
 CALL wrf_error_fatal3("<stdin>",4063,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_btxs. ')
 endif
  NULLIFY(grid%scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxe ) ) THEN 
  DEALLOCATE(grid%scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4022,&
=======
 CALL wrf_error_fatal3("<stdin>",4071,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_btxe. ')
 endif
  NULLIFY(grid%scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_btys ) ) THEN 
  DEALLOCATE(grid%scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4030,&
=======
 CALL wrf_error_fatal3("<stdin>",4079,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_btys. ')
 endif
  NULLIFY(grid%scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%scalar_btye ) ) THEN 
  DEALLOCATE(grid%scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4038,&
=======
 CALL wrf_error_fatal3("<stdin>",4087,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%scalar_btye. ')
 endif
  NULLIFY(grid%scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar ) ) THEN 
  DEALLOCATE(grid%dfi_scalar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4046,&
=======
 CALL wrf_error_fatal3("<stdin>",4095,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4053,&
=======
 CALL wrf_error_fatal3("<stdin>",4102,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxs. ')
 endif
  NULLIFY(grid%dfi_scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4061,&
=======
 CALL wrf_error_fatal3("<stdin>",4110,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxe. ')
 endif
  NULLIFY(grid%dfi_scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4069,&
=======
 CALL wrf_error_fatal3("<stdin>",4118,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bys. ')
 endif
  NULLIFY(grid%dfi_scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4077,&
=======
 CALL wrf_error_fatal3("<stdin>",4126,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bye. ')
 endif
  NULLIFY(grid%dfi_scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4085,&
=======
 CALL wrf_error_fatal3("<stdin>",4134,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxs. ')
 endif
  NULLIFY(grid%dfi_scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4093,&
=======
 CALL wrf_error_fatal3("<stdin>",4142,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxe. ')
 endif
  NULLIFY(grid%dfi_scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4101,&
=======
 CALL wrf_error_fatal3("<stdin>",4150,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btys. ')
 endif
  NULLIFY(grid%dfi_scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4109,&
=======
 CALL wrf_error_fatal3("<stdin>",4158,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btye. ')
 endif
  NULLIFY(grid%dfi_scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%fcx ) ) THEN 
  DEALLOCATE(grid%fcx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4117,&
=======
 CALL wrf_error_fatal3("<stdin>",4166,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gcx ) ) THEN 
  DEALLOCATE(grid%gcx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4124,&
=======
 CALL wrf_error_fatal3("<stdin>",4173,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_layers ) ) THEN 
  DEALLOCATE(grid%soil_layers,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4131,&
=======
 CALL wrf_error_fatal3("<stdin>",4180,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soil_layers. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_levels ) ) THEN 
  DEALLOCATE(grid%soil_levels,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4138,&
=======
 CALL wrf_error_fatal3("<stdin>",4187,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soil_levels. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st ) ) THEN 
  DEALLOCATE(grid%st,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4145,&
=======
 CALL wrf_error_fatal3("<stdin>",4194,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm ) ) THEN 
  DEALLOCATE(grid%sm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4152,&
=======
 CALL wrf_error_fatal3("<stdin>",4201,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw ) ) THEN 
  DEALLOCATE(grid%sw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4159,&
=======
 CALL wrf_error_fatal3("<stdin>",4208,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt ) ) THEN 
  DEALLOCATE(grid%soilt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4166,&
=======
 CALL wrf_error_fatal3("<stdin>",4215,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm ) ) THEN 
  DEALLOCATE(grid%soilm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4173,&
=======
 CALL wrf_error_fatal3("<stdin>",4222,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4180,&
=======
 CALL wrf_error_fatal3("<stdin>",4229,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm000007. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4187,&
=======
 CALL wrf_error_fatal3("<stdin>",4236,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm007028. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4194,&
=======
 CALL wrf_error_fatal3("<stdin>",4243,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm028100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4201,&
=======
 CALL wrf_error_fatal3("<stdin>",4250,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm100255. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4208,&
=======
 CALL wrf_error_fatal3("<stdin>",4257,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st000007. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4215,&
=======
 CALL wrf_error_fatal3("<stdin>",4264,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st007028. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4222,&
=======
 CALL wrf_error_fatal3("<stdin>",4271,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st028100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4229,&
=======
 CALL wrf_error_fatal3("<stdin>",4278,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st100255. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4236,&
=======
 CALL wrf_error_fatal3("<stdin>",4285,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4243,&
=======
 CALL wrf_error_fatal3("<stdin>",4292,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4250,&
=======
 CALL wrf_error_fatal3("<stdin>",4299,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4257,&
=======
 CALL wrf_error_fatal3("<stdin>",4306,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4264,&
=======
 CALL wrf_error_fatal3("<stdin>",4313,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sm010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4271,&
=======
 CALL wrf_error_fatal3("<stdin>",4320,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4278,&
=======
 CALL wrf_error_fatal3("<stdin>",4327,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4285,&
=======
 CALL wrf_error_fatal3("<stdin>",4334,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4292,&
=======
 CALL wrf_error_fatal3("<stdin>",4341,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4299,&
=======
 CALL wrf_error_fatal3("<stdin>",4348,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4306,&
=======
 CALL wrf_error_fatal3("<stdin>",4355,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilm300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4313,&
=======
 CALL wrf_error_fatal3("<stdin>",4362,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4320,&
=======
 CALL wrf_error_fatal3("<stdin>",4369,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4327,&
=======
 CALL wrf_error_fatal3("<stdin>",4376,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4334,&
=======
 CALL wrf_error_fatal3("<stdin>",4383,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4341,&
=======
 CALL wrf_error_fatal3("<stdin>",4390,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sw010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4348,&
=======
 CALL wrf_error_fatal3("<stdin>",4397,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4355,&
=======
 CALL wrf_error_fatal3("<stdin>",4404,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4362,&
=======
 CALL wrf_error_fatal3("<stdin>",4411,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4369,&
=======
 CALL wrf_error_fatal3("<stdin>",4418,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4376,&
=======
 CALL wrf_error_fatal3("<stdin>",4425,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4383,&
=======
 CALL wrf_error_fatal3("<stdin>",4432,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilw300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4390,&
=======
 CALL wrf_error_fatal3("<stdin>",4439,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4397,&
=======
 CALL wrf_error_fatal3("<stdin>",4446,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4404,&
=======
 CALL wrf_error_fatal3("<stdin>",4453,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4411,&
=======
 CALL wrf_error_fatal3("<stdin>",4460,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4418,&
=======
 CALL wrf_error_fatal3("<stdin>",4467,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%st010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4425,&
=======
 CALL wrf_error_fatal3("<stdin>",4474,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4432,&
=======
 CALL wrf_error_fatal3("<stdin>",4481,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4439,&
=======
 CALL wrf_error_fatal3("<stdin>",4488,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4446,&
=======
 CALL wrf_error_fatal3("<stdin>",4495,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4453,&
=======
 CALL wrf_error_fatal3("<stdin>",4502,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4460,&
=======
 CALL wrf_error_fatal3("<stdin>",4509,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4467,&
=======
 CALL wrf_error_fatal3("<stdin>",4516,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%topostdv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4474,&
=======
 CALL wrf_error_fatal3("<stdin>",4523,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%toposlpx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4481,&
=======
 CALL wrf_error_fatal3("<stdin>",4530,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%toposlpy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slope ) ) THEN 
  DEALLOCATE(grid%slope,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4488,&
=======
 CALL wrf_error_fatal3("<stdin>",4537,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slope. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slp_azi ) ) THEN 
  DEALLOCATE(grid%slp_azi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4495,&
=======
 CALL wrf_error_fatal3("<stdin>",4544,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slp_azi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4502,&
=======
 CALL wrf_error_fatal3("<stdin>",4551,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shdmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4509,&
=======
 CALL wrf_error_fatal3("<stdin>",4558,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shdmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4516,&
=======
 CALL wrf_error_fatal3("<stdin>",4565,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snoalb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slopecat ) ) THEN 
  DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4523,&
=======
 CALL wrf_error_fatal3("<stdin>",4572,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slopecat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4530,&
=======
 CALL wrf_error_fatal3("<stdin>",4579,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%toposoil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4537,&
=======
 CALL wrf_error_fatal3("<stdin>",4586,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%landusef. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4544,&
=======
 CALL wrf_error_fatal3("<stdin>",4593,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilctop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4551,&
=======
 CALL wrf_error_fatal3("<stdin>",4600,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilcbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4558,&
=======
 CALL wrf_error_fatal3("<stdin>",4607,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4565,&
=======
 CALL wrf_error_fatal3("<stdin>",4614,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vegcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4572,&
=======
 CALL wrf_error_fatal3("<stdin>",4621,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_hour ) ) THEN 
  DEALLOCATE(grid%ts_hour,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4579,&
=======
 CALL wrf_error_fatal3("<stdin>",4628,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_hour. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_u ) ) THEN 
  DEALLOCATE(grid%ts_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4586,&
=======
 CALL wrf_error_fatal3("<stdin>",4635,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_v ) ) THEN 
  DEALLOCATE(grid%ts_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4593,&
=======
 CALL wrf_error_fatal3("<stdin>",4642,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_q ) ) THEN 
  DEALLOCATE(grid%ts_q,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4600,&
=======
 CALL wrf_error_fatal3("<stdin>",4649,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_q. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_t ) ) THEN 
  DEALLOCATE(grid%ts_t,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4607,&
=======
 CALL wrf_error_fatal3("<stdin>",4656,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_psfc ) ) THEN 
  DEALLOCATE(grid%ts_psfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4614,&
=======
 CALL wrf_error_fatal3("<stdin>",4663,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_psfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_glw ) ) THEN 
  DEALLOCATE(grid%ts_glw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4621,&
=======
 CALL wrf_error_fatal3("<stdin>",4670,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_glw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_gsw ) ) THEN 
  DEALLOCATE(grid%ts_gsw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4628,&
=======
 CALL wrf_error_fatal3("<stdin>",4677,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_gsw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_hfx ) ) THEN 
  DEALLOCATE(grid%ts_hfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4635,&
=======
 CALL wrf_error_fatal3("<stdin>",4684,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_lh ) ) THEN 
  DEALLOCATE(grid%ts_lh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4642,&
=======
 CALL wrf_error_fatal3("<stdin>",4691,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_lh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_tsk ) ) THEN 
  DEALLOCATE(grid%ts_tsk,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4649,&
=======
 CALL wrf_error_fatal3("<stdin>",4698,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_tslb ) ) THEN 
  DEALLOCATE(grid%ts_tslb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4656,&
=======
 CALL wrf_error_fatal3("<stdin>",4705,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_clw ) ) THEN 
  DEALLOCATE(grid%ts_clw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4663,&
=======
 CALL wrf_error_fatal3("<stdin>",4712,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_clw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rainc ) ) THEN 
  DEALLOCATE(grid%ts_rainc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4670,&
=======
 CALL wrf_error_fatal3("<stdin>",4719,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rainnc ) ) THEN 
  DEALLOCATE(grid%ts_rainnc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4677,&
=======
 CALL wrf_error_fatal3("<stdin>",4726,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_u_profile ) ) THEN 
  DEALLOCATE(grid%ts_u_profile,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4684,&
=======
 CALL wrf_error_fatal3("<stdin>",4733,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_u_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_v_profile ) ) THEN 
  DEALLOCATE(grid%ts_v_profile,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4691,&
=======
 CALL wrf_error_fatal3("<stdin>",4740,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_v_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_gph_profile ) ) THEN 
  DEALLOCATE(grid%ts_gph_profile,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4698,&
=======
 CALL wrf_error_fatal3("<stdin>",4747,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_gph_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_th_profile ) ) THEN 
  DEALLOCATE(grid%ts_th_profile,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4705,&
=======
 CALL wrf_error_fatal3("<stdin>",4754,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_th_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_qv_profile ) ) THEN 
  DEALLOCATE(grid%ts_qv_profile,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4712,&
=======
 CALL wrf_error_fatal3("<stdin>",4761,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_qv_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzr ) ) THEN 
  DEALLOCATE(grid%dzr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4719,&
=======
 CALL wrf_error_fatal3("<stdin>",4768,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzb ) ) THEN 
  DEALLOCATE(grid%dzb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4726,&
=======
 CALL wrf_error_fatal3("<stdin>",4775,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzg ) ) THEN 
  DEALLOCATE(grid%dzg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4733,&
=======
 CALL wrf_error_fatal3("<stdin>",4782,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%urb_param ) ) THEN 
  DEALLOCATE(grid%urb_param,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4740,&
=======
 CALL wrf_error_fatal3("<stdin>",4789,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%urb_param. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lp_urb2d ) ) THEN 
  DEALLOCATE(grid%lp_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4747,&
=======
 CALL wrf_error_fatal3("<stdin>",4796,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lp_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hi_urb2d ) ) THEN 
  DEALLOCATE(grid%hi_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4754,&
=======
 CALL wrf_error_fatal3("<stdin>",4803,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hi_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lb_urb2d ) ) THEN 
  DEALLOCATE(grid%lb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4761,&
=======
 CALL wrf_error_fatal3("<stdin>",4810,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgt_urb2d ) ) THEN 
  DEALLOCATE(grid%hgt_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4768,&
=======
 CALL wrf_error_fatal3("<stdin>",4817,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hgt_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad0_urb2d ) ) THEN 
  DEALLOCATE(grid%fad0_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4775,&
=======
 CALL wrf_error_fatal3("<stdin>",4824,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fad0_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad135_urb2d ) ) THEN 
  DEALLOCATE(grid%fad135_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4782,&
=======
 CALL wrf_error_fatal3("<stdin>",4831,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fad135_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad45_urb2d ) ) THEN 
  DEALLOCATE(grid%fad45_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4789,&
=======
 CALL wrf_error_fatal3("<stdin>",4838,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fad45_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pad_urb2d ) ) THEN 
  DEALLOCATE(grid%pad_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4796,&
=======
 CALL wrf_error_fatal3("<stdin>",4845,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pad_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad90_urb2d ) ) THEN 
  DEALLOCATE(grid%fad90_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4803,&
=======
 CALL wrf_error_fatal3("<stdin>",4852,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fad90_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rad_urb2d ) ) THEN 
  DEALLOCATE(grid%rad_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4810,&
=======
 CALL wrf_error_fatal3("<stdin>",4859,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rad_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mh_urb2d ) ) THEN 
  DEALLOCATE(grid%mh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4817,&
=======
 CALL wrf_error_fatal3("<stdin>",4866,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stdh_urb2d ) ) THEN 
  DEALLOCATE(grid%stdh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4824,&
=======
 CALL wrf_error_fatal3("<stdin>",4873,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%stdh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lf_urb2d ) ) THEN 
  DEALLOCATE(grid%lf_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4831,&
=======
 CALL wrf_error_fatal3("<stdin>",4880,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lf_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%car_urb2d ) ) THEN 
  DEALLOCATE(grid%car_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4838,&
=======
 CALL wrf_error_fatal3("<stdin>",4887,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%car_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2w_urb2d ) ) THEN 
  DEALLOCATE(grid%h2w_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4845,&
=======
 CALL wrf_error_fatal3("<stdin>",4894,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2w_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%svf_urb2d ) ) THEN 
  DEALLOCATE(grid%svf_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4852,&
=======
 CALL wrf_error_fatal3("<stdin>",4901,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%svf_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0s_urb2d ) ) THEN 
  DEALLOCATE(grid%z0s_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4859,&
=======
 CALL wrf_error_fatal3("<stdin>",4908,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z0s_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0r_urb2d ) ) THEN 
  DEALLOCATE(grid%z0r_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4866,&
=======
 CALL wrf_error_fatal3("<stdin>",4915,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z0r_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0m_urb2d ) ) THEN 
  DEALLOCATE(grid%z0m_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4873,&
=======
 CALL wrf_error_fatal3("<stdin>",4922,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z0m_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zds_urb2d ) ) THEN 
  DEALLOCATE(grid%zds_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4880,&
=======
 CALL wrf_error_fatal3("<stdin>",4929,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zds_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zdm_urb2d ) ) THEN 
  DEALLOCATE(grid%zdm_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4887,&
=======
 CALL wrf_error_fatal3("<stdin>",4936,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zdm_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zdr_urb2d ) ) THEN 
  DEALLOCATE(grid%zdr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4894,&
=======
 CALL wrf_error_fatal3("<stdin>",4943,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zdr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4901,&
=======
 CALL wrf_error_fatal3("<stdin>",4950,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smois. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4908,&
=======
 CALL wrf_error_fatal3("<stdin>",4957,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh2o. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smcrel ) ) THEN 
  DEALLOCATE(grid%smcrel,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4915,&
=======
 CALL wrf_error_fatal3("<stdin>",4964,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smcrel. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4922,&
=======
 CALL wrf_error_fatal3("<stdin>",4971,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icedepth ) ) THEN 
  DEALLOCATE(grid%icedepth,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4929,&
=======
 CALL wrf_error_fatal3("<stdin>",4978,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icedepth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xicem ) ) THEN 
  DEALLOCATE(grid%xicem,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4936,&
=======
 CALL wrf_error_fatal3("<stdin>",4985,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xicem. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albsi ) ) THEN 
  DEALLOCATE(grid%albsi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4943,&
=======
 CALL wrf_error_fatal3("<stdin>",4992,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albsi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowsi ) ) THEN 
  DEALLOCATE(grid%snowsi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4950,&
=======
 CALL wrf_error_fatal3("<stdin>",4999,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowsi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4957,&
=======
 CALL wrf_error_fatal3("<stdin>",5006,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smstav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4964,&
=======
 CALL wrf_error_fatal3("<stdin>",5013,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soldrain ) ) THEN 
  DEALLOCATE(grid%soldrain,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4971,&
=======
 CALL wrf_error_fatal3("<stdin>",5020,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soldrain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcheadrt ) ) THEN 
  DEALLOCATE(grid%sfcheadrt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4978,&
=======
 CALL wrf_error_fatal3("<stdin>",5027,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfcheadrt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%infxsrt ) ) THEN 
  DEALLOCATE(grid%infxsrt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4985,&
=======
 CALL wrf_error_fatal3("<stdin>",5034,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%infxsrt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4992,&
=======
 CALL wrf_error_fatal3("<stdin>",5041,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfcrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",4999,&
=======
 CALL wrf_error_fatal3("<stdin>",5048,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%udrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5006,&
=======
 CALL wrf_error_fatal3("<stdin>",5055,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ivgtyp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5013,&
=======
 CALL wrf_error_fatal3("<stdin>",5062,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%isltyp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5020,&
=======
 CALL wrf_error_fatal3("<stdin>",5069,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vegfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5027,&
=======
 CALL wrf_error_fatal3("<stdin>",5076,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfcevp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5034,&
=======
 CALL wrf_error_fatal3("<stdin>",5083,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grdflx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acgrdflx ) ) THEN 
  DEALLOCATE(grid%acgrdflx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5041,&
=======
 CALL wrf_error_fatal3("<stdin>",5090,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acgrdflx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5048,&
=======
 CALL wrf_error_fatal3("<stdin>",5097,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfcexc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5055,&
=======
 CALL wrf_error_fatal3("<stdin>",5104,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acsnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acrunoff ) ) THEN 
  DEALLOCATE(grid%acrunoff,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5062,&
=======
 CALL wrf_error_fatal3("<stdin>",5111,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5069,&
=======
 CALL wrf_error_fatal3("<stdin>",5118,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acsnom. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5076,&
=======
 CALL wrf_error_fatal3("<stdin>",5125,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5083,&
=======
 CALL wrf_error_fatal3("<stdin>",5132,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5090,&
=======
 CALL wrf_error_fatal3("<stdin>",5139,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sstsk ) ) THEN 
  DEALLOCATE(grid%sstsk,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5097,&
=======
 CALL wrf_error_fatal3("<stdin>",5146,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sstsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake_depth ) ) THEN 
  DEALLOCATE(grid%lake_depth,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5104,&
=======
 CALL wrf_error_fatal3("<stdin>",5153,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lake_depth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtw ) ) THEN 
  DEALLOCATE(grid%dtw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5111,&
=======
 CALL wrf_error_fatal3("<stdin>",5160,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dtw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uoce ) ) THEN 
  DEALLOCATE(grid%uoce,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5118,&
=======
 CALL wrf_error_fatal3("<stdin>",5167,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uoce. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%voce ) ) THEN 
  DEALLOCATE(grid%voce,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5125,&
=======
 CALL wrf_error_fatal3("<stdin>",5174,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%voce. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hcoeff ) ) THEN 
  DEALLOCATE(grid%hcoeff,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5132,&
=======
 CALL wrf_error_fatal3("<stdin>",5181,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hcoeff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_p ) ) THEN 
  DEALLOCATE(grid%dfi_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5139,&
=======
 CALL wrf_error_fatal3("<stdin>",5188,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_al ) ) THEN 
  DEALLOCATE(grid%dfi_al,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5146,&
=======
 CALL wrf_error_fatal3("<stdin>",5195,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_al. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_mu ) ) THEN 
  DEALLOCATE(grid%dfi_mu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5153,&
=======
 CALL wrf_error_fatal3("<stdin>",5202,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_phb ) ) THEN 
  DEALLOCATE(grid%dfi_phb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5160,&
=======
 CALL wrf_error_fatal3("<stdin>",5209,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_phb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ph0 ) ) THEN 
  DEALLOCATE(grid%dfi_ph0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5167,&
=======
 CALL wrf_error_fatal3("<stdin>",5216,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_ph0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_php ) ) THEN 
  DEALLOCATE(grid%dfi_php,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5174,&
=======
 CALL wrf_error_fatal3("<stdin>",5223,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_php. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_u ) ) THEN 
  DEALLOCATE(grid%dfi_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5181,&
=======
 CALL wrf_error_fatal3("<stdin>",5230,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_v ) ) THEN 
  DEALLOCATE(grid%dfi_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5188,&
=======
 CALL wrf_error_fatal3("<stdin>",5237,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_w ) ) THEN 
  DEALLOCATE(grid%dfi_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5195,&
=======
 CALL wrf_error_fatal3("<stdin>",5244,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ww ) ) THEN 
  DEALLOCATE(grid%dfi_ww,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5202,&
=======
 CALL wrf_error_fatal3("<stdin>",5251,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_ww. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_t ) ) THEN 
  DEALLOCATE(grid%dfi_t,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5209,&
=======
 CALL wrf_error_fatal3("<stdin>",5258,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_rh ) ) THEN 
  DEALLOCATE(grid%dfi_rh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5216,&
=======
 CALL wrf_error_fatal3("<stdin>",5265,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_rh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ph ) ) THEN 
  DEALLOCATE(grid%dfi_ph,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5223,&
=======
 CALL wrf_error_fatal3("<stdin>",5272,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_ph. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_pb ) ) THEN 
  DEALLOCATE(grid%dfi_pb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5230,&
=======
 CALL wrf_error_fatal3("<stdin>",5279,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_pb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_alt ) ) THEN 
  DEALLOCATE(grid%dfi_alt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5237,&
=======
 CALL wrf_error_fatal3("<stdin>",5286,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tke ) ) THEN 
  DEALLOCATE(grid%dfi_tke,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5244,&
=======
 CALL wrf_error_fatal3("<stdin>",5293,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_tke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tten_rad ) ) THEN 
  DEALLOCATE(grid%dfi_tten_rad,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5251,&
=======
 CALL wrf_error_fatal3("<stdin>",5300,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_tten_rad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tslb ) ) THEN 
  DEALLOCATE(grid%dfi_tslb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5258,&
=======
 CALL wrf_error_fatal3("<stdin>",5307,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_smois ) ) THEN 
  DEALLOCATE(grid%dfi_smois,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5265,&
=======
 CALL wrf_error_fatal3("<stdin>",5314,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_smois. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snow ) ) THEN 
  DEALLOCATE(grid%dfi_snow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5272,&
=======
 CALL wrf_error_fatal3("<stdin>",5321,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snowh ) ) THEN 
  DEALLOCATE(grid%dfi_snowh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5279,&
=======
 CALL wrf_error_fatal3("<stdin>",5328,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_snowh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_canwat ) ) THEN 
  DEALLOCATE(grid%dfi_canwat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5286,&
=======
 CALL wrf_error_fatal3("<stdin>",5335,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_canwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_smfr3d ) ) THEN 
  DEALLOCATE(grid%dfi_smfr3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5293,&
=======
 CALL wrf_error_fatal3("<stdin>",5342,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_smfr3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%dfi_keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5300,&
=======
 CALL wrf_error_fatal3("<stdin>",5349,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_keepfr3dflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d ) ) THEN 
  DEALLOCATE(grid%tr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5307,&
=======
 CALL wrf_error_fatal3("<stdin>",5356,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgr_urb2d ) ) THEN 
  DEALLOCATE(grid%tgr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5314,&
=======
 CALL wrf_error_fatal3("<stdin>",5363,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d ) ) THEN 
  DEALLOCATE(grid%tb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5321,&
=======
 CALL wrf_error_fatal3("<stdin>",5370,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d ) ) THEN 
  DEALLOCATE(grid%tg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5328,&
=======
 CALL wrf_error_fatal3("<stdin>",5377,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d ) ) THEN 
  DEALLOCATE(grid%tc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5335,&
=======
 CALL wrf_error_fatal3("<stdin>",5384,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d ) ) THEN 
  DEALLOCATE(grid%qc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5342,&
=======
 CALL wrf_error_fatal3("<stdin>",5391,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d ) ) THEN 
  DEALLOCATE(grid%uc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5349,&
=======
 CALL wrf_error_fatal3("<stdin>",5398,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxr_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5356,&
=======
 CALL wrf_error_fatal3("<stdin>",5405,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xxxr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxb_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5363,&
=======
 CALL wrf_error_fatal3("<stdin>",5412,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xxxb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxg_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5370,&
=======
 CALL wrf_error_fatal3("<stdin>",5419,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xxxg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxc_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5377,&
=======
 CALL wrf_error_fatal3("<stdin>",5426,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xxxc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmcr_urb2d ) ) THEN 
  DEALLOCATE(grid%cmcr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5384,&
=======
 CALL wrf_error_fatal3("<stdin>",5433,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmcr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelr_urb2d ) ) THEN 
  DEALLOCATE(grid%drelr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5391,&
=======
 CALL wrf_error_fatal3("<stdin>",5440,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%drelr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelb_urb2d ) ) THEN 
  DEALLOCATE(grid%drelb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5398,&
=======
 CALL wrf_error_fatal3("<stdin>",5447,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%drelb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelg_urb2d ) ) THEN 
  DEALLOCATE(grid%drelg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5405,&
=======
 CALL wrf_error_fatal3("<stdin>",5454,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%drelg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumr_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5412,&
=======
 CALL wrf_error_fatal3("<stdin>",5461,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flxhumr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumb_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5419,&
=======
 CALL wrf_error_fatal3("<stdin>",5468,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flxhumb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumg_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5426,&
=======
 CALL wrf_error_fatal3("<stdin>",5475,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flxhumg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgrl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgrl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5433,&
=======
 CALL wrf_error_fatal3("<stdin>",5482,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgrl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smr_urb3d ) ) THEN 
  DEALLOCATE(grid%smr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5440,&
=======
 CALL wrf_error_fatal3("<stdin>",5489,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smr_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d ) ) THEN 
  DEALLOCATE(grid%trl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5447,&
=======
 CALL wrf_error_fatal3("<stdin>",5496,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5454,&
=======
 CALL wrf_error_fatal3("<stdin>",5503,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5461,&
=======
 CALL wrf_error_fatal3("<stdin>",5510,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d ) ) THEN 
  DEALLOCATE(grid%sh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5468,&
=======
 CALL wrf_error_fatal3("<stdin>",5517,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d ) ) THEN 
  DEALLOCATE(grid%lh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5475,&
=======
 CALL wrf_error_fatal3("<stdin>",5524,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%g_urb2d ) ) THEN 
  DEALLOCATE(grid%g_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5482,&
=======
 CALL wrf_error_fatal3("<stdin>",5531,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%g_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d ) ) THEN 
  DEALLOCATE(grid%rn_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5489,&
=======
 CALL wrf_error_fatal3("<stdin>",5538,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d ) ) THEN 
  DEALLOCATE(grid%ts_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5496,&
=======
 CALL wrf_error_fatal3("<stdin>",5545,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%frc_urb2d ) ) THEN 
  DEALLOCATE(grid%frc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5503,&
=======
 CALL wrf_error_fatal3("<stdin>",5552,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%frc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%utype_urb2d ) ) THEN 
  DEALLOCATE(grid%utype_urb2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5510,&
=======
 CALL wrf_error_fatal3("<stdin>",5559,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%utype_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trb_urb4d ) ) THEN 
  DEALLOCATE(grid%trb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5517,&
=======
 CALL wrf_error_fatal3("<stdin>",5566,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%trb_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw1_urb4d ) ) THEN 
  DEALLOCATE(grid%tw1_urb4d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5524,&
=======
 CALL wrf_error_fatal3("<stdin>",5573,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tw1_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw2_urb4d ) ) THEN 
  DEALLOCATE(grid%tw2_urb4d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5531,&
=======
 CALL wrf_error_fatal3("<stdin>",5580,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tw2_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgb_urb4d ) ) THEN 
  DEALLOCATE(grid%tgb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5538,&
=======
 CALL wrf_error_fatal3("<stdin>",5587,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgb_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlev_urb3d ) ) THEN 
  DEALLOCATE(grid%tlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5545,&
=======
 CALL wrf_error_fatal3("<stdin>",5594,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tlev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlev_urb3d ) ) THEN 
  DEALLOCATE(grid%qlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5552,&
=======
 CALL wrf_error_fatal3("<stdin>",5601,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qlev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw1lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw1lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5559,&
=======
 CALL wrf_error_fatal3("<stdin>",5608,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tw1lev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw2lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw2lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5566,&
=======
 CALL wrf_error_fatal3("<stdin>",5615,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tw2lev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tglev_urb3d ) ) THEN 
  DEALLOCATE(grid%tglev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5573,&
=======
 CALL wrf_error_fatal3("<stdin>",5622,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tglev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tflev_urb3d ) ) THEN 
  DEALLOCATE(grid%tflev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5580,&
=======
 CALL wrf_error_fatal3("<stdin>",5629,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tflev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%sf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5587,&
=======
 CALL wrf_error_fatal3("<stdin>",5636,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sf_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%lf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5594,&
=======
 CALL wrf_error_fatal3("<stdin>",5643,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lf_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cm_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%cm_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5601,&
=======
 CALL wrf_error_fatal3("<stdin>",5650,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cm_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%sfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5608,&
=======
 CALL wrf_error_fatal3("<stdin>",5657,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfvent_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%lfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5615,&
=======
 CALL wrf_error_fatal3("<stdin>",5664,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lfvent_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfwin1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5622,&
=======
 CALL wrf_error_fatal3("<stdin>",5671,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfwin1_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfwin2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5629,&
=======
 CALL wrf_error_fatal3("<stdin>",5678,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfwin2_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfw1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5636,&
=======
 CALL wrf_error_fatal3("<stdin>",5685,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfw1_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfw2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5643,&
=======
 CALL wrf_error_fatal3("<stdin>",5692,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfw2_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfr_urb3d ) ) THEN 
  DEALLOCATE(grid%sfr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5650,&
=======
 CALL wrf_error_fatal3("<stdin>",5699,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfr_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfg_urb3d ) ) THEN 
  DEALLOCATE(grid%sfg_urb3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5657,&
=======
 CALL wrf_error_fatal3("<stdin>",5706,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sfg_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5664,&
=======
 CALL wrf_error_fatal3("<stdin>",5713,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5671,&
=======
 CALL wrf_error_fatal3("<stdin>",5720,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmc_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5678,&
=======
 CALL wrf_error_fatal3("<stdin>",5727,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmc_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chc_sfcdif ) ) THEN 
  DEALLOCATE(grid%chc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5685,&
=======
 CALL wrf_error_fatal3("<stdin>",5734,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chc_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5692,&
=======
 CALL wrf_error_fatal3("<stdin>",5741,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmgr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5699,&
=======
 CALL wrf_error_fatal3("<stdin>",5748,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chgr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%coszen ) ) THEN 
  DEALLOCATE(grid%coszen,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5706,&
=======
 CALL wrf_error_fatal3("<stdin>",5755,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%coszen. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hrang ) ) THEN 
  DEALLOCATE(grid%hrang,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5713,&
=======
 CALL wrf_error_fatal3("<stdin>",5762,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hrang. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rhosnf ) ) THEN 
  DEALLOCATE(grid%rhosnf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5720,&
=======
 CALL wrf_error_fatal3("<stdin>",5769,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rhosnf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowfallac ) ) THEN 
  DEALLOCATE(grid%snowfallac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5727,&
=======
 CALL wrf_error_fatal3("<stdin>",5776,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowfallac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%precipfr ) ) THEN 
  DEALLOCATE(grid%precipfr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5734,&
=======
 CALL wrf_error_fatal3("<stdin>",5783,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%precipfr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5741,&
=======
 CALL wrf_error_fatal3("<stdin>",5790,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smfr3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5748,&
=======
 CALL wrf_error_fatal3("<stdin>",5797,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%keepfr3dflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swvisdir ) ) THEN 
  DEALLOCATE(grid%swvisdir,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5755,&
=======
 CALL wrf_error_fatal3("<stdin>",5804,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swvisdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swvisdif ) ) THEN 
  DEALLOCATE(grid%swvisdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5762,&
=======
 CALL wrf_error_fatal3("<stdin>",5811,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swvisdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnirdir ) ) THEN 
  DEALLOCATE(grid%swnirdir,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5769,&
=======
 CALL wrf_error_fatal3("<stdin>",5818,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swnirdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnirdif ) ) THEN 
  DEALLOCATE(grid%swnirdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5776,&
=======
 CALL wrf_error_fatal3("<stdin>",5825,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swnirdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswvisdir ) ) THEN 
  DEALLOCATE(grid%alswvisdir,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5783,&
=======
 CALL wrf_error_fatal3("<stdin>",5832,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alswvisdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswvisdif ) ) THEN 
  DEALLOCATE(grid%alswvisdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5790,&
=======
 CALL wrf_error_fatal3("<stdin>",5839,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alswvisdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswnirdir ) ) THEN 
  DEALLOCATE(grid%alswnirdir,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5797,&
=======
 CALL wrf_error_fatal3("<stdin>",5846,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alswnirdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswnirdif ) ) THEN 
  DEALLOCATE(grid%alswnirdif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5804,&
=======
 CALL wrf_error_fatal3("<stdin>",5853,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alswnirdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ra ) ) THEN 
  DEALLOCATE(grid%ra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5811,&
=======
 CALL wrf_error_fatal3("<stdin>",5860,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rs ) ) THEN 
  DEALLOCATE(grid%rs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5818,&
=======
 CALL wrf_error_fatal3("<stdin>",5867,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai ) ) THEN 
  DEALLOCATE(grid%lai,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5825,&
=======
 CALL wrf_error_fatal3("<stdin>",5874,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegf_px ) ) THEN 
  DEALLOCATE(grid%vegf_px,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5832,&
=======
 CALL wrf_error_fatal3("<stdin>",5881,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vegf_px. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2obs ) ) THEN 
  DEALLOCATE(grid%t2obs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5839,&
=======
 CALL wrf_error_fatal3("<stdin>",5888,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2obs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2obs ) ) THEN 
  DEALLOCATE(grid%q2obs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5846,&
=======
 CALL wrf_error_fatal3("<stdin>",5895,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2obs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imperv ) ) THEN 
  DEALLOCATE(grid%imperv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5853,&
=======
 CALL wrf_error_fatal3("<stdin>",5902,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%imperv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canfra ) ) THEN 
  DEALLOCATE(grid%canfra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5860,&
=======
 CALL wrf_error_fatal3("<stdin>",5909,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fm ) ) THEN 
  DEALLOCATE(grid%fm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5867,&
=======
 CALL wrf_error_fatal3("<stdin>",5916,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fh ) ) THEN 
  DEALLOCATE(grid%fh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5874,&
=======
 CALL wrf_error_fatal3("<stdin>",5923,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%br ) ) THEN 
  DEALLOCATE(grid%br,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5881,&
=======
 CALL wrf_error_fatal3("<stdin>",5930,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%br. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zol ) ) THEN 
  DEALLOCATE(grid%zol,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5888,&
=======
 CALL wrf_error_fatal3("<stdin>",5937,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wstar_ysu ) ) THEN 
  DEALLOCATE(grid%wstar_ysu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5895,&
=======
 CALL wrf_error_fatal3("<stdin>",5944,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wstar_ysu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%delta_ysu ) ) THEN 
  DEALLOCATE(grid%delta_ysu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5902,&
=======
 CALL wrf_error_fatal3("<stdin>",5951,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%delta_ysu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5909,&
=======
 CALL wrf_error_fatal3("<stdin>",5958,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exch_h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_m ) ) THEN 
  DEALLOCATE(grid%exch_m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5916,&
=======
 CALL wrf_error_fatal3("<stdin>",5965,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exch_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ct ) ) THEN 
  DEALLOCATE(grid%ct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5923,&
=======
 CALL wrf_error_fatal3("<stdin>",5972,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5930,&
=======
 CALL wrf_error_fatal3("<stdin>",5979,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5937,&
=======
 CALL wrf_error_fatal3("<stdin>",5986,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5944,&
=======
 CALL wrf_error_fatal3("<stdin>",5993,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5951,&
=======
 CALL wrf_error_fatal3("<stdin>",6000,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5958,&
=======
 CALL wrf_error_fatal3("<stdin>",6007,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5965,&
=======
 CALL wrf_error_fatal3("<stdin>",6014,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5972,&
=======
 CALL wrf_error_fatal3("<stdin>",6021,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%akhs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5979,&
=======
 CALL wrf_error_fatal3("<stdin>",6028,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%akms. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kpbl ) ) THEN 
  DEALLOCATE(grid%kpbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5986,&
=======
 CALL wrf_error_fatal3("<stdin>",6035,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kpbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akpbl ) ) THEN 
  DEALLOCATE(grid%akpbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",5993,&
=======
 CALL wrf_error_fatal3("<stdin>",6042,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%akpbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tshltr ) ) THEN 
  DEALLOCATE(grid%tshltr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6000,&
=======
 CALL wrf_error_fatal3("<stdin>",6049,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qshltr ) ) THEN 
  DEALLOCATE(grid%qshltr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6007,&
=======
 CALL wrf_error_fatal3("<stdin>",6056,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pshltr ) ) THEN 
  DEALLOCATE(grid%pshltr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6014,&
=======
 CALL wrf_error_fatal3("<stdin>",6063,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th10 ) ) THEN 
  DEALLOCATE(grid%th10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6021,&
=======
 CALL wrf_error_fatal3("<stdin>",6070,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q10 ) ) THEN 
  DEALLOCATE(grid%q10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6028,&
=======
 CALL wrf_error_fatal3("<stdin>",6077,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%massflux_edkf ) ) THEN 
  DEALLOCATE(grid%massflux_edkf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6035,&
=======
 CALL wrf_error_fatal3("<stdin>",6084,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%massflux_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%entr_edkf ) ) THEN 
  DEALLOCATE(grid%entr_edkf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6042,&
=======
 CALL wrf_error_fatal3("<stdin>",6091,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%entr_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%detr_edkf ) ) THEN 
  DEALLOCATE(grid%detr_edkf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6049,&
=======
 CALL wrf_error_fatal3("<stdin>",6098,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%detr_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thl_up ) ) THEN 
  DEALLOCATE(grid%thl_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6056,&
=======
 CALL wrf_error_fatal3("<stdin>",6105,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thl_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thv_up ) ) THEN 
  DEALLOCATE(grid%thv_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6063,&
=======
 CALL wrf_error_fatal3("<stdin>",6112,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thv_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_up ) ) THEN 
  DEALLOCATE(grid%rv_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6070,&
=======
 CALL wrf_error_fatal3("<stdin>",6119,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rt_up ) ) THEN 
  DEALLOCATE(grid%rt_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6077,&
=======
 CALL wrf_error_fatal3("<stdin>",6126,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rt_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rc_up ) ) THEN 
  DEALLOCATE(grid%rc_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6084,&
=======
 CALL wrf_error_fatal3("<stdin>",6133,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rc_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_up ) ) THEN 
  DEALLOCATE(grid%u_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6091,&
=======
 CALL wrf_error_fatal3("<stdin>",6140,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_up ) ) THEN 
  DEALLOCATE(grid%v_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6098,&
=======
 CALL wrf_error_fatal3("<stdin>",6147,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%frac_up ) ) THEN 
  DEALLOCATE(grid%frac_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6105,&
=======
 CALL wrf_error_fatal3("<stdin>",6154,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%frac_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rc_mf ) ) THEN 
  DEALLOCATE(grid%rc_mf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6112,&
=======
 CALL wrf_error_fatal3("<stdin>",6161,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rc_mf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%te_temf ) ) THEN 
  DEALLOCATE(grid%te_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6119,&
=======
 CALL wrf_error_fatal3("<stdin>",6168,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%te_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kh_temf ) ) THEN 
  DEALLOCATE(grid%kh_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6126,&
=======
 CALL wrf_error_fatal3("<stdin>",6175,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kh_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%km_temf ) ) THEN 
  DEALLOCATE(grid%km_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6133,&
=======
 CALL wrf_error_fatal3("<stdin>",6182,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%km_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shf_temf ) ) THEN 
  DEALLOCATE(grid%shf_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6140,&
=======
 CALL wrf_error_fatal3("<stdin>",6189,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qf_temf ) ) THEN 
  DEALLOCATE(grid%qf_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6147,&
=======
 CALL wrf_error_fatal3("<stdin>",6196,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uw_temf ) ) THEN 
  DEALLOCATE(grid%uw_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6154,&
=======
 CALL wrf_error_fatal3("<stdin>",6203,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uw_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vw_temf ) ) THEN 
  DEALLOCATE(grid%vw_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6161,&
=======
 CALL wrf_error_fatal3("<stdin>",6210,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vw_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wupd_temf ) ) THEN 
  DEALLOCATE(grid%wupd_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6168,&
=======
 CALL wrf_error_fatal3("<stdin>",6217,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wupd_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mf_temf ) ) THEN 
  DEALLOCATE(grid%mf_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6175,&
=======
 CALL wrf_error_fatal3("<stdin>",6224,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thup_temf ) ) THEN 
  DEALLOCATE(grid%thup_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6182,&
=======
 CALL wrf_error_fatal3("<stdin>",6231,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtup_temf ) ) THEN 
  DEALLOCATE(grid%qtup_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6189,&
=======
 CALL wrf_error_fatal3("<stdin>",6238,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlup_temf ) ) THEN 
  DEALLOCATE(grid%qlup_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6196,&
=======
 CALL wrf_error_fatal3("<stdin>",6245,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qlup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cf3d_temf ) ) THEN 
  DEALLOCATE(grid%cf3d_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6203,&
=======
 CALL wrf_error_fatal3("<stdin>",6252,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cf3d_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hd_temf ) ) THEN 
  DEALLOCATE(grid%hd_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6210,&
=======
 CALL wrf_error_fatal3("<stdin>",6259,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hd_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lcl_temf ) ) THEN 
  DEALLOCATE(grid%lcl_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6217,&
=======
 CALL wrf_error_fatal3("<stdin>",6266,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lcl_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hct_temf ) ) THEN 
  DEALLOCATE(grid%hct_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6224,&
=======
 CALL wrf_error_fatal3("<stdin>",6273,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hct_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfm_temf ) ) THEN 
  DEALLOCATE(grid%cfm_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6231,&
=======
 CALL wrf_error_fatal3("<stdin>",6280,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfm_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wm_temf ) ) THEN 
  DEALLOCATE(grid%wm_temf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6238,&
=======
 CALL wrf_error_fatal3("<stdin>",6287,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wm_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qke ) ) THEN 
  DEALLOCATE(grid%qke,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6245,&
=======
 CALL wrf_error_fatal3("<stdin>",6294,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qshear ) ) THEN 
  DEALLOCATE(grid%qshear,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6252,&
=======
 CALL wrf_error_fatal3("<stdin>",6301,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qshear. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qbuoy ) ) THEN 
  DEALLOCATE(grid%qbuoy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6259,&
=======
 CALL wrf_error_fatal3("<stdin>",6308,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qbuoy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qdiss ) ) THEN 
  DEALLOCATE(grid%qdiss,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6266,&
=======
 CALL wrf_error_fatal3("<stdin>",6315,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qdiss. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qwt ) ) THEN 
  DEALLOCATE(grid%qwt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6273,&
=======
 CALL wrf_error_fatal3("<stdin>",6322,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dqke ) ) THEN 
  DEALLOCATE(grid%dqke,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6280,&
=======
 CALL wrf_error_fatal3("<stdin>",6329,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dqke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsq ) ) THEN 
  DEALLOCATE(grid%tsq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6287,&
=======
 CALL wrf_error_fatal3("<stdin>",6336,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsq ) ) THEN 
  DEALLOCATE(grid%qsq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6294,&
=======
 CALL wrf_error_fatal3("<stdin>",6343,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cov ) ) THEN 
  DEALLOCATE(grid%cov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6301,&
=======
 CALL wrf_error_fatal3("<stdin>",6350,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh3d ) ) THEN 
  DEALLOCATE(grid%sh3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6308,&
=======
 CALL wrf_error_fatal3("<stdin>",6357,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ch ) ) THEN 
  DEALLOCATE(grid%ch,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6315,&
=======
 CALL wrf_error_fatal3("<stdin>",6364,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_a ) ) THEN 
  DEALLOCATE(grid%edmf_a,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6322,&
=======
 CALL wrf_error_fatal3("<stdin>",6371,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_w ) ) THEN 
  DEALLOCATE(grid%edmf_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6329,&
=======
 CALL wrf_error_fatal3("<stdin>",6378,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_thl ) ) THEN 
  DEALLOCATE(grid%edmf_thl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6336,&
=======
 CALL wrf_error_fatal3("<stdin>",6385,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_thl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_qt ) ) THEN 
  DEALLOCATE(grid%edmf_qt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6343,&
=======
 CALL wrf_error_fatal3("<stdin>",6392,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_qt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_ent ) ) THEN 
  DEALLOCATE(grid%edmf_ent,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6350,&
=======
 CALL wrf_error_fatal3("<stdin>",6399,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_ent. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_qc ) ) THEN 
  DEALLOCATE(grid%edmf_qc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6357,&
=======
 CALL wrf_error_fatal3("<stdin>",6406,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edmf_qc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgdp ) ) THEN 
  DEALLOCATE(grid%fgdp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6364,&
=======
 CALL wrf_error_fatal3("<stdin>",6413,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fgdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfgdp ) ) THEN 
  DEALLOCATE(grid%dfgdp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6371,&
=======
 CALL wrf_error_fatal3("<stdin>",6420,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfgdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vdfg ) ) THEN 
  DEALLOCATE(grid%vdfg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6378,&
=======
 CALL wrf_error_fatal3("<stdin>",6427,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vdfg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_tke ) ) THEN 
  DEALLOCATE(grid%exch_tke,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6385,&
=======
 CALL wrf_error_fatal3("<stdin>",6434,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exch_tke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtaux3d ) ) THEN 
  DEALLOCATE(grid%dtaux3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6392,&
=======
 CALL wrf_error_fatal3("<stdin>",6441,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dtaux3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtauy3d ) ) THEN 
  DEALLOCATE(grid%dtauy3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6399,&
=======
 CALL wrf_error_fatal3("<stdin>",6448,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dtauy3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dusfcg ) ) THEN 
  DEALLOCATE(grid%dusfcg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6406,&
=======
 CALL wrf_error_fatal3("<stdin>",6455,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dusfcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dvsfcg ) ) THEN 
  DEALLOCATE(grid%dvsfcg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6413,&
=======
 CALL wrf_error_fatal3("<stdin>",6462,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dvsfcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%var2d ) ) THEN 
  DEALLOCATE(grid%var2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6420,&
=======
 CALL wrf_error_fatal3("<stdin>",6469,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%var2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oc12d ) ) THEN 
  DEALLOCATE(grid%oc12d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6427,&
=======
 CALL wrf_error_fatal3("<stdin>",6476,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%oc12d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa1 ) ) THEN 
  DEALLOCATE(grid%oa1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6434,&
=======
 CALL wrf_error_fatal3("<stdin>",6483,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%oa1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa2 ) ) THEN 
  DEALLOCATE(grid%oa2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6441,&
=======
 CALL wrf_error_fatal3("<stdin>",6490,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%oa2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa3 ) ) THEN 
  DEALLOCATE(grid%oa3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6448,&
=======
 CALL wrf_error_fatal3("<stdin>",6497,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%oa3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa4 ) ) THEN 
  DEALLOCATE(grid%oa4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6455,&
=======
 CALL wrf_error_fatal3("<stdin>",6504,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%oa4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol1 ) ) THEN 
  DEALLOCATE(grid%ol1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6462,&
=======
 CALL wrf_error_fatal3("<stdin>",6511,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ol1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol2 ) ) THEN 
  DEALLOCATE(grid%ol2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6469,&
=======
 CALL wrf_error_fatal3("<stdin>",6518,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ol2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol3 ) ) THEN 
  DEALLOCATE(grid%ol3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6476,&
=======
 CALL wrf_error_fatal3("<stdin>",6525,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ol3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol4 ) ) THEN 
  DEALLOCATE(grid%ol4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6483,&
=======
 CALL wrf_error_fatal3("<stdin>",6532,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ol4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ctopo ) ) THEN 
  DEALLOCATE(grid%ctopo,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6490,&
=======
 CALL wrf_error_fatal3("<stdin>",6539,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ctopo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ctopo2 ) ) THEN 
  DEALLOCATE(grid%ctopo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6497,&
=======
 CALL wrf_error_fatal3("<stdin>",6546,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ctopo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_u_bep ) ) THEN 
  DEALLOCATE(grid%a_u_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6504,&
=======
 CALL wrf_error_fatal3("<stdin>",6553,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%a_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_v_bep ) ) THEN 
  DEALLOCATE(grid%a_v_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6511,&
=======
 CALL wrf_error_fatal3("<stdin>",6560,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%a_v_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_t_bep ) ) THEN 
  DEALLOCATE(grid%a_t_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6518,&
=======
 CALL wrf_error_fatal3("<stdin>",6567,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%a_t_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_q_bep ) ) THEN 
  DEALLOCATE(grid%a_q_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6525,&
=======
 CALL wrf_error_fatal3("<stdin>",6574,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%a_q_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_e_bep ) ) THEN 
  DEALLOCATE(grid%a_e_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6532,&
=======
 CALL wrf_error_fatal3("<stdin>",6581,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%a_e_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_u_bep ) ) THEN 
  DEALLOCATE(grid%b_u_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6539,&
=======
 CALL wrf_error_fatal3("<stdin>",6588,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%b_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_v_bep ) ) THEN 
  DEALLOCATE(grid%b_v_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6546,&
=======
 CALL wrf_error_fatal3("<stdin>",6595,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%b_v_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_t_bep ) ) THEN 
  DEALLOCATE(grid%b_t_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6553,&
=======
 CALL wrf_error_fatal3("<stdin>",6602,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%b_t_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_q_bep ) ) THEN 
  DEALLOCATE(grid%b_q_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6560,&
=======
 CALL wrf_error_fatal3("<stdin>",6609,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%b_q_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_e_bep ) ) THEN 
  DEALLOCATE(grid%b_e_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6567,&
=======
 CALL wrf_error_fatal3("<stdin>",6616,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%b_e_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlg_bep ) ) THEN 
  DEALLOCATE(grid%dlg_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6574,&
=======
 CALL wrf_error_fatal3("<stdin>",6623,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dlg_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dl_u_bep ) ) THEN 
  DEALLOCATE(grid%dl_u_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6581,&
=======
 CALL wrf_error_fatal3("<stdin>",6630,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dl_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sf_bep ) ) THEN 
  DEALLOCATE(grid%sf_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6588,&
=======
 CALL wrf_error_fatal3("<stdin>",6637,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sf_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vl_bep ) ) THEN 
  DEALLOCATE(grid%vl_bep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6595,&
=======
 CALL wrf_error_fatal3("<stdin>",6644,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vl_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_pbl ) ) THEN 
  DEALLOCATE(grid%tke_pbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6602,&
=======
 CALL wrf_error_fatal3("<stdin>",6651,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tke_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%el_pbl ) ) THEN 
  DEALLOCATE(grid%el_pbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6609,&
=======
 CALL wrf_error_fatal3("<stdin>",6658,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%el_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wu_tur ) ) THEN 
  DEALLOCATE(grid%wu_tur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6616,&
=======
 CALL wrf_error_fatal3("<stdin>",6665,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wu_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wv_tur ) ) THEN 
  DEALLOCATE(grid%wv_tur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6623,&
=======
 CALL wrf_error_fatal3("<stdin>",6672,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wv_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wt_tur ) ) THEN 
  DEALLOCATE(grid%wt_tur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6630,&
=======
 CALL wrf_error_fatal3("<stdin>",6679,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wt_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wq_tur ) ) THEN 
  DEALLOCATE(grid%wq_tur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6637,&
=======
 CALL wrf_error_fatal3("<stdin>",6686,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wq_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6644,&
=======
 CALL wrf_error_fatal3("<stdin>",6693,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%htop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6651,&
=======
 CALL wrf_error_fatal3("<stdin>",6700,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6658,&
=======
 CALL wrf_error_fatal3("<stdin>",6707,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%htopr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6665,&
=======
 CALL wrf_error_fatal3("<stdin>",6714,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hbotr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cutop ) ) THEN 
  DEALLOCATE(grid%cutop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6672,&
=======
 CALL wrf_error_fatal3("<stdin>",6721,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cutop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cubot ) ) THEN 
  DEALLOCATE(grid%cubot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6679,&
=======
 CALL wrf_error_fatal3("<stdin>",6728,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cubot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6686,&
=======
 CALL wrf_error_fatal3("<stdin>",6735,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cuppt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6693,&
=======
 CALL wrf_error_fatal3("<stdin>",6742,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rswtoa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6700,&
=======
 CALL wrf_error_fatal3("<stdin>",6749,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rlwtoa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6707,&
=======
 CALL wrf_error_fatal3("<stdin>",6756,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%czmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6714,&
=======
 CALL wrf_error_fatal3("<stdin>",6763,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfracl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6721,&
=======
 CALL wrf_error_fatal3("<stdin>",6770,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfracm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6728,&
=======
 CALL wrf_error_fatal3("<stdin>",6777,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfrach. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6735,&
=======
 CALL wrf_error_fatal3("<stdin>",6784,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acfrst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6742,&
=======
 CALL wrf_error_fatal3("<stdin>",6791,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ncfrst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6749,&
=======
 CALL wrf_error_fatal3("<stdin>",6798,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acfrcv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6756,&
=======
 CALL wrf_error_fatal3("<stdin>",6805,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ncfrcv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%o3rad ) ) THEN 
  DEALLOCATE(grid%o3rad,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6763,&
=======
 CALL wrf_error_fatal3("<stdin>",6812,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%o3rad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerodm ) ) THEN 
  DEALLOCATE(grid%aerodm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6770,&
=======
 CALL wrf_error_fatal3("<stdin>",6819,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerodm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pina ) ) THEN 
  DEALLOCATE(grid%pina,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6777,&
=======
 CALL wrf_error_fatal3("<stdin>",6826,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pina. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerod ) ) THEN 
  DEALLOCATE(grid%aerod,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6784,&
=======
 CALL wrf_error_fatal3("<stdin>",6833,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerod. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aodtot ) ) THEN 
  DEALLOCATE(grid%aodtot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6791,&
=======
 CALL wrf_error_fatal3("<stdin>",6840,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aodtot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ozmixm ) ) THEN 
  DEALLOCATE(grid%ozmixm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6798,&
=======
 CALL wrf_error_fatal3("<stdin>",6847,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ozmixm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pin ) ) THEN 
  DEALLOCATE(grid%pin,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6805,&
=======
 CALL wrf_error_fatal3("<stdin>",6854,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_ps_1 ) ) THEN 
  DEALLOCATE(grid%m_ps_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6812,&
=======
 CALL wrf_error_fatal3("<stdin>",6861,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%m_ps_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_ps_2 ) ) THEN 
  DEALLOCATE(grid%m_ps_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6819,&
=======
 CALL wrf_error_fatal3("<stdin>",6868,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%m_ps_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerosolc_1 ) ) THEN 
  DEALLOCATE(grid%aerosolc_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6826,&
=======
 CALL wrf_error_fatal3("<stdin>",6875,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerosolc_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerosolc_2 ) ) THEN 
  DEALLOCATE(grid%aerosolc_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6833,&
=======
 CALL wrf_error_fatal3("<stdin>",6882,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerosolc_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_hybi ) ) THEN 
  DEALLOCATE(grid%m_hybi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6840,&
=======
 CALL wrf_error_fatal3("<stdin>",6889,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%m_hybi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6847,&
=======
 CALL wrf_error_fatal3("<stdin>",6896,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%f_ice_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6854,&
=======
 CALL wrf_error_fatal3("<stdin>",6903,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%f_rain_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6861,&
=======
 CALL wrf_error_fatal3("<stdin>",6910,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%f_rimef_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qndropsource ) ) THEN 
  DEALLOCATE(grid%qndropsource,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6868,&
=======
 CALL wrf_error_fatal3("<stdin>",6917,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qndropsource. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_tmp ) ) THEN 
  DEALLOCATE(grid%om_tmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6875,&
=======
 CALL wrf_error_fatal3("<stdin>",6924,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_tmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_s ) ) THEN 
  DEALLOCATE(grid%om_s,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6882,&
=======
 CALL wrf_error_fatal3("<stdin>",6931,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_s. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_depth ) ) THEN 
  DEALLOCATE(grid%om_depth,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6889,&
=======
 CALL wrf_error_fatal3("<stdin>",6938,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_depth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_u ) ) THEN 
  DEALLOCATE(grid%om_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6896,&
=======
 CALL wrf_error_fatal3("<stdin>",6945,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_v ) ) THEN 
  DEALLOCATE(grid%om_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6903,&
=======
 CALL wrf_error_fatal3("<stdin>",6952,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_lat ) ) THEN 
  DEALLOCATE(grid%om_lat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6910,&
=======
 CALL wrf_error_fatal3("<stdin>",6959,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_lat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_lon ) ) THEN 
  DEALLOCATE(grid%om_lon,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6917,&
=======
 CALL wrf_error_fatal3("<stdin>",6966,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_lon. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_ml ) ) THEN 
  DEALLOCATE(grid%om_ml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6924,&
=======
 CALL wrf_error_fatal3("<stdin>",6973,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_tini ) ) THEN 
  DEALLOCATE(grid%om_tini,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6931,&
=======
 CALL wrf_error_fatal3("<stdin>",6980,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_tini. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_sini ) ) THEN 
  DEALLOCATE(grid%om_sini,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6938,&
=======
 CALL wrf_error_fatal3("<stdin>",6987,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%om_sini. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cupflag ) ) THEN 
  DEALLOCATE(grid%cupflag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6945,&
=======
 CALL wrf_error_fatal3("<stdin>",6994,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cupflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slopesfc ) ) THEN 
  DEALLOCATE(grid%slopesfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6952,&
=======
 CALL wrf_error_fatal3("<stdin>",7001,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slopesfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slopeez ) ) THEN 
  DEALLOCATE(grid%slopeez,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6959,&
=======
 CALL wrf_error_fatal3("<stdin>",7008,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slopeez. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sigmasfc ) ) THEN 
  DEALLOCATE(grid%sigmasfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6966,&
=======
 CALL wrf_error_fatal3("<stdin>",7015,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sigmasfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sigmaez ) ) THEN 
  DEALLOCATE(grid%sigmaez,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6973,&
=======
 CALL wrf_error_fatal3("<stdin>",7022,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sigmaez. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shall ) ) THEN 
  DEALLOCATE(grid%shall,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6980,&
=======
 CALL wrf_error_fatal3("<stdin>",7029,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shall. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucloud ) ) THEN 
  DEALLOCATE(grid%taucloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6987,&
=======
 CALL wrf_error_fatal3("<stdin>",7036,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taucloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tactive ) ) THEN 
  DEALLOCATE(grid%tactive,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",6994,&
=======
 CALL wrf_error_fatal3("<stdin>",7043,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tactive. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tcloud_cup ) ) THEN 
  DEALLOCATE(grid%tcloud_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7001,&
=======
 CALL wrf_error_fatal3("<stdin>",7050,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tcloud_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wcloudbase ) ) THEN 
  DEALLOCATE(grid%wcloudbase,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7008,&
=======
 CALL wrf_error_fatal3("<stdin>",7057,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wcloudbase. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%activefrac ) ) THEN 
  DEALLOCATE(grid%activefrac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7015,&
=======
 CALL wrf_error_fatal3("<stdin>",7064,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%activefrac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfratend_cup ) ) THEN 
  DEALLOCATE(grid%cldfratend_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7022,&
=======
 CALL wrf_error_fatal3("<stdin>",7071,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfratend_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_cup ) ) THEN 
  DEALLOCATE(grid%cldfra_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7029,&
=======
 CALL wrf_error_fatal3("<stdin>",7078,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%updfra_cup ) ) THEN 
  DEALLOCATE(grid%updfra_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7036,&
=======
 CALL wrf_error_fatal3("<stdin>",7085,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%updfra_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_iu_cup ) ) THEN 
  DEALLOCATE(grid%qc_iu_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7043,&
=======
 CALL wrf_error_fatal3("<stdin>",7092,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_iu_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_ic_cup ) ) THEN 
  DEALLOCATE(grid%qc_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7050,&
=======
 CALL wrf_error_fatal3("<stdin>",7099,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_ic_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qndrop_ic_cup ) ) THEN 
  DEALLOCATE(grid%qndrop_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7057,&
=======
 CALL wrf_error_fatal3("<stdin>",7106,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qndrop_ic_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wup_cup ) ) THEN 
  DEALLOCATE(grid%wup_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7064,&
=======
 CALL wrf_error_fatal3("<stdin>",7113,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wup_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wact_cup ) ) THEN 
  DEALLOCATE(grid%wact_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7071,&
=======
 CALL wrf_error_fatal3("<stdin>",7120,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wact_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wulcl_cup ) ) THEN 
  DEALLOCATE(grid%wulcl_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7078,&
=======
 CALL wrf_error_fatal3("<stdin>",7127,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wulcl_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfup_cup ) ) THEN 
  DEALLOCATE(grid%mfup_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7085,&
=======
 CALL wrf_error_fatal3("<stdin>",7134,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mfup_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfup_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfup_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7092,&
=======
 CALL wrf_error_fatal3("<stdin>",7141,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mfup_ent_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfdn_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7099,&
=======
 CALL wrf_error_fatal3("<stdin>",7148,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mfdn_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfdn_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7106,&
=======
 CALL wrf_error_fatal3("<stdin>",7155,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mfdn_ent_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7113,&
=======
 CALL wrf_error_fatal3("<stdin>",7162,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_pr_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_qi_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_qi_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7120,&
=======
 CALL wrf_error_fatal3("<stdin>",7169,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_qi_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qi_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qi_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7127,&
=======
 CALL wrf_error_fatal3("<stdin>",7176,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcvt_qi_to_pr_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tstar ) ) THEN 
  DEALLOCATE(grid%tstar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7134,&
=======
 CALL wrf_error_fatal3("<stdin>",7183,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tstar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lnterms ) ) THEN 
  DEALLOCATE(grid%lnterms,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7141,&
=======
 CALL wrf_error_fatal3("<stdin>",7190,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lnterms. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lnint ) ) THEN 
  DEALLOCATE(grid%lnint,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7148,&
=======
 CALL wrf_error_fatal3("<stdin>",7197,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lnint. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h_diabatic ) ) THEN 
  DEALLOCATE(grid%h_diabatic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7155,&
=======
 CALL wrf_error_fatal3("<stdin>",7204,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_diabatic ) ) THEN 
  DEALLOCATE(grid%qv_diabatic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7162,&
=======
 CALL wrf_error_fatal3("<stdin>",7211,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_diabatic ) ) THEN 
  DEALLOCATE(grid%qc_diabatic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7169,&
=======
 CALL wrf_error_fatal3("<stdin>",7218,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msft ) ) THEN 
  DEALLOCATE(grid%msft,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7176,&
=======
 CALL wrf_error_fatal3("<stdin>",7225,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msft. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfu ) ) THEN 
  DEALLOCATE(grid%msfu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7183,&
=======
 CALL wrf_error_fatal3("<stdin>",7232,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfv ) ) THEN 
  DEALLOCATE(grid%msfv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7190,&
=======
 CALL wrf_error_fatal3("<stdin>",7239,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msftx ) ) THEN 
  DEALLOCATE(grid%msftx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7197,&
=======
 CALL wrf_error_fatal3("<stdin>",7246,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msftx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfty ) ) THEN 
  DEALLOCATE(grid%msfty,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7204,&
=======
 CALL wrf_error_fatal3("<stdin>",7253,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfty. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfux ) ) THEN 
  DEALLOCATE(grid%msfux,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7211,&
=======
 CALL wrf_error_fatal3("<stdin>",7260,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfux. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfuy ) ) THEN 
  DEALLOCATE(grid%msfuy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7218,&
=======
 CALL wrf_error_fatal3("<stdin>",7267,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfuy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvx ) ) THEN 
  DEALLOCATE(grid%msfvx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7225,&
=======
 CALL wrf_error_fatal3("<stdin>",7274,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfvx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvx_inv ) ) THEN 
  DEALLOCATE(grid%msfvx_inv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7232,&
=======
 CALL wrf_error_fatal3("<stdin>",7281,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfvx_inv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvy ) ) THEN 
  DEALLOCATE(grid%msfvy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7239,&
=======
 CALL wrf_error_fatal3("<stdin>",7288,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%msfvy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7246,&
=======
 CALL wrf_error_fatal3("<stdin>",7295,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%e ) ) THEN 
  DEALLOCATE(grid%e,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7253,&
=======
 CALL wrf_error_fatal3("<stdin>",7302,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%e. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sina ) ) THEN 
  DEALLOCATE(grid%sina,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7260,&
=======
 CALL wrf_error_fatal3("<stdin>",7309,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sina. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cosa ) ) THEN 
  DEALLOCATE(grid%cosa,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7267,&
=======
 CALL wrf_error_fatal3("<stdin>",7316,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cosa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht ) ) THEN 
  DEALLOCATE(grid%ht,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7274,&
=======
 CALL wrf_error_fatal3("<stdin>",7323,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_fine ) ) THEN 
  DEALLOCATE(grid%ht_fine,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7281,&
=======
 CALL wrf_error_fatal3("<stdin>",7330,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_int ) ) THEN 
  DEALLOCATE(grid%ht_int,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7288,&
=======
 CALL wrf_error_fatal3("<stdin>",7337,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_int. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_input ) ) THEN 
  DEALLOCATE(grid%ht_input,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7295,&
=======
 CALL wrf_error_fatal3("<stdin>",7344,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_input. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_smooth ) ) THEN 
  DEALLOCATE(grid%ht_smooth,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7302,&
=======
 CALL wrf_error_fatal3("<stdin>",7351,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_smooth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_shad ) ) THEN 
  DEALLOCATE(grid%ht_shad,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7309,&
=======
 CALL wrf_error_fatal3("<stdin>",7358,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7316,&
=======
 CALL wrf_error_fatal3("<stdin>",7365,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxs. ')
 endif
  NULLIFY(grid%ht_shad_bxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7324,&
=======
 CALL wrf_error_fatal3("<stdin>",7373,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxe. ')
 endif
  NULLIFY(grid%ht_shad_bxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bys ) ) THEN 
  DEALLOCATE(grid%ht_shad_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7332,&
=======
 CALL wrf_error_fatal3("<stdin>",7381,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bys. ')
 endif
  NULLIFY(grid%ht_shad_bys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bye ) ) THEN 
  DEALLOCATE(grid%ht_shad_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7340,&
=======
 CALL wrf_error_fatal3("<stdin>",7389,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bye. ')
 endif
  NULLIFY(grid%ht_shad_bye)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7348,&
=======
 CALL wrf_error_fatal3("<stdin>",7397,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxs. ')
 endif
  NULLIFY(grid%ht_shad_btxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7356,&
=======
 CALL wrf_error_fatal3("<stdin>",7405,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxe. ')
 endif
  NULLIFY(grid%ht_shad_btxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btys ) ) THEN 
  DEALLOCATE(grid%ht_shad_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7364,&
=======
 CALL wrf_error_fatal3("<stdin>",7413,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btys. ')
 endif
  NULLIFY(grid%ht_shad_btys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btye ) ) THEN 
  DEALLOCATE(grid%ht_shad_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7372,&
=======
 CALL wrf_error_fatal3("<stdin>",7421,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btye. ')
 endif
  NULLIFY(grid%ht_shad_btye)
ENDIF
IF ( ASSOCIATED( grid%shadowmask ) ) THEN 
  DEALLOCATE(grid%shadowmask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7380,&
=======
 CALL wrf_error_fatal3("<stdin>",7429,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shadowmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk ) ) THEN 
  DEALLOCATE(grid%tsk,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7387,&
=======
 CALL wrf_error_fatal3("<stdin>",7436,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tsk ) ) THEN 
  DEALLOCATE(grid%dfi_tsk,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7394,&
=======
 CALL wrf_error_fatal3("<stdin>",7443,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_save ) ) THEN 
  DEALLOCATE(grid%tsk_save,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7401,&
=======
 CALL wrf_error_fatal3("<stdin>",7450,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_base ) ) THEN 
  DEALLOCATE(grid%u_base,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7408,&
=======
 CALL wrf_error_fatal3("<stdin>",7457,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_base ) ) THEN 
  DEALLOCATE(grid%v_base,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7415,&
=======
 CALL wrf_error_fatal3("<stdin>",7464,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_base ) ) THEN 
  DEALLOCATE(grid%qv_base,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7422,&
=======
 CALL wrf_error_fatal3("<stdin>",7471,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_base ) ) THEN 
  DEALLOCATE(grid%z_base,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7429,&
=======
 CALL wrf_error_fatal3("<stdin>",7478,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlwdn ) ) THEN 
  DEALLOCATE(grid%tlwdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7436,&
=======
 CALL wrf_error_fatal3("<stdin>",7485,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tlwdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlwup ) ) THEN 
  DEALLOCATE(grid%tlwup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7443,&
=======
 CALL wrf_error_fatal3("<stdin>",7492,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tlwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slwdn ) ) THEN 
  DEALLOCATE(grid%slwdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7450,&
=======
 CALL wrf_error_fatal3("<stdin>",7499,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slwdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slwup ) ) THEN 
  DEALLOCATE(grid%slwup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7457,&
=======
 CALL wrf_error_fatal3("<stdin>",7506,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tswdn ) ) THEN 
  DEALLOCATE(grid%tswdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7464,&
=======
 CALL wrf_error_fatal3("<stdin>",7513,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tswdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tswup ) ) THEN 
  DEALLOCATE(grid%tswup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7471,&
=======
 CALL wrf_error_fatal3("<stdin>",7520,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tswup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sswdn ) ) THEN 
  DEALLOCATE(grid%sswdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7478,&
=======
 CALL wrf_error_fatal3("<stdin>",7527,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sswdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sswup ) ) THEN 
  DEALLOCATE(grid%sswup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7485,&
=======
 CALL wrf_error_fatal3("<stdin>",7534,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sswup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rushten ) ) THEN 
  DEALLOCATE(grid%rushten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7492,&
=======
 CALL wrf_error_fatal3("<stdin>",7541,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rushten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvshten ) ) THEN 
  DEALLOCATE(grid%rvshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7499,&
=======
 CALL wrf_error_fatal3("<stdin>",7548,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rvshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthshten ) ) THEN 
  DEALLOCATE(grid%rthshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7506,&
=======
 CALL wrf_error_fatal3("<stdin>",7555,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvshten ) ) THEN 
  DEALLOCATE(grid%rqvshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7513,&
=======
 CALL wrf_error_fatal3("<stdin>",7562,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqrshten ) ) THEN 
  DEALLOCATE(grid%rqrshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7520,&
=======
 CALL wrf_error_fatal3("<stdin>",7569,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqrshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcshten ) ) THEN 
  DEALLOCATE(grid%rqcshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7527,&
=======
 CALL wrf_error_fatal3("<stdin>",7576,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqcshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqsshten ) ) THEN 
  DEALLOCATE(grid%rqsshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7534,&
=======
 CALL wrf_error_fatal3("<stdin>",7583,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqsshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqishten ) ) THEN 
  DEALLOCATE(grid%rqishten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7541,&
=======
 CALL wrf_error_fatal3("<stdin>",7590,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqishten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqgshten ) ) THEN 
  DEALLOCATE(grid%rqgshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7548,&
=======
 CALL wrf_error_fatal3("<stdin>",7597,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqgshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcnshten ) ) THEN 
  DEALLOCATE(grid%rqcnshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7555,&
=======
 CALL wrf_error_fatal3("<stdin>",7604,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqcnshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqinshten ) ) THEN 
  DEALLOCATE(grid%rqinshten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7562,&
=======
 CALL wrf_error_fatal3("<stdin>",7611,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqinshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rucuten ) ) THEN 
  DEALLOCATE(grid%rucuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7569,&
=======
 CALL wrf_error_fatal3("<stdin>",7618,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rucuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvcuten ) ) THEN 
  DEALLOCATE(grid%rvcuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7576,&
=======
 CALL wrf_error_fatal3("<stdin>",7625,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rvcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthcuten ) ) THEN 
  DEALLOCATE(grid%rthcuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7583,&
=======
 CALL wrf_error_fatal3("<stdin>",7632,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvcuten ) ) THEN 
  DEALLOCATE(grid%rqvcuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7590,&
=======
 CALL wrf_error_fatal3("<stdin>",7639,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqrcuten ) ) THEN 
  DEALLOCATE(grid%rqrcuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7597,&
=======
 CALL wrf_error_fatal3("<stdin>",7646,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqrcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqccuten ) ) THEN 
  DEALLOCATE(grid%rqccuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7604,&
=======
 CALL wrf_error_fatal3("<stdin>",7653,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqccuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqscuten ) ) THEN 
  DEALLOCATE(grid%rqscuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7611,&
=======
 CALL wrf_error_fatal3("<stdin>",7660,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqscuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqicuten ) ) THEN 
  DEALLOCATE(grid%rqicuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7618,&
=======
 CALL wrf_error_fatal3("<stdin>",7667,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqicuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcncuten ) ) THEN 
  DEALLOCATE(grid%rqcncuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7625,&
=======
 CALL wrf_error_fatal3("<stdin>",7674,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqcncuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqincuten ) ) THEN 
  DEALLOCATE(grid%rqincuten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7632,&
=======
 CALL wrf_error_fatal3("<stdin>",7681,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqincuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7639,&
=======
 CALL wrf_error_fatal3("<stdin>",7688,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w0avg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainc ) ) THEN 
  DEALLOCATE(grid%rainc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7646,&
=======
 CALL wrf_error_fatal3("<stdin>",7695,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainsh ) ) THEN 
  DEALLOCATE(grid%rainsh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7653,&
=======
 CALL wrf_error_fatal3("<stdin>",7702,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainnc ) ) THEN 
  DEALLOCATE(grid%rainnc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7660,&
=======
 CALL wrf_error_fatal3("<stdin>",7709,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_rainc ) ) THEN 
  DEALLOCATE(grid%i_rainc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7667,&
=======
 CALL wrf_error_fatal3("<stdin>",7716,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_rainnc ) ) THEN 
  DEALLOCATE(grid%i_rainnc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7674,&
=======
 CALL wrf_error_fatal3("<stdin>",7723,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pratec ) ) THEN 
  DEALLOCATE(grid%pratec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7681,&
=======
 CALL wrf_error_fatal3("<stdin>",7730,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pratec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pratesh ) ) THEN 
  DEALLOCATE(grid%pratesh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7688,&
=======
 CALL wrf_error_fatal3("<stdin>",7737,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pratesh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7695,&
=======
 CALL wrf_error_fatal3("<stdin>",7744,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainshv ) ) THEN 
  DEALLOCATE(grid%rainshv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7702,&
=======
 CALL wrf_error_fatal3("<stdin>",7751,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainshv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncv ) ) THEN 
  DEALLOCATE(grid%rainncv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7709,&
=======
 CALL wrf_error_fatal3("<stdin>",7758,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainbl ) ) THEN 
  DEALLOCATE(grid%rainbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7716,&
=======
 CALL wrf_error_fatal3("<stdin>",7765,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snownc ) ) THEN 
  DEALLOCATE(grid%snownc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7723,&
=======
 CALL wrf_error_fatal3("<stdin>",7772,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snownc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%graupelnc ) ) THEN 
  DEALLOCATE(grid%graupelnc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7730,&
=======
 CALL wrf_error_fatal3("<stdin>",7779,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%graupelnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailnc ) ) THEN 
  DEALLOCATE(grid%hailnc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7737,&
=======
 CALL wrf_error_fatal3("<stdin>",7786,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowncv ) ) THEN 
  DEALLOCATE(grid%snowncv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7744,&
=======
 CALL wrf_error_fatal3("<stdin>",7793,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%graupelncv ) ) THEN 
  DEALLOCATE(grid%graupelncv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7751,&
=======
 CALL wrf_error_fatal3("<stdin>",7800,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%graupelncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailncv ) ) THEN 
  DEALLOCATE(grid%hailncv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7758,&
=======
 CALL wrf_error_fatal3("<stdin>",7807,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refl_10cm ) ) THEN 
  DEALLOCATE(grid%refl_10cm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7765,&
=======
 CALL wrf_error_fatal3("<stdin>",7814,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%refl_10cm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_old ) ) THEN 
  DEALLOCATE(grid%th_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7772,&
=======
 CALL wrf_error_fatal3("<stdin>",7821,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_old ) ) THEN 
  DEALLOCATE(grid%qv_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7779,&
=======
 CALL wrf_error_fatal3("<stdin>",7828,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qv_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmi3d ) ) THEN 
  DEALLOCATE(grid%vmi3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7786,&
=======
 CALL wrf_error_fatal3("<stdin>",7835,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vmi3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%di3d ) ) THEN 
  DEALLOCATE(grid%di3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7793,&
=======
 CALL wrf_error_fatal3("<stdin>",7842,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%di3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rhopo3d ) ) THEN 
  DEALLOCATE(grid%rhopo3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7800,&
=======
 CALL wrf_error_fatal3("<stdin>",7849,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rhopo3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nca ) ) THEN 
  DEALLOCATE(grid%nca,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7807,&
=======
 CALL wrf_error_fatal3("<stdin>",7856,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nca. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lowlyr ) ) THEN 
  DEALLOCATE(grid%lowlyr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7814,&
=======
 CALL wrf_error_fatal3("<stdin>",7863,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lowlyr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7821,&
=======
 CALL wrf_error_fatal3("<stdin>",7870,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mass_flux. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_dp ) ) THEN 
  DEALLOCATE(grid%cldfra_dp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7828,&
=======
 CALL wrf_error_fatal3("<stdin>",7877,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_dp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_sh ) ) THEN 
  DEALLOCATE(grid%cldfra_sh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7835,&
=======
 CALL wrf_error_fatal3("<stdin>",7884,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_sh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_up ) ) THEN 
  DEALLOCATE(grid%w_up,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7842,&
=======
 CALL wrf_error_fatal3("<stdin>",7891,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%udr_kf ) ) THEN 
  DEALLOCATE(grid%udr_kf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7849,&
=======
 CALL wrf_error_fatal3("<stdin>",7898,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%udr_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ddr_kf ) ) THEN 
  DEALLOCATE(grid%ddr_kf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7856,&
=======
 CALL wrf_error_fatal3("<stdin>",7905,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ddr_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uer_kf ) ) THEN 
  DEALLOCATE(grid%uer_kf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7863,&
=======
 CALL wrf_error_fatal3("<stdin>",7912,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uer_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%der_kf ) ) THEN 
  DEALLOCATE(grid%der_kf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7870,&
=======
 CALL wrf_error_fatal3("<stdin>",7919,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%der_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%timec_kf ) ) THEN 
  DEALLOCATE(grid%timec_kf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7877,&
=======
 CALL wrf_error_fatal3("<stdin>",7926,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%timec_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7884,&
=======
 CALL wrf_error_fatal3("<stdin>",7933,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_gr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7891,&
=======
 CALL wrf_error_fatal3("<stdin>",7940,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7898,&
=======
 CALL wrf_error_fatal3("<stdin>",7947,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_mc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7905,&
=======
 CALL wrf_error_fatal3("<stdin>",7954,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_st. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7912,&
=======
 CALL wrf_error_fatal3("<stdin>",7961,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_as. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7919,&
=======
 CALL wrf_error_fatal3("<stdin>",7968,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_capma. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7926,&
=======
 CALL wrf_error_fatal3("<stdin>",7975,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_capme. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7933,&
=======
 CALL wrf_error_fatal3("<stdin>",7982,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%apr_capmi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edt_out ) ) THEN 
  DEALLOCATE(grid%edt_out,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7940,&
=======
 CALL wrf_error_fatal3("<stdin>",7989,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edt_out. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xmb_shallow ) ) THEN 
  DEALLOCATE(grid%xmb_shallow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7947,&
=======
 CALL wrf_error_fatal3("<stdin>",7996,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xmb_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%k22_shallow ) ) THEN 
  DEALLOCATE(grid%k22_shallow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7954,&
=======
 CALL wrf_error_fatal3("<stdin>",8003,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%k22_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kbcon_shallow ) ) THEN 
  DEALLOCATE(grid%kbcon_shallow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7961,&
=======
 CALL wrf_error_fatal3("<stdin>",8010,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kbcon_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ktop_shallow ) ) THEN 
  DEALLOCATE(grid%ktop_shallow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7968,&
=======
 CALL wrf_error_fatal3("<stdin>",8017,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ktop_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%k22_deep ) ) THEN 
  DEALLOCATE(grid%k22_deep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7975,&
=======
 CALL wrf_error_fatal3("<stdin>",8024,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%k22_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kbcon_deep ) ) THEN 
  DEALLOCATE(grid%kbcon_deep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7982,&
=======
 CALL wrf_error_fatal3("<stdin>",8031,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kbcon_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ktop_deep ) ) THEN 
  DEALLOCATE(grid%ktop_deep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7989,&
=======
 CALL wrf_error_fatal3("<stdin>",8038,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ktop_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",7996,&
=======
 CALL wrf_error_fatal3("<stdin>",8045,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xf_ens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8003,&
=======
 CALL wrf_error_fatal3("<stdin>",8052,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pr_ens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_tten ) ) THEN 
  DEALLOCATE(grid%cugd_tten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8010,&
=======
 CALL wrf_error_fatal3("<stdin>",8059,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cugd_tten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qvten ) ) THEN 
  DEALLOCATE(grid%cugd_qvten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8017,&
=======
 CALL wrf_error_fatal3("<stdin>",8066,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cugd_qvten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_ttens ) ) THEN 
  DEALLOCATE(grid%cugd_ttens,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8024,&
=======
 CALL wrf_error_fatal3("<stdin>",8073,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cugd_ttens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qvtens ) ) THEN 
  DEALLOCATE(grid%cugd_qvtens,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8031,&
=======
 CALL wrf_error_fatal3("<stdin>",8080,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cugd_qvtens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qcten ) ) THEN 
  DEALLOCATE(grid%cugd_qcten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8038,&
=======
 CALL wrf_error_fatal3("<stdin>",8087,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cugd_qcten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud ) ) THEN 
  DEALLOCATE(grid%gd_cloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8045,&
=======
 CALL wrf_error_fatal3("<stdin>",8094,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gd_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2 ) ) THEN 
  DEALLOCATE(grid%gd_cloud2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8052,&
=======
 CALL wrf_error_fatal3("<stdin>",8101,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cldfr ) ) THEN 
  DEALLOCATE(grid%gd_cldfr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8059,&
=======
 CALL wrf_error_fatal3("<stdin>",8108,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gd_cldfr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv_a ) ) THEN 
  DEALLOCATE(grid%raincv_a,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8066,&
=======
 CALL wrf_error_fatal3("<stdin>",8115,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincv_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv_b ) ) THEN 
  DEALLOCATE(grid%raincv_b,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8073,&
=======
 CALL wrf_error_fatal3("<stdin>",8122,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincv_b. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud_a,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8080,&
=======
 CALL wrf_error_fatal3("<stdin>",8129,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gd_cloud_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud2_a,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8087,&
=======
 CALL wrf_error_fatal3("<stdin>",8136,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_cu ) ) THEN 
  DEALLOCATE(grid%qc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8094,&
=======
 CALL wrf_error_fatal3("<stdin>",8143,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qi_cu ) ) THEN 
  DEALLOCATE(grid%qi_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8101,&
=======
 CALL wrf_error_fatal3("<stdin>",8150,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qi_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_bl ) ) THEN 
  DEALLOCATE(grid%qc_bl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8108,&
=======
 CALL wrf_error_fatal3("<stdin>",8157,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_bl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8115,&
=======
 CALL wrf_error_fatal3("<stdin>",8164,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthften. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8122,&
=======
 CALL wrf_error_fatal3("<stdin>",8171,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvften. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthraten ) ) THEN 
  DEALLOCATE(grid%rthraten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8129,&
=======
 CALL wrf_error_fatal3("<stdin>",8178,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthraten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthratenlw ) ) THEN 
  DEALLOCATE(grid%rthratenlw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8136,&
=======
 CALL wrf_error_fatal3("<stdin>",8185,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthratenlw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthratensw ) ) THEN 
  DEALLOCATE(grid%rthratensw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8143,&
=======
 CALL wrf_error_fatal3("<stdin>",8192,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthratensw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8150,&
=======
 CALL wrf_error_fatal3("<stdin>",8199,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_old ) ) THEN 
  DEALLOCATE(grid%cldfra_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8157,&
=======
 CALL wrf_error_fatal3("<stdin>",8206,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_bl ) ) THEN 
  DEALLOCATE(grid%cldfra_bl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8164,&
=======
 CALL wrf_error_fatal3("<stdin>",8213,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_bl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldt ) ) THEN 
  DEALLOCATE(grid%cldt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8171,&
=======
 CALL wrf_error_fatal3("<stdin>",8220,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdown ) ) THEN 
  DEALLOCATE(grid%swdown,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8178,&
=======
 CALL wrf_error_fatal3("<stdin>",8227,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdown. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdownc ) ) THEN 
  DEALLOCATE(grid%swdownc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8185,&
=======
 CALL wrf_error_fatal3("<stdin>",8234,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdownc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8192,&
=======
 CALL wrf_error_fatal3("<stdin>",8241,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gsw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw ) ) THEN 
  DEALLOCATE(grid%glw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8199,&
=======
 CALL wrf_error_fatal3("<stdin>",8248,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%glw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnorm ) ) THEN 
  DEALLOCATE(grid%swnorm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8206,&
=======
 CALL wrf_error_fatal3("<stdin>",8255,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swnorm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%diffuse_frac ) ) THEN 
  DEALLOCATE(grid%diffuse_frac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8213,&
=======
 CALL wrf_error_fatal3("<stdin>",8262,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%diffuse_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddir ) ) THEN 
  DEALLOCATE(grid%swddir,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8220,&
=======
 CALL wrf_error_fatal3("<stdin>",8269,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swddir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddni ) ) THEN 
  DEALLOCATE(grid%swddni,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8227,&
=======
 CALL wrf_error_fatal3("<stdin>",8276,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swddni. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddif ) ) THEN 
  DEALLOCATE(grid%swddif,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8234,&
=======
 CALL wrf_error_fatal3("<stdin>",8283,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swddif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gx ) ) THEN 
  DEALLOCATE(grid%gx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8241,&
=======
 CALL wrf_error_fatal3("<stdin>",8290,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bx ) ) THEN 
  DEALLOCATE(grid%bx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8248,&
=======
 CALL wrf_error_fatal3("<stdin>",8297,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gg ) ) THEN 
  DEALLOCATE(grid%gg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8255,&
=======
 CALL wrf_error_fatal3("<stdin>",8304,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bb ) ) THEN 
  DEALLOCATE(grid%bb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8262,&
=======
 CALL wrf_error_fatal3("<stdin>",8311,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%coszen_ref ) ) THEN 
  DEALLOCATE(grid%coszen_ref,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8269,&
=======
 CALL wrf_error_fatal3("<stdin>",8318,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%coszen_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdown_ref ) ) THEN 
  DEALLOCATE(grid%swdown_ref,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8276,&
=======
 CALL wrf_error_fatal3("<stdin>",8325,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdown_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddir_ref ) ) THEN 
  DEALLOCATE(grid%swddir_ref,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8283,&
=======
 CALL wrf_error_fatal3("<stdin>",8332,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swddir_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aod5502d ) ) THEN 
  DEALLOCATE(grid%aod5502d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8290,&
=======
 CALL wrf_error_fatal3("<stdin>",8339,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aod5502d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%angexp2d ) ) THEN 
  DEALLOCATE(grid%angexp2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8297,&
=======
 CALL wrf_error_fatal3("<stdin>",8346,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%angexp2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerssa2d ) ) THEN 
  DEALLOCATE(grid%aerssa2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8304,&
=======
 CALL wrf_error_fatal3("<stdin>",8353,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerssa2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerasy2d ) ) THEN 
  DEALLOCATE(grid%aerasy2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8311,&
=======
 CALL wrf_error_fatal3("<stdin>",8360,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aerasy2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aod5503d ) ) THEN 
  DEALLOCATE(grid%aod5503d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8318,&
=======
 CALL wrf_error_fatal3("<stdin>",8367,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aod5503d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taod5503d ) ) THEN 
  DEALLOCATE(grid%taod5503d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8325,&
=======
 CALL wrf_error_fatal3("<stdin>",8374,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taod5503d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taod5502d ) ) THEN 
  DEALLOCATE(grid%taod5502d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8332,&
=======
 CALL wrf_error_fatal3("<stdin>",8381,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taod5502d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2min ) ) THEN 
  DEALLOCATE(grid%t2min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8339,&
=======
 CALL wrf_error_fatal3("<stdin>",8388,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2max ) ) THEN 
  DEALLOCATE(grid%t2max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8346,&
=======
 CALL wrf_error_fatal3("<stdin>",8395,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tt2min ) ) THEN 
  DEALLOCATE(grid%tt2min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8353,&
=======
 CALL wrf_error_fatal3("<stdin>",8402,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tt2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tt2max ) ) THEN 
  DEALLOCATE(grid%tt2max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8360,&
=======
 CALL wrf_error_fatal3("<stdin>",8409,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tt2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mean ) ) THEN 
  DEALLOCATE(grid%t2mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8367,&
=======
 CALL wrf_error_fatal3("<stdin>",8416,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2std ) ) THEN 
  DEALLOCATE(grid%t2std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8374,&
=======
 CALL wrf_error_fatal3("<stdin>",8423,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2min ) ) THEN 
  DEALLOCATE(grid%q2min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8381,&
=======
 CALL wrf_error_fatal3("<stdin>",8430,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2max ) ) THEN 
  DEALLOCATE(grid%q2max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8388,&
=======
 CALL wrf_error_fatal3("<stdin>",8437,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tq2min ) ) THEN 
  DEALLOCATE(grid%tq2min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8395,&
=======
 CALL wrf_error_fatal3("<stdin>",8444,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tq2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tq2max ) ) THEN 
  DEALLOCATE(grid%tq2max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8402,&
=======
 CALL wrf_error_fatal3("<stdin>",8451,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tq2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mean ) ) THEN 
  DEALLOCATE(grid%q2mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8409,&
=======
 CALL wrf_error_fatal3("<stdin>",8458,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2std ) ) THEN 
  DEALLOCATE(grid%q2std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8416,&
=======
 CALL wrf_error_fatal3("<stdin>",8465,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmin ) ) THEN 
  DEALLOCATE(grid%skintempmin,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8423,&
=======
 CALL wrf_error_fatal3("<stdin>",8472,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%skintempmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmax ) ) THEN 
  DEALLOCATE(grid%skintempmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8430,&
=======
 CALL wrf_error_fatal3("<stdin>",8479,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%skintempmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tskintempmin ) ) THEN 
  DEALLOCATE(grid%tskintempmin,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8437,&
=======
 CALL wrf_error_fatal3("<stdin>",8486,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tskintempmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tskintempmax ) ) THEN 
  DEALLOCATE(grid%tskintempmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8444,&
=======
 CALL wrf_error_fatal3("<stdin>",8493,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tskintempmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmean ) ) THEN 
  DEALLOCATE(grid%skintempmean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8451,&
=======
 CALL wrf_error_fatal3("<stdin>",8500,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%skintempmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempstd ) ) THEN 
  DEALLOCATE(grid%skintempstd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8458,&
=======
 CALL wrf_error_fatal3("<stdin>",8507,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%skintempstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10max ) ) THEN 
  DEALLOCATE(grid%u10max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8465,&
=======
 CALL wrf_error_fatal3("<stdin>",8514,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10max ) ) THEN 
  DEALLOCATE(grid%v10max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8472,&
=======
 CALL wrf_error_fatal3("<stdin>",8521,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10max ) ) THEN 
  DEALLOCATE(grid%spduv10max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8479,&
=======
 CALL wrf_error_fatal3("<stdin>",8528,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spduv10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tspduv10max ) ) THEN 
  DEALLOCATE(grid%tspduv10max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8486,&
=======
 CALL wrf_error_fatal3("<stdin>",8535,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tspduv10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10mean ) ) THEN 
  DEALLOCATE(grid%u10mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8493,&
=======
 CALL wrf_error_fatal3("<stdin>",8542,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10mean ) ) THEN 
  DEALLOCATE(grid%v10mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8500,&
=======
 CALL wrf_error_fatal3("<stdin>",8549,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10mean ) ) THEN 
  DEALLOCATE(grid%spduv10mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8507,&
=======
 CALL wrf_error_fatal3("<stdin>",8556,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spduv10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10std ) ) THEN 
  DEALLOCATE(grid%u10std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8514,&
=======
 CALL wrf_error_fatal3("<stdin>",8563,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10std ) ) THEN 
  DEALLOCATE(grid%v10std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8521,&
=======
 CALL wrf_error_fatal3("<stdin>",8570,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10std ) ) THEN 
  DEALLOCATE(grid%spduv10std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8528,&
=======
 CALL wrf_error_fatal3("<stdin>",8577,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spduv10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvmax ) ) THEN 
  DEALLOCATE(grid%raincvmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8535,&
=======
 CALL wrf_error_fatal3("<stdin>",8584,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvmax ) ) THEN 
  DEALLOCATE(grid%rainncvmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8542,&
=======
 CALL wrf_error_fatal3("<stdin>",8591,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainncvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traincvmax ) ) THEN 
  DEALLOCATE(grid%traincvmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8549,&
=======
 CALL wrf_error_fatal3("<stdin>",8598,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%traincvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trainncvmax ) ) THEN 
  DEALLOCATE(grid%trainncvmax,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8556,&
=======
 CALL wrf_error_fatal3("<stdin>",8605,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%trainncvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvmean ) ) THEN 
  DEALLOCATE(grid%raincvmean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8563,&
=======
 CALL wrf_error_fatal3("<stdin>",8612,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincvmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvmean ) ) THEN 
  DEALLOCATE(grid%rainncvmean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8570,&
=======
 CALL wrf_error_fatal3("<stdin>",8619,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainncvmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvstd ) ) THEN 
  DEALLOCATE(grid%raincvstd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8577,&
=======
 CALL wrf_error_fatal3("<stdin>",8626,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%raincvstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvstd ) ) THEN 
  DEALLOCATE(grid%rainncvstd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8584,&
=======
 CALL wrf_error_fatal3("<stdin>",8633,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rainncvstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupt ) ) THEN 
  DEALLOCATE(grid%acswupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8591,&
=======
 CALL wrf_error_fatal3("<stdin>",8640,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswuptc ) ) THEN 
  DEALLOCATE(grid%acswuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8598,&
=======
 CALL wrf_error_fatal3("<stdin>",8647,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnt ) ) THEN 
  DEALLOCATE(grid%acswdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8605,&
=======
 CALL wrf_error_fatal3("<stdin>",8654,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdntc ) ) THEN 
  DEALLOCATE(grid%acswdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8612,&
=======
 CALL wrf_error_fatal3("<stdin>",8661,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupb ) ) THEN 
  DEALLOCATE(grid%acswupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8619,&
=======
 CALL wrf_error_fatal3("<stdin>",8668,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupbc ) ) THEN 
  DEALLOCATE(grid%acswupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8626,&
=======
 CALL wrf_error_fatal3("<stdin>",8675,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnb ) ) THEN 
  DEALLOCATE(grid%acswdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8633,&
=======
 CALL wrf_error_fatal3("<stdin>",8682,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnbc ) ) THEN 
  DEALLOCATE(grid%acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8640,&
=======
 CALL wrf_error_fatal3("<stdin>",8689,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%acswdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupt ) ) THEN 
  DEALLOCATE(grid%aclwupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8647,&
=======
 CALL wrf_error_fatal3("<stdin>",8696,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwuptc ) ) THEN 
  DEALLOCATE(grid%aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8654,&
=======
 CALL wrf_error_fatal3("<stdin>",8703,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnt ) ) THEN 
  DEALLOCATE(grid%aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8661,&
=======
 CALL wrf_error_fatal3("<stdin>",8710,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdntc ) ) THEN 
  DEALLOCATE(grid%aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8668,&
=======
 CALL wrf_error_fatal3("<stdin>",8717,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupb ) ) THEN 
  DEALLOCATE(grid%aclwupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8675,&
=======
 CALL wrf_error_fatal3("<stdin>",8724,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupbc ) ) THEN 
  DEALLOCATE(grid%aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8682,&
=======
 CALL wrf_error_fatal3("<stdin>",8731,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnb ) ) THEN 
  DEALLOCATE(grid%aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8689,&
=======
 CALL wrf_error_fatal3("<stdin>",8738,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnbc ) ) THEN 
  DEALLOCATE(grid%aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8696,&
=======
 CALL wrf_error_fatal3("<stdin>",8745,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupt ) ) THEN 
  DEALLOCATE(grid%i_acswupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8703,&
=======
 CALL wrf_error_fatal3("<stdin>",8752,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswuptc ) ) THEN 
  DEALLOCATE(grid%i_acswuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8710,&
=======
 CALL wrf_error_fatal3("<stdin>",8759,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnt ) ) THEN 
  DEALLOCATE(grid%i_acswdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8717,&
=======
 CALL wrf_error_fatal3("<stdin>",8766,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdntc ) ) THEN 
  DEALLOCATE(grid%i_acswdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8724,&
=======
 CALL wrf_error_fatal3("<stdin>",8773,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupb ) ) THEN 
  DEALLOCATE(grid%i_acswupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8731,&
=======
 CALL wrf_error_fatal3("<stdin>",8780,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupbc ) ) THEN 
  DEALLOCATE(grid%i_acswupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8738,&
=======
 CALL wrf_error_fatal3("<stdin>",8787,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnb ) ) THEN 
  DEALLOCATE(grid%i_acswdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8745,&
=======
 CALL wrf_error_fatal3("<stdin>",8794,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnbc ) ) THEN 
  DEALLOCATE(grid%i_acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8752,&
=======
 CALL wrf_error_fatal3("<stdin>",8801,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_acswdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupt ) ) THEN 
  DEALLOCATE(grid%i_aclwupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8759,&
=======
 CALL wrf_error_fatal3("<stdin>",8808,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwuptc ) ) THEN 
  DEALLOCATE(grid%i_aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8766,&
=======
 CALL wrf_error_fatal3("<stdin>",8815,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnt ) ) THEN 
  DEALLOCATE(grid%i_aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8773,&
=======
 CALL wrf_error_fatal3("<stdin>",8822,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdntc ) ) THEN 
  DEALLOCATE(grid%i_aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8780,&
=======
 CALL wrf_error_fatal3("<stdin>",8829,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupb ) ) THEN 
  DEALLOCATE(grid%i_aclwupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8787,&
=======
 CALL wrf_error_fatal3("<stdin>",8836,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupbc ) ) THEN 
  DEALLOCATE(grid%i_aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8794,&
=======
 CALL wrf_error_fatal3("<stdin>",8843,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnb ) ) THEN 
  DEALLOCATE(grid%i_aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8801,&
=======
 CALL wrf_error_fatal3("<stdin>",8850,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnbc ) ) THEN 
  DEALLOCATE(grid%i_aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8808,&
=======
 CALL wrf_error_fatal3("<stdin>",8857,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt ) ) THEN 
  DEALLOCATE(grid%swupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8815,&
=======
 CALL wrf_error_fatal3("<stdin>",8864,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swuptc ) ) THEN 
  DEALLOCATE(grid%swuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8822,&
=======
 CALL wrf_error_fatal3("<stdin>",8871,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt ) ) THEN 
  DEALLOCATE(grid%swdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8829,&
=======
 CALL wrf_error_fatal3("<stdin>",8878,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdntc ) ) THEN 
  DEALLOCATE(grid%swdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8836,&
=======
 CALL wrf_error_fatal3("<stdin>",8885,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb ) ) THEN 
  DEALLOCATE(grid%swupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8843,&
=======
 CALL wrf_error_fatal3("<stdin>",8892,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupbc ) ) THEN 
  DEALLOCATE(grid%swupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8850,&
=======
 CALL wrf_error_fatal3("<stdin>",8899,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb ) ) THEN 
  DEALLOCATE(grid%swdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8857,&
=======
 CALL wrf_error_fatal3("<stdin>",8906,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnbc ) ) THEN 
  DEALLOCATE(grid%swdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8864,&
=======
 CALL wrf_error_fatal3("<stdin>",8913,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt ) ) THEN 
  DEALLOCATE(grid%lwupt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8871,&
=======
 CALL wrf_error_fatal3("<stdin>",8920,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwuptc ) ) THEN 
  DEALLOCATE(grid%lwuptc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8878,&
=======
 CALL wrf_error_fatal3("<stdin>",8927,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt ) ) THEN 
  DEALLOCATE(grid%lwdnt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8885,&
=======
 CALL wrf_error_fatal3("<stdin>",8934,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdntc ) ) THEN 
  DEALLOCATE(grid%lwdntc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8892,&
=======
 CALL wrf_error_fatal3("<stdin>",8941,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb ) ) THEN 
  DEALLOCATE(grid%lwupb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8899,&
=======
 CALL wrf_error_fatal3("<stdin>",8948,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupbc ) ) THEN 
  DEALLOCATE(grid%lwupbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8906,&
=======
 CALL wrf_error_fatal3("<stdin>",8955,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnb ) ) THEN 
  DEALLOCATE(grid%lwdnb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8913,&
=======
 CALL wrf_error_fatal3("<stdin>",8962,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnbc ) ) THEN 
  DEALLOCATE(grid%lwdnbc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8920,&
=======
 CALL wrf_error_fatal3("<stdin>",8969,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swcf ) ) THEN 
  DEALLOCATE(grid%swcf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8927,&
=======
 CALL wrf_error_fatal3("<stdin>",8976,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwcf ) ) THEN 
  DEALLOCATE(grid%lwcf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8934,&
=======
 CALL wrf_error_fatal3("<stdin>",8983,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%olr ) ) THEN 
  DEALLOCATE(grid%olr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8941,&
=======
 CALL wrf_error_fatal3("<stdin>",8990,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%olr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_u ) ) THEN 
  DEALLOCATE(grid%xlat_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8948,&
=======
 CALL wrf_error_fatal3("<stdin>",8997,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlat_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_u ) ) THEN 
  DEALLOCATE(grid%xlong_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8955,&
=======
 CALL wrf_error_fatal3("<stdin>",9004,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlong_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_v ) ) THEN 
  DEALLOCATE(grid%xlat_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8962,&
=======
 CALL wrf_error_fatal3("<stdin>",9011,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlat_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_v ) ) THEN 
  DEALLOCATE(grid%xlong_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8969,&
=======
 CALL wrf_error_fatal3("<stdin>",9018,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xlong_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8976,&
=======
 CALL wrf_error_fatal3("<stdin>",9025,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albedo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%clat ) ) THEN 
  DEALLOCATE(grid%clat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8983,&
=======
 CALL wrf_error_fatal3("<stdin>",9032,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%clat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8990,&
=======
 CALL wrf_error_fatal3("<stdin>",9039,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albbck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%embck ) ) THEN 
  DEALLOCATE(grid%embck,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",8997,&
=======
 CALL wrf_error_fatal3("<stdin>",9046,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%embck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emiss ) ) THEN 
  DEALLOCATE(grid%emiss,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9004,&
=======
 CALL wrf_error_fatal3("<stdin>",9053,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%emiss. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snotime ) ) THEN 
  DEALLOCATE(grid%snotime,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9011,&
=======
 CALL wrf_error_fatal3("<stdin>",9060,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snotime. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%noahres ) ) THEN 
  DEALLOCATE(grid%noahres,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9018,&
=======
 CALL wrf_error_fatal3("<stdin>",9067,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%noahres. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9025,&
=======
 CALL wrf_error_fatal3("<stdin>",9074,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldefi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rublten ) ) THEN 
  DEALLOCATE(grid%rublten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9032,&
=======
 CALL wrf_error_fatal3("<stdin>",9081,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rublten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvblten ) ) THEN 
  DEALLOCATE(grid%rvblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9039,&
=======
 CALL wrf_error_fatal3("<stdin>",9088,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rvblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthblten ) ) THEN 
  DEALLOCATE(grid%rthblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9046,&
=======
 CALL wrf_error_fatal3("<stdin>",9095,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvblten ) ) THEN 
  DEALLOCATE(grid%rqvblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9053,&
=======
 CALL wrf_error_fatal3("<stdin>",9102,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcblten ) ) THEN 
  DEALLOCATE(grid%rqcblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9060,&
=======
 CALL wrf_error_fatal3("<stdin>",9109,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqcblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqiblten ) ) THEN 
  DEALLOCATE(grid%rqiblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9067,&
=======
 CALL wrf_error_fatal3("<stdin>",9116,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqiblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqniblten ) ) THEN 
  DEALLOCATE(grid%rqniblten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9074,&
=======
 CALL wrf_error_fatal3("<stdin>",9123,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqniblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flx4 ) ) THEN 
  DEALLOCATE(grid%flx4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9081,&
=======
 CALL wrf_error_fatal3("<stdin>",9130,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flx4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fvb ) ) THEN 
  DEALLOCATE(grid%fvb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9088,&
=======
 CALL wrf_error_fatal3("<stdin>",9137,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fvb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fbur ) ) THEN 
  DEALLOCATE(grid%fbur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9095,&
=======
 CALL wrf_error_fatal3("<stdin>",9144,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fbur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgsn ) ) THEN 
  DEALLOCATE(grid%fgsn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9102,&
=======
 CALL wrf_error_fatal3("<stdin>",9151,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fgsn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isnowxy ) ) THEN 
  DEALLOCATE(grid%isnowxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9109,&
=======
 CALL wrf_error_fatal3("<stdin>",9158,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%isnowxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tvxy ) ) THEN 
  DEALLOCATE(grid%tvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9116,&
=======
 CALL wrf_error_fatal3("<stdin>",9165,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgxy ) ) THEN 
  DEALLOCATE(grid%tgxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9123,&
=======
 CALL wrf_error_fatal3("<stdin>",9172,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canicexy ) ) THEN 
  DEALLOCATE(grid%canicexy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9130,&
=======
 CALL wrf_error_fatal3("<stdin>",9179,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canicexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canliqxy ) ) THEN 
  DEALLOCATE(grid%canliqxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9137,&
=======
 CALL wrf_error_fatal3("<stdin>",9186,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canliqxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eahxy ) ) THEN 
  DEALLOCATE(grid%eahxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9144,&
=======
 CALL wrf_error_fatal3("<stdin>",9193,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%eahxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tahxy ) ) THEN 
  DEALLOCATE(grid%tahxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9151,&
=======
 CALL wrf_error_fatal3("<stdin>",9200,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tahxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmxy ) ) THEN 
  DEALLOCATE(grid%cmxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9158,&
=======
 CALL wrf_error_fatal3("<stdin>",9207,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chxy ) ) THEN 
  DEALLOCATE(grid%chxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9165,&
=======
 CALL wrf_error_fatal3("<stdin>",9214,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fwetxy ) ) THEN 
  DEALLOCATE(grid%fwetxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9172,&
=======
 CALL wrf_error_fatal3("<stdin>",9221,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fwetxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sneqvoxy ) ) THEN 
  DEALLOCATE(grid%sneqvoxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9179,&
=======
 CALL wrf_error_fatal3("<stdin>",9228,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sneqvoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alboldxy ) ) THEN 
  DEALLOCATE(grid%alboldxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9186,&
=======
 CALL wrf_error_fatal3("<stdin>",9235,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%alboldxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsnowxy ) ) THEN 
  DEALLOCATE(grid%qsnowxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9193,&
=======
 CALL wrf_error_fatal3("<stdin>",9242,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsnowxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wslakexy ) ) THEN 
  DEALLOCATE(grid%wslakexy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9200,&
=======
 CALL wrf_error_fatal3("<stdin>",9249,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wslakexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zwtxy ) ) THEN 
  DEALLOCATE(grid%zwtxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9207,&
=======
 CALL wrf_error_fatal3("<stdin>",9256,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zwtxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%waxy ) ) THEN 
  DEALLOCATE(grid%waxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9214,&
=======
 CALL wrf_error_fatal3("<stdin>",9263,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%waxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtxy ) ) THEN 
  DEALLOCATE(grid%wtxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9221,&
=======
 CALL wrf_error_fatal3("<stdin>",9270,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wtxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsnoxy ) ) THEN 
  DEALLOCATE(grid%tsnoxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9228,&
=======
 CALL wrf_error_fatal3("<stdin>",9277,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsnoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zsnsoxy ) ) THEN 
  DEALLOCATE(grid%zsnsoxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9235,&
=======
 CALL wrf_error_fatal3("<stdin>",9284,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zsnsoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snicexy ) ) THEN 
  DEALLOCATE(grid%snicexy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9242,&
=======
 CALL wrf_error_fatal3("<stdin>",9291,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snicexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snliqxy ) ) THEN 
  DEALLOCATE(grid%snliqxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9249,&
=======
 CALL wrf_error_fatal3("<stdin>",9298,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snliqxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfmassxy ) ) THEN 
  DEALLOCATE(grid%lfmassxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9256,&
=======
 CALL wrf_error_fatal3("<stdin>",9305,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lfmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rtmassxy ) ) THEN 
  DEALLOCATE(grid%rtmassxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9263,&
=======
 CALL wrf_error_fatal3("<stdin>",9312,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rtmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stmassxy ) ) THEN 
  DEALLOCATE(grid%stmassxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9270,&
=======
 CALL wrf_error_fatal3("<stdin>",9319,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%stmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%woodxy ) ) THEN 
  DEALLOCATE(grid%woodxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9277,&
=======
 CALL wrf_error_fatal3("<stdin>",9326,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%woodxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stblcpxy ) ) THEN 
  DEALLOCATE(grid%stblcpxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9284,&
=======
 CALL wrf_error_fatal3("<stdin>",9333,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%stblcpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fastcpxy ) ) THEN 
  DEALLOCATE(grid%fastcpxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9291,&
=======
 CALL wrf_error_fatal3("<stdin>",9340,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fastcpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xsaixy ) ) THEN 
  DEALLOCATE(grid%xsaixy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9298,&
=======
 CALL wrf_error_fatal3("<stdin>",9347,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xsaixy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taussxy ) ) THEN 
  DEALLOCATE(grid%taussxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9305,&
=======
 CALL wrf_error_fatal3("<stdin>",9354,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taussxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mvxy ) ) THEN 
  DEALLOCATE(grid%t2mvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9312,&
=======
 CALL wrf_error_fatal3("<stdin>",9361,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2mvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mbxy ) ) THEN 
  DEALLOCATE(grid%t2mbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9319,&
=======
 CALL wrf_error_fatal3("<stdin>",9368,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2mbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mvxy ) ) THEN 
  DEALLOCATE(grid%q2mvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9326,&
=======
 CALL wrf_error_fatal3("<stdin>",9375,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2mvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mbxy ) ) THEN 
  DEALLOCATE(grid%q2mbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9333,&
=======
 CALL wrf_error_fatal3("<stdin>",9382,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2mbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tradxy ) ) THEN 
  DEALLOCATE(grid%tradxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9340,&
=======
 CALL wrf_error_fatal3("<stdin>",9389,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tradxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%neexy ) ) THEN 
  DEALLOCATE(grid%neexy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9347,&
=======
 CALL wrf_error_fatal3("<stdin>",9396,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%neexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gppxy ) ) THEN 
  DEALLOCATE(grid%gppxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9354,&
=======
 CALL wrf_error_fatal3("<stdin>",9403,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gppxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nppxy ) ) THEN 
  DEALLOCATE(grid%nppxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9361,&
=======
 CALL wrf_error_fatal3("<stdin>",9410,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nppxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fvegxy ) ) THEN 
  DEALLOCATE(grid%fvegxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9368,&
=======
 CALL wrf_error_fatal3("<stdin>",9417,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fvegxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qinxy ) ) THEN 
  DEALLOCATE(grid%qinxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9375,&
=======
 CALL wrf_error_fatal3("<stdin>",9424,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qinxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%runsfxy ) ) THEN 
  DEALLOCATE(grid%runsfxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9382,&
=======
 CALL wrf_error_fatal3("<stdin>",9431,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%runsfxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%runsbxy ) ) THEN 
  DEALLOCATE(grid%runsbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9389,&
=======
 CALL wrf_error_fatal3("<stdin>",9438,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%runsbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ecanxy ) ) THEN 
  DEALLOCATE(grid%ecanxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9396,&
=======
 CALL wrf_error_fatal3("<stdin>",9445,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ecanxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edirxy ) ) THEN 
  DEALLOCATE(grid%edirxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9403,&
=======
 CALL wrf_error_fatal3("<stdin>",9452,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%edirxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%etranxy ) ) THEN 
  DEALLOCATE(grid%etranxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9410,&
=======
 CALL wrf_error_fatal3("<stdin>",9459,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%etranxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fsaxy ) ) THEN 
  DEALLOCATE(grid%fsaxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9417,&
=======
 CALL wrf_error_fatal3("<stdin>",9466,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fsaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%firaxy ) ) THEN 
  DEALLOCATE(grid%firaxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9424,&
=======
 CALL wrf_error_fatal3("<stdin>",9473,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%firaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aparxy ) ) THEN 
  DEALLOCATE(grid%aparxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9431,&
=======
 CALL wrf_error_fatal3("<stdin>",9480,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aparxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psnxy ) ) THEN 
  DEALLOCATE(grid%psnxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9438,&
=======
 CALL wrf_error_fatal3("<stdin>",9487,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psnxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%savxy ) ) THEN 
  DEALLOCATE(grid%savxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9445,&
=======
 CALL wrf_error_fatal3("<stdin>",9494,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%savxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sagxy ) ) THEN 
  DEALLOCATE(grid%sagxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9452,&
=======
 CALL wrf_error_fatal3("<stdin>",9501,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sagxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rssunxy ) ) THEN 
  DEALLOCATE(grid%rssunxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9459,&
=======
 CALL wrf_error_fatal3("<stdin>",9508,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rssunxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rsshaxy ) ) THEN 
  DEALLOCATE(grid%rsshaxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9466,&
=======
 CALL wrf_error_fatal3("<stdin>",9515,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rsshaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bgapxy ) ) THEN 
  DEALLOCATE(grid%bgapxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9473,&
=======
 CALL wrf_error_fatal3("<stdin>",9522,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bgapxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wgapxy ) ) THEN 
  DEALLOCATE(grid%wgapxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9480,&
=======
 CALL wrf_error_fatal3("<stdin>",9529,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wgapxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgvxy ) ) THEN 
  DEALLOCATE(grid%tgvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9487,&
=======
 CALL wrf_error_fatal3("<stdin>",9536,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgbxy ) ) THEN 
  DEALLOCATE(grid%tgbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9494,&
=======
 CALL wrf_error_fatal3("<stdin>",9543,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chvxy ) ) THEN 
  DEALLOCATE(grid%chvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9501,&
=======
 CALL wrf_error_fatal3("<stdin>",9550,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chbxy ) ) THEN 
  DEALLOCATE(grid%chbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9508,&
=======
 CALL wrf_error_fatal3("<stdin>",9557,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shgxy ) ) THEN 
  DEALLOCATE(grid%shgxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9515,&
=======
 CALL wrf_error_fatal3("<stdin>",9564,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shcxy ) ) THEN 
  DEALLOCATE(grid%shcxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9522,&
=======
 CALL wrf_error_fatal3("<stdin>",9571,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shcxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shbxy ) ) THEN 
  DEALLOCATE(grid%shbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9529,&
=======
 CALL wrf_error_fatal3("<stdin>",9578,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evgxy ) ) THEN 
  DEALLOCATE(grid%evgxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9536,&
=======
 CALL wrf_error_fatal3("<stdin>",9585,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evbxy ) ) THEN 
  DEALLOCATE(grid%evbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9543,&
=======
 CALL wrf_error_fatal3("<stdin>",9592,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ghvxy ) ) THEN 
  DEALLOCATE(grid%ghvxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9550,&
=======
 CALL wrf_error_fatal3("<stdin>",9599,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ghvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ghbxy ) ) THEN 
  DEALLOCATE(grid%ghbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9557,&
=======
 CALL wrf_error_fatal3("<stdin>",9606,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ghbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%irgxy ) ) THEN 
  DEALLOCATE(grid%irgxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9564,&
=======
 CALL wrf_error_fatal3("<stdin>",9613,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%irgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ircxy ) ) THEN 
  DEALLOCATE(grid%ircxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9571,&
=======
 CALL wrf_error_fatal3("<stdin>",9620,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ircxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%irbxy ) ) THEN 
  DEALLOCATE(grid%irbxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9578,&
=======
 CALL wrf_error_fatal3("<stdin>",9627,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%irbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trxy ) ) THEN 
  DEALLOCATE(grid%trxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9585,&
=======
 CALL wrf_error_fatal3("<stdin>",9634,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%trxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evcxy ) ) THEN 
  DEALLOCATE(grid%evcxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9592,&
=======
 CALL wrf_error_fatal3("<stdin>",9641,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evcxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chleafxy ) ) THEN 
  DEALLOCATE(grid%chleafxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9599,&
=======
 CALL wrf_error_fatal3("<stdin>",9648,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chleafxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chucxy ) ) THEN 
  DEALLOCATE(grid%chucxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9606,&
=======
 CALL wrf_error_fatal3("<stdin>",9655,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chucxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chv2xy ) ) THEN 
  DEALLOCATE(grid%chv2xy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9613,&
=======
 CALL wrf_error_fatal3("<stdin>",9662,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chv2xy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chb2xy ) ) THEN 
  DEALLOCATE(grid%chb2xy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9620,&
=======
 CALL wrf_error_fatal3("<stdin>",9669,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chb2xy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chstarxy ) ) THEN 
  DEALLOCATE(grid%chstarxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9627,&
=======
 CALL wrf_error_fatal3("<stdin>",9676,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chstarxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smoiseq ) ) THEN 
  DEALLOCATE(grid%smoiseq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9634,&
=======
 CALL wrf_error_fatal3("<stdin>",9683,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smoiseq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smcwtdxy ) ) THEN 
  DEALLOCATE(grid%smcwtdxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9641,&
=======
 CALL wrf_error_fatal3("<stdin>",9690,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smcwtdxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rechxy ) ) THEN 
  DEALLOCATE(grid%rechxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9648,&
=======
 CALL wrf_error_fatal3("<stdin>",9697,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rechxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%deeprechxy ) ) THEN 
  DEALLOCATE(grid%deeprechxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9655,&
=======
 CALL wrf_error_fatal3("<stdin>",9704,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%deeprechxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%areaxy ) ) THEN 
  DEALLOCATE(grid%areaxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9662,&
=======
 CALL wrf_error_fatal3("<stdin>",9711,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%areaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrfxy ) ) THEN 
  DEALLOCATE(grid%qrfxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9669,&
=======
 CALL wrf_error_fatal3("<stdin>",9718,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qrfxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrfsxy ) ) THEN 
  DEALLOCATE(grid%qrfsxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9676,&
=======
 CALL wrf_error_fatal3("<stdin>",9725,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qrfsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qspringxy ) ) THEN 
  DEALLOCATE(grid%qspringxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9683,&
=======
 CALL wrf_error_fatal3("<stdin>",9732,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qspringxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qspringsxy ) ) THEN 
  DEALLOCATE(grid%qspringsxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9690,&
=======
 CALL wrf_error_fatal3("<stdin>",9739,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qspringsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qslatxy ) ) THEN 
  DEALLOCATE(grid%qslatxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9697,&
=======
 CALL wrf_error_fatal3("<stdin>",9746,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qslatxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pexpxy ) ) THEN 
  DEALLOCATE(grid%pexpxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9704,&
=======
 CALL wrf_error_fatal3("<stdin>",9753,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pexpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rivercondxy ) ) THEN 
  DEALLOCATE(grid%rivercondxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9711,&
=======
 CALL wrf_error_fatal3("<stdin>",9760,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rivercondxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdepthxy ) ) THEN 
  DEALLOCATE(grid%fdepthxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9718,&
=======
 CALL wrf_error_fatal3("<stdin>",9767,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdepthxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eqzwt ) ) THEN 
  DEALLOCATE(grid%eqzwt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9725,&
=======
 CALL wrf_error_fatal3("<stdin>",9774,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%eqzwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rechclim ) ) THEN 
  DEALLOCATE(grid%rechclim,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9732,&
=======
 CALL wrf_error_fatal3("<stdin>",9781,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rechclim. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%riverbedxy ) ) THEN 
  DEALLOCATE(grid%riverbedxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9739,&
=======
 CALL wrf_error_fatal3("<stdin>",9788,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%riverbedxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grainxy ) ) THEN 
  DEALLOCATE(grid%grainxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9746,&
=======
 CALL wrf_error_fatal3("<stdin>",9795,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grainxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gddxy ) ) THEN 
  DEALLOCATE(grid%gddxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9753,&
=======
 CALL wrf_error_fatal3("<stdin>",9802,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%gddxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%croptype ) ) THEN 
  DEALLOCATE(grid%croptype,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9760,&
=======
 CALL wrf_error_fatal3("<stdin>",9809,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%croptype. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%planting ) ) THEN 
  DEALLOCATE(grid%planting,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9767,&
=======
 CALL wrf_error_fatal3("<stdin>",9816,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%planting. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%harvest ) ) THEN 
  DEALLOCATE(grid%harvest,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9774,&
=======
 CALL wrf_error_fatal3("<stdin>",9823,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%harvest. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%season_gdd ) ) THEN 
  DEALLOCATE(grid%season_gdd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9781,&
=======
 CALL wrf_error_fatal3("<stdin>",9830,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%season_gdd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cropcat ) ) THEN 
  DEALLOCATE(grid%cropcat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9788,&
=======
 CALL wrf_error_fatal3("<stdin>",9837,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cropcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pgsxy ) ) THEN 
  DEALLOCATE(grid%pgsxy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9795,&
=======
 CALL wrf_error_fatal3("<stdin>",9844,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pgsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_mosaic ) ) THEN 
  DEALLOCATE(grid%tsk_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9802,&
=======
 CALL wrf_error_fatal3("<stdin>",9851,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsfc_mosaic ) ) THEN 
  DEALLOCATE(grid%qsfc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9809,&
=======
 CALL wrf_error_fatal3("<stdin>",9858,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsfc_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tslb_mosaic ) ) THEN 
  DEALLOCATE(grid%tslb_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9816,&
=======
 CALL wrf_error_fatal3("<stdin>",9865,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tslb_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smois_mosaic ) ) THEN 
  DEALLOCATE(grid%smois_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9823,&
=======
 CALL wrf_error_fatal3("<stdin>",9872,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smois_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh2o_mosaic ) ) THEN 
  DEALLOCATE(grid%sh2o_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9830,&
=======
 CALL wrf_error_fatal3("<stdin>",9879,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh2o_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canwat_mosaic ) ) THEN 
  DEALLOCATE(grid%canwat_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9837,&
=======
 CALL wrf_error_fatal3("<stdin>",9886,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canwat_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow_mosaic ) ) THEN 
  DEALLOCATE(grid%snow_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9844,&
=======
 CALL wrf_error_fatal3("<stdin>",9893,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snow_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowh_mosaic ) ) THEN 
  DEALLOCATE(grid%snowh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9851,&
=======
 CALL wrf_error_fatal3("<stdin>",9900,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowh_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowc_mosaic ) ) THEN 
  DEALLOCATE(grid%snowc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9858,&
=======
 CALL wrf_error_fatal3("<stdin>",9907,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowc_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo_mosaic ) ) THEN 
  DEALLOCATE(grid%albedo_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9865,&
=======
 CALL wrf_error_fatal3("<stdin>",9914,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albedo_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albbck_mosaic ) ) THEN 
  DEALLOCATE(grid%albbck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9872,&
=======
 CALL wrf_error_fatal3("<stdin>",9921,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albbck_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emiss_mosaic ) ) THEN 
  DEALLOCATE(grid%emiss_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9879,&
=======
 CALL wrf_error_fatal3("<stdin>",9928,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%emiss_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%embck_mosaic ) ) THEN 
  DEALLOCATE(grid%embck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9886,&
=======
 CALL wrf_error_fatal3("<stdin>",9935,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%embck_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znt_mosaic ) ) THEN 
  DEALLOCATE(grid%znt_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9893,&
=======
 CALL wrf_error_fatal3("<stdin>",9942,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%znt_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0_mosaic ) ) THEN 
  DEALLOCATE(grid%z0_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9900,&
=======
 CALL wrf_error_fatal3("<stdin>",9949,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z0_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_mosaic ) ) THEN 
  DEALLOCATE(grid%hfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9907,&
=======
 CALL wrf_error_fatal3("<stdin>",9956,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx_mosaic ) ) THEN 
  DEALLOCATE(grid%qfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9914,&
=======
 CALL wrf_error_fatal3("<stdin>",9963,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qfx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9921,&
=======
 CALL wrf_error_fatal3("<stdin>",9970,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grdflx_mosaic ) ) THEN 
  DEALLOCATE(grid%grdflx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9928,&
=======
 CALL wrf_error_fatal3("<stdin>",9977,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grdflx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snotime_mosaic ) ) THEN 
  DEALLOCATE(grid%snotime_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9935,&
=======
 CALL wrf_error_fatal3("<stdin>",9984,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snotime_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tr_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9942,&
=======
 CALL wrf_error_fatal3("<stdin>",9991,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tb_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9949,&
=======
 CALL wrf_error_fatal3("<stdin>",9998,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tg_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9956,&
=======
 CALL wrf_error_fatal3("<stdin>",10005,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9963,&
=======
 CALL wrf_error_fatal3("<stdin>",10012,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9970,&
=======
 CALL wrf_error_fatal3("<stdin>",10019,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rul2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_rul2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9977,&
=======
 CALL wrf_error_fatal3("<stdin>",10026,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ts_rul2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%qc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9984,&
=======
 CALL wrf_error_fatal3("<stdin>",10033,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%uc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9991,&
=======
 CALL wrf_error_fatal3("<stdin>",10040,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%trl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",9998,&
=======
 CALL wrf_error_fatal3("<stdin>",10047,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10005,&
=======
 CALL wrf_error_fatal3("<stdin>",10054,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10012,&
=======
 CALL wrf_error_fatal3("<stdin>",10061,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%sh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10019,&
=======
 CALL wrf_error_fatal3("<stdin>",10068,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10026,&
=======
 CALL wrf_error_fatal3("<stdin>",10075,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%g_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%g_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10033,&
=======
 CALL wrf_error_fatal3("<stdin>",10082,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%g_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%rn_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10040,&
=======
 CALL wrf_error_fatal3("<stdin>",10089,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mosaic_cat_index ) ) THEN 
  DEALLOCATE(grid%mosaic_cat_index,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10047,&
=======
 CALL wrf_error_fatal3("<stdin>",10096,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mosaic_cat_index. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landusef2 ) ) THEN 
  DEALLOCATE(grid%landusef2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10054,&
=======
 CALL wrf_error_fatal3("<stdin>",10103,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%landusef2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10061,&
=======
 CALL wrf_error_fatal3("<stdin>",10110,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mp_restart_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10068,&
=======
 CALL wrf_error_fatal3("<stdin>",10117,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tbpvs_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10075,&
=======
 CALL wrf_error_fatal3("<stdin>",10124,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tbpvs0_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10082,&
=======
 CALL wrf_error_fatal3("<stdin>",10131,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lu_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_phy ) ) THEN 
  DEALLOCATE(grid%t_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10089,&
=======
 CALL wrf_error_fatal3("<stdin>",10138,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_phy ) ) THEN 
  DEALLOCATE(grid%u_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10096,&
=======
 CALL wrf_error_fatal3("<stdin>",10145,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_phy ) ) THEN 
  DEALLOCATE(grid%v_phy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10103,&
=======
 CALL wrf_error_fatal3("<stdin>",10152,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmn ) ) THEN 
  DEALLOCATE(grid%tmn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10110,&
=======
 CALL wrf_error_fatal3("<stdin>",10159,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tmn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tyr ) ) THEN 
  DEALLOCATE(grid%tyr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10117,&
=======
 CALL wrf_error_fatal3("<stdin>",10166,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tyr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tyra ) ) THEN 
  DEALLOCATE(grid%tyra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10124,&
=======
 CALL wrf_error_fatal3("<stdin>",10173,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tyra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tdly ) ) THEN 
  DEALLOCATE(grid%tdly,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10131,&
=======
 CALL wrf_error_fatal3("<stdin>",10180,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tdly. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlag ) ) THEN 
  DEALLOCATE(grid%tlag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10138,&
=======
 CALL wrf_error_fatal3("<stdin>",10187,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tlag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10145,&
=======
 CALL wrf_error_fatal3("<stdin>",10194,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xland. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cplmask ) ) THEN 
  DEALLOCATE(grid%cplmask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10152,&
=======
 CALL wrf_error_fatal3("<stdin>",10201,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cplmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10159,&
=======
 CALL wrf_error_fatal3("<stdin>",10208,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%znt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ck ) ) THEN 
  DEALLOCATE(grid%ck,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10166,&
=======
 CALL wrf_error_fatal3("<stdin>",10215,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cka ) ) THEN 
  DEALLOCATE(grid%cka,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10173,&
=======
 CALL wrf_error_fatal3("<stdin>",10222,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cka. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cd ) ) THEN 
  DEALLOCATE(grid%cd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10180,&
=======
 CALL wrf_error_fatal3("<stdin>",10229,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cda ) ) THEN 
  DEALLOCATE(grid%cda,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10187,&
=======
 CALL wrf_error_fatal3("<stdin>",10236,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cda. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ust ) ) THEN 
  DEALLOCATE(grid%ust,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10194,&
=======
 CALL wrf_error_fatal3("<stdin>",10243,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ust. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ustm ) ) THEN 
  DEALLOCATE(grid%ustm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10201,&
=======
 CALL wrf_error_fatal3("<stdin>",10250,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ustm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10208,&
=======
 CALL wrf_error_fatal3("<stdin>",10257,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rmol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10215,&
=======
 CALL wrf_error_fatal3("<stdin>",10264,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10222,&
=======
 CALL wrf_error_fatal3("<stdin>",10271,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pblh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%capg ) ) THEN 
  DEALLOCATE(grid%capg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10229,&
=======
 CALL wrf_error_fatal3("<stdin>",10278,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%capg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thc ) ) THEN 
  DEALLOCATE(grid%thc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10236,&
=======
 CALL wrf_error_fatal3("<stdin>",10285,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx ) ) THEN 
  DEALLOCATE(grid%hfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10243,&
=======
 CALL wrf_error_fatal3("<stdin>",10292,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx ) ) THEN 
  DEALLOCATE(grid%qfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10250,&
=======
 CALL wrf_error_fatal3("<stdin>",10299,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh ) ) THEN 
  DEALLOCATE(grid%lh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10257,&
=======
 CALL wrf_error_fatal3("<stdin>",10306,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%achfx ) ) THEN 
  DEALLOCATE(grid%achfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10264,&
=======
 CALL wrf_error_fatal3("<stdin>",10313,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%achfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wstar ) ) THEN 
  DEALLOCATE(grid%wstar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10271,&
=======
 CALL wrf_error_fatal3("<stdin>",10320,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wstar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclhf ) ) THEN 
  DEALLOCATE(grid%aclhf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10278,&
=======
 CALL wrf_error_fatal3("<stdin>",10327,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aclhf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10285,&
=======
 CALL wrf_error_fatal3("<stdin>",10334,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flhc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10292,&
=======
 CALL wrf_error_fatal3("<stdin>",10341,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flqc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10299,&
=======
 CALL wrf_error_fatal3("<stdin>",10348,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10306,&
=======
 CALL wrf_error_fatal3("<stdin>",10355,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qvg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_qvg ) ) THEN 
  DEALLOCATE(grid%dfi_qvg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10313,&
=======
 CALL wrf_error_fatal3("<stdin>",10362,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_qvg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10320,&
=======
 CALL wrf_error_fatal3("<stdin>",10369,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dew ) ) THEN 
  DEALLOCATE(grid%dew,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10327,&
=======
 CALL wrf_error_fatal3("<stdin>",10376,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dew. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10334,&
=======
 CALL wrf_error_fatal3("<stdin>",10383,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soilt1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_soilt1 ) ) THEN 
  DEALLOCATE(grid%dfi_soilt1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10341,&
=======
 CALL wrf_error_fatal3("<stdin>",10390,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_soilt1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10348,&
=======
 CALL wrf_error_fatal3("<stdin>",10397,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsnav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tsnav ) ) THEN 
  DEALLOCATE(grid%dfi_tsnav,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10355,&
=======
 CALL wrf_error_fatal3("<stdin>",10404,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_tsnav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%regime ) ) THEN 
  DEALLOCATE(grid%regime,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10362,&
=======
 CALL wrf_error_fatal3("<stdin>",10411,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%regime. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowc ) ) THEN 
  DEALLOCATE(grid%snowc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10369,&
=======
 CALL wrf_error_fatal3("<stdin>",10418,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snowc ) ) THEN 
  DEALLOCATE(grid%dfi_snowc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10376,&
=======
 CALL wrf_error_fatal3("<stdin>",10425,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfi_snowc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10383,&
=======
 CALL wrf_error_fatal3("<stdin>",10432,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mavail. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkesfcf ) ) THEN 
  DEALLOCATE(grid%tkesfcf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10390,&
=======
 CALL wrf_error_fatal3("<stdin>",10439,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tkesfcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sr ) ) THEN 
  DEALLOCATE(grid%sr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10397,&
=======
 CALL wrf_error_fatal3("<stdin>",10446,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%potevp ) ) THEN 
  DEALLOCATE(grid%potevp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10404,&
=======
 CALL wrf_error_fatal3("<stdin>",10453,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%potevp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snopcx ) ) THEN 
  DEALLOCATE(grid%snopcx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10411,&
=======
 CALL wrf_error_fatal3("<stdin>",10460,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snopcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soiltb ) ) THEN 
  DEALLOCATE(grid%soiltb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10418,&
=======
 CALL wrf_error_fatal3("<stdin>",10467,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%soiltb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucldi ) ) THEN 
  DEALLOCATE(grid%taucldi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10425,&
=======
 CALL wrf_error_fatal3("<stdin>",10474,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taucldi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucldc ) ) THEN 
  DEALLOCATE(grid%taucldc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10432,&
=======
 CALL wrf_error_fatal3("<stdin>",10481,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%taucldc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor11 ) ) THEN 
  DEALLOCATE(grid%defor11,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10439,&
=======
 CALL wrf_error_fatal3("<stdin>",10488,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor11. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor22 ) ) THEN 
  DEALLOCATE(grid%defor22,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10446,&
=======
 CALL wrf_error_fatal3("<stdin>",10495,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor22. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor12 ) ) THEN 
  DEALLOCATE(grid%defor12,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10453,&
=======
 CALL wrf_error_fatal3("<stdin>",10502,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor12. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor33 ) ) THEN 
  DEALLOCATE(grid%defor33,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10460,&
=======
 CALL wrf_error_fatal3("<stdin>",10509,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor33. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor13 ) ) THEN 
  DEALLOCATE(grid%defor13,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10467,&
=======
 CALL wrf_error_fatal3("<stdin>",10516,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor13. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor23 ) ) THEN 
  DEALLOCATE(grid%defor23,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10474,&
=======
 CALL wrf_error_fatal3("<stdin>",10523,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%defor23. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkmv ) ) THEN 
  DEALLOCATE(grid%xkmv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10481,&
=======
 CALL wrf_error_fatal3("<stdin>",10530,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xkmv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkmh ) ) THEN 
  DEALLOCATE(grid%xkmh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10488,&
=======
 CALL wrf_error_fatal3("<stdin>",10537,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xkmh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkhv ) ) THEN 
  DEALLOCATE(grid%xkhv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10495,&
=======
 CALL wrf_error_fatal3("<stdin>",10544,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xkhv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkhh ) ) THEN 
  DEALLOCATE(grid%xkhh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10502,&
=======
 CALL wrf_error_fatal3("<stdin>",10551,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xkhh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10509,&
=======
 CALL wrf_error_fatal3("<stdin>",10558,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%div. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bn2 ) ) THEN 
  DEALLOCATE(grid%bn2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10516,&
=======
 CALL wrf_error_fatal3("<stdin>",10565,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bn2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rundgdten ) ) THEN 
  DEALLOCATE(grid%rundgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10523,&
=======
 CALL wrf_error_fatal3("<stdin>",10572,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rundgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvndgdten ) ) THEN 
  DEALLOCATE(grid%rvndgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10530,&
=======
 CALL wrf_error_fatal3("<stdin>",10579,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rvndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthndgdten ) ) THEN 
  DEALLOCATE(grid%rthndgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10537,&
=======
 CALL wrf_error_fatal3("<stdin>",10586,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rphndgdten ) ) THEN 
  DEALLOCATE(grid%rphndgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10544,&
=======
 CALL wrf_error_fatal3("<stdin>",10593,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rphndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvndgdten ) ) THEN 
  DEALLOCATE(grid%rqvndgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10551,&
=======
 CALL wrf_error_fatal3("<stdin>",10600,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rmundgdten ) ) THEN 
  DEALLOCATE(grid%rmundgdten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10558,&
=======
 CALL wrf_error_fatal3("<stdin>",10607,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rmundgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdda3d ) ) THEN 
  DEALLOCATE(grid%fdda3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10565,&
=======
 CALL wrf_error_fatal3("<stdin>",10614,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdda3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdda2d ) ) THEN 
  DEALLOCATE(grid%fdda2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10572,&
=======
 CALL wrf_error_fatal3("<stdin>",10621,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdda2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_old ) ) THEN 
  DEALLOCATE(grid%u10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10579,&
=======
 CALL wrf_error_fatal3("<stdin>",10628,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_new ) ) THEN 
  DEALLOCATE(grid%u10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10586,&
=======
 CALL wrf_error_fatal3("<stdin>",10635,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_old ) ) THEN 
  DEALLOCATE(grid%v10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10593,&
=======
 CALL wrf_error_fatal3("<stdin>",10642,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_new ) ) THEN 
  DEALLOCATE(grid%v10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10600,&
=======
 CALL wrf_error_fatal3("<stdin>",10649,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_old ) ) THEN 
  DEALLOCATE(grid%t2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10607,&
=======
 CALL wrf_error_fatal3("<stdin>",10656,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_new ) ) THEN 
  DEALLOCATE(grid%t2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10614,&
=======
 CALL wrf_error_fatal3("<stdin>",10663,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_old ) ) THEN 
  DEALLOCATE(grid%th2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10621,&
=======
 CALL wrf_error_fatal3("<stdin>",10670,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_new ) ) THEN 
  DEALLOCATE(grid%th2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10628,&
=======
 CALL wrf_error_fatal3("<stdin>",10677,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_old ) ) THEN 
  DEALLOCATE(grid%q2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10635,&
=======
 CALL wrf_error_fatal3("<stdin>",10684,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_new ) ) THEN 
  DEALLOCATE(grid%q2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10642,&
=======
 CALL wrf_error_fatal3("<stdin>",10691,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_old ) ) THEN 
  DEALLOCATE(grid%rh_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10649,&
=======
 CALL wrf_error_fatal3("<stdin>",10698,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_new ) ) THEN 
  DEALLOCATE(grid%rh_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10656,&
=======
 CALL wrf_error_fatal3("<stdin>",10705,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_old ) ) THEN 
  DEALLOCATE(grid%psl_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10663,&
=======
 CALL wrf_error_fatal3("<stdin>",10712,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_new ) ) THEN 
  DEALLOCATE(grid%psl_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10670,&
=======
 CALL wrf_error_fatal3("<stdin>",10719,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_old ) ) THEN 
  DEALLOCATE(grid%ps_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10677,&
=======
 CALL wrf_error_fatal3("<stdin>",10726,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_new ) ) THEN 
  DEALLOCATE(grid%ps_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10684,&
=======
 CALL wrf_error_fatal3("<stdin>",10733,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_old ) ) THEN 
  DEALLOCATE(grid%tob_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10691,&
=======
 CALL wrf_error_fatal3("<stdin>",10740,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_old ) ) THEN 
  DEALLOCATE(grid%odis_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10698,&
=======
 CALL wrf_error_fatal3("<stdin>",10747,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_new ) ) THEN 
  DEALLOCATE(grid%tob_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10705,&
=======
 CALL wrf_error_fatal3("<stdin>",10754,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_new ) ) THEN 
  DEALLOCATE(grid%odis_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10712,&
=======
 CALL wrf_error_fatal3("<stdin>",10761,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_new ) ) THEN 
  DEALLOCATE(grid%sn_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10719,&
=======
 CALL wrf_error_fatal3("<stdin>",10768,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_old ) ) THEN 
  DEALLOCATE(grid%sn_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10726,&
=======
 CALL wrf_error_fatal3("<stdin>",10775,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sda_hfx ) ) THEN 
  DEALLOCATE(grid%sda_hfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10733,&
=======
 CALL wrf_error_fatal3("<stdin>",10782,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sda_hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sda_qfx ) ) THEN 
  DEALLOCATE(grid%sda_qfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10740,&
=======
 CALL wrf_error_fatal3("<stdin>",10789,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sda_qfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnorm ) ) THEN 
  DEALLOCATE(grid%qnorm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10747,&
=======
 CALL wrf_error_fatal3("<stdin>",10796,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qnorm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_both ) ) THEN 
  DEALLOCATE(grid%hfx_both,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10754,&
=======
 CALL wrf_error_fatal3("<stdin>",10803,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_both. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx_both ) ) THEN 
  DEALLOCATE(grid%qfx_both,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10761,&
=======
 CALL wrf_error_fatal3("<stdin>",10810,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qfx_both. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_fdda ) ) THEN 
  DEALLOCATE(grid%hfx_fdda,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10768,&
=======
 CALL wrf_error_fatal3("<stdin>",10817,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_fdda. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%abstot ) ) THEN 
  DEALLOCATE(grid%abstot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10775,&
=======
 CALL wrf_error_fatal3("<stdin>",10824,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%abstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%absnxt ) ) THEN 
  DEALLOCATE(grid%absnxt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10782,&
=======
 CALL wrf_error_fatal3("<stdin>",10831,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%absnxt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emstot ) ) THEN 
  DEALLOCATE(grid%emstot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10789,&
=======
 CALL wrf_error_fatal3("<stdin>",10838,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%emstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dpsdt ) ) THEN 
  DEALLOCATE(grid%dpsdt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10796,&
=======
 CALL wrf_error_fatal3("<stdin>",10845,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dpsdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dmudt ) ) THEN 
  DEALLOCATE(grid%dmudt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10803,&
=======
 CALL wrf_error_fatal3("<stdin>",10852,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dmudt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pk1m ) ) THEN 
  DEALLOCATE(grid%pk1m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10810,&
=======
 CALL wrf_error_fatal3("<stdin>",10859,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pk1m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_2m ) ) THEN 
  DEALLOCATE(grid%mu_2m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10817,&
=======
 CALL wrf_error_fatal3("<stdin>",10866,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu_2m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wspd10max ) ) THEN 
  DEALLOCATE(grid%wspd10max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10824,&
=======
 CALL wrf_error_fatal3("<stdin>",10873,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wspd10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_up_max ) ) THEN 
  DEALLOCATE(grid%w_up_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10831,&
=======
 CALL wrf_error_fatal3("<stdin>",10880,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_up_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_dn_max ) ) THEN 
  DEALLOCATE(grid%w_dn_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10838,&
=======
 CALL wrf_error_fatal3("<stdin>",10887,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_dn_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd_max ) ) THEN 
  DEALLOCATE(grid%refd_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10845,&
=======
 CALL wrf_error_fatal3("<stdin>",10894,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%refd_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%up_heli_max ) ) THEN 
  DEALLOCATE(grid%up_heli_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10852,&
=======
 CALL wrf_error_fatal3("<stdin>",10901,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%up_heli_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_mean ) ) THEN 
  DEALLOCATE(grid%w_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10859,&
=======
 CALL wrf_error_fatal3("<stdin>",10908,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_max ) ) THEN 
  DEALLOCATE(grid%grpl_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10866,&
=======
 CALL wrf_error_fatal3("<stdin>",10915,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grpl_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uh ) ) THEN 
  DEALLOCATE(grid%uh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10873,&
=======
 CALL wrf_error_fatal3("<stdin>",10922,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_colmean ) ) THEN 
  DEALLOCATE(grid%w_colmean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10880,&
=======
 CALL wrf_error_fatal3("<stdin>",10929,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_colmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%numcolpts ) ) THEN 
  DEALLOCATE(grid%numcolpts,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10887,&
=======
 CALL wrf_error_fatal3("<stdin>",10936,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%numcolpts. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_colint ) ) THEN 
  DEALLOCATE(grid%grpl_colint,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10894,&
=======
 CALL wrf_error_fatal3("<stdin>",10943,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grpl_colint. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hail_maxk1 ) ) THEN 
  DEALLOCATE(grid%hail_maxk1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10901,&
=======
 CALL wrf_error_fatal3("<stdin>",10950,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hail_maxk1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hail_max2d ) ) THEN 
  DEALLOCATE(grid%hail_max2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10908,&
=======
 CALL wrf_error_fatal3("<stdin>",10957,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hail_max2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prec_acc_c ) ) THEN 
  DEALLOCATE(grid%prec_acc_c,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10915,&
=======
 CALL wrf_error_fatal3("<stdin>",10964,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%prec_acc_c. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prec_acc_nc ) ) THEN 
  DEALLOCATE(grid%prec_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10922,&
=======
 CALL wrf_error_fatal3("<stdin>",10971,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%prec_acc_nc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow_acc_nc ) ) THEN 
  DEALLOCATE(grid%snow_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10929,&
=======
 CALL wrf_error_fatal3("<stdin>",10978,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snow_acc_nc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%advh_t ) ) THEN 
  DEALLOCATE(grid%advh_t,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10936,&
=======
 CALL wrf_error_fatal3("<stdin>",10985,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%advh_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%advz_t ) ) THEN 
  DEALLOCATE(grid%advz_t,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10943,&
=======
 CALL wrf_error_fatal3("<stdin>",10992,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%advz_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tml ) ) THEN 
  DEALLOCATE(grid%tml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10950,&
=======
 CALL wrf_error_fatal3("<stdin>",10999,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t0ml ) ) THEN 
  DEALLOCATE(grid%t0ml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10957,&
=======
 CALL wrf_error_fatal3("<stdin>",11006,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t0ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hml ) ) THEN 
  DEALLOCATE(grid%hml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10964,&
=======
 CALL wrf_error_fatal3("<stdin>",11013,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h0ml ) ) THEN 
  DEALLOCATE(grid%h0ml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10971,&
=======
 CALL wrf_error_fatal3("<stdin>",11020,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h0ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%huml ) ) THEN 
  DEALLOCATE(grid%huml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10978,&
=======
 CALL wrf_error_fatal3("<stdin>",11027,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%huml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hvml ) ) THEN 
  DEALLOCATE(grid%hvml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10985,&
=======
 CALL wrf_error_fatal3("<stdin>",11034,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hvml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmoml ) ) THEN 
  DEALLOCATE(grid%tmoml,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10992,&
=======
 CALL wrf_error_fatal3("<stdin>",11041,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tmoml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_z ) ) THEN 
  DEALLOCATE(grid%track_z,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",10999,&
=======
 CALL wrf_error_fatal3("<stdin>",11048,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_z. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_t ) ) THEN 
  DEALLOCATE(grid%track_t,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11006,&
=======
 CALL wrf_error_fatal3("<stdin>",11055,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_p ) ) THEN 
  DEALLOCATE(grid%track_p,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11013,&
=======
 CALL wrf_error_fatal3("<stdin>",11062,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_u ) ) THEN 
  DEALLOCATE(grid%track_u,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11020,&
=======
 CALL wrf_error_fatal3("<stdin>",11069,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_v ) ) THEN 
  DEALLOCATE(grid%track_v,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11027,&
=======
 CALL wrf_error_fatal3("<stdin>",11076,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_w ) ) THEN 
  DEALLOCATE(grid%track_w,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11034,&
=======
 CALL wrf_error_fatal3("<stdin>",11083,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_rh ) ) THEN 
  DEALLOCATE(grid%track_rh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11041,&
=======
 CALL wrf_error_fatal3("<stdin>",11090,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_rh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_alt ) ) THEN 
  DEALLOCATE(grid%track_alt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11048,&
=======
 CALL wrf_error_fatal3("<stdin>",11097,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_ele ) ) THEN 
  DEALLOCATE(grid%track_ele,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11055,&
=======
 CALL wrf_error_fatal3("<stdin>",11104,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_ele. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_aircraft ) ) THEN 
  DEALLOCATE(grid%track_aircraft,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11062,&
=======
 CALL wrf_error_fatal3("<stdin>",11111,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_aircraft. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qcloud ) ) THEN 
  DEALLOCATE(grid%track_qcloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11069,&
=======
 CALL wrf_error_fatal3("<stdin>",11118,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qcloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qrain ) ) THEN 
  DEALLOCATE(grid%track_qrain,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11076,&
=======
 CALL wrf_error_fatal3("<stdin>",11125,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qrain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qice ) ) THEN 
  DEALLOCATE(grid%track_qice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11083,&
=======
 CALL wrf_error_fatal3("<stdin>",11132,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qsnow ) ) THEN 
  DEALLOCATE(grid%track_qsnow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11090,&
=======
 CALL wrf_error_fatal3("<stdin>",11139,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qsnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qgraup ) ) THEN 
  DEALLOCATE(grid%track_qgraup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11097,&
=======
 CALL wrf_error_fatal3("<stdin>",11146,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qgraup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qvapor ) ) THEN 
  DEALLOCATE(grid%track_qvapor,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11104,&
=======
 CALL wrf_error_fatal3("<stdin>",11153,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%track_qvapor. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail1 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11111,&
=======
 CALL wrf_error_fatal3("<stdin>",11160,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail2 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11118,&
=======
 CALL wrf_error_fatal3("<stdin>",11167,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail3 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11125,&
=======
 CALL wrf_error_fatal3("<stdin>",11174,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail4 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11132,&
=======
 CALL wrf_error_fatal3("<stdin>",11181,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail5 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11139,&
=======
 CALL wrf_error_fatal3("<stdin>",11188,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_mean ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11146,&
=======
 CALL wrf_error_fatal3("<stdin>",11195,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_std ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_std,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11153,&
=======
 CALL wrf_error_fatal3("<stdin>",11202,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_wup_mask ) ) THEN 
  DEALLOCATE(grid%hailcast_wup_mask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11160,&
=======
 CALL wrf_error_fatal3("<stdin>",11209,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_wup_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_wdur ) ) THEN 
  DEALLOCATE(grid%hailcast_wdur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11167,&
=======
 CALL wrf_error_fatal3("<stdin>",11216,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hailcast_wdur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ic_flashcount ) ) THEN 
  DEALLOCATE(grid%ic_flashcount,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11174,&
=======
 CALL wrf_error_fatal3("<stdin>",11223,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ic_flashcount. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ic_flashrate ) ) THEN 
  DEALLOCATE(grid%ic_flashrate,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11181,&
=======
 CALL wrf_error_fatal3("<stdin>",11230,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ic_flashrate. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cg_flashcount ) ) THEN 
  DEALLOCATE(grid%cg_flashcount,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11188,&
=======
 CALL wrf_error_fatal3("<stdin>",11237,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cg_flashcount. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cg_flashrate ) ) THEN 
  DEALLOCATE(grid%cg_flashrate,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11195,&
=======
 CALL wrf_error_fatal3("<stdin>",11244,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cg_flashrate. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iccg_in_num ) ) THEN 
  DEALLOCATE(grid%iccg_in_num,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11202,&
=======
 CALL wrf_error_fatal3("<stdin>",11251,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iccg_in_num. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iccg_in_den ) ) THEN 
  DEALLOCATE(grid%iccg_in_den,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11209,&
=======
 CALL wrf_error_fatal3("<stdin>",11258,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iccg_in_den. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%varobs ) ) THEN 
  DEALLOCATE(grid%fdob%varobs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11216,&
=======
 CALL wrf_error_fatal3("<stdin>",11265,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%varobs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%errf ) ) THEN 
  DEALLOCATE(grid%fdob%errf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11223,&
=======
 CALL wrf_error_fatal3("<stdin>",11272,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%errf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%timeob ) ) THEN 
  DEALLOCATE(grid%fdob%timeob,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11230,&
=======
 CALL wrf_error_fatal3("<stdin>",11279,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%timeob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%nlevs_ob ) ) THEN 
  DEALLOCATE(grid%fdob%nlevs_ob,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11237,&
=======
 CALL wrf_error_fatal3("<stdin>",11286,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%nlevs_ob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%lev_in_ob ) ) THEN 
  DEALLOCATE(grid%fdob%lev_in_ob,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11244,&
=======
 CALL wrf_error_fatal3("<stdin>",11293,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%lev_in_ob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%plfo ) ) THEN 
  DEALLOCATE(grid%fdob%plfo,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11251,&
=======
 CALL wrf_error_fatal3("<stdin>",11300,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%plfo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%elevob ) ) THEN 
  DEALLOCATE(grid%fdob%elevob,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11258,&
=======
 CALL wrf_error_fatal3("<stdin>",11307,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%elevob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rio ) ) THEN 
  DEALLOCATE(grid%fdob%rio,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11265,&
=======
 CALL wrf_error_fatal3("<stdin>",11314,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%rio. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rjo ) ) THEN 
  DEALLOCATE(grid%fdob%rjo,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11272,&
=======
 CALL wrf_error_fatal3("<stdin>",11321,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%rjo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rko ) ) THEN 
  DEALLOCATE(grid%fdob%rko,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11279,&
=======
 CALL wrf_error_fatal3("<stdin>",11328,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%rko. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%obsprt ) ) THEN 
  DEALLOCATE(grid%fdob%obsprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11286,&
=======
 CALL wrf_error_fatal3("<stdin>",11335,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%obsprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%latprt ) ) THEN 
  DEALLOCATE(grid%fdob%latprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11293,&
=======
 CALL wrf_error_fatal3("<stdin>",11342,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%latprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%lonprt ) ) THEN 
  DEALLOCATE(grid%fdob%lonprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11300,&
=======
 CALL wrf_error_fatal3("<stdin>",11349,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%lonprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%mlatprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlatprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11307,&
=======
 CALL wrf_error_fatal3("<stdin>",11356,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%mlatprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%mlonprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlonprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11314,&
=======
 CALL wrf_error_fatal3("<stdin>",11363,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%mlonprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%stnidprt ) ) THEN 
  DEALLOCATE(grid%fdob%stnidprt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11321,&
=======
 CALL wrf_error_fatal3("<stdin>",11370,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%stnidprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%base_state ) ) THEN 
  DEALLOCATE(grid%fdob%base_state,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11328,&
=======
 CALL wrf_error_fatal3("<stdin>",11377,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdob%base_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_xxx ) ) THEN 
  DEALLOCATE(grid%t_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11335,&
=======
 CALL wrf_error_fatal3("<stdin>",11384,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_xxx ) ) THEN 
  DEALLOCATE(grid%u_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11342,&
=======
 CALL wrf_error_fatal3("<stdin>",11391,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_xxx ) ) THEN 
  DEALLOCATE(grid%ru_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11349,&
=======
 CALL wrf_error_fatal3("<stdin>",11398,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ru_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_xxx ) ) THEN 
  DEALLOCATE(grid%v_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11356,&
=======
 CALL wrf_error_fatal3("<stdin>",11405,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_xxx ) ) THEN 
  DEALLOCATE(grid%rv_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11363,&
=======
 CALL wrf_error_fatal3("<stdin>",11412,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_xxx ) ) THEN 
  DEALLOCATE(grid%w_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11370,&
=======
 CALL wrf_error_fatal3("<stdin>",11419,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ww_xxx ) ) THEN 
  DEALLOCATE(grid%ww_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11377,&
=======
 CALL wrf_error_fatal3("<stdin>",11426,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ww_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_xxx ) ) THEN 
  DEALLOCATE(grid%ph_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11384,&
=======
 CALL wrf_error_fatal3("<stdin>",11433,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ph_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dum_yyy ) ) THEN 
  DEALLOCATE(grid%dum_yyy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11391,&
=======
 CALL wrf_error_fatal3("<stdin>",11440,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dum_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fourd_xxx ) ) THEN 
  DEALLOCATE(grid%fourd_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11398,&
=======
 CALL wrf_error_fatal3("<stdin>",11447,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fourd_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%clat_xxx ) ) THEN 
  DEALLOCATE(grid%clat_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11405,&
=======
 CALL wrf_error_fatal3("<stdin>",11454,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%clat_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_xxx ) ) THEN 
  DEALLOCATE(grid%ht_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11412,&
=======
 CALL wrf_error_fatal3("<stdin>",11461,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ht_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mf_xxx ) ) THEN 
  DEALLOCATE(grid%mf_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11419,&
=======
 CALL wrf_error_fatal3("<stdin>",11468,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mf_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_analysis ) ) THEN 
  DEALLOCATE(grid%dif_analysis,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11426,&
=======
 CALL wrf_error_fatal3("<stdin>",11475,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dif_analysis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_xxx ) ) THEN 
  DEALLOCATE(grid%dif_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11433,&
=======
 CALL wrf_error_fatal3("<stdin>",11482,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dif_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_yyy ) ) THEN 
  DEALLOCATE(grid%dif_yyy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11440,&
=======
 CALL wrf_error_fatal3("<stdin>",11489,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dif_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_hist ) ) THEN 
  DEALLOCATE(grid%lfn_hist,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11447,&
=======
 CALL wrf_error_fatal3("<stdin>",11496,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lfn_hist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_time ) ) THEN 
  DEALLOCATE(grid%lfn_time,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11454,&
=======
 CALL wrf_error_fatal3("<stdin>",11503,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lfn_time. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nfuel_cat ) ) THEN 
  DEALLOCATE(grid%nfuel_cat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11461,&
=======
 CALL wrf_error_fatal3("<stdin>",11510,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nfuel_cat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zsf ) ) THEN 
  DEALLOCATE(grid%zsf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11468,&
=======
 CALL wrf_error_fatal3("<stdin>",11517,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zsf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzdxf ) ) THEN 
  DEALLOCATE(grid%dzdxf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11475,&
=======
 CALL wrf_error_fatal3("<stdin>",11524,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzdxf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzdyf ) ) THEN 
  DEALLOCATE(grid%dzdyf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11482,&
=======
 CALL wrf_error_fatal3("<stdin>",11531,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzdyf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tign_g ) ) THEN 
  DEALLOCATE(grid%tign_g,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11489,&
=======
 CALL wrf_error_fatal3("<stdin>",11538,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tign_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthfrten ) ) THEN 
  DEALLOCATE(grid%rthfrten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11496,&
=======
 CALL wrf_error_fatal3("<stdin>",11545,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rthfrten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvfrten ) ) THEN 
  DEALLOCATE(grid%rqvfrten,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11503,&
=======
 CALL wrf_error_fatal3("<stdin>",11552,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rqvfrten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avg_fuel_frac ) ) THEN 
  DEALLOCATE(grid%avg_fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11510,&
=======
 CALL wrf_error_fatal3("<stdin>",11559,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avg_fuel_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnhfx ) ) THEN 
  DEALLOCATE(grid%grnhfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11517,&
=======
 CALL wrf_error_fatal3("<stdin>",11566,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grnhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnqfx ) ) THEN 
  DEALLOCATE(grid%grnqfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11524,&
=======
 CALL wrf_error_fatal3("<stdin>",11573,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grnqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canhfx ) ) THEN 
  DEALLOCATE(grid%canhfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11531,&
=======
 CALL wrf_error_fatal3("<stdin>",11580,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canqfx ) ) THEN 
  DEALLOCATE(grid%canqfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11538,&
=======
 CALL wrf_error_fatal3("<stdin>",11587,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%canqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uah ) ) THEN 
  DEALLOCATE(grid%uah,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11545,&
=======
 CALL wrf_error_fatal3("<stdin>",11594,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uah. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vah ) ) THEN 
  DEALLOCATE(grid%vah,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11552,&
=======
 CALL wrf_error_fatal3("<stdin>",11601,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vah. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn ) ) THEN 
  DEALLOCATE(grid%lfn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11559,&
=======
 CALL wrf_error_fatal3("<stdin>",11608,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lfn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fuel_frac ) ) THEN 
  DEALLOCATE(grid%fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11566,&
=======
 CALL wrf_error_fatal3("<stdin>",11615,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fuel_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fire_area ) ) THEN 
  DEALLOCATE(grid%fire_area,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11573,&
=======
 CALL wrf_error_fatal3("<stdin>",11622,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fire_area. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uf ) ) THEN 
  DEALLOCATE(grid%uf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11580,&
=======
 CALL wrf_error_fatal3("<stdin>",11629,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vf ) ) THEN 
  DEALLOCATE(grid%vf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11587,&
=======
 CALL wrf_error_fatal3("<stdin>",11636,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgrnhfx ) ) THEN 
  DEALLOCATE(grid%fgrnhfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11594,&
=======
 CALL wrf_error_fatal3("<stdin>",11643,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fgrnhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgrnqfx ) ) THEN 
  DEALLOCATE(grid%fgrnqfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11601,&
=======
 CALL wrf_error_fatal3("<stdin>",11650,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fgrnqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcanhfx ) ) THEN 
  DEALLOCATE(grid%fcanhfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11608,&
=======
 CALL wrf_error_fatal3("<stdin>",11657,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcanhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcanqfx ) ) THEN 
  DEALLOCATE(grid%fcanqfx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11615,&
=======
 CALL wrf_error_fatal3("<stdin>",11664,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fcanqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ros ) ) THEN 
  DEALLOCATE(grid%ros,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11622,&
=======
 CALL wrf_error_fatal3("<stdin>",11671,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ros. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fxlong ) ) THEN 
  DEALLOCATE(grid%fxlong,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11629,&
=======
 CALL wrf_error_fatal3("<stdin>",11678,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fxlong. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fxlat ) ) THEN 
  DEALLOCATE(grid%fxlat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11636,&
=======
 CALL wrf_error_fatal3("<stdin>",11685,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fxlat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fuel_time ) ) THEN 
  DEALLOCATE(grid%fuel_time,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11643,&
=======
 CALL wrf_error_fatal3("<stdin>",11692,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fuel_time. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bbb ) ) THEN 
  DEALLOCATE(grid%bbb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11650,&
=======
 CALL wrf_error_fatal3("<stdin>",11699,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bbb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%betafl ) ) THEN 
  DEALLOCATE(grid%betafl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11657,&
=======
 CALL wrf_error_fatal3("<stdin>",11706,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%betafl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%phiwc ) ) THEN 
  DEALLOCATE(grid%phiwc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11664,&
=======
 CALL wrf_error_fatal3("<stdin>",11713,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%phiwc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%r_0 ) ) THEN 
  DEALLOCATE(grid%r_0,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11671,&
=======
 CALL wrf_error_fatal3("<stdin>",11720,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%r_0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgip ) ) THEN 
  DEALLOCATE(grid%fgip,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11678,&
=======
 CALL wrf_error_fatal3("<stdin>",11727,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fgip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ischap ) ) THEN 
  DEALLOCATE(grid%ischap,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11685,&
=======
 CALL wrf_error_fatal3("<stdin>",11734,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ischap. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_rum ) ) THEN 
  DEALLOCATE(grid%avgflx_rum,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11692,&
=======
 CALL wrf_error_fatal3("<stdin>",11741,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_rum. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_rvm ) ) THEN 
  DEALLOCATE(grid%avgflx_rvm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11699,&
=======
 CALL wrf_error_fatal3("<stdin>",11748,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_rvm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_wwm ) ) THEN 
  DEALLOCATE(grid%avgflx_wwm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11706,&
=======
 CALL wrf_error_fatal3("<stdin>",11755,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_wwm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11713,&
=======
 CALL wrf_error_fatal3("<stdin>",11762,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11720,&
=======
 CALL wrf_error_fatal3("<stdin>",11769,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11727,&
=======
 CALL wrf_error_fatal3("<stdin>",11776,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_efu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11734,&
=======
 CALL wrf_error_fatal3("<stdin>",11783,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_efu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11741,&
=======
 CALL wrf_error_fatal3("<stdin>",11790,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_efd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11748,&
=======
 CALL wrf_error_fatal3("<stdin>",11797,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%avgflx_efd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfu1 ) ) THEN 
  DEALLOCATE(grid%cfu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11755,&
=======
 CALL wrf_error_fatal3("<stdin>",11804,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfd1 ) ) THEN 
  DEALLOCATE(grid%cfd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11762,&
=======
 CALL wrf_error_fatal3("<stdin>",11811,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfu1 ) ) THEN 
  DEALLOCATE(grid%dfu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11769,&
=======
 CALL wrf_error_fatal3("<stdin>",11818,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efu1 ) ) THEN 
  DEALLOCATE(grid%efu1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11776,&
=======
 CALL wrf_error_fatal3("<stdin>",11825,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%efu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfd1 ) ) THEN 
  DEALLOCATE(grid%dfd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11783,&
=======
 CALL wrf_error_fatal3("<stdin>",11832,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efd1 ) ) THEN 
  DEALLOCATE(grid%efd1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11790,&
=======
 CALL wrf_error_fatal3("<stdin>",11839,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%efd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertstrucc ) ) THEN 
  DEALLOCATE(grid%vertstrucc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11797,&
=======
 CALL wrf_error_fatal3("<stdin>",11846,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vertstrucc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertstrucs ) ) THEN 
  DEALLOCATE(grid%vertstrucs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11804,&
=======
 CALL wrf_error_fatal3("<stdin>",11853,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vertstrucs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_sf ) ) THEN 
  DEALLOCATE(grid%field_sf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11811,&
=======
 CALL wrf_error_fatal3("<stdin>",11860,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_sf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_pbl ) ) THEN 
  DEALLOCATE(grid%field_pbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11818,&
=======
 CALL wrf_error_fatal3("<stdin>",11867,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_conv ) ) THEN 
  DEALLOCATE(grid%field_conv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11825,&
=======
 CALL wrf_error_fatal3("<stdin>",11874,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%ru_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11832,&
=======
 CALL wrf_error_fatal3("<stdin>",11881,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ru_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rv_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11839,&
=======
 CALL wrf_error_fatal3("<stdin>",11888,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rv_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rt_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rt_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11846,&
=======
 CALL wrf_error_fatal3("<stdin>",11895,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rt_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_pert ) ) THEN 
  DEALLOCATE(grid%rand_pert,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11853,&
=======
 CALL wrf_error_fatal3("<stdin>",11902,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_pert. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_conv ) ) THEN 
  DEALLOCATE(grid%pattern_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11860,&
=======
 CALL wrf_error_fatal3("<stdin>",11909,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_pbl ) ) THEN 
  DEALLOCATE(grid%pattern_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11867,&
=======
 CALL wrf_error_fatal3("<stdin>",11916,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_lsm ) ) THEN 
  DEALLOCATE(grid%pattern_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11874,&
=======
 CALL wrf_error_fatal3("<stdin>",11923,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_lsm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rstoch ) ) THEN 
  DEALLOCATE(grid%rstoch,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11881,&
=======
 CALL wrf_error_fatal3("<stdin>",11930,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rstoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real ) ) THEN 
  DEALLOCATE(grid%rand_real,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11888,&
=======
 CALL wrf_error_fatal3("<stdin>",11937,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_real. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag ) ) THEN 
  DEALLOCATE(grid%rand_imag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11895,&
=======
 CALL wrf_error_fatal3("<stdin>",11944,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_imag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstreamforcc ) ) THEN 
  DEALLOCATE(grid%spstreamforcc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11902,&
=======
 CALL wrf_error_fatal3("<stdin>",11951,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spstreamforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstreamforcs ) ) THEN 
  DEALLOCATE(grid%spstreamforcs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11909,&
=======
 CALL wrf_error_fatal3("<stdin>",11958,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spstreamforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstream_amp ) ) THEN 
  DEALLOCATE(grid%spstream_amp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11916,&
=======
 CALL wrf_error_fatal3("<stdin>",11965,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spstream_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sptforcc ) ) THEN 
  DEALLOCATE(grid%sptforcc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11923,&
=======
 CALL wrf_error_fatal3("<stdin>",11972,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sptforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sptforcs ) ) THEN 
  DEALLOCATE(grid%sptforcs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11930,&
=======
 CALL wrf_error_fatal3("<stdin>",11979,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sptforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spt_amp ) ) THEN 
  DEALLOCATE(grid%spt_amp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11937,&
=======
 CALL wrf_error_fatal3("<stdin>",11986,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spt_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc ) ) THEN 
  DEALLOCATE(grid%spforcc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11944,&
=======
 CALL wrf_error_fatal3("<stdin>",11993,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs ) ) THEN 
  DEALLOCATE(grid%spforcs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11951,&
=======
 CALL wrf_error_fatal3("<stdin>",12000,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp ) ) THEN 
  DEALLOCATE(grid%sp_amp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11958,&
=======
 CALL wrf_error_fatal3("<stdin>",12007,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sp_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc2 ) ) THEN 
  DEALLOCATE(grid%spforcc2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11965,&
=======
 CALL wrf_error_fatal3("<stdin>",12014,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcc2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs2 ) ) THEN 
  DEALLOCATE(grid%spforcs2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11972,&
=======
 CALL wrf_error_fatal3("<stdin>",12021,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcs2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp2 ) ) THEN 
  DEALLOCATE(grid%sp_amp2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11979,&
=======
 CALL wrf_error_fatal3("<stdin>",12028,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sp_amp2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc3 ) ) THEN 
  DEALLOCATE(grid%spforcc3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11986,&
=======
 CALL wrf_error_fatal3("<stdin>",12035,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcc3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs3 ) ) THEN 
  DEALLOCATE(grid%spforcs3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",11993,&
=======
 CALL wrf_error_fatal3("<stdin>",12042,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcs3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp3 ) ) THEN 
  DEALLOCATE(grid%sp_amp3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12000,&
=======
 CALL wrf_error_fatal3("<stdin>",12049,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sp_amp3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc4 ) ) THEN 
  DEALLOCATE(grid%spforcc4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12007,&
=======
 CALL wrf_error_fatal3("<stdin>",12056,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcc4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs4 ) ) THEN 
  DEALLOCATE(grid%spforcs4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12014,&
=======
 CALL wrf_error_fatal3("<stdin>",12063,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcs4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp4 ) ) THEN 
  DEALLOCATE(grid%sp_amp4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12021,&
=======
 CALL wrf_error_fatal3("<stdin>",12070,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sp_amp4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc5 ) ) THEN 
  DEALLOCATE(grid%spforcc5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12028,&
=======
 CALL wrf_error_fatal3("<stdin>",12077,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcc5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs5 ) ) THEN 
  DEALLOCATE(grid%spforcs5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12035,&
=======
 CALL wrf_error_fatal3("<stdin>",12084,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spforcs5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp5 ) ) THEN 
  DEALLOCATE(grid%sp_amp5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12042,&
=======
 CALL wrf_error_fatal3("<stdin>",12091,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sp_amp5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spptforcc ) ) THEN 
  DEALLOCATE(grid%spptforcc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12049,&
=======
 CALL wrf_error_fatal3("<stdin>",12098,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spptforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spptforcs ) ) THEN 
  DEALLOCATE(grid%spptforcs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12056,&
=======
 CALL wrf_error_fatal3("<stdin>",12105,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%spptforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sppt_amp ) ) THEN 
  DEALLOCATE(grid%sppt_amp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12063,&
=======
 CALL wrf_error_fatal3("<stdin>",12112,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sppt_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertampt ) ) THEN 
  DEALLOCATE(grid%vertampt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12070,&
=======
 CALL wrf_error_fatal3("<stdin>",12119,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vertampt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertampuv ) ) THEN 
  DEALLOCATE(grid%vertampuv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12077,&
=======
 CALL wrf_error_fatal3("<stdin>",12126,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vertampuv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_sppt ) ) THEN 
  DEALLOCATE(grid%iseedarr_sppt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12084,&
=======
 CALL wrf_error_fatal3("<stdin>",12133,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_sppt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_skebs ) ) THEN 
  DEALLOCATE(grid%iseedarr_skebs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12091,&
=======
 CALL wrf_error_fatal3("<stdin>",12140,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_skebs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_rand_pert ) ) THEN 
  DEALLOCATE(grid%iseedarr_rand_pert,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12098,&
=======
 CALL wrf_error_fatal3("<stdin>",12147,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_rand_pert. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_conv ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12105,&
=======
 CALL wrf_error_fatal3("<stdin>",12154,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_pbl ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12112,&
=======
 CALL wrf_error_fatal3("<stdin>",12161,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_lsm ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12119,&
=======
 CALL wrf_error_fatal3("<stdin>",12168,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_lsm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real_xxx ) ) THEN 
  DEALLOCATE(grid%rand_real_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12126,&
=======
 CALL wrf_error_fatal3("<stdin>",12175,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_real_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real_yyy ) ) THEN 
  DEALLOCATE(grid%rand_real_yyy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12133,&
=======
 CALL wrf_error_fatal3("<stdin>",12182,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_real_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag_xxx ) ) THEN 
  DEALLOCATE(grid%rand_imag_xxx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12140,&
=======
 CALL wrf_error_fatal3("<stdin>",12189,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_imag_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag_yyy ) ) THEN 
  DEALLOCATE(grid%rand_imag_yyy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12147,&
=======
 CALL wrf_error_fatal3("<stdin>",12196,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rand_imag_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nba_mij ) ) THEN 
  DEALLOCATE(grid%nba_mij,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12154,&
=======
 CALL wrf_error_fatal3("<stdin>",12203,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nba_mij. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nba_rij ) ) THEN 
  DEALLOCATE(grid%nba_rij,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12161,&
=======
 CALL wrf_error_fatal3("<stdin>",12210,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nba_rij. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tauresx2d ) ) THEN 
  DEALLOCATE(grid%tauresx2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12168,&
=======
 CALL wrf_error_fatal3("<stdin>",12217,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tauresx2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tauresy2d ) ) THEN 
  DEALLOCATE(grid%tauresy2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12175,&
=======
 CALL wrf_error_fatal3("<stdin>",12224,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tauresy2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tpert2d ) ) THEN 
  DEALLOCATE(grid%tpert2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12182,&
=======
 CALL wrf_error_fatal3("<stdin>",12231,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qpert2d ) ) THEN 
  DEALLOCATE(grid%qpert2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12189,&
=======
 CALL wrf_error_fatal3("<stdin>",12238,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wpert2d ) ) THEN 
  DEALLOCATE(grid%wpert2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12196,&
=======
 CALL wrf_error_fatal3("<stdin>",12245,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%turbtype3d ) ) THEN 
  DEALLOCATE(grid%turbtype3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12203,&
=======
 CALL wrf_error_fatal3("<stdin>",12252,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%turbtype3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smaw3d ) ) THEN 
  DEALLOCATE(grid%smaw3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12210,&
=======
 CALL wrf_error_fatal3("<stdin>",12259,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%smaw3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wsedl3d ) ) THEN 
  DEALLOCATE(grid%wsedl3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12217,&
=======
 CALL wrf_error_fatal3("<stdin>",12266,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wsedl3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rliq ) ) THEN 
  DEALLOCATE(grid%rliq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12224,&
=======
 CALL wrf_error_fatal3("<stdin>",12273,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rliq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlf ) ) THEN 
  DEALLOCATE(grid%dlf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12231,&
=======
 CALL wrf_error_fatal3("<stdin>",12280,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dlf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%precz ) ) THEN 
  DEALLOCATE(grid%precz,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12238,&
=======
 CALL wrf_error_fatal3("<stdin>",12287,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%precz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdt ) ) THEN 
  DEALLOCATE(grid%zmdt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12245,&
=======
 CALL wrf_error_fatal3("<stdin>",12294,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdq ) ) THEN 
  DEALLOCATE(grid%zmdq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12252,&
=======
 CALL wrf_error_fatal3("<stdin>",12301,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmdq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdice ) ) THEN 
  DEALLOCATE(grid%zmdice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12259,&
=======
 CALL wrf_error_fatal3("<stdin>",12308,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmdice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdliq ) ) THEN 
  DEALLOCATE(grid%zmdliq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12266,&
=======
 CALL wrf_error_fatal3("<stdin>",12315,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmdliq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evaptzm ) ) THEN 
  DEALLOCATE(grid%evaptzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12273,&
=======
 CALL wrf_error_fatal3("<stdin>",12322,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evaptzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fzsntzm ) ) THEN 
  DEALLOCATE(grid%fzsntzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12280,&
=======
 CALL wrf_error_fatal3("<stdin>",12329,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fzsntzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evsntzm ) ) THEN 
  DEALLOCATE(grid%evsntzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12287,&
=======
 CALL wrf_error_fatal3("<stdin>",12336,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evsntzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapqzm ) ) THEN 
  DEALLOCATE(grid%evapqzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12294,&
=======
 CALL wrf_error_fatal3("<stdin>",12343,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evapqzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmflxprc ) ) THEN 
  DEALLOCATE(grid%zmflxprc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12301,&
=======
 CALL wrf_error_fatal3("<stdin>",12350,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmflxprc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmflxsnw ) ) THEN 
  DEALLOCATE(grid%zmflxsnw,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12308,&
=======
 CALL wrf_error_fatal3("<stdin>",12357,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmflxsnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmntprpd ) ) THEN 
  DEALLOCATE(grid%zmntprpd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12315,&
=======
 CALL wrf_error_fatal3("<stdin>",12364,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmntprpd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmntsnpd ) ) THEN 
  DEALLOCATE(grid%zmntsnpd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12322,&
=======
 CALL wrf_error_fatal3("<stdin>",12371,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmntsnpd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmeiheat ) ) THEN 
  DEALLOCATE(grid%zmeiheat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12329,&
=======
 CALL wrf_error_fatal3("<stdin>",12378,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmeiheat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmcdzm ) ) THEN 
  DEALLOCATE(grid%cmfmcdzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12336,&
=======
 CALL wrf_error_fatal3("<stdin>",12385,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmfmcdzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%preccdzm ) ) THEN 
  DEALLOCATE(grid%preccdzm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12343,&
=======
 CALL wrf_error_fatal3("<stdin>",12392,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%preccdzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pconvb ) ) THEN 
  DEALLOCATE(grid%pconvb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12350,&
=======
 CALL wrf_error_fatal3("<stdin>",12399,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pconvb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pconvt ) ) THEN 
  DEALLOCATE(grid%pconvt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12357,&
=======
 CALL wrf_error_fatal3("<stdin>",12406,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pconvt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cape ) ) THEN 
  DEALLOCATE(grid%cape,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12364,&
=======
 CALL wrf_error_fatal3("<stdin>",12413,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cape. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmtu ) ) THEN 
  DEALLOCATE(grid%zmmtu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12371,&
=======
 CALL wrf_error_fatal3("<stdin>",12420,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmmtu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmtv ) ) THEN 
  DEALLOCATE(grid%zmmtv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12378,&
=======
 CALL wrf_error_fatal3("<stdin>",12427,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmmtv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmu ) ) THEN 
  DEALLOCATE(grid%zmmu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12385,&
=======
 CALL wrf_error_fatal3("<stdin>",12434,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmmu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmd ) ) THEN 
  DEALLOCATE(grid%zmmd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12392,&
=======
 CALL wrf_error_fatal3("<stdin>",12441,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmmd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmupgu ) ) THEN 
  DEALLOCATE(grid%zmupgu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12399,&
=======
 CALL wrf_error_fatal3("<stdin>",12448,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmupgu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmupgd ) ) THEN 
  DEALLOCATE(grid%zmupgd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12406,&
=======
 CALL wrf_error_fatal3("<stdin>",12455,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmupgd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmvpgu ) ) THEN 
  DEALLOCATE(grid%zmvpgu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12413,&
=======
 CALL wrf_error_fatal3("<stdin>",12462,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmvpgu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmvpgd ) ) THEN 
  DEALLOCATE(grid%zmvpgd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12420,&
=======
 CALL wrf_error_fatal3("<stdin>",12469,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmvpgd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicuu ) ) THEN 
  DEALLOCATE(grid%zmicuu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12427,&
=======
 CALL wrf_error_fatal3("<stdin>",12476,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmicuu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicud ) ) THEN 
  DEALLOCATE(grid%zmicud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12434,&
=======
 CALL wrf_error_fatal3("<stdin>",12483,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmicud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicvu ) ) THEN 
  DEALLOCATE(grid%zmicvu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12441,&
=======
 CALL wrf_error_fatal3("<stdin>",12490,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmicvu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicvd ) ) THEN 
  DEALLOCATE(grid%zmicvd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12448,&
=======
 CALL wrf_error_fatal3("<stdin>",12497,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zmicvd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapcdp3d ) ) THEN 
  DEALLOCATE(grid%evapcdp3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12455,&
=======
 CALL wrf_error_fatal3("<stdin>",12504,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evapcdp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icwmrdp3d ) ) THEN 
  DEALLOCATE(grid%icwmrdp3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12462,&
=======
 CALL wrf_error_fatal3("<stdin>",12511,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icwmrdp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rprddp3d ) ) THEN 
  DEALLOCATE(grid%rprddp3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12469,&
=======
 CALL wrf_error_fatal3("<stdin>",12518,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rprddp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dp3d ) ) THEN 
  DEALLOCATE(grid%dp3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12476,&
=======
 CALL wrf_error_fatal3("<stdin>",12525,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%du3d ) ) THEN 
  DEALLOCATE(grid%du3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12483,&
=======
 CALL wrf_error_fatal3("<stdin>",12532,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%du3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ed3d ) ) THEN 
  DEALLOCATE(grid%ed3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12490,&
=======
 CALL wrf_error_fatal3("<stdin>",12539,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ed3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eu3d ) ) THEN 
  DEALLOCATE(grid%eu3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12497,&
=======
 CALL wrf_error_fatal3("<stdin>",12546,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%eu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%md3d ) ) THEN 
  DEALLOCATE(grid%md3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12504,&
=======
 CALL wrf_error_fatal3("<stdin>",12553,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%md3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu3d ) ) THEN 
  DEALLOCATE(grid%mu3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12511,&
=======
 CALL wrf_error_fatal3("<stdin>",12560,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%mu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dsubcld2d ) ) THEN 
  DEALLOCATE(grid%dsubcld2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12518,&
=======
 CALL wrf_error_fatal3("<stdin>",12567,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dsubcld2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ideep2d ) ) THEN 
  DEALLOCATE(grid%ideep2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12525,&
=======
 CALL wrf_error_fatal3("<stdin>",12574,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ideep2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%jt2d ) ) THEN 
  DEALLOCATE(grid%jt2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12532,&
=======
 CALL wrf_error_fatal3("<stdin>",12581,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%jt2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%maxg2d ) ) THEN 
  DEALLOCATE(grid%maxg2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12539,&
=======
 CALL wrf_error_fatal3("<stdin>",12588,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%maxg2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lengath2d ) ) THEN 
  DEALLOCATE(grid%lengath2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12546,&
=======
 CALL wrf_error_fatal3("<stdin>",12595,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lengath2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfsl ) ) THEN 
  DEALLOCATE(grid%cmfsl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12553,&
=======
 CALL wrf_error_fatal3("<stdin>",12602,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmfsl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmflq ) ) THEN 
  DEALLOCATE(grid%cmflq,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12560,&
=======
 CALL wrf_error_fatal3("<stdin>",12609,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmflq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmc ) ) THEN 
  DEALLOCATE(grid%cmfmc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12567,&
=======
 CALL wrf_error_fatal3("<stdin>",12616,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmfmc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmc2 ) ) THEN 
  DEALLOCATE(grid%cmfmc2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12574,&
=======
 CALL wrf_error_fatal3("<stdin>",12623,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cmfmc2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfrash ) ) THEN 
  DEALLOCATE(grid%cldfrash,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12581,&
=======
 CALL wrf_error_fatal3("<stdin>",12630,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfrash. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cush ) ) THEN 
  DEALLOCATE(grid%cush,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12588,&
=======
 CALL wrf_error_fatal3("<stdin>",12637,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cush. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapcsh ) ) THEN 
  DEALLOCATE(grid%evapcsh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12595,&
=======
 CALL wrf_error_fatal3("<stdin>",12644,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%evapcsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icwmrsh ) ) THEN 
  DEALLOCATE(grid%icwmrsh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12602,&
=======
 CALL wrf_error_fatal3("<stdin>",12651,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icwmrsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowsh ) ) THEN 
  DEALLOCATE(grid%snowsh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12609,&
=======
 CALL wrf_error_fatal3("<stdin>",12658,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rprdsh ) ) THEN 
  DEALLOCATE(grid%rprdsh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12616,&
=======
 CALL wrf_error_fatal3("<stdin>",12665,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rprdsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rliq2 ) ) THEN 
  DEALLOCATE(grid%rliq2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12623,&
=======
 CALL wrf_error_fatal3("<stdin>",12672,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rliq2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlf2 ) ) THEN 
  DEALLOCATE(grid%dlf2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12630,&
=======
 CALL wrf_error_fatal3("<stdin>",12679,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dlf2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shfrc3d ) ) THEN 
  DEALLOCATE(grid%shfrc3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12637,&
=======
 CALL wrf_error_fatal3("<stdin>",12686,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%shfrc3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtflx_cu ) ) THEN 
  DEALLOCATE(grid%qtflx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12644,&
=======
 CALL wrf_error_fatal3("<stdin>",12693,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slflx_cu ) ) THEN 
  DEALLOCATE(grid%slflx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12651,&
=======
 CALL wrf_error_fatal3("<stdin>",12700,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uflx_cu ) ) THEN 
  DEALLOCATE(grid%uflx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12658,&
=======
 CALL wrf_error_fatal3("<stdin>",12707,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vflx_cu ) ) THEN 
  DEALLOCATE(grid%vflx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12665,&
=======
 CALL wrf_error_fatal3("<stdin>",12714,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtten_cu ) ) THEN 
  DEALLOCATE(grid%qtten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12672,&
=======
 CALL wrf_error_fatal3("<stdin>",12721,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slten_cu ) ) THEN 
  DEALLOCATE(grid%slten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12679,&
=======
 CALL wrf_error_fatal3("<stdin>",12728,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%slten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uten_cu ) ) THEN 
  DEALLOCATE(grid%uten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12686,&
=======
 CALL wrf_error_fatal3("<stdin>",12735,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vten_cu ) ) THEN 
  DEALLOCATE(grid%vten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12693,&
=======
 CALL wrf_error_fatal3("<stdin>",12742,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qvten_cu ) ) THEN 
  DEALLOCATE(grid%qvten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12700,&
=======
 CALL wrf_error_fatal3("<stdin>",12749,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qvten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlten_cu ) ) THEN 
  DEALLOCATE(grid%qlten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12707,&
=======
 CALL wrf_error_fatal3("<stdin>",12756,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qlten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qiten_cu ) ) THEN 
  DEALLOCATE(grid%qiten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12714,&
=======
 CALL wrf_error_fatal3("<stdin>",12763,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qiten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cbmf_cu ) ) THEN 
  DEALLOCATE(grid%cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12721,&
=======
 CALL wrf_error_fatal3("<stdin>",12770,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cbmf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrcinvbase_cu ) ) THEN 
  DEALLOCATE(grid%ufrcinvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12728,&
=======
 CALL wrf_error_fatal3("<stdin>",12777,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ufrcinvbase_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrclcl_cu ) ) THEN 
  DEALLOCATE(grid%ufrclcl_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12735,&
=======
 CALL wrf_error_fatal3("<stdin>",12784,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ufrclcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%winvbase_cu ) ) THEN 
  DEALLOCATE(grid%winvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12742,&
=======
 CALL wrf_error_fatal3("<stdin>",12791,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%winvbase_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wlcl_cu ) ) THEN 
  DEALLOCATE(grid%wlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12749,&
=======
 CALL wrf_error_fatal3("<stdin>",12798,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%plcl_cu ) ) THEN 
  DEALLOCATE(grid%plcl_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12756,&
=======
 CALL wrf_error_fatal3("<stdin>",12805,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%plcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pinv_cu ) ) THEN 
  DEALLOCATE(grid%pinv_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12763,&
=======
 CALL wrf_error_fatal3("<stdin>",12812,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pinv_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%plfc_cu ) ) THEN 
  DEALLOCATE(grid%plfc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12770,&
=======
 CALL wrf_error_fatal3("<stdin>",12819,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%plfc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pbup_cu ) ) THEN 
  DEALLOCATE(grid%pbup_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12777,&
=======
 CALL wrf_error_fatal3("<stdin>",12826,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pbup_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ppen_cu ) ) THEN 
  DEALLOCATE(grid%ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12784,&
=======
 CALL wrf_error_fatal3("<stdin>",12833,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ppen_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtsrc_cu ) ) THEN 
  DEALLOCATE(grid%qtsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12791,&
=======
 CALL wrf_error_fatal3("<stdin>",12840,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12798,&
=======
 CALL wrf_error_fatal3("<stdin>",12847,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thlsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thvlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thvlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12805,&
=======
 CALL wrf_error_fatal3("<stdin>",12854,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thvlsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emkfbup_cu ) ) THEN 
  DEALLOCATE(grid%emkfbup_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12812,&
=======
 CALL wrf_error_fatal3("<stdin>",12861,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%emkfbup_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cin_cu ) ) THEN 
  DEALLOCATE(grid%cin_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12819,&
=======
 CALL wrf_error_fatal3("<stdin>",12868,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12826,&
=======
 CALL wrf_error_fatal3("<stdin>",12875,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cinlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cbmflimit_cu ) ) THEN 
  DEALLOCATE(grid%cbmflimit_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12833,&
=======
 CALL wrf_error_fatal3("<stdin>",12882,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cbmflimit_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkeavg_cu ) ) THEN 
  DEALLOCATE(grid%tkeavg_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12840,&
=======
 CALL wrf_error_fatal3("<stdin>",12889,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tkeavg_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zinv_cu ) ) THEN 
  DEALLOCATE(grid%zinv_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12847,&
=======
 CALL wrf_error_fatal3("<stdin>",12896,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zinv_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rcwp_cu ) ) THEN 
  DEALLOCATE(grid%rcwp_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12854,&
=======
 CALL wrf_error_fatal3("<stdin>",12903,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rcwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rlwp_cu ) ) THEN 
  DEALLOCATE(grid%rlwp_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12861,&
=======
 CALL wrf_error_fatal3("<stdin>",12910,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rlwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%riwp_cu ) ) THEN 
  DEALLOCATE(grid%riwp_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12868,&
=======
 CALL wrf_error_fatal3("<stdin>",12917,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%riwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tophgt_cu ) ) THEN 
  DEALLOCATE(grid%tophgt_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12875,&
=======
 CALL wrf_error_fatal3("<stdin>",12924,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tophgt_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wu_cu ) ) THEN 
  DEALLOCATE(grid%wu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12882,&
=======
 CALL wrf_error_fatal3("<stdin>",12931,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrc_cu ) ) THEN 
  DEALLOCATE(grid%ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12889,&
=======
 CALL wrf_error_fatal3("<stdin>",12938,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtu_cu ) ) THEN 
  DEALLOCATE(grid%qtu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12896,&
=======
 CALL wrf_error_fatal3("<stdin>",12945,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlu_cu ) ) THEN 
  DEALLOCATE(grid%thlu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12903,&
=======
 CALL wrf_error_fatal3("<stdin>",12952,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thlu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thvu_cu ) ) THEN 
  DEALLOCATE(grid%thvu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12910,&
=======
 CALL wrf_error_fatal3("<stdin>",12959,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thvu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uu_cu ) ) THEN 
  DEALLOCATE(grid%uu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12917,&
=======
 CALL wrf_error_fatal3("<stdin>",12966,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vu_cu ) ) THEN 
  DEALLOCATE(grid%vu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12924,&
=======
 CALL wrf_error_fatal3("<stdin>",12973,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtu_emf_cu ) ) THEN 
  DEALLOCATE(grid%qtu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12931,&
=======
 CALL wrf_error_fatal3("<stdin>",12980,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qtu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlu_emf_cu ) ) THEN 
  DEALLOCATE(grid%thlu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12938,&
=======
 CALL wrf_error_fatal3("<stdin>",12987,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%thlu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uu_emf_cu ) ) THEN 
  DEALLOCATE(grid%uu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12945,&
=======
 CALL wrf_error_fatal3("<stdin>",12994,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vu_emf_cu ) ) THEN 
  DEALLOCATE(grid%vu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12952,&
=======
 CALL wrf_error_fatal3("<stdin>",13001,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%umf_cu ) ) THEN 
  DEALLOCATE(grid%umf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12959,&
=======
 CALL wrf_error_fatal3("<stdin>",13008,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%umf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uemf_cu ) ) THEN 
  DEALLOCATE(grid%uemf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12966,&
=======
 CALL wrf_error_fatal3("<stdin>",13015,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%uemf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qcu_cu ) ) THEN 
  DEALLOCATE(grid%qcu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12973,&
=======
 CALL wrf_error_fatal3("<stdin>",13022,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlu_cu ) ) THEN 
  DEALLOCATE(grid%qlu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12980,&
=======
 CALL wrf_error_fatal3("<stdin>",13029,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qlu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qiu_cu ) ) THEN 
  DEALLOCATE(grid%qiu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12987,&
=======
 CALL wrf_error_fatal3("<stdin>",13036,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qiu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cufrc_cu ) ) THEN 
  DEALLOCATE(grid%cufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",12994,&
=======
 CALL wrf_error_fatal3("<stdin>",13043,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fer_cu ) ) THEN 
  DEALLOCATE(grid%fer_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13001,&
=======
 CALL wrf_error_fatal3("<stdin>",13050,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fer_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdr_cu ) ) THEN 
  DEALLOCATE(grid%fdr_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13008,&
=======
 CALL wrf_error_fatal3("<stdin>",13057,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fdr_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dwten_cu ) ) THEN 
  DEALLOCATE(grid%dwten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13015,&
=======
 CALL wrf_error_fatal3("<stdin>",13064,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dwten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%diten_cu ) ) THEN 
  DEALLOCATE(grid%diten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13022,&
=======
 CALL wrf_error_fatal3("<stdin>",13071,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%diten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrten_cu ) ) THEN 
  DEALLOCATE(grid%qrten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13029,&
=======
 CALL wrf_error_fatal3("<stdin>",13078,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qrten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsten_cu ) ) THEN 
  DEALLOCATE(grid%qsten_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13036,&
=======
 CALL wrf_error_fatal3("<stdin>",13085,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qsten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxrain_cu ) ) THEN 
  DEALLOCATE(grid%flxrain_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13043,&
=======
 CALL wrf_error_fatal3("<stdin>",13092,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flxrain_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxsnow_cu ) ) THEN 
  DEALLOCATE(grid%flxsnow_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13050,&
=======
 CALL wrf_error_fatal3("<stdin>",13099,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flxsnow_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ntraprd_cu ) ) THEN 
  DEALLOCATE(grid%ntraprd_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13057,&
=======
 CALL wrf_error_fatal3("<stdin>",13106,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ntraprd_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ntsnprd_cu ) ) THEN 
  DEALLOCATE(grid%ntsnprd_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13064,&
=======
 CALL wrf_error_fatal3("<stdin>",13113,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ntsnprd_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%excessu_cu ) ) THEN 
  DEALLOCATE(grid%excessu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13071,&
=======
 CALL wrf_error_fatal3("<stdin>",13120,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%excessu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%excessu0_cu ) ) THEN 
  DEALLOCATE(grid%excessu0_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13078,&
=======
 CALL wrf_error_fatal3("<stdin>",13127,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%excessu0_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xc_cu ) ) THEN 
  DEALLOCATE(grid%xc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13085,&
=======
 CALL wrf_error_fatal3("<stdin>",13134,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%xc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aquad_cu ) ) THEN 
  DEALLOCATE(grid%aquad_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13092,&
=======
 CALL wrf_error_fatal3("<stdin>",13141,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%aquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bquad_cu ) ) THEN 
  DEALLOCATE(grid%bquad_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13099,&
=======
 CALL wrf_error_fatal3("<stdin>",13148,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cquad_cu ) ) THEN 
  DEALLOCATE(grid%cquad_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13106,&
=======
 CALL wrf_error_fatal3("<stdin>",13155,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bogbot_cu ) ) THEN 
  DEALLOCATE(grid%bogbot_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13113,&
=======
 CALL wrf_error_fatal3("<stdin>",13162,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bogbot_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bogtop_cu ) ) THEN 
  DEALLOCATE(grid%bogtop_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13120,&
=======
 CALL wrf_error_fatal3("<stdin>",13169,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bogtop_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_uwcu_cu ) ) THEN 
  DEALLOCATE(grid%exit_uwcu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13127,&
=======
 CALL wrf_error_fatal3("<stdin>",13176,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_uwcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_conden_cu ) ) THEN 
  DEALLOCATE(grid%exit_conden_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13134,&
=======
 CALL wrf_error_fatal3("<stdin>",13183,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_conden_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_klclmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klclmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13141,&
=======
 CALL wrf_error_fatal3("<stdin>",13190,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_klclmkx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_klfcmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klfcmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13148,&
=======
 CALL wrf_error_fatal3("<stdin>",13197,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_klfcmkx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%exit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13155,&
=======
 CALL wrf_error_fatal3("<stdin>",13204,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_wtw_cu ) ) THEN 
  DEALLOCATE(grid%exit_wtw_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13162,&
=======
 CALL wrf_error_fatal3("<stdin>",13211,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_wtw_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_drycore_cu ) ) THEN 
  DEALLOCATE(grid%exit_drycore_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13169,&
=======
 CALL wrf_error_fatal3("<stdin>",13218,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_drycore_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_wu_cu ) ) THEN 
  DEALLOCATE(grid%exit_wu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13176,&
=======
 CALL wrf_error_fatal3("<stdin>",13225,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_wu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_cufliter_cu ) ) THEN 
  DEALLOCATE(grid%exit_cufliter_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13183,&
=======
 CALL wrf_error_fatal3("<stdin>",13232,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_cufliter_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_kinv1_cu ) ) THEN 
  DEALLOCATE(grid%exit_kinv1_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13190,&
=======
 CALL wrf_error_fatal3("<stdin>",13239,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_kinv1_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_rei_cu ) ) THEN 
  DEALLOCATE(grid%exit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13197,&
=======
 CALL wrf_error_fatal3("<stdin>",13246,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%exit_rei_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_shcu_cu ) ) THEN 
  DEALLOCATE(grid%limit_shcu_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13204,&
=======
 CALL wrf_error_fatal3("<stdin>",13253,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_shcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_negcon_cu ) ) THEN 
  DEALLOCATE(grid%limit_negcon_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13211,&
=======
 CALL wrf_error_fatal3("<stdin>",13260,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_negcon_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%limit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13218,&
=======
 CALL wrf_error_fatal3("<stdin>",13267,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_ppen_cu ) ) THEN 
  DEALLOCATE(grid%limit_ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13225,&
=======
 CALL wrf_error_fatal3("<stdin>",13274,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_ppen_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_emf_cu ) ) THEN 
  DEALLOCATE(grid%limit_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13232,&
=======
 CALL wrf_error_fatal3("<stdin>",13281,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%limit_cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13239,&
=======
 CALL wrf_error_fatal3("<stdin>",13288,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_cinlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cin_cu ) ) THEN 
  DEALLOCATE(grid%limit_cin_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13246,&
=======
 CALL wrf_error_fatal3("<stdin>",13295,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_cin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cbmf_cu ) ) THEN 
  DEALLOCATE(grid%limit_cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13253,&
=======
 CALL wrf_error_fatal3("<stdin>",13302,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_cbmf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_rei_cu ) ) THEN 
  DEALLOCATE(grid%limit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13260,&
=======
 CALL wrf_error_fatal3("<stdin>",13309,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%limit_rei_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ind_delcin_cu ) ) THEN 
  DEALLOCATE(grid%ind_delcin_cu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13267,&
=======
 CALL wrf_error_fatal3("<stdin>",13316,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ind_delcin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_old_mp ) ) THEN 
  DEALLOCATE(grid%rh_old_mp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13274,&
=======
 CALL wrf_error_fatal3("<stdin>",13323,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lcd_old_mp ) ) THEN 
  DEALLOCATE(grid%lcd_old_mp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13281,&
=======
 CALL wrf_error_fatal3("<stdin>",13330,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lcd_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_old_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_old_mp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13288,&
=======
 CALL wrf_error_fatal3("<stdin>",13337,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_mp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13295,&
=======
 CALL wrf_error_fatal3("<stdin>",13344,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp_all ) ) THEN 
  DEALLOCATE(grid%cldfra_mp_all,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13302,&
=======
 CALL wrf_error_fatal3("<stdin>",13351,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp_all. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iradius ) ) THEN 
  DEALLOCATE(grid%iradius,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13309,&
=======
 CALL wrf_error_fatal3("<stdin>",13358,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%iradius. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lradius ) ) THEN 
  DEALLOCATE(grid%lradius,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13316,&
=======
 CALL wrf_error_fatal3("<stdin>",13365,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lradius. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_conv ) ) THEN 
  DEALLOCATE(grid%cldfra_conv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13323,&
=======
 CALL wrf_error_fatal3("<stdin>",13372,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfra_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfrai ) ) THEN 
  DEALLOCATE(grid%cldfrai,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13330,&
=======
 CALL wrf_error_fatal3("<stdin>",13379,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfrai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfral ) ) THEN 
  DEALLOCATE(grid%cldfral,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13337,&
=======
 CALL wrf_error_fatal3("<stdin>",13386,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cldfral. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%numc ) ) THEN 
  DEALLOCATE(grid%numc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13344,&
=======
 CALL wrf_error_fatal3("<stdin>",13393,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%numc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nump ) ) THEN 
  DEALLOCATE(grid%nump,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13351,&
=======
 CALL wrf_error_fatal3("<stdin>",13400,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nump. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabv ) ) THEN 
  DEALLOCATE(grid%sabv,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13358,&
=======
 CALL wrf_error_fatal3("<stdin>",13407,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sabv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabg ) ) THEN 
  DEALLOCATE(grid%sabg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13365,&
=======
 CALL wrf_error_fatal3("<stdin>",13414,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sabg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwup ) ) THEN 
  DEALLOCATE(grid%lwup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13372,&
=======
 CALL wrf_error_fatal3("<stdin>",13421,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhsoi ) ) THEN 
  DEALLOCATE(grid%lhsoi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13379,&
=======
 CALL wrf_error_fatal3("<stdin>",13428,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lhsoi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhveg ) ) THEN 
  DEALLOCATE(grid%lhveg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13386,&
=======
 CALL wrf_error_fatal3("<stdin>",13435,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lhveg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhtran ) ) THEN 
  DEALLOCATE(grid%lhtran,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13393,&
=======
 CALL wrf_error_fatal3("<stdin>",13442,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lhtran. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snl ) ) THEN 
  DEALLOCATE(grid%snl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13400,&
=======
 CALL wrf_error_fatal3("<stdin>",13449,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdp ) ) THEN 
  DEALLOCATE(grid%snowdp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13407,&
=======
 CALL wrf_error_fatal3("<stdin>",13456,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtc ) ) THEN 
  DEALLOCATE(grid%wtc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13414,&
=======
 CALL wrf_error_fatal3("<stdin>",13463,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wtc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtp ) ) THEN 
  DEALLOCATE(grid%wtp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13421,&
=======
 CALL wrf_error_fatal3("<stdin>",13470,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wtp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osno ) ) THEN 
  DEALLOCATE(grid%h2osno,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13428,&
=======
 CALL wrf_error_fatal3("<stdin>",13477,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osno. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_grnd ) ) THEN 
  DEALLOCATE(grid%t_grnd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13435,&
=======
 CALL wrf_error_fatal3("<stdin>",13484,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_grnd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_veg ) ) THEN 
  DEALLOCATE(grid%t_veg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13442,&
=======
 CALL wrf_error_fatal3("<stdin>",13491,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_veg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2ocan ) ) THEN 
  DEALLOCATE(grid%h2ocan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13449,&
=======
 CALL wrf_error_fatal3("<stdin>",13498,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2ocan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2ocan_col ) ) THEN 
  DEALLOCATE(grid%h2ocan_col,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13456,&
=======
 CALL wrf_error_fatal3("<stdin>",13505,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2ocan_col. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2m_max ) ) THEN 
  DEALLOCATE(grid%t2m_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13463,&
=======
 CALL wrf_error_fatal3("<stdin>",13512,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2m_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2m_min ) ) THEN 
  DEALLOCATE(grid%t2m_min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13470,&
=======
 CALL wrf_error_fatal3("<stdin>",13519,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2m_min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2clm ) ) THEN 
  DEALLOCATE(grid%t2clm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13477,&
=======
 CALL wrf_error_fatal3("<stdin>",13526,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2clm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_ref2m ) ) THEN 
  DEALLOCATE(grid%t_ref2m,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13484,&
=======
 CALL wrf_error_fatal3("<stdin>",13533,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_ref2m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13491,&
=======
 CALL wrf_error_fatal3("<stdin>",13540,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13498,&
=======
 CALL wrf_error_fatal3("<stdin>",13547,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13505,&
=======
 CALL wrf_error_fatal3("<stdin>",13554,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13512,&
=======
 CALL wrf_error_fatal3("<stdin>",13561,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13519,&
=======
 CALL wrf_error_fatal3("<stdin>",13568,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13526,&
=======
 CALL wrf_error_fatal3("<stdin>",13575,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13533,&
=======
 CALL wrf_error_fatal3("<stdin>",13582,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13540,&
=======
 CALL wrf_error_fatal3("<stdin>",13589,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13547,&
=======
 CALL wrf_error_fatal3("<stdin>",13596,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13554,&
=======
 CALL wrf_error_fatal3("<stdin>",13603,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq6,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13561,&
=======
 CALL wrf_error_fatal3("<stdin>",13610,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq7,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13568,&
=======
 CALL wrf_error_fatal3("<stdin>",13617,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq8,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13575,&
=======
 CALL wrf_error_fatal3("<stdin>",13624,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq9,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13582,&
=======
 CALL wrf_error_fatal3("<stdin>",13631,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13589,&
=======
 CALL wrf_error_fatal3("<stdin>",13638,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13596,&
=======
 CALL wrf_error_fatal3("<stdin>",13645,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13603,&
=======
 CALL wrf_error_fatal3("<stdin>",13652,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13610,&
=======
 CALL wrf_error_fatal3("<stdin>",13659,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13617,&
=======
 CALL wrf_error_fatal3("<stdin>",13666,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13624,&
=======
 CALL wrf_error_fatal3("<stdin>",13673,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13631,&
=======
 CALL wrf_error_fatal3("<stdin>",13680,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13638,&
=======
 CALL wrf_error_fatal3("<stdin>",13687,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13645,&
=======
 CALL wrf_error_fatal3("<stdin>",13694,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13652,&
=======
 CALL wrf_error_fatal3("<stdin>",13701,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13659,&
=======
 CALL wrf_error_fatal3("<stdin>",13708,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice6,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13666,&
=======
 CALL wrf_error_fatal3("<stdin>",13715,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice7,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13673,&
=======
 CALL wrf_error_fatal3("<stdin>",13722,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice8,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13680,&
=======
 CALL wrf_error_fatal3("<stdin>",13729,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice9,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13687,&
=======
 CALL wrf_error_fatal3("<stdin>",13736,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13694,&
=======
 CALL wrf_error_fatal3("<stdin>",13743,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s1 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13701,&
=======
 CALL wrf_error_fatal3("<stdin>",13750,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s2 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13708,&
=======
 CALL wrf_error_fatal3("<stdin>",13757,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s3 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13715,&
=======
 CALL wrf_error_fatal3("<stdin>",13764,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s4 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13722,&
=======
 CALL wrf_error_fatal3("<stdin>",13771,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s5 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13729,&
=======
 CALL wrf_error_fatal3("<stdin>",13778,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno1 ) ) THEN 
  DEALLOCATE(grid%t_soisno1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13736,&
=======
 CALL wrf_error_fatal3("<stdin>",13785,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno2 ) ) THEN 
  DEALLOCATE(grid%t_soisno2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13743,&
=======
 CALL wrf_error_fatal3("<stdin>",13792,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno3 ) ) THEN 
  DEALLOCATE(grid%t_soisno3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13750,&
=======
 CALL wrf_error_fatal3("<stdin>",13799,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno4 ) ) THEN 
  DEALLOCATE(grid%t_soisno4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13757,&
=======
 CALL wrf_error_fatal3("<stdin>",13806,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno5 ) ) THEN 
  DEALLOCATE(grid%t_soisno5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13764,&
=======
 CALL wrf_error_fatal3("<stdin>",13813,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno6 ) ) THEN 
  DEALLOCATE(grid%t_soisno6,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13771,&
=======
 CALL wrf_error_fatal3("<stdin>",13820,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno7 ) ) THEN 
  DEALLOCATE(grid%t_soisno7,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13778,&
=======
 CALL wrf_error_fatal3("<stdin>",13827,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno8 ) ) THEN 
  DEALLOCATE(grid%t_soisno8,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13785,&
=======
 CALL wrf_error_fatal3("<stdin>",13834,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno9 ) ) THEN 
  DEALLOCATE(grid%t_soisno9,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13792,&
=======
 CALL wrf_error_fatal3("<stdin>",13841,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno10 ) ) THEN 
  DEALLOCATE(grid%t_soisno10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13799,&
=======
 CALL wrf_error_fatal3("<stdin>",13848,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow1 ) ) THEN 
  DEALLOCATE(grid%dzsnow1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13806,&
=======
 CALL wrf_error_fatal3("<stdin>",13855,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzsnow1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow2 ) ) THEN 
  DEALLOCATE(grid%dzsnow2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13813,&
=======
 CALL wrf_error_fatal3("<stdin>",13862,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzsnow2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow3 ) ) THEN 
  DEALLOCATE(grid%dzsnow3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13820,&
=======
 CALL wrf_error_fatal3("<stdin>",13869,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzsnow3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow4 ) ) THEN 
  DEALLOCATE(grid%dzsnow4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13827,&
=======
 CALL wrf_error_fatal3("<stdin>",13876,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzsnow4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow5 ) ) THEN 
  DEALLOCATE(grid%dzsnow5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13834,&
=======
 CALL wrf_error_fatal3("<stdin>",13883,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzsnow5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds1 ) ) THEN 
  DEALLOCATE(grid%snowrds1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13841,&
=======
 CALL wrf_error_fatal3("<stdin>",13890,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowrds1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds2 ) ) THEN 
  DEALLOCATE(grid%snowrds2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13848,&
=======
 CALL wrf_error_fatal3("<stdin>",13897,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowrds2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds3 ) ) THEN 
  DEALLOCATE(grid%snowrds3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13855,&
=======
 CALL wrf_error_fatal3("<stdin>",13904,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowrds3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds4 ) ) THEN 
  DEALLOCATE(grid%snowrds4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13862,&
=======
 CALL wrf_error_fatal3("<stdin>",13911,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowrds4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds5 ) ) THEN 
  DEALLOCATE(grid%snowrds5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13869,&
=======
 CALL wrf_error_fatal3("<stdin>",13918,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowrds5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake1 ) ) THEN 
  DEALLOCATE(grid%t_lake1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13876,&
=======
 CALL wrf_error_fatal3("<stdin>",13925,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake2 ) ) THEN 
  DEALLOCATE(grid%t_lake2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13883,&
=======
 CALL wrf_error_fatal3("<stdin>",13932,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake3 ) ) THEN 
  DEALLOCATE(grid%t_lake3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13890,&
=======
 CALL wrf_error_fatal3("<stdin>",13939,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake4 ) ) THEN 
  DEALLOCATE(grid%t_lake4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13897,&
=======
 CALL wrf_error_fatal3("<stdin>",13946,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake5 ) ) THEN 
  DEALLOCATE(grid%t_lake5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13904,&
=======
 CALL wrf_error_fatal3("<stdin>",13953,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake6 ) ) THEN 
  DEALLOCATE(grid%t_lake6,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13911,&
=======
 CALL wrf_error_fatal3("<stdin>",13960,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake7 ) ) THEN 
  DEALLOCATE(grid%t_lake7,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13918,&
=======
 CALL wrf_error_fatal3("<stdin>",13967,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake8 ) ) THEN 
  DEALLOCATE(grid%t_lake8,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13925,&
=======
 CALL wrf_error_fatal3("<stdin>",13974,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake9 ) ) THEN 
  DEALLOCATE(grid%t_lake9,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13932,&
=======
 CALL wrf_error_fatal3("<stdin>",13981,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake10 ) ) THEN 
  DEALLOCATE(grid%t_lake10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13939,&
=======
 CALL wrf_error_fatal3("<stdin>",13988,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13946,&
=======
 CALL wrf_error_fatal3("<stdin>",13995,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13953,&
=======
 CALL wrf_error_fatal3("<stdin>",14002,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13960,&
=======
 CALL wrf_error_fatal3("<stdin>",14009,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13967,&
=======
 CALL wrf_error_fatal3("<stdin>",14016,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol5,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13974,&
=======
 CALL wrf_error_fatal3("<stdin>",14023,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol6,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13981,&
=======
 CALL wrf_error_fatal3("<stdin>",14030,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol7,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13988,&
=======
 CALL wrf_error_fatal3("<stdin>",14037,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol8,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",13995,&
=======
 CALL wrf_error_fatal3("<stdin>",14044,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol9,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14002,&
=======
 CALL wrf_error_fatal3("<stdin>",14051,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol10,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14009,&
=======
 CALL wrf_error_fatal3("<stdin>",14058,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedosubgrid ) ) THEN 
  DEALLOCATE(grid%albedosubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14016,&
=======
 CALL wrf_error_fatal3("<stdin>",14065,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%albedosubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhsubgrid ) ) THEN 
  DEALLOCATE(grid%lhsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14023,&
=======
 CALL wrf_error_fatal3("<stdin>",14072,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lhsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfxsubgrid ) ) THEN 
  DEALLOCATE(grid%hfxsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14030,&
=======
 CALL wrf_error_fatal3("<stdin>",14079,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfxsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupsubgrid ) ) THEN 
  DEALLOCATE(grid%lwupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14037,&
=======
 CALL wrf_error_fatal3("<stdin>",14086,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2subgrid ) ) THEN 
  DEALLOCATE(grid%q2subgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14044,&
=======
 CALL wrf_error_fatal3("<stdin>",14093,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2subgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabvsubgrid ) ) THEN 
  DEALLOCATE(grid%sabvsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14051,&
=======
 CALL wrf_error_fatal3("<stdin>",14100,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sabvsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabgsubgrid ) ) THEN 
  DEALLOCATE(grid%sabgsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14058,&
=======
 CALL wrf_error_fatal3("<stdin>",14107,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sabgsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nrasubgrid ) ) THEN 
  DEALLOCATE(grid%nrasubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14065,&
=======
 CALL wrf_error_fatal3("<stdin>",14114,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%nrasubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupsubgrid ) ) THEN 
  DEALLOCATE(grid%swupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14072,&
=======
 CALL wrf_error_fatal3("<stdin>",14121,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_fm ) ) THEN 
  DEALLOCATE(grid%ssib_fm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14079,&
=======
 CALL wrf_error_fatal3("<stdin>",14128,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_fm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_fh ) ) THEN 
  DEALLOCATE(grid%ssib_fh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14086,&
=======
 CALL wrf_error_fatal3("<stdin>",14135,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_fh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_cm ) ) THEN 
  DEALLOCATE(grid%ssib_cm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14093,&
=======
 CALL wrf_error_fatal3("<stdin>",14142,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_cm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssibxdd ) ) THEN 
  DEALLOCATE(grid%ssibxdd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14100,&
=======
 CALL wrf_error_fatal3("<stdin>",14149,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssibxdd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_br ) ) THEN 
  DEALLOCATE(grid%ssib_br,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14107,&
=======
 CALL wrf_error_fatal3("<stdin>",14156,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_br. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lhf ) ) THEN 
  DEALLOCATE(grid%ssib_lhf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14114,&
=======
 CALL wrf_error_fatal3("<stdin>",14163,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_lhf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shf ) ) THEN 
  DEALLOCATE(grid%ssib_shf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14121,&
=======
 CALL wrf_error_fatal3("<stdin>",14170,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_shf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ghf ) ) THEN 
  DEALLOCATE(grid%ssib_ghf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14128,&
=======
 CALL wrf_error_fatal3("<stdin>",14177,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_ghf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egs ) ) THEN 
  DEALLOCATE(grid%ssib_egs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14135,&
=======
 CALL wrf_error_fatal3("<stdin>",14184,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_egs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_eci ) ) THEN 
  DEALLOCATE(grid%ssib_eci,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14142,&
=======
 CALL wrf_error_fatal3("<stdin>",14191,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_eci. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ect ) ) THEN 
  DEALLOCATE(grid%ssib_ect,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14149,&
=======
 CALL wrf_error_fatal3("<stdin>",14198,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_ect. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egi ) ) THEN 
  DEALLOCATE(grid%ssib_egi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14156,&
=======
 CALL wrf_error_fatal3("<stdin>",14205,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_egi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egt ) ) THEN 
  DEALLOCATE(grid%ssib_egt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14163,&
=======
 CALL wrf_error_fatal3("<stdin>",14212,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_egt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_sdn ) ) THEN 
  DEALLOCATE(grid%ssib_sdn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14170,&
=======
 CALL wrf_error_fatal3("<stdin>",14219,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_sdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_sup ) ) THEN 
  DEALLOCATE(grid%ssib_sup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14177,&
=======
 CALL wrf_error_fatal3("<stdin>",14226,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_sup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ldn ) ) THEN 
  DEALLOCATE(grid%ssib_ldn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14184,&
=======
 CALL wrf_error_fatal3("<stdin>",14233,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_ldn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lup ) ) THEN 
  DEALLOCATE(grid%ssib_lup,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14191,&
=======
 CALL wrf_error_fatal3("<stdin>",14240,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_lup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_wat ) ) THEN 
  DEALLOCATE(grid%ssib_wat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14198,&
=======
 CALL wrf_error_fatal3("<stdin>",14247,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_wat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shc ) ) THEN 
  DEALLOCATE(grid%ssib_shc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14205,&
=======
 CALL wrf_error_fatal3("<stdin>",14254,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_shc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shg ) ) THEN 
  DEALLOCATE(grid%ssib_shg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14212,&
=======
 CALL wrf_error_fatal3("<stdin>",14261,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_shg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lai ) ) THEN 
  DEALLOCATE(grid%ssib_lai,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14219,&
=======
 CALL wrf_error_fatal3("<stdin>",14268,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_lai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_vcf ) ) THEN 
  DEALLOCATE(grid%ssib_vcf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14226,&
=======
 CALL wrf_error_fatal3("<stdin>",14275,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_vcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_z00 ) ) THEN 
  DEALLOCATE(grid%ssib_z00,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14233,&
=======
 CALL wrf_error_fatal3("<stdin>",14282,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_z00. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_veg ) ) THEN 
  DEALLOCATE(grid%ssib_veg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14240,&
=======
 CALL wrf_error_fatal3("<stdin>",14289,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ssib_veg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isnow ) ) THEN 
  DEALLOCATE(grid%isnow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14247,&
=======
 CALL wrf_error_fatal3("<stdin>",14296,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%isnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swe ) ) THEN 
  DEALLOCATE(grid%swe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14254,&
=======
 CALL wrf_error_fatal3("<stdin>",14303,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swe. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowden ) ) THEN 
  DEALLOCATE(grid%snowden,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14261,&
=======
 CALL wrf_error_fatal3("<stdin>",14310,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowden. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdepth ) ) THEN 
  DEALLOCATE(grid%snowdepth,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14268,&
=======
 CALL wrf_error_fatal3("<stdin>",14317,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowdepth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkair ) ) THEN 
  DEALLOCATE(grid%tkair,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14275,&
=======
 CALL wrf_error_fatal3("<stdin>",14324,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tkair. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo1 ) ) THEN 
  DEALLOCATE(grid%dzo1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14282,&
=======
 CALL wrf_error_fatal3("<stdin>",14331,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo1 ) ) THEN 
  DEALLOCATE(grid%wo1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14289,&
=======
 CALL wrf_error_fatal3("<stdin>",14338,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn1 ) ) THEN 
  DEALLOCATE(grid%tssn1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14296,&
=======
 CALL wrf_error_fatal3("<stdin>",14345,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssn1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno1 ) ) THEN 
  DEALLOCATE(grid%tssno1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14303,&
=======
 CALL wrf_error_fatal3("<stdin>",14352,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssno1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo1 ) ) THEN 
  DEALLOCATE(grid%bwo1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14310,&
=======
 CALL wrf_error_fatal3("<stdin>",14359,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bwo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto1 ) ) THEN 
  DEALLOCATE(grid%bto1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14317,&
=======
 CALL wrf_error_fatal3("<stdin>",14366,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bto1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto1 ) ) THEN 
  DEALLOCATE(grid%cto1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14324,&
=======
 CALL wrf_error_fatal3("<stdin>",14373,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cto1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio1 ) ) THEN 
  DEALLOCATE(grid%fio1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14331,&
=======
 CALL wrf_error_fatal3("<stdin>",14380,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fio1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo1 ) ) THEN 
  DEALLOCATE(grid%flo1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14338,&
=======
 CALL wrf_error_fatal3("<stdin>",14387,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio1 ) ) THEN 
  DEALLOCATE(grid%bio1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14345,&
=======
 CALL wrf_error_fatal3("<stdin>",14394,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bio1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo1 ) ) THEN 
  DEALLOCATE(grid%blo1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14352,&
=======
 CALL wrf_error_fatal3("<stdin>",14401,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%blo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho1 ) ) THEN 
  DEALLOCATE(grid%ho1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14359,&
=======
 CALL wrf_error_fatal3("<stdin>",14408,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ho1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo2 ) ) THEN 
  DEALLOCATE(grid%dzo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14366,&
=======
 CALL wrf_error_fatal3("<stdin>",14415,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo2 ) ) THEN 
  DEALLOCATE(grid%wo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14373,&
=======
 CALL wrf_error_fatal3("<stdin>",14422,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn2 ) ) THEN 
  DEALLOCATE(grid%tssn2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14380,&
=======
 CALL wrf_error_fatal3("<stdin>",14429,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssn2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno2 ) ) THEN 
  DEALLOCATE(grid%tssno2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14387,&
=======
 CALL wrf_error_fatal3("<stdin>",14436,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssno2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo2 ) ) THEN 
  DEALLOCATE(grid%bwo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14394,&
=======
 CALL wrf_error_fatal3("<stdin>",14443,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bwo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto2 ) ) THEN 
  DEALLOCATE(grid%bto2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14401,&
=======
 CALL wrf_error_fatal3("<stdin>",14450,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bto2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto2 ) ) THEN 
  DEALLOCATE(grid%cto2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14408,&
=======
 CALL wrf_error_fatal3("<stdin>",14457,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cto2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio2 ) ) THEN 
  DEALLOCATE(grid%fio2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14415,&
=======
 CALL wrf_error_fatal3("<stdin>",14464,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fio2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo2 ) ) THEN 
  DEALLOCATE(grid%flo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14422,&
=======
 CALL wrf_error_fatal3("<stdin>",14471,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio2 ) ) THEN 
  DEALLOCATE(grid%bio2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14429,&
=======
 CALL wrf_error_fatal3("<stdin>",14478,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bio2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo2 ) ) THEN 
  DEALLOCATE(grid%blo2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14436,&
=======
 CALL wrf_error_fatal3("<stdin>",14485,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%blo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho2 ) ) THEN 
  DEALLOCATE(grid%ho2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14443,&
=======
 CALL wrf_error_fatal3("<stdin>",14492,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ho2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo3 ) ) THEN 
  DEALLOCATE(grid%dzo3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14450,&
=======
 CALL wrf_error_fatal3("<stdin>",14499,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo3 ) ) THEN 
  DEALLOCATE(grid%wo3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14457,&
=======
 CALL wrf_error_fatal3("<stdin>",14506,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn3 ) ) THEN 
  DEALLOCATE(grid%tssn3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14464,&
=======
 CALL wrf_error_fatal3("<stdin>",14513,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssn3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno3 ) ) THEN 
  DEALLOCATE(grid%tssno3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14471,&
=======
 CALL wrf_error_fatal3("<stdin>",14520,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssno3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo3 ) ) THEN 
  DEALLOCATE(grid%bwo3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14478,&
=======
 CALL wrf_error_fatal3("<stdin>",14527,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bwo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto3 ) ) THEN 
  DEALLOCATE(grid%bto3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14485,&
=======
 CALL wrf_error_fatal3("<stdin>",14534,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bto3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto3 ) ) THEN 
  DEALLOCATE(grid%cto3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14492,&
=======
 CALL wrf_error_fatal3("<stdin>",14541,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cto3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio3 ) ) THEN 
  DEALLOCATE(grid%fio3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14499,&
=======
 CALL wrf_error_fatal3("<stdin>",14548,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fio3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo3 ) ) THEN 
  DEALLOCATE(grid%flo3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14506,&
=======
 CALL wrf_error_fatal3("<stdin>",14555,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio3 ) ) THEN 
  DEALLOCATE(grid%bio3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14513,&
=======
 CALL wrf_error_fatal3("<stdin>",14562,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bio3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo3 ) ) THEN 
  DEALLOCATE(grid%blo3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14520,&
=======
 CALL wrf_error_fatal3("<stdin>",14569,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%blo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho3 ) ) THEN 
  DEALLOCATE(grid%ho3,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14527,&
=======
 CALL wrf_error_fatal3("<stdin>",14576,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ho3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo4 ) ) THEN 
  DEALLOCATE(grid%dzo4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14534,&
=======
 CALL wrf_error_fatal3("<stdin>",14583,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dzo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo4 ) ) THEN 
  DEALLOCATE(grid%wo4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14541,&
=======
 CALL wrf_error_fatal3("<stdin>",14590,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%wo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn4 ) ) THEN 
  DEALLOCATE(grid%tssn4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14548,&
=======
 CALL wrf_error_fatal3("<stdin>",14597,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssn4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno4 ) ) THEN 
  DEALLOCATE(grid%tssno4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14555,&
=======
 CALL wrf_error_fatal3("<stdin>",14604,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tssno4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo4 ) ) THEN 
  DEALLOCATE(grid%bwo4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14562,&
=======
 CALL wrf_error_fatal3("<stdin>",14611,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bwo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto4 ) ) THEN 
  DEALLOCATE(grid%bto4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14569,&
=======
 CALL wrf_error_fatal3("<stdin>",14618,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bto4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto4 ) ) THEN 
  DEALLOCATE(grid%cto4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14576,&
=======
 CALL wrf_error_fatal3("<stdin>",14625,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%cto4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio4 ) ) THEN 
  DEALLOCATE(grid%fio4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14583,&
=======
 CALL wrf_error_fatal3("<stdin>",14632,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fio4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo4 ) ) THEN 
  DEALLOCATE(grid%flo4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14590,&
=======
 CALL wrf_error_fatal3("<stdin>",14639,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio4 ) ) THEN 
  DEALLOCATE(grid%bio4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14597,&
=======
 CALL wrf_error_fatal3("<stdin>",14646,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bio4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo4 ) ) THEN 
  DEALLOCATE(grid%blo4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14604,&
=======
 CALL wrf_error_fatal3("<stdin>",14653,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%blo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho4 ) ) THEN 
  DEALLOCATE(grid%ho4,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14611,&
=======
 CALL wrf_error_fatal3("<stdin>",14660,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ho4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake2d ) ) THEN 
  DEALLOCATE(grid%lake2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14618,&
=======
 CALL wrf_error_fatal3("<stdin>",14667,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lake2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lakedepth2d ) ) THEN 
  DEALLOCATE(grid%lakedepth2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14625,&
=======
 CALL wrf_error_fatal3("<stdin>",14674,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lakedepth2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%savedtke12d ) ) THEN 
  DEALLOCATE(grid%savedtke12d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14632,&
=======
 CALL wrf_error_fatal3("<stdin>",14681,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%savedtke12d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdp2d ) ) THEN 
  DEALLOCATE(grid%snowdp2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14639,&
=======
 CALL wrf_error_fatal3("<stdin>",14688,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snowdp2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osno2d ) ) THEN 
  DEALLOCATE(grid%h2osno2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14646,&
=======
 CALL wrf_error_fatal3("<stdin>",14695,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osno2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snl2d ) ) THEN 
  DEALLOCATE(grid%snl2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14653,&
=======
 CALL wrf_error_fatal3("<stdin>",14702,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%snl2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_grnd2d ) ) THEN 
  DEALLOCATE(grid%t_grnd2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14660,&
=======
 CALL wrf_error_fatal3("<stdin>",14709,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_grnd2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake3d ) ) THEN 
  DEALLOCATE(grid%t_lake3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14667,&
=======
 CALL wrf_error_fatal3("<stdin>",14716,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake_icefrac3d ) ) THEN 
  DEALLOCATE(grid%lake_icefrac3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14674,&
=======
 CALL wrf_error_fatal3("<stdin>",14723,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lake_icefrac3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_lake3d ) ) THEN 
  DEALLOCATE(grid%z_lake3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14681,&
=======
 CALL wrf_error_fatal3("<stdin>",14730,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dz_lake3d ) ) THEN 
  DEALLOCATE(grid%dz_lake3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14688,&
=======
 CALL wrf_error_fatal3("<stdin>",14737,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dz_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno3d ) ) THEN 
  DEALLOCATE(grid%t_soisno3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14695,&
=======
 CALL wrf_error_fatal3("<stdin>",14744,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_soisno3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14702,&
=======
 CALL wrf_error_fatal3("<stdin>",14751,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14709,&
=======
 CALL wrf_error_fatal3("<stdin>",14758,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14716,&
=======
 CALL wrf_error_fatal3("<stdin>",14765,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z3d ) ) THEN 
  DEALLOCATE(grid%z3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14723,&
=======
 CALL wrf_error_fatal3("<stdin>",14772,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dz3d ) ) THEN 
  DEALLOCATE(grid%dz3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14730,&
=======
 CALL wrf_error_fatal3("<stdin>",14779,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%dz3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zi3d ) ) THEN 
  DEALLOCATE(grid%zi3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14737,&
=======
 CALL wrf_error_fatal3("<stdin>",14786,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%zi3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%watsat3d ) ) THEN 
  DEALLOCATE(grid%watsat3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14744,&
=======
 CALL wrf_error_fatal3("<stdin>",14793,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%watsat3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%csol3d ) ) THEN 
  DEALLOCATE(grid%csol3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14751,&
=======
 CALL wrf_error_fatal3("<stdin>",14800,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%csol3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkmg3d ) ) THEN 
  DEALLOCATE(grid%tkmg3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14758,&
=======
 CALL wrf_error_fatal3("<stdin>",14807,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tkmg3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkdry3d ) ) THEN 
  DEALLOCATE(grid%tkdry3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14765,&
=======
 CALL wrf_error_fatal3("<stdin>",14814,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tkdry3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tksatu3d ) ) THEN 
  DEALLOCATE(grid%tksatu3d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14772,&
=======
 CALL wrf_error_fatal3("<stdin>",14821,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tksatu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_pl ) ) THEN 
  DEALLOCATE(grid%p_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14779,&
=======
 CALL wrf_error_fatal3("<stdin>",14828,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_pl ) ) THEN 
  DEALLOCATE(grid%u_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14786,&
=======
 CALL wrf_error_fatal3("<stdin>",14835,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_pl ) ) THEN 
  DEALLOCATE(grid%v_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14793,&
=======
 CALL wrf_error_fatal3("<stdin>",14842,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_pl ) ) THEN 
  DEALLOCATE(grid%t_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14800,&
=======
 CALL wrf_error_fatal3("<stdin>",14849,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_pl ) ) THEN 
  DEALLOCATE(grid%rh_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14807,&
=======
 CALL wrf_error_fatal3("<stdin>",14856,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_pl ) ) THEN 
  DEALLOCATE(grid%ght_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14814,&
=======
 CALL wrf_error_fatal3("<stdin>",14863,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ght_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%s_pl ) ) THEN 
  DEALLOCATE(grid%s_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14821,&
=======
 CALL wrf_error_fatal3("<stdin>",14870,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%s_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%td_pl ) ) THEN 
  DEALLOCATE(grid%td_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14828,&
=======
 CALL wrf_error_fatal3("<stdin>",14877,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%td_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_pl ) ) THEN 
  DEALLOCATE(grid%q_pl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14835,&
=======
 CALL wrf_error_fatal3("<stdin>",14884,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_zl ) ) THEN 
  DEALLOCATE(grid%z_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14842,&
=======
 CALL wrf_error_fatal3("<stdin>",14891,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%z_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_zl ) ) THEN 
  DEALLOCATE(grid%u_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14849,&
=======
 CALL wrf_error_fatal3("<stdin>",14898,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_zl ) ) THEN 
  DEALLOCATE(grid%v_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14856,&
=======
 CALL wrf_error_fatal3("<stdin>",14905,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_zl ) ) THEN 
  DEALLOCATE(grid%t_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14863,&
=======
 CALL wrf_error_fatal3("<stdin>",14912,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_zl ) ) THEN 
  DEALLOCATE(grid%rh_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14870,&
=======
 CALL wrf_error_fatal3("<stdin>",14919,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rh_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_zl ) ) THEN 
  DEALLOCATE(grid%ght_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14877,&
=======
 CALL wrf_error_fatal3("<stdin>",14926,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%ght_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%s_zl ) ) THEN 
  DEALLOCATE(grid%s_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14884,&
=======
 CALL wrf_error_fatal3("<stdin>",14933,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%s_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%td_zl ) ) THEN 
  DEALLOCATE(grid%td_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14891,&
=======
 CALL wrf_error_fatal3("<stdin>",14940,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%td_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_zl ) ) THEN 
  DEALLOCATE(grid%q_zl,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14898,&
=======
 CALL wrf_error_fatal3("<stdin>",14947,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tcoli_max ) ) THEN 
  DEALLOCATE(grid%tcoli_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14905,&
=======
 CALL wrf_error_fatal3("<stdin>",14954,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tcoli_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_flx_max ) ) THEN 
  DEALLOCATE(grid%grpl_flx_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14912,&
=======
 CALL wrf_error_fatal3("<stdin>",14961,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%grpl_flx_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd_com ) ) THEN 
  DEALLOCATE(grid%refd_com,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14919,&
=======
 CALL wrf_error_fatal3("<stdin>",14968,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%refd_com. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd ) ) THEN 
  DEALLOCATE(grid%refd,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14926,&
=======
 CALL wrf_error_fatal3("<stdin>",14975,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%refd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vil ) ) THEN 
  DEALLOCATE(grid%vil,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14933,&
=======
 CALL wrf_error_fatal3("<stdin>",14982,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%vil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%radarvil ) ) THEN 
  DEALLOCATE(grid%radarvil,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14940,&
=======
 CALL wrf_error_fatal3("<stdin>",14989,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%radarvil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%echotop ) ) THEN 
  DEALLOCATE(grid%echotop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14947,&
=======
 CALL wrf_error_fatal3("<stdin>",14996,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%echotop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fzlev ) ) THEN 
  DEALLOCATE(grid%fzlev,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14954,&
=======
 CALL wrf_error_fatal3("<stdin>",15003,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%fzlev. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icingtop ) ) THEN 
  DEALLOCATE(grid%icingtop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14961,&
=======
 CALL wrf_error_fatal3("<stdin>",15010,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icingtop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icingbot ) ) THEN 
  DEALLOCATE(grid%icingbot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14968,&
=======
 CALL wrf_error_fatal3("<stdin>",15017,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icingbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_lg ) ) THEN 
  DEALLOCATE(grid%qicing_lg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14975,&
=======
 CALL wrf_error_fatal3("<stdin>",15024,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qicing_lg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_sm ) ) THEN 
  DEALLOCATE(grid%qicing_sm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14982,&
=======
 CALL wrf_error_fatal3("<stdin>",15031,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qicing_sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_lg_max ) ) THEN 
  DEALLOCATE(grid%qicing_lg_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14989,&
=======
 CALL wrf_error_fatal3("<stdin>",15038,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qicing_lg_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_sm_max ) ) THEN 
  DEALLOCATE(grid%qicing_sm_max,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",14996,&
=======
 CALL wrf_error_fatal3("<stdin>",15045,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%qicing_sm_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icing_lg ) ) THEN 
  DEALLOCATE(grid%icing_lg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15003,&
=======
 CALL wrf_error_fatal3("<stdin>",15052,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icing_lg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icing_sm ) ) THEN 
  DEALLOCATE(grid%icing_sm,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15010,&
=======
 CALL wrf_error_fatal3("<stdin>",15059,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%icing_sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_mslp ) ) THEN 
  DEALLOCATE(grid%afwa_mslp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15017,&
=======
 CALL wrf_error_fatal3("<stdin>",15066,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_mslp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_heatidx ) ) THEN 
  DEALLOCATE(grid%afwa_heatidx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15024,&
=======
 CALL wrf_error_fatal3("<stdin>",15073,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_heatidx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_wchill ) ) THEN 
  DEALLOCATE(grid%afwa_wchill,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15031,&
=======
 CALL wrf_error_fatal3("<stdin>",15080,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_wchill. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_fits ) ) THEN 
  DEALLOCATE(grid%afwa_fits,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15038,&
=======
 CALL wrf_error_fatal3("<stdin>",15087,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_fits. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrbot ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrbot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15045,&
=======
 CALL wrf_error_fatal3("<stdin>",15094,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrtop ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrtop,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15052,&
=======
 CALL wrf_error_fatal3("<stdin>",15101,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrtop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_turb ) ) THEN 
  DEALLOCATE(grid%afwa_turb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15059,&
=======
 CALL wrf_error_fatal3("<stdin>",15108,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_turb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturb ) ) THEN 
  DEALLOCATE(grid%afwa_llturb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15066,&
=======
 CALL wrf_error_fatal3("<stdin>",15115,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_llturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturblgt ) ) THEN 
  DEALLOCATE(grid%afwa_llturblgt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15073,&
=======
 CALL wrf_error_fatal3("<stdin>",15122,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_llturblgt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbmdt ) ) THEN 
  DEALLOCATE(grid%afwa_llturbmdt,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15080,&
=======
 CALL wrf_error_fatal3("<stdin>",15129,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbmdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbsvr ) ) THEN 
  DEALLOCATE(grid%afwa_llturbsvr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15087,&
=======
 CALL wrf_error_fatal3("<stdin>",15136,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbsvr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_precip ) ) THEN 
  DEALLOCATE(grid%afwa_precip,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15094,&
=======
 CALL wrf_error_fatal3("<stdin>",15143,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_precip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_totprecip ) ) THEN 
  DEALLOCATE(grid%afwa_totprecip,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15101,&
=======
 CALL wrf_error_fatal3("<stdin>",15150,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_totprecip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_rain ) ) THEN 
  DEALLOCATE(grid%afwa_rain,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15108,&
=======
 CALL wrf_error_fatal3("<stdin>",15157,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_rain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_snow ) ) THEN 
  DEALLOCATE(grid%afwa_snow,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15115,&
=======
 CALL wrf_error_fatal3("<stdin>",15164,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_ice ) ) THEN 
  DEALLOCATE(grid%afwa_ice,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15122,&
=======
 CALL wrf_error_fatal3("<stdin>",15171,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_fzra ) ) THEN 
  DEALLOCATE(grid%afwa_fzra,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15129,&
=======
 CALL wrf_error_fatal3("<stdin>",15178,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_fzra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_snowfall ) ) THEN 
  DEALLOCATE(grid%afwa_snowfall,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15136,&
=======
 CALL wrf_error_fatal3("<stdin>",15185,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_snowfall. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis ) ) THEN 
  DEALLOCATE(grid%afwa_vis,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15143,&
=======
 CALL wrf_error_fatal3("<stdin>",15192,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_vis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_alpha ) ) THEN 
  DEALLOCATE(grid%afwa_vis_alpha,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15150,&
=======
 CALL wrf_error_fatal3("<stdin>",15199,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_alpha. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_dust ) ) THEN 
  DEALLOCATE(grid%afwa_vis_dust,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15157,&
=======
 CALL wrf_error_fatal3("<stdin>",15206,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_dust. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud ) ) THEN 
  DEALLOCATE(grid%afwa_cloud,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15164,&
=======
 CALL wrf_error_fatal3("<stdin>",15213,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud_ceil ) ) THEN 
  DEALLOCATE(grid%afwa_cloud_ceil,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15171,&
=======
 CALL wrf_error_fatal3("<stdin>",15220,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud_ceil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cape ) ) THEN 
  DEALLOCATE(grid%afwa_cape,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15178,&
=======
 CALL wrf_error_fatal3("<stdin>",15227,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cape. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cin ) ) THEN 
  DEALLOCATE(grid%afwa_cin,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15185,&
=======
 CALL wrf_error_fatal3("<stdin>",15234,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cape_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cape_mu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15192,&
=======
 CALL wrf_error_fatal3("<stdin>",15241,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cape_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cin_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cin_mu,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15199,&
=======
 CALL wrf_error_fatal3("<stdin>",15248,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_cin_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_zlfc ) ) THEN 
  DEALLOCATE(grid%afwa_zlfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15206,&
=======
 CALL wrf_error_fatal3("<stdin>",15255,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_zlfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_plfc ) ) THEN 
  DEALLOCATE(grid%afwa_plfc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15213,&
=======
 CALL wrf_error_fatal3("<stdin>",15262,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_plfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_lidx ) ) THEN 
  DEALLOCATE(grid%afwa_lidx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15220,&
=======
 CALL wrf_error_fatal3("<stdin>",15269,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_lidx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_pwat ) ) THEN 
  DEALLOCATE(grid%afwa_pwat,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15227,&
=======
 CALL wrf_error_fatal3("<stdin>",15276,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_pwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%midrh_min ) ) THEN 
  DEALLOCATE(grid%midrh_min,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15234,&
=======
 CALL wrf_error_fatal3("<stdin>",15283,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%midrh_min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%midrh_min_old ) ) THEN 
  DEALLOCATE(grid%midrh_min_old,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15241,&
=======
 CALL wrf_error_fatal3("<stdin>",15290,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%midrh_min_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_hail ) ) THEN 
  DEALLOCATE(grid%afwa_hail,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15248,&
=======
 CALL wrf_error_fatal3("<stdin>",15297,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_hail. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llws ) ) THEN 
  DEALLOCATE(grid%afwa_llws,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15255,&
=======
 CALL wrf_error_fatal3("<stdin>",15304,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_llws. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tornado ) ) THEN 
  DEALLOCATE(grid%afwa_tornado,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15262,&
=======
 CALL wrf_error_fatal3("<stdin>",15311,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%afwa_tornado. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tornado_mask ) ) THEN 
  DEALLOCATE(grid%tornado_mask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15269,&
=======
 CALL wrf_error_fatal3("<stdin>",15318,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tornado_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tornado_dur ) ) THEN 
  DEALLOCATE(grid%tornado_dur,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15276,&
=======
 CALL wrf_error_fatal3("<stdin>",15325,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tornado_dur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_mean ) ) THEN 
  DEALLOCATE(grid%psfc_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15283,&
=======
 CALL wrf_error_fatal3("<stdin>",15332,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psfc_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_mean ) ) THEN 
  DEALLOCATE(grid%tsk_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15290,&
=======
 CALL wrf_error_fatal3("<stdin>",15339,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmsl_mean ) ) THEN 
  DEALLOCATE(grid%pmsl_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15297,&
=======
 CALL wrf_error_fatal3("<stdin>",15346,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pmsl_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_mean ) ) THEN 
  DEALLOCATE(grid%t2_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15304,&
=======
 CALL wrf_error_fatal3("<stdin>",15353,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_mean ) ) THEN 
  DEALLOCATE(grid%th2_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15311,&
=======
 CALL wrf_error_fatal3("<stdin>",15360,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_mean ) ) THEN 
  DEALLOCATE(grid%q2_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15318,&
=======
 CALL wrf_error_fatal3("<stdin>",15367,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_mean ) ) THEN 
  DEALLOCATE(grid%u10_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15325,&
=======
 CALL wrf_error_fatal3("<stdin>",15374,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_mean ) ) THEN 
  DEALLOCATE(grid%v10_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15332,&
=======
 CALL wrf_error_fatal3("<stdin>",15381,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_mean ) ) THEN 
  DEALLOCATE(grid%hfx_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15339,&
=======
 CALL wrf_error_fatal3("<stdin>",15388,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_mean ) ) THEN 
  DEALLOCATE(grid%lh_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15346,&
=======
 CALL wrf_error_fatal3("<stdin>",15395,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_mean ) ) THEN 
  DEALLOCATE(grid%swdnb_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15353,&
=======
 CALL wrf_error_fatal3("<stdin>",15402,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_mean ) ) THEN 
  DEALLOCATE(grid%glw_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15360,&
=======
 CALL wrf_error_fatal3("<stdin>",15409,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%glw_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_mean ) ) THEN 
  DEALLOCATE(grid%lwupb_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15367,&
=======
 CALL wrf_error_fatal3("<stdin>",15416,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_mean ) ) THEN 
  DEALLOCATE(grid%swupb_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15374,&
=======
 CALL wrf_error_fatal3("<stdin>",15423,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_mean ) ) THEN 
  DEALLOCATE(grid%swupt_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15381,&
=======
 CALL wrf_error_fatal3("<stdin>",15430,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_mean ) ) THEN 
  DEALLOCATE(grid%swdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15388,&
=======
 CALL wrf_error_fatal3("<stdin>",15437,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_mean ) ) THEN 
  DEALLOCATE(grid%lwupt_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15395,&
=======
 CALL wrf_error_fatal3("<stdin>",15444,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_mean ) ) THEN 
  DEALLOCATE(grid%lwdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15402,&
=======
 CALL wrf_error_fatal3("<stdin>",15451,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_diurn ) ) THEN 
  DEALLOCATE(grid%psfc_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15409,&
=======
 CALL wrf_error_fatal3("<stdin>",15458,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psfc_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_diurn ) ) THEN 
  DEALLOCATE(grid%tsk_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15416,&
=======
 CALL wrf_error_fatal3("<stdin>",15465,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_diurn ) ) THEN 
  DEALLOCATE(grid%t2_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15423,&
=======
 CALL wrf_error_fatal3("<stdin>",15472,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_diurn ) ) THEN 
  DEALLOCATE(grid%th2_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15430,&
=======
 CALL wrf_error_fatal3("<stdin>",15479,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_diurn ) ) THEN 
  DEALLOCATE(grid%q2_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15437,&
=======
 CALL wrf_error_fatal3("<stdin>",15486,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_diurn ) ) THEN 
  DEALLOCATE(grid%u10_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15444,&
=======
 CALL wrf_error_fatal3("<stdin>",15493,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_diurn ) ) THEN 
  DEALLOCATE(grid%v10_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15451,&
=======
 CALL wrf_error_fatal3("<stdin>",15500,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_diurn ) ) THEN 
  DEALLOCATE(grid%hfx_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15458,&
=======
 CALL wrf_error_fatal3("<stdin>",15507,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_diurn ) ) THEN 
  DEALLOCATE(grid%lh_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15465,&
=======
 CALL wrf_error_fatal3("<stdin>",15514,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_diurn ) ) THEN 
  DEALLOCATE(grid%swdnb_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15472,&
=======
 CALL wrf_error_fatal3("<stdin>",15521,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_diurn ) ) THEN 
  DEALLOCATE(grid%glw_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15479,&
=======
 CALL wrf_error_fatal3("<stdin>",15528,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%glw_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_diurn ) ) THEN 
  DEALLOCATE(grid%lwupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15486,&
=======
 CALL wrf_error_fatal3("<stdin>",15535,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_diurn ) ) THEN 
  DEALLOCATE(grid%swupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15493,&
=======
 CALL wrf_error_fatal3("<stdin>",15542,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_diurn ) ) THEN 
  DEALLOCATE(grid%swupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15500,&
=======
 CALL wrf_error_fatal3("<stdin>",15549,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_diurn ) ) THEN 
  DEALLOCATE(grid%swdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15507,&
=======
 CALL wrf_error_fatal3("<stdin>",15556,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_diurn ) ) THEN 
  DEALLOCATE(grid%lwupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15514,&
=======
 CALL wrf_error_fatal3("<stdin>",15563,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_diurn ) ) THEN 
  DEALLOCATE(grid%lwdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15521,&
=======
 CALL wrf_error_fatal3("<stdin>",15570,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_dtmp ) ) THEN 
  DEALLOCATE(grid%psfc_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15528,&
=======
 CALL wrf_error_fatal3("<stdin>",15577,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%psfc_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_dtmp ) ) THEN 
  DEALLOCATE(grid%tsk_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15535,&
=======
 CALL wrf_error_fatal3("<stdin>",15584,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tsk_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_dtmp ) ) THEN 
  DEALLOCATE(grid%t2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15542,&
=======
 CALL wrf_error_fatal3("<stdin>",15591,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%t2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_dtmp ) ) THEN 
  DEALLOCATE(grid%th2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15549,&
=======
 CALL wrf_error_fatal3("<stdin>",15598,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%th2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_dtmp ) ) THEN 
  DEALLOCATE(grid%q2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15556,&
=======
 CALL wrf_error_fatal3("<stdin>",15605,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%q2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_dtmp ) ) THEN 
  DEALLOCATE(grid%u10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15563,&
=======
 CALL wrf_error_fatal3("<stdin>",15612,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%u10_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_dtmp ) ) THEN 
  DEALLOCATE(grid%v10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15570,&
=======
 CALL wrf_error_fatal3("<stdin>",15619,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%v10_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_dtmp ) ) THEN 
  DEALLOCATE(grid%hfx_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15577,&
=======
 CALL wrf_error_fatal3("<stdin>",15626,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%hfx_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_dtmp ) ) THEN 
  DEALLOCATE(grid%lh_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15584,&
=======
 CALL wrf_error_fatal3("<stdin>",15633,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lh_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15591,&
=======
 CALL wrf_error_fatal3("<stdin>",15640,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_dtmp ) ) THEN 
  DEALLOCATE(grid%glw_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15598,&
=======
 CALL wrf_error_fatal3("<stdin>",15647,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%glw_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15605,&
=======
 CALL wrf_error_fatal3("<stdin>",15654,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_dtmp ) ) THEN 
  DEALLOCATE(grid%swupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15612,&
=======
 CALL wrf_error_fatal3("<stdin>",15661,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_dtmp ) ) THEN 
  DEALLOCATE(grid%swupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15619,&
=======
 CALL wrf_error_fatal3("<stdin>",15668,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swupt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15626,&
=======
 CALL wrf_error_fatal3("<stdin>",15675,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%swdnt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15633,&
=======
 CALL wrf_error_fatal3("<stdin>",15682,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwupt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15640,&
=======
 CALL wrf_error_fatal3("<stdin>",15689,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lwdnt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ql ) ) THEN 
  DEALLOCATE(grid%kext_ql,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15647,&
=======
 CALL wrf_error_fatal3("<stdin>",15696,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ql. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qic ) ) THEN 
  DEALLOCATE(grid%kext_qic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15654,&
=======
 CALL wrf_error_fatal3("<stdin>",15703,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qip ) ) THEN 
  DEALLOCATE(grid%kext_qip,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15661,&
=======
 CALL wrf_error_fatal3("<stdin>",15710,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qid ) ) THEN 
  DEALLOCATE(grid%kext_qid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15668,&
=======
 CALL wrf_error_fatal3("<stdin>",15717,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qs ) ) THEN 
  DEALLOCATE(grid%kext_qs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15675,&
=======
 CALL wrf_error_fatal3("<stdin>",15724,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qg ) ) THEN 
  DEALLOCATE(grid%kext_qg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15682,&
=======
 CALL wrf_error_fatal3("<stdin>",15731,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qh ) ) THEN 
  DEALLOCATE(grid%kext_qh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15689,&
=======
 CALL wrf_error_fatal3("<stdin>",15738,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qa ) ) THEN 
  DEALLOCATE(grid%kext_qa,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15696,&
=======
 CALL wrf_error_fatal3("<stdin>",15745,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_qa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qic ) ) THEN 
  DEALLOCATE(grid%kext_ft_qic,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15703,&
=======
 CALL wrf_error_fatal3("<stdin>",15752,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qip ) ) THEN 
  DEALLOCATE(grid%kext_ft_qip,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15710,&
=======
 CALL wrf_error_fatal3("<stdin>",15759,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qid ) ) THEN 
  DEALLOCATE(grid%kext_ft_qid,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15717,&
=======
 CALL wrf_error_fatal3("<stdin>",15766,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qs ) ) THEN 
  DEALLOCATE(grid%kext_ft_qs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15724,&
=======
 CALL wrf_error_fatal3("<stdin>",15773,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qg ) ) THEN 
  DEALLOCATE(grid%kext_ft_qg,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15731,&
=======
 CALL wrf_error_fatal3("<stdin>",15780,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%height ) ) THEN 
  DEALLOCATE(grid%height,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15738,&
=======
 CALL wrf_error_fatal3("<stdin>",15787,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%height. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tempc ) ) THEN 
  DEALLOCATE(grid%tempc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15745,&
=======
 CALL wrf_error_fatal3("<stdin>",15794,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tempc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rscghis_2d ) ) THEN 
  DEALLOCATE(grid%rscghis_2d,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15752,&
=======
 CALL wrf_error_fatal3("<stdin>",15801,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%rscghis_2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%induc ) ) THEN 
  DEALLOCATE(grid%induc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15759,&
=======
 CALL wrf_error_fatal3("<stdin>",15808,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%induc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%noninduc ) ) THEN 
  DEALLOCATE(grid%noninduc,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15766,&
=======
 CALL wrf_error_fatal3("<stdin>",15815,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%noninduc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sctot ) ) THEN 
  DEALLOCATE(grid%sctot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15773,&
=======
 CALL wrf_error_fatal3("<stdin>",15822,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sctot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecmag ) ) THEN 
  DEALLOCATE(grid%elecmag,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15780,&
=======
 CALL wrf_error_fatal3("<stdin>",15829,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%elecmag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecx ) ) THEN 
  DEALLOCATE(grid%elecx,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15787,&
=======
 CALL wrf_error_fatal3("<stdin>",15836,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%elecx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecy ) ) THEN 
  DEALLOCATE(grid%elecy,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15794,&
=======
 CALL wrf_error_fatal3("<stdin>",15843,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%elecy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecz ) ) THEN 
  DEALLOCATE(grid%elecz,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15801,&
=======
 CALL wrf_error_fatal3("<stdin>",15850,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%elecz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pot ) ) THEN 
  DEALLOCATE(grid%pot,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15808,&
=======
 CALL wrf_error_fatal3("<stdin>",15857,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%light ) ) THEN 
  DEALLOCATE(grid%light,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15815,&
=======
 CALL wrf_error_fatal3("<stdin>",15864,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%light. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lightdens ) ) THEN 
  DEALLOCATE(grid%lightdens,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15822,&
=======
 CALL wrf_error_fatal3("<stdin>",15871,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lightdens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lightdis ) ) THEN 
  DEALLOCATE(grid%lightdis,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15829,&
=======
 CALL wrf_error_fatal3("<stdin>",15878,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lightdis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshi ) ) THEN 
  DEALLOCATE(grid%flshi,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15836,&
=======
 CALL wrf_error_fatal3("<stdin>",15885,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flshi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshn ) ) THEN 
  DEALLOCATE(grid%flshn,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15843,&
=======
 CALL wrf_error_fatal3("<stdin>",15892,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flshn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshp ) ) THEN 
  DEALLOCATE(grid%flshp,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15850,&
=======
 CALL wrf_error_fatal3("<stdin>",15899,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%flshp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_u_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_u_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15857,&
=======
 CALL wrf_error_fatal3("<stdin>",15906,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_u_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_v_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_v_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15864,&
=======
 CALL wrf_error_fatal3("<stdin>",15913,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_v_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_t_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_t_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15871,&
=======
 CALL wrf_error_fatal3("<stdin>",15920,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%field_t_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bf ) ) THEN 
  DEALLOCATE(grid%bf,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15878,&
=======
 CALL wrf_error_fatal3("<stdin>",15927,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c1h ) ) THEN 
  DEALLOCATE(grid%c1h,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15885,&
=======
 CALL wrf_error_fatal3("<stdin>",15934,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c1h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c2h ) ) THEN 
  DEALLOCATE(grid%c2h,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15892,&
=======
 CALL wrf_error_fatal3("<stdin>",15941,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c2h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bh ) ) THEN 
  DEALLOCATE(grid%bh,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15899,&
=======
 CALL wrf_error_fatal3("<stdin>",15948,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%bh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c1f ) ) THEN 
  DEALLOCATE(grid%c1f,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15906,&
=======
 CALL wrf_error_fatal3("<stdin>",15955,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c1f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c2f ) ) THEN 
  DEALLOCATE(grid%c2f,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15913,&
=======
 CALL wrf_error_fatal3("<stdin>",15962,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c2f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c3h ) ) THEN 
  DEALLOCATE(grid%c3h,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15920,&
=======
 CALL wrf_error_fatal3("<stdin>",15969,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c3h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c4h ) ) THEN 
  DEALLOCATE(grid%c4h,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15927,&
=======
 CALL wrf_error_fatal3("<stdin>",15976,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c4h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c3f ) ) THEN 
  DEALLOCATE(grid%c3f,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15934,&
=======
 CALL wrf_error_fatal3("<stdin>",15983,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c3f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c4f ) ) THEN 
  DEALLOCATE(grid%c4f,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15941,&
=======
 CALL wrf_error_fatal3("<stdin>",15990,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%c4f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pcb ) ) THEN 
  DEALLOCATE(grid%pcb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15948,&
=======
 CALL wrf_error_fatal3("<stdin>",15997,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pcb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_1 ) ) THEN 
  DEALLOCATE(grid%pc_1,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15955,&
=======
 CALL wrf_error_fatal3("<stdin>",16004,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_2 ) ) THEN 
  DEALLOCATE(grid%pc_2,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15962,&
=======
 CALL wrf_error_fatal3("<stdin>",16011,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_bxs ) ) THEN 
  DEALLOCATE(grid%pc_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15969,&
=======
 CALL wrf_error_fatal3("<stdin>",16018,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_bxs. ')
 endif
  NULLIFY(grid%pc_bxs)
ENDIF
IF ( ASSOCIATED( grid%pc_bxe ) ) THEN 
  DEALLOCATE(grid%pc_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15977,&
=======
 CALL wrf_error_fatal3("<stdin>",16026,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_bxe. ')
 endif
  NULLIFY(grid%pc_bxe)
ENDIF
IF ( ASSOCIATED( grid%pc_bys ) ) THEN 
  DEALLOCATE(grid%pc_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15985,&
=======
 CALL wrf_error_fatal3("<stdin>",16034,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_bys. ')
 endif
  NULLIFY(grid%pc_bys)
ENDIF
IF ( ASSOCIATED( grid%pc_bye ) ) THEN 
  DEALLOCATE(grid%pc_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",15993,&
=======
 CALL wrf_error_fatal3("<stdin>",16042,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_bye. ')
 endif
  NULLIFY(grid%pc_bye)
ENDIF
IF ( ASSOCIATED( grid%pc_btxs ) ) THEN 
  DEALLOCATE(grid%pc_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16001,&
=======
 CALL wrf_error_fatal3("<stdin>",16050,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_btxs. ')
 endif
  NULLIFY(grid%pc_btxs)
ENDIF
IF ( ASSOCIATED( grid%pc_btxe ) ) THEN 
  DEALLOCATE(grid%pc_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16009,&
=======
 CALL wrf_error_fatal3("<stdin>",16058,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_btxe. ')
 endif
  NULLIFY(grid%pc_btxe)
ENDIF
IF ( ASSOCIATED( grid%pc_btys ) ) THEN 
  DEALLOCATE(grid%pc_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16017,&
=======
 CALL wrf_error_fatal3("<stdin>",16066,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_btys. ')
 endif
  NULLIFY(grid%pc_btys)
ENDIF
IF ( ASSOCIATED( grid%pc_btye ) ) THEN 
  DEALLOCATE(grid%pc_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16025,&
=======
 CALL wrf_error_fatal3("<stdin>",16074,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%pc_btye. ')
 endif
  NULLIFY(grid%pc_btye)
ENDIF
IF ( ASSOCIATED( grid%p_wif_now ) ) THEN 
  DEALLOCATE(grid%p_wif_now,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16033,&
=======
 CALL wrf_error_fatal3("<stdin>",16082,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jan ) ) THEN 
  DEALLOCATE(grid%p_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16040,&
=======
 CALL wrf_error_fatal3("<stdin>",16089,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_feb ) ) THEN 
  DEALLOCATE(grid%p_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16047,&
=======
 CALL wrf_error_fatal3("<stdin>",16096,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_mar ) ) THEN 
  DEALLOCATE(grid%p_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16054,&
=======
 CALL wrf_error_fatal3("<stdin>",16103,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_apr ) ) THEN 
  DEALLOCATE(grid%p_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16061,&
=======
 CALL wrf_error_fatal3("<stdin>",16110,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_may ) ) THEN 
  DEALLOCATE(grid%p_wif_may,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16068,&
=======
 CALL wrf_error_fatal3("<stdin>",16117,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jun ) ) THEN 
  DEALLOCATE(grid%p_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16075,&
=======
 CALL wrf_error_fatal3("<stdin>",16124,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jul ) ) THEN 
  DEALLOCATE(grid%p_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16082,&
=======
 CALL wrf_error_fatal3("<stdin>",16131,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_aug ) ) THEN 
  DEALLOCATE(grid%p_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16089,&
=======
 CALL wrf_error_fatal3("<stdin>",16138,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_sep ) ) THEN 
  DEALLOCATE(grid%p_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16096,&
=======
 CALL wrf_error_fatal3("<stdin>",16145,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_oct ) ) THEN 
  DEALLOCATE(grid%p_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16103,&
=======
 CALL wrf_error_fatal3("<stdin>",16152,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_nov ) ) THEN 
  DEALLOCATE(grid%p_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16110,&
=======
 CALL wrf_error_fatal3("<stdin>",16159,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_dec ) ) THEN 
  DEALLOCATE(grid%p_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16117,&
=======
 CALL wrf_error_fatal3("<stdin>",16166,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%p_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_now ) ) THEN 
  DEALLOCATE(grid%w_wif_now,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16124,&
=======
 CALL wrf_error_fatal3("<stdin>",16173,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jan ) ) THEN 
  DEALLOCATE(grid%w_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16131,&
=======
 CALL wrf_error_fatal3("<stdin>",16180,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_feb ) ) THEN 
  DEALLOCATE(grid%w_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16138,&
=======
 CALL wrf_error_fatal3("<stdin>",16187,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_mar ) ) THEN 
  DEALLOCATE(grid%w_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16145,&
=======
 CALL wrf_error_fatal3("<stdin>",16194,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_apr ) ) THEN 
  DEALLOCATE(grid%w_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16152,&
=======
 CALL wrf_error_fatal3("<stdin>",16201,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_may ) ) THEN 
  DEALLOCATE(grid%w_wif_may,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16159,&
=======
 CALL wrf_error_fatal3("<stdin>",16208,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jun ) ) THEN 
  DEALLOCATE(grid%w_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16166,&
=======
 CALL wrf_error_fatal3("<stdin>",16215,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jul ) ) THEN 
  DEALLOCATE(grid%w_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16173,&
=======
 CALL wrf_error_fatal3("<stdin>",16222,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_aug ) ) THEN 
  DEALLOCATE(grid%w_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16180,&
=======
 CALL wrf_error_fatal3("<stdin>",16229,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_sep ) ) THEN 
  DEALLOCATE(grid%w_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16187,&
=======
 CALL wrf_error_fatal3("<stdin>",16236,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_oct ) ) THEN 
  DEALLOCATE(grid%w_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16194,&
=======
 CALL wrf_error_fatal3("<stdin>",16243,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_nov ) ) THEN 
  DEALLOCATE(grid%w_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16201,&
=======
 CALL wrf_error_fatal3("<stdin>",16250,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_dec ) ) THEN 
  DEALLOCATE(grid%w_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16208,&
=======
 CALL wrf_error_fatal3("<stdin>",16257,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%w_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_now ) ) THEN 
  DEALLOCATE(grid%i_wif_now,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16215,&
=======
 CALL wrf_error_fatal3("<stdin>",16264,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jan ) ) THEN 
  DEALLOCATE(grid%i_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16222,&
=======
 CALL wrf_error_fatal3("<stdin>",16271,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_feb ) ) THEN 
  DEALLOCATE(grid%i_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16229,&
=======
 CALL wrf_error_fatal3("<stdin>",16278,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_mar ) ) THEN 
  DEALLOCATE(grid%i_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16236,&
=======
 CALL wrf_error_fatal3("<stdin>",16285,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_apr ) ) THEN 
  DEALLOCATE(grid%i_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16243,&
=======
 CALL wrf_error_fatal3("<stdin>",16292,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_may ) ) THEN 
  DEALLOCATE(grid%i_wif_may,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16250,&
=======
 CALL wrf_error_fatal3("<stdin>",16299,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jun ) ) THEN 
  DEALLOCATE(grid%i_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16257,&
=======
 CALL wrf_error_fatal3("<stdin>",16306,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jul ) ) THEN 
  DEALLOCATE(grid%i_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16264,&
=======
 CALL wrf_error_fatal3("<stdin>",16313,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_aug ) ) THEN 
  DEALLOCATE(grid%i_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16271,&
=======
 CALL wrf_error_fatal3("<stdin>",16320,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_sep ) ) THEN 
  DEALLOCATE(grid%i_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16278,&
=======
 CALL wrf_error_fatal3("<stdin>",16327,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_oct ) ) THEN 
  DEALLOCATE(grid%i_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16285,&
=======
 CALL wrf_error_fatal3("<stdin>",16334,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_nov ) ) THEN 
  DEALLOCATE(grid%i_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16292,&
=======
 CALL wrf_error_fatal3("<stdin>",16341,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_dec ) ) THEN 
  DEALLOCATE(grid%i_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16299,&
=======
 CALL wrf_error_fatal3("<stdin>",16348,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%i_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16306,&
=======
 CALL wrf_error_fatal3("<stdin>",16355,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%landmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lakemask ) ) THEN 
  DEALLOCATE(grid%lakemask,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16313,&
=======
 CALL wrf_error_fatal3("<stdin>",16362,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%lakemask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16320,&
=======
 CALL wrf_error_fatal3("<stdin>",16369,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sst_input ) ) THEN 
  DEALLOCATE(grid%sst_input,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16327,&
=======
 CALL wrf_error_fatal3("<stdin>",16376,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%sst_input. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16334,&
=======
 CALL wrf_error_fatal3("<stdin>",16383,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%chem. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tracer ) ) THEN 
  DEALLOCATE(grid%tracer,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16341,&
=======
 CALL wrf_error_fatal3("<stdin>",16390,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tracer_bxs ) ) THEN 
  DEALLOCATE(grid%tracer_bxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16348,&
=======
 CALL wrf_error_fatal3("<stdin>",16397,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_bxs. ')
 endif
  NULLIFY(grid%tracer_bxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_bxe ) ) THEN 
  DEALLOCATE(grid%tracer_bxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16356,&
=======
 CALL wrf_error_fatal3("<stdin>",16405,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_bxe. ')
 endif
  NULLIFY(grid%tracer_bxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_bys ) ) THEN 
  DEALLOCATE(grid%tracer_bys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16364,&
=======
 CALL wrf_error_fatal3("<stdin>",16413,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_bys. ')
 endif
  NULLIFY(grid%tracer_bys)
ENDIF
IF ( ASSOCIATED( grid%tracer_bye ) ) THEN 
  DEALLOCATE(grid%tracer_bye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16372,&
=======
 CALL wrf_error_fatal3("<stdin>",16421,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_bye. ')
 endif
  NULLIFY(grid%tracer_bye)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxs ) ) THEN 
  DEALLOCATE(grid%tracer_btxs,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16380,&
=======
 CALL wrf_error_fatal3("<stdin>",16429,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_btxs. ')
 endif
  NULLIFY(grid%tracer_btxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxe ) ) THEN 
  DEALLOCATE(grid%tracer_btxe,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16388,&
=======
 CALL wrf_error_fatal3("<stdin>",16437,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_btxe. ')
 endif
  NULLIFY(grid%tracer_btxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_btys ) ) THEN 
  DEALLOCATE(grid%tracer_btys,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16396,&
=======
 CALL wrf_error_fatal3("<stdin>",16445,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_btys. ')
 endif
  NULLIFY(grid%tracer_btys)
ENDIF
IF ( ASSOCIATED( grid%tracer_btye ) ) THEN 
  DEALLOCATE(grid%tracer_btye,STAT=ierr)
 if (ierr.ne.0) then
<<<<<<< HEAD
 CALL wrf_error_fatal3("<stdin>",16404,&
=======
 CALL wrf_error_fatal3("<stdin>",16453,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
'frame/module_domain.f: Failed to deallocate grid%tracer_btye. ')
 endif
  NULLIFY(grid%tracer_btye)
ENDIF


   END SUBROUTINE dealloc_space_field



   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid






      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      NULLIFY(result_grid)
      IF ( ASSOCIATED( in_grid ) ) THEN
        IF ( in_grid%id .EQ. id ) THEN
           result_grid => in_grid
        ELSE
           grid_ptr => in_grid
           DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
              DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                    CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                    IF ( ASSOCIATED( result_grid ) ) THEN
                      IF ( result_grid%id .EQ. id ) found = .TRUE.
                    ENDIF
                 ENDIF
              ENDDO
              IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
           ENDDO
        ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      

      INTEGER                             :: loc






      
      

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer

   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain










      FUNCTION domain_get_current_time ( grid ) RESULT ( current_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: current_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=current_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16512,&
=======
          CALL wrf_error_fatal3("<stdin>",16562,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_get_current_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_current_time


      FUNCTION domain_get_start_time ( grid ) RESULT ( start_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: start_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StartTime=start_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16532,&
=======
          CALL wrf_error_fatal3("<stdin>",16582,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_get_start_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_start_time


      FUNCTION domain_get_stop_time ( grid ) RESULT ( stop_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: stop_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StopTime=stop_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16552,&
=======
          CALL wrf_error_fatal3("<stdin>",16602,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_get_stop_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_stop_time


      FUNCTION domain_get_time_step ( grid ) RESULT ( time_step ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_step
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, timeStep=time_step, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16572,&
=======
          CALL wrf_error_fatal3("<stdin>",16622,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_get_time_step:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_time_step


      FUNCTION domain_get_advanceCount ( grid ) RESULT ( advanceCount ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        INTEGER :: advanceCount
        
        INTEGER(WRFU_KIND_I8) :: advanceCountLcl
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, &
                            advanceCount=advanceCountLcl, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16595,&
=======
          CALL wrf_error_fatal3("<stdin>",16645,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_get_advanceCount:  WRFU_ClockGet failed' )
        ENDIF
        advanceCount = advanceCountLcl
      END FUNCTION domain_get_advanceCount


      SUBROUTINE domain_alarms_destroy ( grid )
        IMPLICIT NONE





        TYPE(domain), INTENT(INOUT) :: grid
        
        INTEGER                     :: alarmid

        IF ( ASSOCIATED( grid%alarms ) .AND. &
             ASSOCIATED( grid%alarms_created ) ) THEN
          DO alarmid = 1, MAX_WRF_ALARMS
            IF ( grid%alarms_created( alarmid ) ) THEN
              CALL WRFU_AlarmDestroy( grid%alarms( alarmid ) )
              grid%alarms_created( alarmid ) = .FALSE.
            ENDIF
          ENDDO
          DEALLOCATE( grid%alarms )
          NULLIFY( grid%alarms )
          DEALLOCATE( grid%alarms_created )
          NULLIFY( grid%alarms_created )
        ENDIF
      END SUBROUTINE domain_alarms_destroy


      SUBROUTINE domain_clock_destroy ( grid )
        IMPLICIT NONE




        TYPE(domain), INTENT(INOUT) :: grid
        IF ( ASSOCIATED( grid%domain_clock ) ) THEN
          IF ( grid%domain_clock_created ) THEN
            CALL WRFU_ClockDestroy( grid%domain_clock )
            grid%domain_clock_created = .FALSE.
          ENDIF
          DEALLOCATE( grid%domain_clock )
          NULLIFY( grid%domain_clock )
        ENDIF
      END SUBROUTINE domain_clock_destroy


      FUNCTION domain_last_time_step ( grid ) RESULT ( LAST_TIME ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: LAST_TIME
        LAST_TIME =   domain_get_stop_time( grid ) .EQ. &
                    ( domain_get_current_time( grid ) + &
                      domain_get_time_step( grid ) )
      END FUNCTION domain_last_time_step



      FUNCTION domain_clockisstoptime ( grid ) RESULT ( is_stop_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_time
        INTEGER :: rc
        is_stop_time = WRFU_ClockIsStopTime( grid%domain_clock , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",16677,&
=======
          CALL wrf_error_fatal3("<stdin>",16727,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_clockisstoptime:  WRFU_ClockIsStopTime() failed' )
        ENDIF
      END FUNCTION domain_clockisstoptime



      FUNCTION domain_clockisstopsubtime ( grid ) RESULT ( is_stop_subtime ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_subtime
        INTEGER :: rc
        TYPE(WRFU_TimeInterval) :: timeStep
        TYPE(WRFU_Time) :: currentTime
        LOGICAL :: positive_timestep
        is_stop_subtime = .FALSE.
        CALL domain_clock_get( grid, time_step=timeStep, &
                                     current_time=currentTime )
        positive_timestep = ESMF_TimeIntervalIsPositive( timeStep )
        IF ( positive_timestep ) THEN


          IF ( ESMF_TimeGE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ELSE


          IF ( ESMF_TimeLE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ENDIF
      END FUNCTION domain_clockisstopsubtime




      FUNCTION domain_get_sim_start_time ( grid ) RESULT ( simulationStartTime ) 
        IMPLICIT NONE












        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: simulationStartTime
        
        INTEGER :: rc
        INTEGER :: simulation_start_year,   simulation_start_month, &
                   simulation_start_day,    simulation_start_hour , &
                   simulation_start_minute, simulation_start_second
        CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
        CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
        CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
        CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
        CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
        CALL nl_get_simulation_start_second ( 1, simulation_start_second )
        CALL WRFU_TimeSet( simulationStartTime,       &
                           YY=simulation_start_year,  &
                           MM=simulation_start_month, &
                           DD=simulation_start_day,   &
                           H=simulation_start_hour,   &
                           M=simulation_start_minute, &
                           S=simulation_start_second, &
                           rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL nl_get_start_year   ( 1, simulation_start_year   )
          CALL nl_get_start_month  ( 1, simulation_start_month  )
          CALL nl_get_start_day    ( 1, simulation_start_day    )
          CALL nl_get_start_hour   ( 1, simulation_start_hour   )
          CALL nl_get_start_minute ( 1, simulation_start_minute )
          CALL nl_get_start_second ( 1, simulation_start_second )
          CALL wrf_debug( 150, "WARNING:  domain_get_sim_start_time using head_grid start time from namelist" )
          CALL WRFU_TimeSet( simulationStartTime,       &
                             YY=simulation_start_year,  &
                             MM=simulation_start_month, &
                             DD=simulation_start_day,   &
                             H=simulation_start_hour,   &
                             M=simulation_start_minute, &
                             S=simulation_start_second, &
                             rc=rc )
        ENDIF
        RETURN
      END FUNCTION domain_get_sim_start_time

      FUNCTION domain_get_time_since_sim_start ( grid ) RESULT ( time_since_sim_start ) 
        IMPLICIT NONE









        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_since_sim_start
        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_simstarttime
        lcl_simstarttime = domain_get_sim_start_time( grid )
        lcl_currtime = domain_get_current_time ( grid )
        time_since_sim_start = lcl_currtime - lcl_simstarttime
      END FUNCTION domain_get_time_since_sim_start




      SUBROUTINE domain_clock_get( grid, current_time,                &
                                         current_timestr,             &
                                         current_timestr_frac,        &
                                         start_time, start_timestr,   &
                                         stop_time, stop_timestr,     &
                                         time_step, time_stepstr,     &
                                         time_stepstr_frac,           &
                                         advanceCount,                &
                                         currentDayOfYearReal,        &
                                         minutesSinceSimulationStart, &
                                         timeSinceSimulationStart,    &
                                         simulationStartTime,         &
                                         simulationStartTimeStr )
        IMPLICIT NONE
        TYPE(domain),            INTENT(IN)              :: grid
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: current_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr_frac
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: start_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: start_timestr
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: stop_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: stop_timestr
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: time_step
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr_frac
        INTEGER,                 INTENT(  OUT), OPTIONAL :: advanceCount
        
        
        REAL,                    INTENT(  OUT), OPTIONAL :: currentDayOfYearReal
        
        
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: simulationStartTime
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: simulationStartTimeStr
        
        
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: timeSinceSimulationStart
        
        REAL,                    INTENT(  OUT), OPTIONAL :: minutesSinceSimulationStart






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime, lcl_starttime
        TYPE(WRFU_Time) :: lcl_simulationStartTime
        TYPE(WRFU_TimeInterval) :: lcl_time_step, lcl_timeSinceSimulationStart
        INTEGER :: days, seconds, Sn, Sd, rc
        CHARACTER (LEN=256) :: tmp_str
        CHARACTER (LEN=256) :: frac_str
        REAL(WRFU_KIND_R8) :: currentDayOfYearR8
        IF ( PRESENT( start_time ) ) THEN
          start_time = domain_get_start_time ( grid )
        ENDIF
        IF ( PRESENT( start_timestr ) ) THEN
          lcl_starttime = domain_get_start_time ( grid )
          CALL wrf_timetoa ( lcl_starttime, start_timestr )
        ENDIF
        IF ( PRESENT( time_step ) ) THEN
          time_step = domain_get_time_step ( grid )
        ENDIF
        IF ( PRESENT( time_stepstr ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, &
                                     timeString=time_stepstr, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",16867,&
=======
            CALL wrf_error_fatal3("<stdin>",16917,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",16876,&
=======
            CALL wrf_error_fatal3("<stdin>",16926,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          time_stepstr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( advanceCount ) ) THEN
          advanceCount = domain_get_advanceCount ( grid )
        ENDIF
        
        
        
        
        
        
        IF ( PRESENT( current_time ) ) THEN
          current_time = domain_get_current_time ( grid )
        ENDIF
        IF ( PRESENT( current_timestr ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, current_timestr )
        ENDIF
        
        IF ( PRESENT( current_timestr_frac ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, tmp_str )
          CALL WRFU_TimeGet( lcl_currtime, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",16904,&
=======
            CALL wrf_error_fatal3("<stdin>",16954,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_get:  WRFU_TimeGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          current_timestr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( stop_time ) ) THEN
          stop_time = domain_get_stop_time ( grid )
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          lcl_stoptime = domain_get_stop_time ( grid )
          CALL wrf_timetoa ( lcl_stoptime, stop_timestr )
        ENDIF
        IF ( PRESENT( currentDayOfYearReal ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL WRFU_TimeGet( lcl_currtime, dayOfYear_r8=currentDayOfYearR8, &
                             rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",16922,&
=======
            CALL wrf_error_fatal3("<stdin>",16972,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
                   'domain_clock_get:  WRFU_TimeGet(dayOfYear_r8) failed' )
          ENDIF
          currentDayOfYearReal = REAL( currentDayOfYearR8 ) - 1.0
        ENDIF
        IF ( PRESENT( simulationStartTime ) ) THEN
          simulationStartTime = domain_get_sim_start_time( grid )
        ENDIF
        IF ( PRESENT( simulationStartTimeStr ) ) THEN
          lcl_simulationStartTime = domain_get_sim_start_time( grid )
          CALL wrf_timetoa ( lcl_simulationStartTime, simulationStartTimeStr )
        ENDIF
        IF ( PRESENT( timeSinceSimulationStart ) ) THEN
          timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
        ENDIF
        IF ( PRESENT( minutesSinceSimulationStart ) ) THEN
          lcl_timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
          CALL WRFU_TimeIntervalGet( lcl_timeSinceSimulationStart, &
                                     D=days, S=seconds, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",16942,&
=======
            CALL wrf_error_fatal3("<stdin>",16992,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
                   'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          
          minutesSinceSimulationStart = ( REAL( days ) * 24. * 60. ) + &
                                        ( REAL( seconds ) / 60. )
          IF ( Sd /= 0 ) THEN
            minutesSinceSimulationStart = minutesSinceSimulationStart + &
                                          ( ( REAL( Sn ) / REAL( Sd ) ) / 60. )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_get

      FUNCTION domain_clockisstarttime ( grid ) RESULT ( is_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_start_time
        TYPE(WRFU_Time) :: start_time, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     start_time=start_time )
        is_start_time = ( current_time == start_time )
      END FUNCTION domain_clockisstarttime

      FUNCTION domain_clockissimstarttime ( grid ) RESULT ( is_sim_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_sim_start_time
        TYPE(WRFU_Time) :: simulationStartTime, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     simulationStartTime=simulationStartTime )
        is_sim_start_time = ( current_time == simulationStartTime )
      END FUNCTION domain_clockissimstarttime




      SUBROUTINE domain_clock_create( grid, StartTime, &
                                            StopTime,  &
                                            TimeStep )
        IMPLICIT NONE
        TYPE(domain),            INTENT(INOUT) :: grid
        TYPE(WRFU_Time),         INTENT(IN   ) :: StartTime
        TYPE(WRFU_Time),         INTENT(IN   ) :: StopTime
        TYPE(WRFU_TimeInterval), INTENT(IN   ) :: TimeStep





        
        INTEGER :: rc
        grid%domain_clock = WRFU_ClockCreate( TimeStep= TimeStep,  &
                                              StartTime=StartTime, &
                                              StopTime= StopTime,  &
                                              rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",17011,&
=======
          CALL wrf_error_fatal3("<stdin>",17061,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_clock_create:  WRFU_ClockCreate() failed' )
        ENDIF
        grid%domain_clock_created = .TRUE.
        RETURN
      END SUBROUTINE domain_clock_create



      SUBROUTINE domain_alarm_create( grid, alarm_id, interval, &
                                            begin_time, end_time )
        USE module_utility
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid
        INTEGER, INTENT(IN) :: alarm_id
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: interval
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: begin_time
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: end_time





        
        INTEGER :: rc




        LOGICAL :: interval_only, all_args, no_args
        TYPE(WRFU_Time) :: startTime
        interval_only = .FALSE.
        all_args = .FALSE.
        no_args = .FALSE.
        IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
             ( .NOT. PRESENT( end_time   ) ) .AND. &
             (       PRESENT( interval   ) ) ) THEN
           interval_only = .TRUE.
        ELSE IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
                  ( .NOT. PRESENT( end_time   ) ) .AND. &
                  ( .NOT. PRESENT( interval   ) ) ) THEN
           no_args = .TRUE.
        ELSE IF ( (       PRESENT( begin_time ) ) .AND. &
                  (       PRESENT( end_time   ) ) .AND. &
                  (       PRESENT( interval   ) ) ) THEN
           all_args = .TRUE.
        ELSE
<<<<<<< HEAD
           CALL wrf_error_fatal3("<stdin>",17058,&
=======
           CALL wrf_error_fatal3("<stdin>",17108,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
             'ERROR in domain_alarm_create:  bad argument list' )
        ENDIF
        CALL domain_clock_get( grid, start_time=startTime )
        IF ( interval_only ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingInterval=interval,   &
                               rc=rc )
        ELSE IF ( no_args ) THEN
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingTime=startTime,      &
                               rc=rc )
        ELSE IF ( all_args ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock,         &
                               RingTime=startTime + begin_time, &
                               RingInterval=interval,           &
                               StopTime=startTime + end_time,   &
                               rc=rc )
        ENDIF
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",17083,&
=======
          CALL wrf_error_fatal3("<stdin>",17133,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_alarm_create:  WRFU_AlarmCreate() failed' )
        ENDIF
        CALL WRFU_AlarmRingerOff( grid%alarms( alarm_id ) , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",17088,&
=======
          CALL wrf_error_fatal3("<stdin>",17138,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_alarm_create:  WRFU_AlarmRingerOff() failed' )
        ENDIF
        grid%alarms_created( alarm_id ) = .TRUE.
      END SUBROUTINE domain_alarm_create



      SUBROUTINE domain_clock_set( grid, current_timestr, &
                                         stop_timestr,    &
                                         time_step_seconds )
        IMPLICIT NONE
        TYPE(domain),      INTENT(INOUT)           :: grid
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: current_timestr
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: stop_timestr
        INTEGER,           INTENT(IN   ), OPTIONAL :: time_step_seconds






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime
        TYPE(WRFU_TimeInterval) :: tmpTimeInterval
        INTEGER :: rc
        IF ( PRESENT( current_timestr ) ) THEN
          CALL wrf_atotime( current_timestr(1:19), lcl_currtime )
          CALL WRFU_ClockSet( grid%domain_clock, currTime=lcl_currtime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",17119,&
=======
            CALL wrf_error_fatal3("<stdin>",17169,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",17128,&
=======
            CALL wrf_error_fatal3("<stdin>",17178,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",17136,&
=======
            CALL wrf_error_fatal3("<stdin>",17186,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",17143,&
=======
            CALL wrf_error_fatal3("<stdin>",17193,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_clock_set:  WRFU_ClockSet(TimeStep) failed' )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_set


      
      
      SUBROUTINE domain_clockprint ( level, grid, pre_str )
        IMPLICIT NONE
        INTEGER,           INTENT( IN) :: level
        TYPE(domain),      INTENT( IN) :: grid
        CHARACTER (LEN=*), INTENT( IN) :: pre_str
        CALL wrf_clockprint ( level, grid%domain_clock, pre_str )
        RETURN
      END SUBROUTINE domain_clockprint


      
      
      SUBROUTINE domain_clockadvance ( grid )
        IMPLICIT NONE
        TYPE(domain), INTENT(INOUT) :: grid
        INTEGER :: rc
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  before WRFU_ClockAdvance,' )
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",17173,&
=======
          CALL wrf_error_fatal3("<stdin>",17223,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_clockadvance:  WRFU_ClockAdvance() failed' )
        ENDIF
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  after WRFU_ClockAdvance,' )
        
        
        CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
        CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
        RETURN
      END SUBROUTINE domain_clockadvance



      
      
      SUBROUTINE domain_setgmtetc ( grid, start_of_simulation )
        IMPLICIT NONE
        TYPE (domain), INTENT(INOUT) :: grid
        LOGICAL,       INTENT(  OUT) :: start_of_simulation
        
        CHARACTER (LEN=132)          :: message
        TYPE(WRFU_Time)              :: simStartTime
        INTEGER                      :: hr, mn, sec, ms, rc
        CALL domain_clockprint(150, grid, &
          'DEBUG domain_setgmtetc():  get simStartTime from clock,')
        CALL domain_clock_get( grid, simulationStartTime=simStartTime, &
                                     simulationStartTimeStr=message )
        CALL WRFU_TimeGet( simStartTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
          CALL wrf_error_fatal3("<stdin>",17204,&
=======
          CALL wrf_error_fatal3("<stdin>",17254,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
            'domain_setgmtetc:  WRFU_TimeGet() failed' )
        ENDIF
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  simulation start time = [',TRIM( message ),']'
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  julyr,hr,mn,sec,ms,julday = ', &
                                     grid%julyr,hr,mn,sec,ms,grid%julday
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        start_of_simulation = domain_ClockIsSimStartTime(grid)
        RETURN
      END SUBROUTINE domain_setgmtetc
     


      
      
      SUBROUTINE set_current_grid_ptr( grid_ptr )
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid_ptr






        current_grid_set = .TRUE.
        current_grid => grid_ptr

      END SUBROUTINE set_current_grid_ptr








      LOGICAL FUNCTION Is_alarm_tstep( grid_clock, alarm )

        IMPLICIT NONE

        TYPE (WRFU_Clock), INTENT(in)  :: grid_clock
        TYPE (WRFU_Alarm), INTENT(in)  :: alarm

        LOGICAL :: pred1, pred2, pred3

        Is_alarm_tstep = .FALSE.

        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              IF ( alarm%alarmint%StopTimeSet ) THEN
                 PRED1 = ( grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep > &
                      alarm%alarmint%StopTime )
              ENDIF
              IF ( alarm%alarmint%RingTimeSet ) THEN
                 PRED2 = ( ( alarm%alarmint%RingTime - &
                      grid_clock%clockint%TimeStep <= &
                      grid_clock%clockint%CurrTime )     &
                      .AND. ( grid_clock%clockint%CurrTime < alarm%alarmint%RingTime ) )
              ENDIF
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                 PRED3 = ( alarm%alarmint%PrevRingTime + &
                      alarm%alarmint%RingInterval <= &
                      grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep )
              ENDIF
              IF ( ( .NOT. ( pred1 ) ) .AND. &
                   ( ( pred2 ) .OR. ( pred3 ) ) ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
              IF ( alarm%alarmint%RingTime -&
                   grid_clock%clockint%TimeStep <= &
                   grid_clock%clockint%CurrTime ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      END FUNCTION Is_alarm_tstep










      
      SUBROUTINE domain_time_test_print ( pre_str, name_str, res_str )
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        CHARACTER (LEN=*), INTENT(IN) :: name_str
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=512) :: out_str
        WRITE (out_str,                                            &
          FMT="('DOMAIN_TIME_TEST ',A,':  ',A,' = ',A)") &
          TRIM(pre_str), TRIM(name_str), TRIM(res_str)
        CALL wrf_debug( 0, TRIM(out_str) )
      END SUBROUTINE domain_time_test_print

      
      SUBROUTINE test_adjust_io_timestr( TI_h, TI_m, TI_s, &
        CT_yy,  CT_mm,  CT_dd,  CT_h,  CT_m,  CT_s,        &
        ST_yy,  ST_mm,  ST_dd,  ST_h,  ST_m,  ST_s,        &
        res_str, testname )
        INTEGER, INTENT(IN) :: TI_H
        INTEGER, INTENT(IN) :: TI_M
        INTEGER, INTENT(IN) :: TI_S
        INTEGER, INTENT(IN) :: CT_YY
        INTEGER, INTENT(IN) :: CT_MM  
        INTEGER, INTENT(IN) :: CT_DD  
        INTEGER, INTENT(IN) :: CT_H
        INTEGER, INTENT(IN) :: CT_M
        INTEGER, INTENT(IN) :: CT_S
        INTEGER, INTENT(IN) :: ST_YY
        INTEGER, INTENT(IN) :: ST_MM  
        INTEGER, INTENT(IN) :: ST_DD  
        INTEGER, INTENT(IN) :: ST_H
        INTEGER, INTENT(IN) :: ST_M
        INTEGER, INTENT(IN) :: ST_S
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=*), INTENT(IN) :: testname
        
        TYPE(WRFU_TimeInterval) :: TI
        TYPE(WRFU_Time) :: CT, ST
        LOGICAL :: test_passed
        INTEGER :: rc
        CHARACTER(LEN=WRFU_MAXSTR) :: TI_str, CT_str, ST_str, computed_str
        
        CALL WRFU_TimeIntervalSet( TI, H=TI_H, M=TI_M, S=TI_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeIntervalSet() ', &
                              "module_domain.F" , &
                              2746  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2751  )
        
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2758  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2763  )
        
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2770  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2775  )
        
        CALL adjust_io_timestr ( TI, CT, ST, computed_str )
        
        test_passed = .FALSE.
        IF ( LEN_TRIM(res_str) == LEN_TRIM(computed_str) ) THEN
          IF ( res_str(1:LEN_TRIM(res_str)) == computed_str(1:LEN_TRIM(computed_str)) ) THEN
            test_passed = .TRUE.
          ENDIF
        ENDIF
        
        IF ( test_passed ) THEN
          WRITE(*,FMT='(A)') 'PASS:  '//TRIM(testname)
        ELSE
          WRITE(*,*) 'FAIL:  ',TRIM(testname),':  adjust_io_timestr(',    &
            TRIM(TI_str),',',TRIM(CT_str),',',TRIM(ST_str),')  expected <', &
            TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
        ENDIF
      END SUBROUTINE test_adjust_io_timestr

      
      
      
      
      
      SUBROUTINE domain_time_test ( grid, pre_str )
        IMPLICIT NONE
        TYPE(domain),      INTENT(IN) :: grid
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        
        LOGICAL, SAVE :: one_time_tests_done = .FALSE.
        REAL :: minutesSinceSimulationStart
        INTEGER :: advance_count, rc
        REAL :: currentDayOfYearReal
        TYPE(WRFU_TimeInterval) :: timeSinceSimulationStart
        TYPE(WRFU_Time) :: simulationStartTime
        CHARACTER (LEN=512) :: res_str
        LOGICAL :: self_test_domain
        
        
        
        
        
        
        CALL nl_get_self_test_domain( 1, self_test_domain )
        IF ( self_test_domain ) THEN
          CALL domain_clock_get( grid, advanceCount=advance_count )
          WRITE ( res_str, FMT="(I8.8)" ) advance_count
          CALL domain_time_test_print( pre_str, 'advanceCount', res_str )
          CALL domain_clock_get( grid, currentDayOfYearReal=currentDayOfYearReal )
          WRITE ( res_str, FMT='(F10.6)' ) currentDayOfYearReal
          CALL domain_time_test_print( pre_str, 'currentDayOfYearReal', res_str )
          CALL domain_clock_get( grid, minutesSinceSimulationStart=minutesSinceSimulationStart )
          WRITE ( res_str, FMT='(F10.6)' ) minutesSinceSimulationStart
          CALL domain_time_test_print( pre_str, 'minutesSinceSimulationStart', res_str )
          CALL domain_clock_get( grid, current_timestr=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr', res_str )
          CALL domain_clock_get( grid, current_timestr_frac=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr_frac', res_str )
          CALL domain_clock_get( grid, timeSinceSimulationStart=timeSinceSimulationStart )
          CALL WRFU_TimeIntervalGet( timeSinceSimulationStart, timeString=res_str, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
<<<<<<< HEAD
            CALL wrf_error_fatal3("<stdin>",17436,&
=======
            CALL wrf_error_fatal3("<stdin>",17487,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
              'domain_time_test:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL domain_time_test_print( pre_str, 'timeSinceSimulationStart', res_str )
          
          
          IF ( .NOT. one_time_tests_done ) THEN
            one_time_tests_done = .TRUE.
            CALL domain_clock_get( grid, simulationStartTimeStr=res_str )
            CALL domain_time_test_print( pre_str, 'simulationStartTime', res_str )
            CALL domain_clock_get( grid, start_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'start_timestr', res_str )
            CALL domain_clock_get( grid, stop_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'stop_timestr', res_str )
            CALL domain_clock_get( grid, time_stepstr=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr', res_str )
            CALL domain_clock_get( grid, time_stepstr_frac=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr_frac', res_str )
            
            
            
            
            
            
            CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
              CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
              ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
              res_str='2000-01-26_00:00:00', testname='adjust_io_timestr_1' )
            
            
            
            
            
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_time_test






END MODULE module_domain









SUBROUTINE get_current_time_string( time_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: time_str
  
  INTEGER :: debug_level_lcl

  time_str = ''
  IF ( current_grid_set ) THEN








    IF ( current_grid%time_set ) THEN

      
      CALL get_wrf_debug_level( debug_level_lcl )
      CALL set_wrf_debug_level ( 0 )
      current_grid_set = .FALSE.
      CALL domain_clock_get( current_grid, current_timestr_frac=time_str )
      
      CALL set_wrf_debug_level ( debug_level_lcl )
      current_grid_set = .TRUE.

    ENDIF
  ENDIF

END SUBROUTINE get_current_time_string






SUBROUTINE get_current_grid_name( grid_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
  IF ( current_grid_set ) THEN
    WRITE(grid_str,FMT="('d',I2.2)") current_grid%id
  ENDIF
END SUBROUTINE get_current_grid_name




   SUBROUTINE get_ijk_from_grid_ext (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid_ext




   SUBROUTINE get_ijk_from_subgrid_ext (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid_ext


   SUBROUTINE get_dims_from_grid_id (  id   &
                          ,ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, DIMENSION(3), INTENT(INOUT) ::                   &
                           ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye

     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
           ds(1) = grid%sd31 ; de(1) = grid%ed31 ; ds(2) = grid%sd32 ; de(2) = grid%ed32 ; ds(3) = grid%sd33 ; de(3) = grid%ed33 ;
           ms(1) = grid%sm31 ; me(1) = grid%em31 ; ms(2) = grid%sm32 ; me(2) = grid%em32 ; ms(3) = grid%sm33 ; me(3) = grid%em33 ;
           ps(1) = grid%sp31 ; pe(1) = grid%ep31 ; ps(2) = grid%sp32 ; pe(2) = grid%ep32 ; ps(3) = grid%sp33 ; pe(3) = grid%ep33 ;
           mxs(1) = grid%sm31x ; mxe(1) = grid%em31x 
           mxs(2) = grid%sm32x ; mxe(2) = grid%em32x 
           mxs(3) = grid%sm33x ; mxe(3) = grid%em33x 
           pxs(1) = grid%sp31x ; pxe(1) = grid%ep31x 
           pxs(2) = grid%sp32x ; pxe(2) = grid%ep32x 
           pxs(3) = grid%sp33x ; pxe(3) = grid%ep33x
           mys(1) = grid%sm31y ; mye(1) = grid%em31y 
           mys(2) = grid%sm32y ; mye(2) = grid%em32y 
           mys(3) = grid%sm33y ; mye(3) = grid%em33y 
           pys(1) = grid%sp31y ; pye(1) = grid%ep31y 
           pys(2) = grid%sp32y ; pye(2) = grid%ep32y 
           pys(3) = grid%sp33y ; pye(3) = grid%ep33y 
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
<<<<<<< HEAD
        CALL wrf_error_fatal3("<stdin>",17690,&
=======
        CALL wrf_error_fatal3("<stdin>",17741,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
TRIM(mess))
     ENDIF

   END SUBROUTINE get_dims_from_grid_id


   SUBROUTINE get_ijk_from_grid_id (  id ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id, get_ijk_from_grid
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey
     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
     CALL get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
<<<<<<< HEAD
        CALL wrf_error_fatal3("<stdin>",17734,&
=======
        CALL wrf_error_fatal3("<stdin>",17785,&
>>>>>>> 209d9cb64a4bc3351e19abdb062e8a77bdf546aa
TRIM(mess))
     ENDIF

   END SUBROUTINE get_ijk_from_grid_id



   SUBROUTINE modify_io_masks ( id )
     USE module_domain, ONLY : domain, modify_io_masks1, head_grid, find_grid_by_id
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: id
     TYPE(domain), POINTER :: grid
     CALL find_grid_by_id( id, head_grid, grid )
     IF ( ASSOCIATED( grid ) ) CALL modify_io_masks1( grid, id ) 
     RETURN 
   END SUBROUTINE modify_io_masks

