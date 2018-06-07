


MODULE module_machine

   USE module_driver_constants

   

   
   INTEGER, PARAMETER :: TILE_NONE = 0, TILE_X = 1, TILE_Y = 2, TILE_XY = 3

   CONTAINS

   RECURSIVE SUBROUTINE rlocproc(p,maxi,nproc,ml,mr,ret)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: p, maxi, nproc, ml, mr
   INTEGER, INTENT(OUT) :: ret
   INTEGER              :: width, rem, ret2, bl, br, mid, adjust, &
                           p_r, maxi_r, nproc_r, zero
   adjust = 0
   rem = mod( maxi, nproc )
   width = maxi / nproc
   mid = maxi / 2
   IF ( rem>0 .AND. (((mod(rem,2).EQ.0).OR.(rem.GT.2)).OR.(p.LE.mid))) THEN
     width = width + 1
   END IF
   IF ( p.LE.mid .AND. mod(rem,2).NE.0 ) THEN
     adjust = adjust + 1
   END IF
   bl = max(width,ml) ;
   br = max(width,mr) ;
   IF      (p<bl) THEN
     ret = 0
   ELSE IF (p>maxi-br-1) THEN
     ret = nproc-1
   ELSE
     p_r = p - bl
     maxi_r = maxi-bl-br+adjust
     nproc_r = max(nproc-2,1)
     zero = 0
     CALL rlocproc( p_r, maxi_r, nproc_r, zero, zero, ret2 )  
     ret = ret2 + 1
   END IF
   RETURN
   END SUBROUTINE rlocproc

   INTEGER FUNCTION locproc( i, m, numpart )
   implicit none
   integer, intent(in) :: i, m, numpart 
   integer             :: retval, ii, im, inumpart, zero
   ii = i
   im = m
   inumpart = numpart
   zero = 0
   CALL rlocproc( ii, im, inumpart, zero, zero, retval )
   locproc = retval
   RETURN
   END FUNCTION locproc

   SUBROUTINE patchmap( res, y, x, py, px )
   implicit none
   INTEGER, INTENT(IN)                    :: y, x, py, px
   INTEGER, DIMENSION(x,y), INTENT(OUT)   :: res
   INTEGER                                :: i, j, p_min, p_maj
   DO j = 0,y-1
     p_maj = locproc( j, y, py )
     DO i = 0,x-1
       p_min = locproc( i, x, px )
       res(i+1,j+1) = p_min + px*p_maj
     END DO
   END DO
   RETURN
   END SUBROUTINE patchmap

   SUBROUTINE region_bounds( region_start, region_end, &
                             num_p, p,                 &
                             patch_start, patch_end )
   
   
   
   
   
   
   
   
   
   
                      
   IMPLICIT NONE
   INTEGER, INTENT(IN)                    :: region_start, region_end, num_p, p
   INTEGER, INTENT(OUT)                   :: patch_start, patch_end
   INTEGER                                :: offset, i
   patch_end = -999999999
   patch_start = 999999999
   offset = region_start
   do i = 0, region_end - offset
     if ( locproc( i, region_end-region_start+1, num_p ) == p ) then
       patch_end = max(patch_end,i)
       patch_start = min(patch_start,i)
     endif
   enddo
   patch_start = patch_start + offset
   patch_end   = patch_end   + offset
   RETURN
   END SUBROUTINE region_bounds

   SUBROUTINE least_aspect( nparts, minparts_y, minparts_x, nparts_y, nparts_x )
   IMPLICIT NONE
   
   INTEGER, INTENT(IN)           :: nparts,                &
                                    minparts_y, minparts_x
   
   INTEGER, INTENT(OUT)          :: nparts_y, nparts_x
   
   INTEGER                       :: x, y, mini
   mini = 2*nparts
   nparts_y = 1
   nparts_x = nparts
   DO y = 1, nparts
      IF ( mod( nparts, y ) .eq. 0 ) THEN
         x = nparts / y
         IF (       abs( y-x ) .LT. mini       &
              .AND. y .GE. minparts_y                &
              .AND. x .GE. minparts_x    ) THEN
            mini = abs( y-x )
            nparts_y = y
            nparts_x = x
         END IF
      END IF
   END DO
   END SUBROUTINE least_aspect

   SUBROUTINE init_module_machine
     RETURN
   END SUBROUTINE init_module_machine

END MODULE module_machine

SUBROUTINE wrf_sizeof_integer( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_integer

SUBROUTINE wrf_sizeof_real( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_real

SUBROUTINE wrf_sizeof_doubleprecision( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 8
  RETURN
END SUBROUTINE wrf_sizeof_doubleprecision

SUBROUTINE wrf_sizeof_logical( retval )
  IMPLICIT NONE
  INTEGER retval

  retval = 4
  RETURN
END SUBROUTINE wrf_sizeof_logical

