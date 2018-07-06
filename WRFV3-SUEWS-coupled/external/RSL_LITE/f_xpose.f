      subroutine trans_z2x ( np, comm, dir, r_wordsize, i_wordsize, memorder, &
                               a, &
                               sd1, ed1, sd2, ed2, sd3, ed3, & 
                               sp1, ep1, sp2, ep2, sp3, ep3, & 
                               sm1, em1, sm2, em2, sm3, em3, & 
                               ax, &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, & 
                               sm1x, em1x, sm2x, em2x, sm3x, em3x )
         USE duplicate_of_driver_constants
         implicit none
         integer, intent(in) :: sd1, ed1, sd2, ed2, sd3, ed3, & 
                                sp1, ep1, sp2, ep2, sp3, ep3, & 
                                sm1, em1, sm2, em2, sm3, em3, & 
                                sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, & 
                                sm1x, em1x, sm2x, em2x, sm3x, em3x
         integer, intent(in) :: np, comm, r_wordsize, i_wordsize
         integer, intent(in) :: dir ! 1 is a->ax, otherwise ax->a
         integer, intent(in) :: memorder
         integer, dimension((ep1-sp1+1)*(ep2-sp2+1)*(ep3-sp3+1)*max(1,(r_wordsize/i_wordsize)))         :: a
         integer, dimension((ep1x-sp1x+1)*(ep2x-ep2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize)))   :: ax

         return
      end subroutine trans_z2x

      subroutine trans_x2y ( np, comm, dir, r_wordsize, i_wordsize, memorder, &
                               ax, &
                               sd1, ed1, sd2, ed2, sd3, ed3, &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                               ay, &
                               sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                               sm1y, em1y, sm2y, em2y, sm3y, em3y )
         USE duplicate_of_driver_constants
         implicit none
         integer, intent(in) :: memorder
         integer, intent(in) ::  sd1, ed1, sd2, ed2, sd3, ed3, &
                                 sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                                 sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                                 sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                                 sm1y, em1y, sm2y, em2y, sm3y, em3y

         integer, intent(in) :: np, comm, r_wordsize, i_wordsize
         integer, intent(in) :: dir ! 1 is a->ax, otherwise ax->a
         integer, dimension((ep1x-sp1x+1)*(ep2x-ep2x+1)*(ep3x-sp3x+1)*max(1,(r_wordsize/i_wordsize)))   :: ax
         integer, dimension((ep1y-sp1y+1)*(ep2y-sp2y+1)*(ep3y-sp3y+1)*max(1,(r_wordsize/i_wordsize)))   :: ay

         return
      end subroutine trans_x2y


