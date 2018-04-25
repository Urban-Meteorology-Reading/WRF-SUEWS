!***********************************************************************
!  DESCRIPTION:
!       Module to convert USGS/MODIS Land Use Categories to SUEWS surface
!       types
!
!  REVISION  HISTORY:
!       Prototype 04/2018 by Li Zhenkun, SCC
!
!***********************************************************************
  module WRFcat2SUEWScat
     
     implicit none

     public   ::  USGScat2SUEWScat
     public   ::  MODIScat2SUEWScat

     contains

!----------------------------------------------------------------------
!    Public subroutine to convert USGS Land Use Categories to SUEWS
!    surface types
!----------------------------------------------------------------------
     subroutine USGScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, NLCAT_suews, landuse_suews)

        implicit none

!------------------------------------------------------------------
!       dummy arguments
!------------------------------------------------------------------
        integer, intent(in)                ::  ims, ime, NLCAT, jms, jme
        real, intent(in)                   ::  landusef(ims:ime, NLCAT, jms:jme)
        integer, intent(in)                ::  NLCAT_suews
        real, intent(out)                  ::  landuse_suews(ims:ime, NLCAT_suews, jms:jme)

!------------------------------------------------------------------
!       local variables
!------------------------------------------------------------------
        integer                            ::  i, j, k

        do i = ims, ime
           do j = jms, jme
              landuse_suews(i, :, j) = 0.
              do k = 1, NLCAT

                 select case( landusef(i, k, l) )
                 case( 1 )
                    landuse_suews(i, 1, j) = landuse_suews(i, 1, j) + landusef(i, k, l) * 0.5
                    landuse_suews(i, 2, j) = landuse_suews(i, 2, j) + landusef(i, k, l) * 0.5
                 case( 2:10 )
                    landuse_suews(i, 5, j) = landuse_suews(i, 5, j) + landusef(i, k, l)
                 case( 11, 12 )
                    landuse_suews(i, 4, j) = landuse_suews(i, 4, j) + landusef(i, k, l)
                 case( 13:15 )
                    landuse_suews(i, 3, j) = landuse_suews(i, 3, j) + landusef(i, k, l)
                 case( 16:18 )
                    landuse_suews(i, 7, j) = landuse_suews(i, 7, j) + landusef(i, k, l)
                 case( 19:23 )
                    landuse_suews(i, 6, j) = landuse_suews(i, 6, j) + landusef(i, k, l)
                 case( 24 )
                    !TODO
                 end select

              end do
           end do
        end do

     end subroutine USGScat2SUEWScat

!----------------------------------------------------------------------
!    Public subroutine to convert MODIS Land Use Categories to SUEWS
!    surface types
!----------------------------------------------------------------------
     subroutine MODIScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, NLCAT_suews, landuse_suews)

        implicit none

!------------------------------------------------------------------
!       dummy arguments
!------------------------------------------------------------------
        integer, intent(in)                ::  ims, ime, NLCAT, jms, jme
        real, intent(in)                   ::  landusef(ims:ime, NLCAT, jms:jme)
        integer, intent(in)                ::  NLCAT_suews
        real, intent(out)                  ::  landuse_suews(ims:ime, NLCAT_suews, jms:jme)

!------------------------------------------------------------------
!       local variables
!------------------------------------------------------------------
        integer                            ::  i, j, k

        do i = ims, ime
           do j = jms, jme
              landuse_suews(i, :, j) = 0.
              do k = 1, NLCAT

                 select case( landusef(i, k, l) )
                 case( 1, 2, 5 )
                    landuse_suews(i, 3, j) = landuse_suews(i, 3, j) + landusef(i, k, l)
                 case( 3:4 )
                    landuse_suews(i, 4, j) = landuse_suews(i, 4, j) + landusef(i, k, l)
                 case( 6:10 )
                    landuse_suews(i, 5, j) = landuse_suews(i, 5, j) + landusef(i, k, l)
                 case( 11, 17 )
                    landuse_suews(i, 7, j) = landuse_suews(i, 7, j) + landusef(i, k, l)
                 case( 12, 14 )
                    landuse_suews(i, 5, j) = landuse_suews(i, 5, j) + landusef(i, k, l)
                 case( 13 )
                    landuse_suews(i, 1, j) = landuse_suews(i, 1, j) + landusef(i, k, l) * 0.5
                    landuse_suews(i, 2, j) = landuse_suews(i, 2, j) + landusef(i, k, l) * 0.5
                 case( 15 )
                    !TODO
                 case( 16 )
                    landuse_suews(i, 6, j) = landuse_suews(i, 6, j) + landusef(i, k, l)
                 case( 18:20 )
                    landuse_suews(i, 6, j) = landuse_suews(i, 6, j) + landusef(i, k, l)
                 
                 end select

              end do
           end do
        end do

     end subroutine MODIScat2SUEWScat

  end module WRFcat2SUEWScat