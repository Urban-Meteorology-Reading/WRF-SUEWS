






module module_fr_fire_util









integer,save::          &
 fire_print_msg=1,      &  
 fire_print_file=1,     &  
 fuel_left_method=1,    &  
 fuel_left_irl=2,       &  
 fuel_left_jrl=2,       &
 boundary_guard=-1,     &  
 fire_grows_only=1,     &  
 fire_upwinding=3,      &  
 fire_upwind_split=0,   &  
 fire_test_steps=0,     &  
 fire_topo_from_atm=1,  &  
 fire_advection=0          
 

real, save::            &
 fire_const_time=-1,    &  
 fire_const_grnhfx=-1., &  
 fire_const_grnqfx=-1., &  
 fire_atm_feedback=1. , &  
 fire_back_weight=0.5,  &  
 fire_viscosity=0.4,    &  
 fire_lfn_ext_up=1.        

integer, parameter:: REAL_SUM=10, REAL_MAX=20, RNRM_SUM=30, RNRM_MAX=40

contains




subroutine crash(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='crash: '//s
call message(msg,level=0)
!$OMP CRITICAL(FIRE_MESSAGE_CRIT)
call wrf_error_fatal3("<stdin>",57,&
msg)
!$OMP END CRITICAL(FIRE_MESSAGE_CRIT)
end subroutine crash





subroutine message(s,level)
use module_wrf_error
implicit none

character(len=*), intent(in)::s
integer,intent(in),optional::level

character(len=128)::msg
character(len=118)::t
integer m,mlevel
logical op

if(present(level))then
    mlevel=level
else
    mlevel=2  
endif
if(fire_print_msg.ge.mlevel)then
      m=0
!$OMP CRITICAL(FIRE_MESSAGE_CRIT)
      msg='FIRE:'//s
      call wrf_message(msg)


!$OMP END CRITICAL(FIRE_MESSAGE_CRIT)
endif
end subroutine message






integer function open_text_file(filename,rw)
implicit none
character(len=*),intent(in):: filename,rw
character(len=128):: msg
character(len=1)::act
integer::iounit,ierr
logical::op


    do iounit=19,99
       inquire(iounit,opened=op)
       if(.not.op)goto 1
    enddo
    call crash('open_text_file: Cannot find any available I/O unit')
1   continue
    act=rw(1:1)
    select case (act)
        case ('r','R')
            OPEN(iounit, FILE=filename,FORM='FORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=ierr)
        case ('w','W')
            OPEN(iounit, FILE=filename,FORM='FORMATTED',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ierr)
        case default
            write(msg,*)'open_text_file: bad mode ',trim(rw),' for file ',trim(filename)
    end select
    
    if(ierr.ne.0)then 
	write(msg,*)'open_text_file: Cannot open file ',filename
        call crash(msg)
    endif
    open_text_file=iounit

end function open_text_file






subroutine set_ideal_coord( dxf,dyf, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                fxlong,fxlat          &
            )
implicit none

real, intent(in)::dxf,dyf
integer, intent(in):: &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte
real, intent(out),dimension(ifms:ifme,jfms:jfme)::fxlong,fxlat

integer::i,j
                
                do j=jfts,jfte
                    do i=ifts,ifte
                        
                        fxlong(i,j)=(i-ifds+0.5)*dxf
                        fxlat (i,j)=(j-jfds+0.5)*dyf
                    enddo
                enddo
end subroutine set_ideal_coord






subroutine continue_at_boundary(ix,iy,bias, & 
    ims,ime,jms,jme, &                
    ids,ide,jds,jde, &                
    ips,ipe,jps,jpe, &                
    its,ite,jts,jte, &                
    itso,iteo,jtso,jteo, &            
    lfn)                              
implicit none



integer, intent(in)::ix,iy              
real,intent(in)::bias                   
integer, intent(in)::ims,ime,jms,jme, &                
    ids,ide,jds,jde, &                
    ips,ipe,jps,jpe, &                
    its,ite,jts,jte                   
integer, intent(out)::itso,jtso,iteo,jteo 
real,intent(inout),dimension(ims:ime,jms:jme)::lfn

integer i,j
character(len=128)::msg
integer::its1,ite1,jts1,jte1
integer,parameter::halo=1           


call check_mesh_2dim(its-1,ite+1,jts-1,jte+1,ims,ime,jms,jme)

itso=its
jtso=jts
iteo=ite
jteo=jte





its1=its
jts1=jts
ite1=ite
jte1=jte
if(its.eq.ips.and..not.its.eq.ids)its1=its-halo
if(jts.eq.jps.and..not.jts.eq.jds)jts1=jts-halo
if(ite.eq.ipe.and..not.ite.eq.ide)ite1=ite+halo
if(jte.eq.jpe.and..not.jte.eq.jde)jte1=jte+halo
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,'(a,2i5,a,f5.2)')'continue_at_boundary: directions',ix,iy,' bias ',bias 
call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
if(ix.ne.0)then
    if(its.eq.ids)then
        do j=jts1,jte1
            lfn(ids-1,j)=EX(lfn(ids,j),lfn(ids+1,j))
        enddo
        itso=ids-1
    endif
    if(ite.eq.ide)then
        do j=jts1,jte1
            lfn(ide+1,j)=EX(lfn(ide,j),lfn(ide-1,j))
        enddo
        iteo=ide+1
    endif
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,'(8(a,i5))')'continue_at_boundary: x:',its,':',ite,',',jts,':',jte,' ->',itso,':',iteo,',',jts1,':',jte1
    call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
endif
if(iy.ne.0)then
    if(jts.eq.jds)then
        do i=its1,ite1
            lfn(i,jds-1)=EX(lfn(i,jds),lfn(i,jds+1))
        enddo
        jtso=jds-1
    endif
    if(jte.eq.jde)then
        do i=its1,ite1
            lfn(i,jde+1)=EX(lfn(i,jde),lfn(i,jde-1))
        enddo
        jteo=jde+1
    endif
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,'(8(a,i5))')'continue_at_boundary: y:',its,':',ite,',',jts,':',jte,' ->',its1,':',ite1,',',jtso,':',jteo
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
    call message(msg)
endif

if(ix.ne.0.and.iy.ne.0)then
    if(its.eq.ids.and.jts.eq.jds)lfn(ids-1,jds-1)=EX(lfn(ids,jds),lfn(ids+1,jds+1))
    if(its.eq.ids.and.jte.eq.jde)lfn(ids-1,jde+1)=EX(lfn(ids,jde),lfn(ids+1,jde-1))
    if(ite.eq.ide.and.jts.eq.jds)lfn(ide+1,jds-1)=EX(lfn(ide,jds),lfn(ide-1,jds+1))
    if(ite.eq.ide.and.jte.eq.jde)lfn(ide+1,jde+1)=EX(lfn(ide,jde),lfn(ide-1,jde-1))
endif
return
contains
real function EX(a,b)

real a,b
EX=(1.-bias)*(2.*a-b)+bias*max(2.*a-b,a,b)   
end function EX
end subroutine continue_at_boundary





subroutine check_mesh_2dim(ids,ide,jds,jde,ims,ime,jms,jme)
implicit none
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
character(len=128)msg
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme)then
!$OMP CRITICAL(FIRE_UTIL_CRIT)
    write(msg,*)'mesh dimensions:  ',ids,ide,jds,jde
    call message(msg)
    write(msg,*)'memory dimensions:',ims,ime,jms,jme
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
    call message(msg)
    call crash('check_mesh_2dim: memory dimensions too small')
endif
end subroutine check_mesh_2dim





subroutine check_mesh_3dim(ids,ide,kds,kde,jds,jde,ims,ime,kms,kme,jms,jme)
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme,kds,kde,kms,kme
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme.or.kds<kms.or.kde>kme) then
    call crash('memory dimensions too small')
endif
end subroutine check_mesh_3dim





subroutine sum_2d_cells(       &
       ims2,ime2,jms2,jme2,    &
       its2,ite2,jts2,jte2,    &
       v2,                     &  
       ims1,ime1,jms1,jme1,    &
       its1,ite1,jts1,jte1,    &
       v1)                        
implicit none







integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)


integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff,ibase,jbase
real t
character(len=128)msg




call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)


isz1 = ite1-its1+1
jsz1 = jte1-jts1+1
isz2 = ite2-its2+1
jsz2 = jte2-jts2+1


if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)then
    call message('all mesh sizes must be positive')
    goto 9
endif


ir=isz2/isz1
jr=jsz2/jsz1

if(isz2.ne.isz1*ir .or. jsz2.ne.jsz1*jr)then
    call message('input mesh size must be multiple of output mesh size')
    goto 9
endif



do j1=jts1,jte1
    jbase=jts2+jr*(j1-jts1)
    do i1=its1,ite1
       ibase=its2+ir*(i1-its1)
       t=0.
       do joff=0,jr-1
           j2=joff+jbase
           do ioff=0,ir-1
               i2=ioff+ibase
               t=t+v2(i2,j2)
           enddo
       enddo
       v1(i1,j1)=t
    enddo
enddo

return

9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')

end subroutine sum_2d_cells




subroutine interpolate_2d(  &
    ims2,ime2,jms2,jme2, & 
    its2,ite2,jts2,jte2, & 
    ims1,ime1,jms1,jme1, & 
    its1,ite1,jts1,jte1, & 
    ir,jr,               & 
    rip2,rjp2,rip1,rjp1, & 
    v2, &                  
    v1  )                  
implicit none








integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
integer, intent(in)::ir,jr
real,intent(in):: rjp1,rip1,rjp2,rip2
real, intent(out)::v1(ims1:ime1,jms1:jme1)
real, intent(in)::v2(ims2:ime2,jms2:jme2)


integer:: i1,i2,j1,j2,is,ie,js,je
real:: tx,ty,rx,ry
real:: rio,rjo
intrinsic::ceiling,floor




call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)


rx=1./ir
ry=1./jr

do j2=jts2,jte2-1               
    rjo=rjp1+jr*(j2-rjp2)       
    js=max(jts1,ceiling(rjo))   
    je=min(jte1,floor(rjo)+jr)  
    do i2=its2,ite2-1
        rio=rip1+ir*(i2-rip2)
        is=max(its1,ceiling(rio))
        ie=min(ite1,floor(rio)+ir)
        do j1=js,je
            ty = (j1-rjo)*ry
            do i1=is,ie
                
                
                
                tx = (i1-rio)*rx

                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  





           enddo
       enddo
    enddo
enddo

end subroutine interpolate_2d





subroutine interpolate_2d_cells2cells(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  
implicit none







integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)








integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg




call check_mesh_2dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)


isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1


if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9


ir=isz1/isz2
jr=jsz1/jsz2




















ih=ir/2
jh=jr/2

ip=mod(ir+1,2)
jp=mod(jr+1,2)

call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  

return

9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_fire_util:interpolate_2dmesh_cells: bad mesh sizes")
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine interpolate_2d_cells2cells





subroutine interpolate_2d_cells2nodes(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  
implicit none







integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)








integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg




call check_mesh_2dim(ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)


isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1


if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9


ir=isz1/isz2
jr=jsz1/jsz2











ih=(ir+1)/2
jh=(jr+1)/2

ip=mod(ir,2)
jp=mod(jr,2)


call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  
      ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1,v1  )  


return
9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_fire_util:interpolate_2d_cells2nodes: bad mesh sizes")
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine interpolate_2d_cells2nodes




subroutine interpolate_2d_w(ip,jp,ih,jh,ir,jr,             &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  
implicit none


integer, intent(in)::ip,jp,ih,jh,ir,jr
integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

real:: tx,ty,rx,ry,half,xoff,yoff
integer:: i1,i2,j1,j2,ioff,joff
parameter(half=0.5)

rx=ir
ry=jr

xoff = ip*half
yoff = jp*half


do j2=jds2,jde2-1     
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
            do joff=0,jr-jp
                
                i1=ioff+(ih+ids1)+ir*(i2-ids2)
                j1=joff+(jh+jds1)+jr*(j2-jds2)
                
                tx = (ioff+xoff)/rx
                ty = (joff+yoff)/ry
                
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  




           enddo
       enddo
    enddo
enddo


do ioff=0,ih-1  
    do j2=jds2,jde2-1
        do joff=0,jr-jp
           j1=joff+(jh+jds1)+jr*(j2-jds2)
           
           ty = (joff+yoff)/ry
           
           v1(ids1+ioff,j1)=(1-ty)*v2(ids2,j2)+ty*v2(ids2,j2+1)
           v1(ide1-ioff,j1)=(1-ty)*v2(ide2,j2)+ty*v2(ide2,j2+1)
       enddo
    enddo
enddo
do joff=0,jh-1  
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
           i1=ioff+(ih+ids1)+ir*(i2-ids2)
           
           tx = (ioff+xoff)/rx
           
           v1(i1,jds1+joff)=(1-tx)*v2(i2,jds2)+tx*v2(i2+1,jds2)
           v1(i1,jde1-joff)=(1-tx)*v2(i2,jde2)+tx*v2(i2+1,jde2)
       enddo
    enddo
enddo

do ioff=0,ih-1  
    do joff=0,jh-1
        v1(ids1+ioff,jds1+joff)=v2(ids2,jds2)
        v1(ide1-ioff,jds1+joff)=v2(ide2,jds2)
        v1(ids1+ioff,jde1-joff)=v2(ids2,jde2)
        v1(ide1-ioff,jde1-joff)=v2(ide2,jde2)
    enddo
enddo         
end subroutine interpolate_2d_w  




                
real function interp(ids,ide,jds,jde,ims,ime,jms,jme,x,y,v)
implicit none





integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in)::x,y,v(ims:ime,jms:jme)



intrinsic floor,min,max


integer i,j
real tx,ty




i = floor(x)
i=max(min(i,ide),ids)
j = floor(y)
j=max(min(j,jde),jds)


tx = x - real(i)
ty = y - real(j)


interp = &
                    (1-tx)*(1-ty)*v(i,j)    &
                 +    tx*(1-ty)  *v(i+1,j)  &
                 +    (1-tx)*ty  *v(i,j+1)  &
                 +        tx*ty  *v(i+1,j+1)  


end function interp

subroutine meshdiffc_2d(ids, ide, jds,jde , &    
                   ims1,ime1,jms1,jme1, &       
                   dx,dy,               &       
                   lfn,                 &       
                   diffCx,diffCy) 
implicit none






integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffCx,diffCy


integer:: i,j
real, dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy


call meshdiff_2d(ids, ide, jds,jde , &    
                   ims1,ime1,jms1,jme1, &       
                   dx,dy,               &       
                   lfn,                 &       
                   diffLx,diffRx,diffLy,diffRy) 


do j=jds,jde+1
    do i=ids,ide+1
        diffCx(i,j)=0.5*(diffLx(i,j) + diffRx(i,j))
        diffCy(i,j)=0.5*(diffLy(i,j) + diffRy(i,j))
    enddo
enddo
end subroutine meshdiffc_2d 

subroutine meshdiff_2d(ids, ide, jds,jde , &    
                   ims1,ime1,jms1,jme1, &       
                   dx,dy,               &       
                   lfn,                 &       
                   diffLx,diffRx,diffLy,diffRy) 
implicit none






integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy


integer:: i,j
real:: tmpx,tmpy



    call check_mesh_2dim(ids,ide+1,jds,jde+1,ims1,ime1,jms1,jme1)
  
    
    do j=jds,jde
        do i=ids,ide
            tmpx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx(i+1,j) = tmpx
            diffRx(i,j)   = tmpx
            tmpy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy(i,j+1) = tmpy
            diffRy(i,j)   = tmpy
        enddo
        
        diffLx(ids,j)  = diffLx(ids+1,j)
        diffRx(ide+1,j)= diffRx(ide,j)
    enddo
    
    
    do i=ids,ide
        tmpx = (lfn(i+1,j)-lfn(i,j))/dx
        diffLx(i+1,j) = tmpx
        diffRx(i,j)   = tmpx
    enddo
    
    do j=jds,jde
        tmpy = (lfn(i,j+1)-lfn(i,j))/dy
        diffLy(i,j+1) = tmpy
        diffRy(i,j)   = tmpy
    enddo
    
    
    diffLx(ids,j)   = diffLx(ids+1,j)
    diffRx(ide+1,j) = diffRx(ide,j)
    do i=ids,ide+1
        diffLy(i,jds)   = diffLy(i,jds+1)
        diffRy(i,jde+1) = diffRy(i,jde)
    enddo    

end subroutine meshdiff_2d




real pure function sum_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)

integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+a(i,j)
    enddo
enddo
sum_2darray = t
end function sum_2darray

real pure function max_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)

integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=max(t,a(i,j))
    enddo
enddo
max_2darray = t
end function max_2darray

subroutine print_2d_stats_vec(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,ay,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::ax,ay
character(len=*),intent(in)::name
integer:: i,j
real:: t 
real:: avg_a,max_a,min_a 
character(len=25)::id
id=name
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,id//'/x ')
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ay,id//'/y ')
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
do j=jps,jpe
    do i=ips,ipe
        t=sqrt(ax(i,j)**2+ay(i,j)**2)
        max_a=max(max_a,t)
        min_a=min(min_a,t)
        avg_a=avg_a+t
    enddo
enddo
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1))
call print_stat_line(id//'/sz',ips,ipe,jps,jpe,min_a,max_a,avg_a)
end subroutine print_2d_stats_vec


subroutine print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a)

implicit none

integer, intent(in)::ips,ipe,jps,jpe
character(len=*),intent(in)::name
real,intent(in)::min_a,max_a,avg_a

character(len=128)::msg
character(len=24)::id
if(fire_print_msg.eq.0)return
id=name
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,'(a,4i4,3g11.3)')id,ips,ipe,jps,jpe,min_a,max_a,avg_a
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call message(msg)
if(.not.avg_a.eq.avg_a)call crash('NaN detected')
end subroutine print_stat_line


subroutine print_3d_stats(ips,ipe,kps,kpe,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme,kms,kme,kps,kpe
real, intent(in)::a(ims:ime,kms:kme,jms:jme)
character(len=*),intent(in)::name
integer:: i,j,k
real:: avg_a,max_a,min_a,t,aa,bb
character(len=128)::msg


bb=0.
do j=jps,jpe
  do k=kps,kpe
    do i=ips,ipe
       bb=bb+a(i,k,j)
    enddo
  enddo
enddo
if(bb.eq.bb.and.fire_print_msg.eq.0)return
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
t=huge(t)
do j=jps,jpe
  do k=kps,kpe
    do i=ips,ipe
        aa=a(i,k,j)
        if(aa.ne.aa.or..not.aa.le.t.or..not.aa.ge.-t)goto 9
        max_a=max(max_a,aa)
        min_a=min(min_a,aa)
        avg_a=avg_a+aa
    enddo
  enddo
enddo
if(bb.ne.bb)goto 10 
if(fire_print_msg.eq.0)return
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1))
call print_stat_line(name,ips,ipe,jps,jpe,min_a,max_a,avg_a)
return
9 continue
!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,1)name,i,k,j,aa
call message(msg)
1 format(a30,'(',i6,',',i6,',',i6,') = ',g13.5)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call print_stat_line(name,ips,ipe,jps,jpe,aa,aa,aa)
if(aa.ne.aa)goto 10
msg='Invalid floating point number detected in '//name
call crash(msg)
10 msg='NaN detected in '//name
call crash(msg)
end subroutine print_3d_stats

subroutine print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
character(len=*),intent(in)::name


call print_3d_stats(ips,ipe,1,1,jps,jpe, &
                         ims,ime,1,1,jms,jme, &
                         a,name)


end subroutine print_2d_stats

real pure function avg_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)


avg_2darray = sum_2darray( its,ite,jts,jte,               &
                           ims,ime,jms,jme,               &
                           a)/((ite-its+1)*(jte-jts+1))
end function avg_2darray

real pure function avg_2darray_vec( its,ite,jts,jte,           &
                                ims,ime,jms,jme,               &
                                ax,ay)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme):: ax,ay

integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+sqrt(ax(i,j)**2+ay(i,j)**2)
    enddo
enddo
t = t/((ite-its+1)*(jte-jts+1))
avg_2darray_vec = t
end function avg_2darray_vec


subroutine print_array(its,ite,jts,jte,           &
                         ims,ime,jms,jme,               &
                         a,name,id)


integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,id
real, intent(in), dimension(ims:ime,jms:jme):: a
character(len=*),intent(in)::name

integer i,j
character(len=128)::msg

!$OMP CRITICAL(FIRE_UTIL_CRIT)
write(msg,*)name,' start ',id,' dim ',its,ite,jts,jte
call message(msg)
do j=jts,jte
    do i=its,ite
         write(msg,*)i,j,a(i,j)
         call message(msg)
    enddo
enddo
write(msg,*)name,' end ',id
call message(msg)
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
end subroutine print_array

subroutine write_array_m(its,ite,jts,jte,           &
                         ims,ime,jms,jme,               &
                         a,name,id)


integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,id
real, intent(in), dimension(ims:ime,jms:jme):: a
character(len=*),intent(in)::name

call write_array_m3(its,ite,1,1,jts,jte,           &
                         ims,ime,1,1,jms,jme,               &
                         a,name,id)
end subroutine write_array_m


subroutine write_array_m3(its,ite,kts,kte,jts,jte,           &
                         ims,ime,kms,kme,jms,jme,               &
                         a,name,id)

implicit none


integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,kts,kte,kms,kme,id
real, intent(in), dimension(ims:ime,kms:kme,jms:jme):: a
character(len=*),intent(in)::name

integer i,j,k,iu,ilen,myproc,nprocs
logical op
character(len=128)::fname,msg

if(fire_print_file.eq.0.or.id.le.0)return
call check_mesh_2dim(its,ite,jts,jte,ims,ime,jms,jme)
call wrf_get_nproc (nprocs)
call wrf_get_myproc(myproc)

!$OMP CRITICAL(FIRE_UTIL_CRIT)
if(nprocs.eq.1)then
    write(fname,3)name,'_',id,'.txt'
else
    write(fname,4)name,'_',id,'.',myproc,'.txt'
endif

iu=0
do i=6,99
    inquire(unit=i,opened=op)
    if(.not.op.and.iu.le.0)iu=i
enddo
if(iu.gt.0)open(iu,file=trim(fname),form='formatted',status='unknown')

if(iu.le.0)call crash('write_array_m: cannot find available fortran unit')

write(iu,1)real(its)
write(iu,1)real(ite)
write(iu,1)real(jts)
write(iu,1)real(jte)
write(iu,1)real(kts)
write(iu,1)real(kte)
write(iu,1)(((a(i,k,j),i=its,ite),j=jts,jte),k=kts,kte)
close(iu)
write(msg,2)name,'(',its,':',ite,',',jts,':',jte,',', &
kts,':',kte,') -> ',trim(fname) 
!$OMP END CRITICAL(FIRE_UTIL_CRIT)
call message(msg)
return

1 format(e20.12)
2 format(2a,3(i5,a,i5,a),2a)
3 format(a,a,i8.8,a)
4 format(a,a,i8.8,a,i4.4,a)


end subroutine write_array_m3



subroutine read_array_2d_integer(filename,ia,its,ite,jts,jte,ims,ime,jms,jme)

integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
integer, intent(out), dimension(ims:ime,jms:jme):: ia
character(len=*),intent(in)::filename

real, allocatable, dimension(:,:)::a
integer::i,j
real::r
character(len=256)msg

allocate(a(ims:ime,jms:jme))
call read_array_2d_real(filename,a,its,ite,jts,jte,ims,ime,jms,jme)
do j=jts,jte
    do i=its,ite
        ia(i,j)=a(i,j)
	r=ia(i,j)
	if(r.ne.a(i,j))then
           write(6,*)'Reading file ',trim(filename)
           write(6,*)'value at position ',i,j,' cannot be converted to integer'
           write(6,*)'read ',a(i,j),' converted as',ia(i,j),' converted as ',r
	   msg=trim(filename)//' is not integer file'
	   call crash(msg)
        endif
    enddo
enddo
end subroutine read_array_2d_integer

        

subroutine read_array_2d_real(filename,a,its,ite,jts,jte,ims,ime,jms,jme)
implicit none





integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(out), dimension(ims:ime,jms:jme):: a
character(len=*),intent(in)::filename

integer i,j,ni,nj,mi,mj,nprocs,myproc,mythread,iu
logical op
character(len=128)::fname,msg


call wrf_get_nproc (nprocs)
call wrf_get_myproc( myproc )
mythread=0
if(nprocs.ne.1.or.myproc.ne.0.or.mythread.ne.0) &
   call crash('read_array_2d: parallel execution not supported')


mi=ite-its+1
mj=jte-jts+1
write(msg,2)'reading array size ',mi,mj,' from file ',trim(filename)
2 format(a,2i6,2a)
call message(msg)


call check_mesh_2dim(its,ite,jts,jte,ims,ime,jms,jme)


iu=0
do i=11,99
    inquire(unit=i,opened=op)
    if(.not.op.and.iu.le.0)iu=i
enddo
if(iu.le.0)call crash('read_array_2d: cannot find available fortran unit')

if(iu.gt.0)open(iu,file=filename,form='formatted',status='old',err=9)
rewind(iu,err=9)

read(iu,*,err=10)ni,nj
if(ni.ne.mi.or.nj.ne.mj)then
    write(msg,'(a,2i6,a,2i6)')'Array dimensions',ni,nj,' in the input file should be ',mi,mj
    call message(msg)
    goto 10
endif
do i=its,ite
   read(iu,*,err=10)(a(i,j),j=jts,jte)
enddo
close(iu,err=11)
return

9  msg='Error opening file '//trim(filename)
call crash(msg)
10 msg='Error reading file '//trim(filename)
call crash(msg)
11 msg='Error closing file '//trim(filename)
call crash(msg)
end subroutine read_array_2d_real





pure integer function ifval(l,i,j)
implicit none
logical, intent(in)::l
integer, intent(in)::i,j
if(l)then
	ifval=i
else
	ifval=j
endif
end function ifval


pure integer function snode(t,d,i)
implicit none
integer, intent(in)::t,d,i
if(t.ne.d)then
    snode=t
else
    snode=t+i
endif
end function snode

subroutine print_chsum( id, & 
    ims,ime,kms,kme,jms,jme, &                
    ids,ide,kds,kde,jds,jde, &                
    ips,ipe,kps,kpe,jps,jpe, &                
    istag,kstag,jstag,       &
    a,name)                      


integer, intent(in):: id, &
    ims,ime,kms,kme,jms,jme, &                
    ids,ide,kds,kde,jds,jde, &                
    ips,ipe,kps,kpe,jps,jpe, &                
    istag,kstag,jstag
real, intent(in),dimension(ims:ime,kms:kme,jms:jme)::a
character(len=*)::name



integer::lsum
integer::i,j,k,n,ipe1,jpe1,kpe1,iel,thread,is,js,ks
integer, save::psum,gsum
real::rel
equivalence(rel,iel)
character(len=256)msg

if(fire_print_msg.le.0)return

ipe1=ifval(ipe.eq.ide.and.istag.ne.0,ipe+1,ipe)
kpe1=ifval(kpe.eq.kde.and.kstag.ne.0,kpe+1,kpe)
jpe1=ifval(jpe.eq.jde.and.jstag.ne.0,jpe+1,jpe)
is=ifval(istag.ne.0,1,0)
ks=ifval(kstag.ne.0,1,0)
js=ifval(jstag.ne.0,1,0)

lsum=0
do j=jps,jpe1
  do k=kps,kpe1
    do i=ips,ipe1
      rel=a(i,k,j)
      lsum=ieor(lsum,iel)
    enddo
  enddo
enddo


thread=0
if(thread.eq.0)psum=0
!$OMP BARRIER
!$OMP CRITICAL(CHSUM)
psum=ieor(psum,lsum)
!$OMP END CRITICAL(CHSUM)
!$OMP BARRIER


if(thread.eq.0)then
    gsum = psum
    write(msg,1)id,name,ids,ide+is,kds,kde+ks,jds,jde+js,gsum
1   format(i6,1x,a10,' dims',6i5,' chsum ',z8.8)
    call message(msg)
endif

end subroutine print_chsum 


real function fun_real(fun,  & 
    ims,ime,kms,kme,jms,jme, &                
    ids,ide,kds,kde,jds,jde, &                
    ips,ipe,kps,kpe,jps,jpe, &                
    istag,kstag,jstag,       &                
    a,b)                      


integer, intent(in)::  fun, &
    ims,ime,kms,kme,jms,jme, &                
    ids,ide,kds,kde,jds,jde, &                
    ips,ipe,kps,kpe,jps,jpe, &                
    istag,kstag,jstag                         
real, intent(in),dimension(ims:ime,kms:kme,jms:jme)::a,b


real::lsum,void
integer::i,j,k,n,ipe1,jpe1,kpe1,iel,thread,is,js,ks
real, save::psum,gsum
real::rel
logical:: dosum,domax
character(len=256)msg

ipe1=ifval(ipe.eq.ide.and.istag.ne.0,ipe+1,ipe)
kpe1=ifval(kpe.eq.kde.and.kstag.ne.0,kpe+1,kpe)
jpe1=ifval(jpe.eq.jde.and.jstag.ne.0,jpe+1,jpe)
is=ifval(istag.ne.0,1,0)
ks=ifval(kstag.ne.0,1,0)
js=ifval(jstag.ne.0,1,0)

if(fun.eq.REAL_SUM)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=lsum+a(i,k,j)
      enddo
    enddo
  enddo
elseif(fun.eq.RNRM_SUM)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=lsum+sqrt(a(i,k,j)*a(i,k,j)+b(i,k,j)*b(i,k,j))
      enddo
    enddo
  enddo
elseif(fun.eq.REAL_MAX)then
  void=-huge(lsum)
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=max(lsum,a(i,k,j))
      enddo
    enddo
  enddo
elseif(fun.eq.RNRM_MAX)then
  void=0.
  lsum=void
  do j=jps,jpe1
    do k=kps,kpe1
      do i=ips,ipe1
        lsum=max(lsum,sqrt(a(i,k,j)*a(i,k,j)+b(i,k,j)*b(i,k,j)))
      enddo
    enddo
  enddo
else
  call crash('fun_real: bad fun')
endif

if(lsum.ne.lsum)call crash('fun_real: NaN detected')

dosum=fun.eq.REAL_SUM.or.fun.eq.RNRM_SUM
domax=fun.eq.REAL_MAX.or.fun.eq.RNRM_MAX


!$OMP SINGLE

psum=void
!$OMP END SINGLE
!$OMP BARRIER


!$OMP CRITICAL(RDSUM)

if(dosum)psum=psum+lsum
if(domax)psum=max(psum,lsum)
!$OMP END CRITICAL(RDSUM)


!$OMP BARRIER


!$OMP SINGLE

    gsum = psum
if(gsum.ne.gsum)call crash('fun_real: NaN detected')
!$OMP END SINGLE

!$OMP BARRIER


fun_real=gsum

end function fun_real

end module module_fr_fire_util
