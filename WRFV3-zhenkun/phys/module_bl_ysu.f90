







module module_bl_ysu
contains




   subroutine ysu(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,               &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qi,                          &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znu,znw,p_top,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,                                                      &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uoce,voce,                                                   &
                  rthraten,ysu_topdown_pblmix,                                 &
                  ctopo,ctopo2,                                                &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                
                  regime                                           )

   implicit none









































































   integer,parameter ::  ndiff = 3
   real,parameter    ::  rcl = 1.0

   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte

   integer,  intent(in)      ::      ysu_topdown_pblmix

   real,     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv

   real,     intent(in )     ::      ep1,ep2,karman

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         th3d, &
                                                                          t3d, &
                                                                         dz8w, &
                                                                     rthraten
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          p3di

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                        exch_h
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                         wstar
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                         delta
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                          uoce, &
                                                                         voce

   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                           br, &
                                                                         psfc
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           znt, &
                                                                          ust, &
                                                                         hpbl, &
                                                                          wspd

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d

   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
   logical,  intent(in)      ::                                       flag_qi



   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(inout)   ::                                        regime

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             optional                                                        , &
             intent(inout)   ::                                       rqiblten

   real,     dimension( kms:kme )                                            , &
             optional                                                        , &
             intent(in   )   ::                                           znu, &
                                                                          znw


   real,     optional, intent(in   )   ::                               p_top

   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2

   integer ::  i,j,k
   real,     dimension( its:ite, kts:kte*ndiff )  ::                 rqvbl2dt, &
                                                                         qv2d
   real,     dimension( its:ite, kts:kte )  ::                            pdh
   real,     dimension( its:ite, kts:kte+1 )  ::                         pdhi
   real,     dimension( its:ite )  ::                                          &
                                                                        dusfc, &
                                                                        dvsfc, &
                                                                        dtsfc, &
                                                                        dqsfc

   qv2d(its:ite,:) = 0.0

   do j = jts,jte
      do k = kts,kte+1
        do i = its,ite
          if(k.le.kte)pdh(i,k) = p3d(i,k,j)
          pdhi(i,k) = p3di(i,k,j)
        enddo
      enddo
      do k = kts,kte
        do i = its,ite
          qv2d(i,k) = qv3d(i,k,j)
          qv2d(i,k+kte) = qc3d(i,k,j)
          if(present(rqiblten)) qv2d(i,k+kte+kte) = qi3d(i,k,j)
        enddo
      enddo

      call ysu2d(J=j,ux=u3d(ims,kms,j),vx=v3d(ims,kms,j)                       &
              ,tx=t3d(ims,kms,j)                                               &
              ,qx=qv2d(its,kts)                                                &
              ,p2d=pdh(its,kts),p2di=pdhi(its,kts)                             &
              ,pi2d=pi3d(ims,kms,j)                                            &
              ,utnp=rublten(ims,kms,j),vtnp=rvblten(ims,kms,j)                 &
              ,ttnp=rthblten(ims,kms,j),qtnp=rqvbl2dt(its,kts),ndiff=ndiff     &
              ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                           &    
              ,xlv=xlv,rv=rv                                                   &
              ,ep1=ep1,ep2=ep2,karman=karman                                   &
              ,dz8w2d=dz8w(ims,kms,j)                                          &
              ,psfcpa=psfc(ims,j),znt=znt(ims,j),ust=ust(ims,j)                &
              ,hpbl=hpbl(ims,j)                                                &
              ,regime=regime(ims,j),psim=psim(ims,j)                           &
              ,psih=psih(ims,j),xland=xland(ims,j)                             &
              ,hfx=hfx(ims,j),qfx=qfx(ims,j)                                   &
              ,wspd=wspd(ims,j),br=br(ims,j)                                   &
              ,dusfc=dusfc,dvsfc=dvsfc,dtsfc=dtsfc,dqsfc=dqsfc                 &
              ,dt=dt,rcl=1.0,kpbl1d=kpbl2d(ims,j)                              &
              ,exch_hx=exch_h(ims,kms,j)                                       &
              ,wstar=wstar(ims,j)                                              &
              ,delta=delta(ims,j)                                              &
              ,u10=u10(ims,j),v10=v10(ims,j)                                   &
              ,uox=uoce(ims,j),vox=voce(ims,j)                                 &
              ,rthraten=rthraten(ims,kms,j),p2diORG=p3di(ims,kms,j)            &
              ,ysu_topdown_pblmix=ysu_topdown_pblmix                           &
              ,ctopo=ctopo(ims,j),ctopo2=ctopo2(ims,j)                         &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )

     do k = kts,kte
       do i = its,ite
         rthblten(i,k,j) = rthblten(i,k,j)/pi3d(i,k,j)
         rqvblten(i,k,j) = rqvbl2dt(i,k)
         rqcblten(i,k,j) = rqvbl2dt(i,k+kte)
         if(present(rqiblten)) rqiblten(i,k,j) = rqvbl2dt(i,k+kte+kte)
       enddo
     enddo

   enddo

   end subroutine ysu



   subroutine ysu2d(j,ux,vx,tx,qx,p2d,p2di,pi2d,                               &
                  utnp,vtnp,ttnp,qtnp,ndiff,                                   &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w2d,psfcpa,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dusfc,dvsfc,dtsfc,dqsfc,                                     &
                  dt,rcl,kpbl1d,                                               &
                  exch_hx,                                                     &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uox,vox,                                                     &
                  rthraten,p2diORG,                                            &
                  ysu_topdown_pblmix,                                          &
                  ctopo,ctopo2,                                                &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                
                  regime )

   implicit none



























































   real,parameter    ::  xkzminm = 0.1,xkzminh = 0.01
   real,parameter    ::  xkzmin = 0.01,xkzmax = 1000.,rimin = -100.
   real,parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real,parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real,parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real,parameter    ::  phifac = 8.,sfcfrac = 0.1
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real,parameter    ::  zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real,parameter    ::  tmin=1.e-2
   real,parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real,parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1

   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte,                 &
                                    j,ndiff

   integer,  intent(in)      ::     ysu_topdown_pblmix 

   real,     intent(in   )   ::     dt,rcl,cp,g,rovcp,rovg,rd,xlv,rv

   real,     intent(in )     ::     ep1,ep2,karman

   real,     dimension( ims:ime, kms:kme ),                                    &
             intent(in)      ::                                        dz8w2d, &
                                                                         pi2d, &
                                                                      p2diorg

   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            tx
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(in   )   ::                                            qx

   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(inout)   ::                                          qtnp

   real,     dimension( its:ite, kts:kte+1 )                                 , &
             intent(in   )   ::                                          p2di

   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                           p2d

   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::                                           ust, &
                                                                         hpbl, &
                                                                          znt
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx

   real,     dimension( ims:ime ), intent(inout)   ::                    wspd
   real,     dimension( ims:ime ), intent(in  )    ::                      br

   real,     dimension( ims:ime ), intent(in   )   ::                    psim, &
                                                                         psih

   real,     dimension( ims:ime ), intent(in   )   ::                  psfcpa
   integer,  dimension( ims:ime ), intent(out  )   ::                  kpbl1d

   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            ux, &
                                                                           vx, &
                                                                      rthraten
   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(inout)   ::                                        regime



   real,     dimension( its:ite )            ::                           hol
   real,     dimension( its:ite, kts:kte+1 ) ::                            zq

   real,     dimension( its:ite, kts:kte )   ::                                &
                                                               thx,thvx,thlix, &
                                                                          del, &
                                                                          dza, &
                                                                          dzq, &
                                                                        xkzom, &
                                                                        xkzoh, &
                                                                           za

   real,    dimension( its:ite )             ::                                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                                  dusfc,dvsfc, &
                                                                  dtsfc,dqsfc, &
                                                                        prpbl, &
                                                              wspd1,thermalli

   real,    dimension( its:ite, kts:kte )    ::                     xkzm,xkzh, &
                                                                        f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al, &
                                                                         xkzq, &
                                                                         zfac, &
                                                                        rhox2, &
                                                                       hgamt2



   real,    dimension( ims:ime, kms:kme )                                    , &
            intent(inout)   ::                                        exch_hx

   real,    dimension( ims:ime )                                             , &
            intent(inout)    ::                                           u10, &
                                                                          v10
   real,    dimension( ims:ime )                                             , &
            intent(in  )    ::                                            uox, &
                                                                          vox
   real,    dimension( its:ite )    ::                                         &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro

   real,    dimension( its:ite, kts:kte, ndiff)  ::                     r3,f3
   integer, dimension( its:ite )             ::                  kpbl,kpblold

   logical, dimension( its:ite )             ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable, &
                                                                     cloudflg

   logical                                   ::                     definebrup

   integer ::  n,i,k,l,ic,is,kk
   integer ::  klpbl, ktrace1, ktrace2, ktrace3


   real    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real    ::  ss,ri,qmean,tmean,alph,chi,zk,rl2,dk,sri
   real    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real    ::  utend,vtend,ttend,qtend
   real    ::  dtstep,govrthv
   real    ::  cont, conq, conw, conwrc


   real, dimension( its:ite, kts:kte )     ::                wscalek,wscalek2
   real, dimension( ims:ime )              ::                           wstar
   real, dimension( ims:ime )              ::                           delta
   real, dimension( its:ite, kts:kte )     ::                     xkzml,xkzhl, &
                                                               zfacent,entfac
   real, dimension( its:ite )              ::                            ust3, &
                                                                       wstar3, &
                                                                     wstar3_2, &
                                                                  hgamu,hgamv, &
                                                                      wm2, we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl, &
                                                                        dthvx
   real    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,                           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z,radsum,tmp1,templ,rvls,temps,ent_eff,    &
               rcldb,bruptmp,radflux,vconvlim,vconvnew,fluxc,vconvc,vconv

   real,    dimension( ims:ime, kms:kme )    ::                          fric, &
                                                                       tke_ysu,&
                                                                        el_ysu,&
                                                                     shear_ysu,&
                                                                     buoy_ysu
   real,    dimension( ims:ime )             ::                       pblh_ysu,&
                                                                      vconvfx



   klpbl = kte

   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac



   ktrace1 = 0
   ktrace2 = 0 + kte
   ktrace3 = 0 + kte*2

   do k = kts,kte
     do i = its,ite
       thx(i,k) = tx(i,k)/pi2d(i,k)
       thlix(i,k) = (tx(i,k)-xlv*qx(i,ktrace2+k)/cp-2.834E6*qx(i,ktrace3+k)/cp)/pi2d(i,k)
     enddo
   enddo

   do k = kts,kte
     do i = its,ite
       tvcon = (1.+ep1*qx(i,k))
       thvx(i,k) = thx(i,k)*tvcon
     enddo
   enddo

   do i = its,ite
     tvcon = (1.+ep1*qx(i,1))
     rhox(i) = psfcpa(i)/(rd*tx(i,1)*tvcon)
     govrth(i) = g/thx(i,1)
   enddo




   do i = its,ite
     zq(i,1) = 0.
   enddo

   do k = kts,kte
     do i = its,ite
       zq(i,k+1) = dz8w2d(i,k)+zq(i,k)
       tvcon = (1.+ep1*qx(i,k))
       rhox2(i,k) = p2d(i,k)/(rd*tx(i,k)*tvcon)
     enddo
   enddo

   do k = kts,kte
     do i = its,ite
       za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
       dzq(i,k) = zq(i,k+1)-zq(i,k)
       del(i,k) = p2di(i,k)-p2di(i,k+1)
     enddo
   enddo

   do i = its,ite
     dza(i,1) = za(i,1)
   enddo

   do k = kts+1,kte
     do i = its,ite
       dza(i,k) = za(i,k)-za(i,k-1)
     enddo
   enddo




   utnp(its:ite,:) = 0.
   vtnp(its:ite,:) = 0.
   ttnp(its:ite,:) = 0.
   qtnp(its:ite,:) = 0.

   do i = its,ite
     wspd1(i) = sqrt( (ux(i,1)-uox(i))*(ux(i,1)-uox(i)) + (vx(i,1)-vox(i))*(vx(i,1)-vox(i)) )+1.e-9
   enddo






   dtstep = dt
   dt2 = 2.*dtstep
   rdt = 1./dt2

   do i = its,ite
     bfxpbl(i) = 0.0
     hfxpbl(i) = 0.0
     qfxpbl(i) = 0.0
     ufxpbl(i) = 0.0
     vfxpbl(i) = 0.0
     hgamu(i)  = 0.0
     hgamv(i)  = 0.0
     delta(i)  = 0.0
     wstar3_2(i) =  0.0
   enddo

   do k = kts,klpbl
     do i = its,ite
       wscalek(i,k) = 0.0
       wscalek2(i,k) = 0.0
     enddo
   enddo

   do k = kts,klpbl
     do i = its,ite
       zfac(i,k) = 0.0
     enddo
   enddo
   do k = kts,klpbl-1
     do i = its,ite
       xkzom(i,k) = xkzminm
       xkzoh(i,k) = xkzminh
     enddo
   enddo

   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
     dtsfc(i) = 0.
     dqsfc(i) = 0.
   enddo

   do i = its,ite
     hgamt(i)  = 0.
     hgamq(i)  = 0.
     wscale(i) = 0.
     kpbl(i)   = 1
     hpbl(i)   = zq(i,1)
     zl1(i)    = za(i,1)
     thermal(i)= thvx(i,1)
     thermalli(i) = thlix(i,1)
     pblflg(i) = .true.
     sfcflg(i) = .true.
     sflux(i) = hfx(i)/rhox(i)/cp + qfx(i)/rhox(i)*ep1*thx(i,1)
     if(br(i).gt.0.0) sfcflg(i) = .false.
   enddo



   do i = its,ite
     stable(i) = .false.
     brup(i) = br(i)
     brcr(i) = brcr_ub
   enddo

   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo

   do i = its,ite
     k = kpbl(i)
     if(brdn(i).ge.brcr(i))then
       brint = 0.
     elseif(brup(i).le.brcr(i))then
       brint = 1.
     else
       brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
     endif
     hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
     if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
     if(kpbl(i).le.1) pblflg(i) = .false.
   enddo

   do i = its,ite
     fm = psim(i)
     fh = psih(i)
     zol1(i) = max(br(i)*fm*fm/fh,rimin)
     if(sfcflg(i))then
       zol1(i) = min(zol1(i),-zfmin)
     else
       zol1(i) = max(zol1(i),zfmin)
     endif
     hol1 = zol1(i)*hpbl(i)/zl1(i)*sfcfrac
     if(sfcflg(i))then
       phim(i) = (1.-aphi16*hol1)**(-1./4.)
       phih(i) = (1.-aphi16*hol1)**(-1./2.)
       bfx0 = max(sflux(i),0.)
       hfx0 = max(hfx(i)/rhox(i)/cp,0.)
       qfx0 = max(ep1*thx(i,1)*qfx(i)/rhox(i),0.)
       wstar3(i) = (govrth(i)*bfx0*hpbl(i))
       wstar(i) = (wstar3(i))**h1
     else
       phim(i) = (1.+aphi5*hol1)
       phih(i) = phim(i)
       wstar(i)  = 0.
       wstar3(i) = 0.
     endif
     ust3(i)   = ust(i)**3.
     wscale(i) = (ust3(i)+phifac*karman*wstar3(i)*0.5)**h1
     wscale(i) = min(wscale(i),ust(i)*aphi16)
     wscale(i) = max(wscale(i),ust(i)/aphi5)
   enddo




   do i = its,ite
     if(sfcflg(i).and.sflux(i).gt.0.0)then
       gamfac   = bfac/rhox(i)/wscale(i)
       hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
       hgamq(i) = min(gamfac*qfx(i),gamcrq)
       vpert = (hgamt(i)+ep1*thx(i,1)*hgamq(i))/bfac*afac
       thermal(i) = thermal(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       thermalli(i)= thermalli(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       hgamt(i) = max(hgamt(i),0.0)
       hgamq(i) = max(hgamq(i),0.0)
       brint    = -15.9*ust(i)*ust(i)/wspd(i)*wstar3(i)/(wscale(i)**4.)
       hgamu(i) = brint*ux(i,1)
       hgamv(i) = brint*vx(i,1)
     else
       pblflg(i) = .false.
     endif
   enddo



   do i = its,ite
     if(pblflg(i))then
       kpbl(i) = 1
       hpbl(i) = zq(i,1)
     endif
   enddo

   do i = its,ite
     if(pblflg(i))then
       stable(i) = .false.
       brup(i) = br(i)
       brcr(i) = brcr_ub
     endif
   enddo

   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i).and.pblflg(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo



   if (ysu_topdown_pblmix.eq.1)then
     do i = its,ite
        kpblold(i) = kpbl(i)
        definebrup=.false.
        do k = kpblold(i), kte-1
           spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
           bruptmp = (thlix(i,k)-thermalli(i))*(g*za(i,k)/thlix(i,1))/spdk2
           stable(i) = bruptmp.ge.brcr(i)
           if (definebrup) then
           kpbl(i) = k
           brup(i) = bruptmp
           definebrup=.false.
           endif
           if (.not.stable(i)) then 
           brdn(i)=bruptmp
           definebrup=.true.
           pblflg(i)=.true.
           endif
        enddo
     enddo
   endif

   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo



   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       brup(i) = br(i)
       stable(i) = .false.
     else
       stable(i) = .true.
     endif
   enddo

   do i = its,ite
     if((.not.stable(i)).and.((xland(i)-1.5).ge.0))then
       wspd10 = u10(i)*u10(i) + v10(i)*v10(i)
       wspd10 = sqrt(wspd10)
       ross = wspd10 / (cori*znt(i))
       brcr_sbro(i) = min(0.16*(1.e-7*ross)**(-0.18),.3)
     endif
   enddo

   do i = its,ite
     if(.not.stable(i))then
       if((xland(i)-1.5).ge.0)then
         brcr(i) = brcr_sbro(i)
       else
         brcr(i) = brcr_sb
       endif
     endif
   enddo

   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo

   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo



   do i = its,ite
     cloudflg(i)=.false. 
     if(pblflg(i)) then
       k = kpbl(i) - 1
       wm3       = wstar3(i) + 5. * ust3(i)
       wm2(i)    = wm3**h2
       bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
       dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
       we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))
       if((qx(i,ktrace2+k)+qx(i,ktrace3+k)).gt.0.01e-3.and.ysu_topdown_pblmix.eq.1)then
           if ( kpbl(i) .ge. 2) then
                cloudflg(i)=.true. 
                templ=thlix(i,k)*(p2di(i,k+1)/100000)**rovcp
                
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep2/p2di(i,k+1))
                temps=templ + ((qx(i,k)+qx(i,ktrace2+k))-rvls)/(cp/xlv  + &
                ep2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep2/p2di(i,k+1))
                rcldb=max((qx(i,k)+qx(i,ktrace2+k))-rvls,0.)
                
                dthvx(i)  = (thlix(i,k+2)+thx(i,k+2)*ep1*(qx(i,k+2)+qx(i,ktrace2+k+2))) &
                          - (thlix(i,k) + thx(i,k)  *ep1*(qx(i,k)  +qx(i,ktrace2+k)))
                dthvx(i)  = max(dthvx(i),0.1)
                tmp1      = xlv/cp * rcldb/(pi2d(i,k)*dthvx(i))
                ent_eff   = 0.2 * 8. * tmp1 +0.2

                radsum=0.
                do kk = 1,kpbl(i)-1
                   radflux=rthraten(i,kk)*pi2d(i,kk) 
                   radflux=radflux*cp/g*(p2diORG(i,kk)-p2diORG(i,kk+1)) 
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                enddo
                radsum=max(radsum,0.0)

                
                bfx0 = max(max(sflux(i),0.0)-radsum/rhox2(i,k)/cp,0.)
                bfx0 = max(sflux(i),0.0)
                wm3 = (govrth(i)*bfx0*hpbl(i))+5. * ust3(i)
                wm2(i)    = wm3**h2
                bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
                we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))

                
                bfx0 = max(radsum/rhox2(i,k)/cp-max(sflux(i),0.0),0.)
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wm3       = (g/thvx(i,k)*bfx0*hpbl(i)) 
                wm2(i)    = wm2(i)+wm3**h2
                bfxpbl(i) = - ent_eff * bfx0
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),0.1)
                we(i) = we(i) + max(bfxpbl(i)/dthvx(i),-sqrt(wm3**h2))

                
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wstar3_2(i) =  (g/thvx(i,k)*bfx0*hpbl(i))
                
                wscale(i) = (ust3(i)+phifac*karman*(wstar3(i)+wstar3_2(i))*0.5)**h1
                wscale(i) = min(wscale(i),ust(i)*aphi16)
                wscale(i) = max(wscale(i),ust(i)/aphi5)
                gamfac   = bfac/rhox(i)/wscale(i)
                hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
                hgamq(i) = min(gamfac*qfx(i),gamcrq)
                gamfac   = bfac/rhox2(i,k)/wscale(i)
                hgamt2(i,k) = min(gamfac*radsum/cp,gamcrt)
                hgamt(i) = max(hgamt(i),0.0) + max(hgamt2(i,k),0.0)
                brint    = -15.9*ust(i)*ust(i)/wspd(i)*(wstar3(i)+wstar3_2(i))/(wscale(i)**4.)
                hgamu(i) = brint*ux(i,1)
                hgamv(i) = brint*vx(i,1)
           endif
       endif
       prpbl(i) = 1.0
       dthx  = max(thx(i,k+1)-thx(i,k),tmin)
       dqx   = min(qx(i,k+1)-qx(i,k),0.0)
       hfxpbl(i) = we(i)*dthx
       qfxpbl(i) = we(i)*dqx

       dux = ux(i,k+1)-ux(i,k)
       dvx = vx(i,k+1)-vx(i,k)
       if(dux.gt.tmin) then
         ufxpbl(i) = max(prpbl(i)*we(i)*dux,-ust(i)*ust(i))
       elseif(dux.lt.-tmin) then
         ufxpbl(i) = min(prpbl(i)*we(i)*dux,ust(i)*ust(i))
       else
         ufxpbl(i) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i) = max(prpbl(i)*we(i)*dvx,-ust(i)*ust(i))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i) = min(prpbl(i)*we(i)*dvx,ust(i)*ust(i))
       else
         vfxpbl(i) = 0.0
       endif
       delb  = govrth(i)*d3*hpbl(i)
       delta(i) = min(d1*hpbl(i) + d2*wm2(i)/delb,100.)
     endif
   enddo

   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i).and.k.ge.kpbl(i))then
         entfac(i,k) = ((zq(i,k+1)-hpbl(i))/delta(i))**2.
       else
         entfac(i,k) = 1.e30
       endif
     enddo
   enddo



   do k = kts,klpbl
     do i = its,ite
       if(k.lt.kpbl(i)) then
         zfac(i,k) = min(max((1.-(zq(i,k+1)-zl1(i))/(hpbl(i)-zl1(i))),zfmin),1.)
         zfacent(i,k) = (1.-zfac(i,k))**3.
         wscalek(i,k) = (ust3(i)+phifac*karman*wstar3(i)*(1.-zfac(i,k)))**h1
         wscalek2(i,k) = (phifac*karman*wstar3_2(i)*(zfac(i,k)))**h1
         if(sfcflg(i)) then
           prfac = conpr
           prfac2 = 15.9*(wstar3(i)+wstar3_2(i))/ust3(i)/(1.+4.*karman*(wstar3(i)+wstar3_2(i))/ust3(i))
           prnumfac = -3.*(max(zq(i,k+1)-sfcfrac*hpbl(i),0.))**2./hpbl(i)**2.
         else
           prfac = 0.
           prfac2 = 0.
           prnumfac = 0.
           phim8z = 1.+aphi5*zol1(i)*zq(i,k+1)/zl1(i)
           wscalek(i,k) = ust(i)/phim8z
           wscalek(i,k) = max(wscalek(i,k),0.001)
         endif
         prnum0 = (phih(i)/phim(i)+prfac)
         prnum0 = max(min(prnum0,prmax),prmin)
           xkzm(i,k) = wscalek(i,k) *karman*    zq(i,k+1)      *    zfac(i,k)**pfac+ &
                       wscalek2(i,k)*karman*(hpbl(i)-zq(i,k+1))*(1-zfac(i,k))**pfac
         
         if (k.eq.kpbl(i)-1.and.cloudflg(i).and.we(i).lt.0.0) then
           xkzm(i,k) = 0.0
         endif
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzq(i,k) = xkzm(i,k)/prnum*zfac(i,k)**(pfac_q-pfac)
         prnum0 = prnum0/(1.+prfac2*karman*sfcfrac)
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzh(i,k) = xkzm(i,k)/prnum
         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzq(i,k) = xkzq(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
       endif
     enddo
   enddo



   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         ss = ((ux(i,k+1)-ux(i,k))*(ux(i,k+1)-ux(i,k))                         &
              +(vx(i,k+1)-vx(i,k))*(vx(i,k+1)-vx(i,k)))                        &
              /(dza(i,k+1)*dza(i,k+1))+1.e-9
         govrthv = g/(0.5*(thvx(i,k+1)+thvx(i,k)))
         ri = govrthv*(thvx(i,k+1)-thvx(i,k))/(ss*dza(i,k+1))
         if(imvdif.eq.1.and.ndiff.ge.3)then
           if((qx(i,ktrace2+k)+qx(i,ktrace3+k)).gt.0.01e-3.and.(qx(i           &
             ,ktrace2+k+1)+qx(i,ktrace3+k+1)).gt.0.01e-3)then

             qmean = 0.5*(qx(i,k)+qx(i,k+1))
             tmean = 0.5*(tx(i,k)+tx(i,k+1))
             alph  = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alph)*(ri-g*g/ss/tmean/cp*((chi-alph)/(1.+chi)))
           endif
         endif
         zk = karman*zq(i,k+1)
         rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
         rlamdz = min(dza(i,k+1),rlamdz)
         rl2 = (zk*rlamdz/(rlamdz+zk))**2
         dk = rl2*sqrt(ss)
         if(ri.lt.0.)then

           ri = max(ri, rimin)
           sri = sqrt(-ri)
           xkzm(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
           xkzh(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
         else

           xkzh(i,k) = dk/(1+5.*ri)**2
           prnum = 1.0+2.1*ri
           prnum = min(prnum,prmax)
           xkzm(i,k) = xkzh(i,k)*prnum
         endif

         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzml(i,k) = xkzm(i,k)
         xkzhl(i,k) = xkzh(i,k)
       endif
     enddo
   enddo



   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
     enddo
   enddo

   do i = its,ite
     ad(i,1) = 1.
     f1(i,1) = thx(i,1)-300.+hfx(i)/cont/del(i,1)*dt2
   enddo

   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzh(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzt = tem1*(-hgamt(i)/hpbl(i)-hfxpbl(i)*zfacent(i,k)/xkzh(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzt
         f1(i,k+1) = thx(i,k+1)-300.-dtodsu*dsdzt
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzh(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzh(i,k) = sqrt(xkzh(i,k)*xkzhl(i,k))
         xkzh(i,k) = max(xkzh(i,k),xkzoh(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         f1(i,k+1) = thx(i,k+1)-300.
       else
         f1(i,k+1) = thx(i,k+1)-300.
       endif
       tem1   = dsig*xkzh(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo



   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
     enddo
   enddo

   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)



   do k = kte,kts,-1
     do i = its,ite
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt*pi2d(i,k)
       ttnp(i,k) = ttnp(i,k)+ttend
       dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)/pi2d(i,k)
     enddo
   enddo



   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
     enddo
   enddo

   do ic = 1,ndiff
     do i = its,ite
       do k = kts,kte
         f3(i,k,ic) = 0.
       enddo
     enddo
   enddo

   do i = its,ite
     ad(i,1) = 1.
     f3(i,1,1) = qx(i,1)+qfx(i)*g/del(i,1)*dt2
   enddo

   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do i = its,ite
         f3(i,1,ic) = qx(i,1+is)
       enddo
     enddo
   endif

   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         xkzq(i,k) = xkzh(i,k)
       endif
     enddo
   enddo

   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzq(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzq = tem1*(-qfxpbl(i)*zfacent(i,k)/xkzq(i,k))
         f3(i,k,1) = f3(i,k,1)+dtodsd*dsdzq
         f3(i,k+1,1) = qx(i,k+1)-dtodsu*dsdzq
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzq(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzq(i,k) = sqrt(xkzq(i,k)*xkzhl(i,k))
         xkzq(i,k) = max(xkzq(i,k),xkzoh(i,k))
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
         f3(i,k+1,1) = qx(i,k+1)
       else
         f3(i,k+1,1) = qx(i,k+1)
       endif
       tem1   = dsig*xkzq(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)

     enddo
   enddo

   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kts,kte-1
         do i = its,ite
           f3(i,k+1,ic) = qx(i,k+1+is)
         enddo
       enddo
     enddo
   endif



   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
     enddo
   enddo

   do ic = 1,ndiff
     do k = kts,kte
       do i = its,ite
         r3(i,k,ic) = f3(i,k,ic)
       enddo
     enddo
   enddo



   call tridin_ysu(al,ad,cu,r3,au,f3,its,ite,kts,kte,ndiff)



   do k = kte,kts,-1
     do i = its,ite
       qtend = (f3(i,k,1)-qx(i,k))*rdt
       qtnp(i,k) = qtnp(i,k)+qtend
       dqsfc(i) = dqsfc(i)+qtend*conq*del(i,k)
     enddo
   enddo

   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kte,kts,-1
         do i = its,ite
           qtend = (f3(i,k,ic)-qx(i,k+is))*rdt
           qtnp(i,k+is) = qtnp(i,k+is)+qtend
         enddo
       enddo
     enddo
   endif



   do i = its,ite
     do k = kts,kte
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
       f2(i,k) = 0.
     enddo
   enddo



   do i = its,ite
      do k= kts, kte-1
        shear_ysu(i,k)=xkzm(i,k)*((-hgamu(i)/hpbl(i)+(ux(i,k+1)-ux(i,k))/dza(i,k+1))*(ux(i,k+1)-ux(i,k))/dza(i,k+1) &
        + (-hgamv(i)/hpbl(i)+(vx(i,k+1)-vx(i,k))/dza(i,k+1))*(vx(i,k+1)-vx(i,k))/dza(i,k+1))
         buoy_ysu(i,k)=xkzh(i,k)*g*(1.0/thx(i,k))*(-hgamt(i)/hpbl(i)+(thx(i,k+1)-thx(i,k))/dza(i,k+1))

       zk = karman*zq(i,k+1)
 
       if (k.ge.kpbl(i)) then
        rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
        rlamdz = min(dza(i,k+1),rlamdz)
       else
 
        rlamdz = 150.0
       endif
       el_ysu(i,k) = zk*rlamdz/(rlamdz+zk)
       tke_ysu(i,k)=16.6*el_ysu(i,k)*(shear_ysu(i,k)-buoy_ysu(i,k))
 
       if(tke_ysu(i,k).le.0) then
        tke_ysu(i,k)=0.0
       else
        tke_ysu(i,k)=(tke_ysu(i,k))**0.66
       endif
      enddo
 
 
      CALL GET_PBLH(KTS,KTE,pblh_ysu(i),thvx(i,kts:kte),&
      &    tke_ysu(i,kts:kte),zq(i,kts:kte+1),dzq(i,kts:kte),xland(i))




        if (xland(i).lt.1.5) then
        fluxc = max(sflux(i),0.0)
        vconvc=1.
        VCONV = vconvc*(g/thvx(i,1)*pblh_ysu(i)*fluxc)**.33
        else

        VCONV = 0.
        endif
        vconvfx(i) = vconv


      fric(i,1)=ust(i)**2/wspd1(i)*rhox(i)*g/del(i,1)*dt2         &
        *(wspd1(i)/wspd(i))**2
      if(present(ctopo)) then
        vconvnew=0.9*vconvfx(i)+1.5*(max((pblh_ysu(i)-500)/1000.0,0.0))
        vconvlim = min(vconvnew,1.0)
        ad(i,1) = 1.+fric(i,1)*vconvlim+ctopo(i)*fric(i,1)*(1-vconvlim)
      else
       ad(i,1) = 1.+fric(i,1)
      endif
     f1(i,1) = ux(i,1)+uox(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
     f2(i,1) = vx(i,1)+vox(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
   enddo

   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzm(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i))then
         dsdzu     = tem1*(-hgamu(i)/hpbl(i)-ufxpbl(i)*zfacent(i,k)/xkzm(i,k))
         dsdzv     = tem1*(-hgamv(i)/hpbl(i)-vfxpbl(i)*zfacent(i,k)/xkzm(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzu
         f1(i,k+1) = ux(i,k+1)-dtodsu*dsdzu
         f2(i,k)   = f2(i,k)+dtodsd*dsdzv
         f2(i,k+1) = vx(i,k+1)-dtodsu*dsdzv
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzm(i,k) = prpbl(i)*xkzh(i,k)
         xkzm(i,k) = sqrt(xkzm(i,k)*xkzml(i,k))
         xkzm(i,k) = max(xkzm(i,k),xkzom(i,k))
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       else
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       endif
       tem1   = dsig*xkzm(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
     enddo
   enddo



   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
       r2(i,k) = f2(i,k)
     enddo
   enddo



   call tridi1n(al,ad,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1)



   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k)-ux(i,k))*rdt
       vtend = (f2(i,k)-vx(i,k))*rdt
       utnp(i,k) = utnp(i,k)+utend
       vtnp(i,k) = vtnp(i,k)+vtend
       dusfc(i) = dusfc(i) + utend*conwrc*del(i,k)
       dvsfc(i) = dvsfc(i) + vtend*conwrc*del(i,k)
     enddo
   enddo



   do i = its,ite
     if(present(ctopo).and.present(ctopo2)) then 
       u10(i) = ctopo2(i)*u10(i)+(1-ctopo2(i))*ux(i,1)
       v10(i) = ctopo2(i)*v10(i)+(1-ctopo2(i))*vx(i,1)
     endif 
   enddo



   do i = its,ite
     kpbl1d(i) = kpbl(i)
   enddo


   end subroutine ysu2d



   subroutine tridi1n(cl,cm,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt)

   implicit none


   integer, intent(in )      ::     its,ite, kts,kte, nt

   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl

   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm, &
                                                                           r1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2

   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2

   real    :: fk
   integer :: i,k,l,n,it



   l = ite
   n = kte

   do i = its,l
     fk = 1./cm(i,1)
     au(i,1) = fk*cu(i,1)
     f1(i,1) = fk*r1(i,1)
   enddo

   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo

   do k = kts+1,n-1
     do i = its,l
       fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
       au(i,k) = fk*cu(i,k)
       f1(i,k) = fk*(r1(i,k)-cl(i,k)*f1(i,k-1))
     enddo
   enddo

   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo

   do i = its,l
     fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
     f1(i,n) = fk*(r1(i,n)-cl(i,n)*f1(i,n-1))
   enddo

   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo

   do k = n-1,kts,-1
     do i = its,l
       f1(i,k) = f1(i,k)-au(i,k)*f1(i,k+1)
     enddo
   enddo

   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo

   end subroutine tridi1n



   subroutine tridin_ysu(cl,cm,cu,r2,au,f2,its,ite,kts,kte,nt)

   implicit none


   integer, intent(in )      ::     its,ite, kts,kte, nt

   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl

   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2

   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2

   real    :: fk
   integer :: i,k,l,n,it



   l = ite
   n = kte

   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       au(i,1) = fk*cu(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo

   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         au(i,k) = fk*cu(i,k)
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo

   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo

   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo

   end subroutine tridin_ysu



   subroutine ysuinit(rublten,rvblten,rthblten,rqvblten,                       &
                      rqcblten,rqiblten,p_qi,p_first_scalar,                   &
                      restart, allowed_to_read,                                &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )

   implicit none


   logical , intent(in)          :: restart, allowed_to_read
   integer , intent(in)          ::  ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
   integer , intent(in)          ::  p_qi,p_first_scalar
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
   integer :: i, j, k, itf, jtf, ktf

   jtf = min0(jte,jde-1)
   ktf = min0(kte,kde-1)
   itf = min0(ite,ide-1)

   if(.not.restart)then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
            rublten(i,k,j) = 0.
            rvblten(i,k,j) = 0.
            rthblten(i,k,j) = 0.
            rqvblten(i,k,j) = 0.
            rqcblten(i,k,j) = 0.
         enddo
       enddo
     enddo
   endif

   if (p_qi .ge. p_first_scalar .and. .not.restart) then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
           rqiblten(i,k,j) = 0.
         enddo
       enddo
     enddo
   endif

   end subroutine ysuinit



      SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea)


      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      INTEGER,INTENT(IN) :: KTS,KTE
      REAL, INTENT(OUT) :: zi
      REAL, INTENT(IN) :: landsea
      REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
      REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
      
      REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
      REAL :: delt_thv   
      REAL, PARAMETER :: sbl_lim  = 200. 
      REAL, PARAMETER :: sbl_damp = 400. 
      INTEGER :: I,J,K,kthv,ktke

      
      k = kts+1
      kthv = 1
      ktke = 1
      maxqke = 0.
      minthv = 9.E9

      DO WHILE (zw1D(k) .LE. 500.)
        qtke  =MAX(Qke1D(k),0.)   
         IF (maxqke < qtke) then
            maxqke = qtke
            ktke = k
         ENDIF
         IF (minthv > thetav1D(k)) then
             minthv = thetav1D(k)
             kthv = k
         ENDIF
         k = k+1
      ENDDO
      
      TKEeps = maxqke/40.
      TKEeps = MAX(TKEeps,0.025)
      TKEeps = MIN(TKEeps,0.25)

      
      zi=0.
      k = kthv+1
      IF((landsea-1.5).GE.0)THEN
      
          delt_thv = 0.75
      ELSE
      
          delt_thv = 1.5
      ENDIF

      zi=0.
      k = kthv+1
      DO WHILE (zi .EQ. 0.)
         IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
            zi = zw1D(k) - dz1D(k-1)* &
              & MIN((thetav1D(k)-(minthv + delt_thv))/MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
        ENDIF
        k = k+1
         IF (k .EQ. kte-1) zi = zw1D(kts+1) 
      ENDDO

      
      
      
      
      
      

      PBLH_TKE=0.
      k = ktke+1
     DO WHILE (PBLH_TKE .EQ. 0.)
        
         qtke  =MAX(Qke1D(k)/2.,0.)      
         qtkem1=MAX(Qke1D(k-1)/2.,0.)
         IF (qtke .LE. TKEeps) THEN
               PBLH_TKE = zw1D(k) - dz1D(k-1)* &
               & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
             
             PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
             
         ENDIF
         k = k+1
         IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) 
      ENDDO

    

      wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
      zi=PBLH_TKE*(1.-wt) + zi*wt

   END SUBROUTINE GET_PBLH


end module module_bl_ysu

