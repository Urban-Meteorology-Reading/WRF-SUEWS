





























































MODULE module_bl_mynn

  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, r_v, rcp, xlv, xlf, xls, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2, rvovrd, &
       &cpv, cliq, cice

  USE module_state_description, only: param_first_scalar, &
       &p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni

  USE module_wrf_error

  IMPLICIT NONE



  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

  REAL, PARAMETER :: xlvcp=xlv/cp, xlscp=(xlv+xlf)/cp, ev=xlv, rd=r_d, &
       &rk=cp/rd, svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2

  REAL, PARAMETER :: tref=300.0     
  REAL, PARAMETER :: TKmin=253.0    
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref


  REAL, PARAMETER :: &
       &vk  = karman, &
       &pr  =  0.74,  &
       &g1  =  0.235, &  
       &b1  = 24.0, &
       &b2  = 15.0, &    
       &c2  =  0.729, &  
       &c3  =  0.340, &  
       &c4  =  0.0, &
       &c5  =  0.2, &
       &a1  = b1*( 1.0-3.0*g1 )/6.0, &

       &c1  = g1 -1.0/( 3.0*a1*2.88449914061481660), &
       &a2  = a1*( g1-c1 )/( g1*pr ), &
       &g2  = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )

  REAL, PARAMETER :: &
       &cc2 =  1.0-c2, &
       &cc3 =  1.0-c3, &
       &e1c =  3.0*a2*b2*cc3, &
       &e2c =  9.0*a1*a2*cc2, &
       &e3c =  9.0*a2*a2*cc2*( 1.0-c5 ), &
       &e4c = 12.0*a1*a2*cc2, &
       &e5c =  6.0*a1*a1



  REAL, PARAMETER :: qmin=0.0, zmax=1.0, Sqfac=3.0





  REAL, PARAMETER :: gno=1.0  
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12


  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423


  REAL, PARAMETER  :: zero = 0.0, half = 0.5, one = 1.0, two = 2.0

  
  
  
  
  
  
  
  
  REAL, PARAMETER :: CKmod=1.

  
  
  
  REAL, PARAMETER :: scaleaware=1.


  
  INTEGER, PARAMETER :: bl_mynn_mixchem = 0






  REAL, PARAMETER:: J0= .611583699E03
  REAL, PARAMETER:: J1= .444606896E02
  REAL, PARAMETER:: J2= .143177157E01
  REAL, PARAMETER:: J3= .264224321E-1
  REAL, PARAMETER:: J4= .299291081E-3
  REAL, PARAMETER:: J5= .203154182E-5
  REAL, PARAMETER:: J6= .702620698E-8
  REAL, PARAMETER:: J7= .379534310E-11
  REAL, PARAMETER:: J8=-.321582393E-13

  REAL, PARAMETER:: K0= .609868993E03
  REAL, PARAMETER:: K1= .499320233E02
  REAL, PARAMETER:: K2= .184672631E01
  REAL, PARAMETER:: K3= .402737184E-1
  REAL, PARAMETER:: K4= .565392987E-3
  REAL, PARAMETER:: K5= .521693933E-5
  REAL, PARAMETER:: K6= .307839583E-7
  REAL, PARAMETER:: K7= .105785160E-9
  REAL, PARAMETER:: K8= .161444444E-12




























  INTEGER :: mynn_level

  CHARACTER*128 :: mynn_message

  INTEGER, PARAMETER :: kdebug=27

CONTAINS






























































































































  SUBROUTINE  mym_initialize (                                & 
       &            kts,kte,                                  &
       &            dz, zw,                                   &
       &            u, v, thl, qw,                            &

       &            zi, theta, sh,                            &
       &            ust, rmo, el,                             &
       &            Qke, Tsq, Qsq, Cov, Psig_bl, cldfra_bl1D, &
       &            bl_mynn_mixlength,                        &
       &            edmf_a1,edmf_qc1,bl_mynn_edmf,            &
       &            spp_pbl,rstoch_col)


    
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf

    REAL, INTENT(IN)   :: ust, rmo, Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,cldfra_bl1D,&
                                            edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke

    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta

    REAL, DIMENSION(kts:kte) :: rstoch_col
    INTEGER ::spp_pbl


    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO

    CALL mym_level2 ( kts,kte,&
         &            dz,  &
         &            u, v, thl, qw, &
         &            ql, vt, vq, &
         &            dtl, dqw, dtv, gm, gh, sm, sh )



    el (kts) = 0.0
    qke(kts) = ust**2 * ( b1*pmz )**(2.0/3.0)

    phm      = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )

    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
       qke(k) = 0.0

       tsq(k) = 0.0
       qsq(k) = 0.0
       cov(k) = 0.0
    END DO



    lmax = 5 

    DO l = 1,lmax

       CALL mym_length (                     &
            &            kts,kte,            &
            &            dz, zw,             &
            &            rmo, flt, flq,      &
            &            vt, vq,             &
            &            qke,                &
            &            dtv,                &
            &            el,                 &
            &            zi,theta,           &
            &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
            &            edmf_a1,edmf_qc1,bl_mynn_edmf,&
            &            spp_pbl,rstoch_col)

       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm (k)+&
               &sh(k)*gh (k) )
          pdt(k) = elq*  sh(k)*dtl(k)**2
          pdq(k) = elq*  sh(k)*dqw(k)**2
          pdc(k) = elq*  sh(k)*dtl(k)*dqw(k)
       END DO


       vkz = vk*0.5*dz(kts)

       elv = 0.5*( el(kts+1)+el(kts) ) /  vkz 
       qke(kts) = ust**2 * ( b1*pmz*elv    )**(2.0/3.0)

       phm      = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )

       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          tmpq=MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin)

          qke(k) = tmpq**(2.0/3.0)


          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO


    END DO






    qke(kte)=qke(kte-1)
    tsq(kte)=tsq(kte-1)
    qsq(kte)=qsq(kte-1)
    cov(kte)=cov(kte-1)




  END SUBROUTINE mym_initialize
  

















  SUBROUTINE  mym_level2 (kts,kte,&
       &            dz, &
       &            u, v, thl, qw, &
       &            ql, vt, vq, &
       &            dtl, dqw, dtv, gm, gh, sm, sh )



    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf


    REAL ::   a2den







    rfc = g1/( g1+g2 )
    f1  = b1*( g1-c1 ) +3.0*a2*( 1.0    -c2 )*( 1.0-c5 ) &
    &                   +2.0*a1*( 3.0-2.0*c2 )
    f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
    rf1 = b1*( g1-c1 )/f1
    rf2 = b1*  g1     /f2
    smc = a1 /a2*  f1/f2
    shc = 3.0*a2*( g1+g2 )

    ri1 = 0.5/smc
    ri2 = rf1*smc
    ri3 = 4.0*rf2*smc -2.0*ri2
    ri4 = ri2**2

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       dtz = ( thl(k)-thl(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )

       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk  
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk  
       dtq =  vtt*dtz +vqq*dqz

       dtl(k) =  dtz
       dqw(k) =  dqz
       dtv(k) =  dtq




       gm (k) =  duz
       gh (k) = -dtq*gtr


       ri = -gh(k)/MAX( duz, 1.0e-10 )


    IF (CKmod .eq. 1) THEN
       a2den = 1. + MAX(ri,0.0)
    ELSE
       a2den = 1. + 0.0
    ENDIF

       rfc = g1/( g1+g2 )
       f1  = b1*( g1-c1 ) +3.0*(a2/a2den)*( 1.0    -c2 )*( 1.0-c5 ) &
    &                     +2.0*a1*( 3.0-2.0*c2 )
       f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
       rf1 = b1*( g1-c1 )/f1
       rf2 = b1*  g1     /f2
       smc = a1 /(a2/a2den)*  f1/f2
       shc = 3.0*(a2/a2den)*( g1+g2 )

       ri1 = 0.5/smc
       ri2 = rf1*smc
       ri3 = 4.0*rf2*smc -2.0*ri2
       ri4 = ri2**2



       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )

       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO

    RETURN

  END SUBROUTINE mym_level2
















  SUBROUTINE  mym_length (                     & 
    &            kts,kte,                      &
    &            dz, zw,                       &
    &            rmo, flt, flq,                &
    &            vt, vq,                       &
    &            qke,                          &
    &            dtv,                          &
    &            el,                           &
    &            zi,theta,                     &
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_a1,edmf_qc1,bl_mynn_edmf,&
    &            spp_pbl,rstoch_col)
    


    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: qke,vt,vq,cldfra_bl1D,&
                                              edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc

    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,wt2,zi,zi2,h1,h2,hs,elBLmin0,elBLavg0

    
    
    REAL :: cns,   &   
            alp1,  &   
            alp2,  &   
            alp3,  &   
            alp4,  &   
            alp5       

    
    
    
    
    REAL, PARAMETER :: minzi = 300.  
    REAL, PARAMETER :: maxdz = 750.  
                                     
                                     
    REAL, PARAMETER :: mindz = 300.  

    
    REAL, PARAMETER :: ZSLH = 100. 
    REAL, PARAMETER :: CSL = 2.    
    REAL :: z_m


    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,zwk1,dzk,qdz,vflx,bv,tau_cloud,elb,els,els1,elf, &
            & el_stab,el_unstab,el_mf,el_stab_mf,elb_mf,PBLH_PLUS_ENT

    INTEGER, INTENT(IN)   :: spp_pbl
    REAL, DIMENSION(kts:kte), INTENT(in)   :: rstoch_col

    IF ( bl_mynn_mixlength .EQ. 0 ) THEN
       cns  = 2.7
       alp1 = 0.23
       alp2 = 1.0
       alp3 = 5.0
       alp4 = 100.
       alp5 = 0.4
    ELSEIF ( bl_mynn_mixlength .EQ. 1 ) THEN
       cns  = 2.3
       alp1 = 0.23
       alp2 = 0.6
       alp3 = 3.0
       alp4 = 20.
       alp5 = 0.4
    ELSEIF ( bl_mynn_mixlength .GE. 2 ) THEN
       cns  = 3.5
       alp1 = 0.23
       alp2 = 0.25
       alp3 = 3.0
       alp4 = 10.
       alp5 = 0.3
    ENDIF






    IF ( bl_mynn_mixlength .EQ. 0 ) THEN
       zi2=10000.  
    ELSE
       zi2=MAX(zi,minzi)
    ENDIF
    h1=MAX(0.3*zi2,mindz)
    h1=MIN(h1,maxdz)         
    h2=h1/2.0                

    qtke(kts)=MAX(qke(kts)/2.,0.01) 
    thetaw(kts)=theta(kts)          
    qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

    DO k = kts+1,kte
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
       qtke(k) = (qkw(k)**2.)/2.    
       thetaw(k)= theta(k)*abk + theta(k-1)*afk
    END DO

    elt = 1.0e-5
    vsc = 1.0e-5





    IF ( bl_mynn_mixlength .EQ. 2 ) THEN
       PBLH_PLUS_ENT = MAX(zi, 100.)
    ELSE
       PBLH_PLUS_ENT = zi2+h1
    ENDIF
    k = kts+1
    zwk = zw(k)
    DO WHILE (zwk .LE. PBLH_PLUS_ENT)
       dzk = 0.5*( dz(k)+dz(k-1) )
       qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
             elt = elt +qdz*zwk
             vsc = vsc +qdz
       k   = k+1
       zwk = zw(k)
    END DO

    elt =  MAX(alp1*elt/vsc, 10.)
    vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
    vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)


    el(kts) = 0.0

    IF ( bl_mynn_mixlength .EQ. 1 ) THEN
       
       CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)
    ENDIF

    DO k = kts+1,kte
       zwk = zw(k)              
       IF (k .EQ. kts+1) zwk1=zwk 

       

       IF ( dtv(k) .GT. 0.0 ) THEN
         bv  = SQRT( gtr*dtv(k) )

         IF ( bl_mynn_mixlength .EQ. 0 ) THEN 
           elb = alp2*qkw(k) / bv &     
                &       *( 1.0 + alp3/alp2*&
                &SQRT( vsc/( bv*elt ) ) )
           elf = alp2 * qkw(k)/bv

         ELSE IF ( bl_mynn_mixlength .EQ. 1 ) THEN 
           elb = alp2*qkw(k) / bv &                
                &       *( 1.0 + alp3/alp2*&       
                &SQRT( vsc/( bv*elt ) ) )          
           elb = MIN(elb, zwk)                     
           elf = alp2 * qkw(k)/bv                  

         ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN 
           elb_mf = alp2*qkw(k) / bv &
                &       *( 1.0 + alp3/alp2*&
                &SQRT( vsc/( bv*elt ) ) )
           elb = MIN(alp2*qkw(k)/bv, zwk)
           elf = elb
           IF (zwk > zi .AND. elf > 500.) THEN
             
             CALL boulac_length0(k,kts,kte,zw,dz,qtke,thetaw,elBLmin0,elBLavg0)
             elf = alp5*elBLavg0
           ENDIF

         END IF 

       ELSE

         IF ( bl_mynn_mixlength .LE. 1 ) THEN 
           elb = 1.0e10
           elf = elb

         ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN
           
           
           
           
           
           
           
           
           
           

           tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(flt,1.0e-4))**(1.0/3.0)),10.),100.)
           
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
           tau_cloud = tau_cloud*(1.-wt) + 50.*wt

           elb = MIN(tau_cloud*SQRT(MIN(qtke(k),20.)), zwk)
           elf = elb
           elb_mf = elb
           IF (zwk > zi .AND. elf > 500.) THEN
             
             CALL boulac_length0(k,kts,kte,zw,dz,qtke,thetaw,elBLmin0,elBLavg0)
             elf = alp5*elBLavg0*(1.-cldfra_bl1D(k)) + elf*cldfra_bl1D(k)
           END IF
           elf = elf*(1.-cldfra_bl1D(k)) + elb*cldfra_bl1D(k)

         END IF
       END IF

       z_m = MAX(0.,zwk - 4.)


       IF ( rmo .GT. 0.0 ) THEN
          els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
          els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
       ELSE
          els  =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
          els1 =  vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
       END IF






       wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

       IF ( bl_mynn_mixlength .EQ. 0 ) THEN
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)

       ELSE IF ( bl_mynn_mixlength .EQ. 1 ) THEN 
          
          
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
          el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt

       
       ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN
          el_unstab = els/(1. + (els1/elt))

          el_stab    = MIN(el_unstab, elb) 
          el_stab_mf = MIN(el_unstab, elb_mf)

          IF (bl_mynn_edmf > 0 .AND. edmf_a1(kts)>0.0) THEN
             
             
              el(k) = el_stab_mf
          ELSE
              el(k) = el_stab
          ENDIF
          el(k) = el(k)*(1.-wt) + elf*wt
       END IF

       IF ( bl_mynn_mixlength .GE. 1 ) THEN 
         el(k) = el(k)*Psig_bl
       END IF


       if (spp_pbl==1) then
          if (k.lt.25) then
             el(k)= el(k) + el(k)* rstoch_col(k) * 1.5 * MAX(exp(-MAX(zwk-3000.,0.0)/2000.),0.01)
          endif
       endif


       IF ( wrf_at_debug_level(3000) ) THEN
         IF (el(k) > 1000.) THEN
           WRITE ( mynn_message , FMT='(A,F7.0,I5,4F7.0)' ) &
           ' MYNN; mym_length; LARGE el,k,elb,elt,elf,tau:'&
                                , el(k),k,elb,elt,elf,tau_cloud
           CALL wrf_debug ( 0 , mynn_message )
         ENDIF
       ENDIF

    END DO

    RETURN

  END SUBROUTINE mym_length




  SUBROUTINE boulac_length0(k,kts,kte,zw,dz,qtke,theta,lb1,lb2)

















     INTEGER, INTENT(IN) :: k,kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw

     
     INTEGER :: izz, found
     REAL :: dlu,dld
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz


     
     
     
     zup=0.
     dlu=zw(kte+1)-zw(k)-dz(k)/2.
     zzz=0.
     zup_inf=0.
     beta=g/theta(k)           

     

     if (k .lt. kte) then      
        found = 0
        izz=k
        DO WHILE (found .EQ. 0)

           if (izz .lt. kte) then
              dzt=dz(izz)                    
              zup=zup-beta*theta(k)*dzt     
              
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2. 
              zzz=zzz+dzt                   
              
              if (qtke(k).lt.zup .and. qtke(k).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    
                    tl=(-beta*(theta(izz)-theta(k)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      &       2.*bbb*beta*(qtke(k)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k))then
                       tl=(qtke(k)-zup_inf)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dlu=zzz-dzt+tl
                 
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
           ELSE
              found = 1
           ENDIF

        ENDDO

     endif

     
     
     
     zdo=0.
     zdo_sup=0.
     dld=zw(k)
     zzz=0.

     
     if (k .gt. kts) then  

        found = 0
        izz=k
        DO WHILE (found .EQ. 0)

           if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(k)*dzt
              
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              
              if (qtke(k).lt.zdo .and. qtke(k).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(k))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      &       2.*bbb*beta*(qtke(k)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k)) then
                       tl=(qtke(k)-zdo_sup)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dld=zzz-dzt+tl
                 
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
           ELSE
              found = 1
           ENDIF
        ENDDO

     endif

     
     
     
     
     
     dld = min(dld,zw(k+1))
     lb1 = min(dlu,dld)     
     
     dlu=MAX(0.1,MIN(dlu,1000.))
     dld=MAX(0.1,MIN(dld,1000.))
     lb2 = sqrt(dlu*dld)    
     

     if (k .eq. kte) then
        lb1 = 0.
        lb2 = 0.
     endif
     
     

  END SUBROUTINE boulac_length0


  SUBROUTINE boulac_length(kts,kte,zw,dz,qtke,theta,lb1,lb2)

















     INTEGER, INTENT(IN) :: kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, DIMENSION(kts:kte), INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw

     
     INTEGER :: iz, izz, found
     REAL, DIMENSION(kts:kte) :: dlu,dld
     REAL, PARAMETER :: Lmax=2000.  
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz

     

     do iz=kts,kte

        
        
        
        zup=0.
        dlu(iz)=zw(kte+1)-zw(iz)-dz(iz)/2.
        zzz=0.
        zup_inf=0.
        beta=g/theta(iz)           

        

        if (iz .lt. kte) then      

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .lt. kte) then
              dzt=dz(izz)                    
              zup=zup-beta*theta(iz)*dzt     
              
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2. 
              zzz=zzz+dzt                   
              
              if (qtke(iz).lt.zup .and. qtke(iz).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    
                    tl=(-beta*(theta(izz)-theta(iz)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz))then
                       tl=(qtke(iz)-zup_inf)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dlu(iz)=zzz-dzt+tl
                 
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
             ELSE
              found = 1
            ENDIF

          ENDDO

        endif
                   
        
        
        
        zdo=0.
        zdo_sup=0.
        dld(iz)=zw(iz)
        zzz=0.

        
        if (iz .gt. kts) then  

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(iz)*dzt
              
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              
              if (qtke(iz).lt.zdo .and. qtke(iz).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(iz))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz)) then
                       tl=(qtke(iz)-zdo_sup)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dld(iz)=zzz-dzt+tl
                 
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
            ELSE
              found = 1
            ENDIF
          ENDDO

        endif

        
        
        
        
        
        dld(iz) = min(dld(iz),zw(iz+1))
        lb1(iz) = min(dlu(iz),dld(iz))     
        
        dlu(iz)=MAX(0.1,MIN(dlu(iz),1000.))
        dld(iz)=MAX(0.1,MIN(dld(iz),1000.))
        lb2(iz) = sqrt(dlu(iz)*dld(iz))    
        

        
        lb1(iz) = lb1(iz)/(1. + (lb1(iz)/Lmax))
        lb2(iz) = lb2(iz)/(1. + (lb2(iz)/Lmax))
 
        if (iz .eq. kte) then
           lb1(kte) = lb1(kte-1)
           lb2(kte) = lb2(kte-1)
        endif
        
        

     ENDDO
                   
  END SUBROUTINE boulac_length



































  SUBROUTINE  mym_turbulence (                                &
    &            kts,kte,                                     &
    &            levflag,                                     &
    &            dz, zw,                                      &
    &            u, v, thl, ql, qw,                           &
    &            qke, tsq, qsq, cov,                          &
    &            vt, vq,                                      &
    &            rmo, flt, flq,                               &
    &            zi,theta,                                    &
    &            sh,                                          &
    &            El,                                          &
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
    &		 qWT1D,qSHEAR1D,qBUOY1D,qDISS1D,              &
    &            bl_mynn_tkebudget,                           &
    &            Psig_bl,Psig_shcu,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_a1,edmf_qc1,bl_mynn_edmf,&
    &            spp_pbl,rstoch_col)



    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: levflag,bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,Psig_shcu
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov,cldfra_bl1D,edmf_a1,edmf_qc1

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el


    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp
    INTEGER, INTENT(in) :: bl_mynn_tkebudget


    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh

    REAL :: zi
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta

    REAL ::  a2den, duz, ri, HLmod  

    REAL:: auh,aum,adh,adm,aeh,aem,Req,Rsl,Rsl2


    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden


    INTEGER,  INTENT(IN)                          ::    spp_pbl
    REAL, DIMENSION(KTS:KTE)                      ::    rstoch_col
    REAL :: prlimit















    CALL mym_level2 (kts,kte,&
    &            dz, &
    &            u, v, thl, qw, &
    &            ql, vt, vq, &
    &            dtl, dqw, dtv, gm, gh, sm, sh )

    CALL mym_length (                           &
    &            kts,kte,                       &
    &            dz, zw,                        &
    &            rmo, flt, flq,                 &
    &            vt, vq,                        &
    &            qke,                           &
    &            dtv,                           &
    &            el,                            &
    &            zi,theta,                      &
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength, &
    &            edmf_a1,edmf_qc1,bl_mynn_edmf, &
    &            spp_pbl,rstoch_col)


    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )
       q3sq = qkw(k)**2


       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       
       ri = -gh(k)/MAX( duz, 1.0e-10 )
       IF (CKmod .eq. 1) THEN
          a2den = 1. + MAX(ri,0.0)
       ELSE
          a2den = 1. + 0.0
       ENDIF



       gmel = gm (k)*elsq
       ghel = gh (k)*elsq


       
       IF ( wrf_at_debug_level(3000) ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
           WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
           " MYNN; mym_turbulence2.0; sh=",sh(k)," k=",k
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " qke=",qke(k)," el=",el(k)," ri=",ri
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " PBLH=",zi," u=",u(k)," v=",v(k)
           CALL wrf_debug ( 0 , mynn_message )
         ENDIF
       ENDIF



       IF (CKmod .eq. 1) THEN
          HLmod = q2sq -1.
       ELSE
          HLmod = q3sq
       ENDIF





          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)


       IF ( q3sq .LT. q2sq ) THEN
       
          
          qdiv = SQRT( q3sq/q2sq )   
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv

          
          
          
          
          
          e1   = q3sq - e1c*ghel/a2den * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = e1   + e3c*ghel/(a2den**2) * qdiv**2
          e4   = e1   - e4c*ghel/a2den * qdiv**2
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE
          
          
          
          
          
          e1   = q3sq - e1c*ghel/a2den
          e2   = q3sq - e2c*ghel/a2den
          e3   = e1   + e3c*ghel/(a2den**2)
          e4   = e1   - e4c*ghel/a2den
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )

          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
          
          
          sh(k) = q3sq*(a2/a2den)*( e2+3.0*c1*e5c*gmel )/eden
       END IF 

       
       
       IF ( wrf_at_debug_level(3000) ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
           WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
           " MYNN; mym_turbulence2.5; sh=",sh(k)," k=",k
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " qke=",qke(k)," el=",el(k)," ri=",ri
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " PBLH=",zi," u=",u(k)," v=",v(k)
           CALL wrf_debug ( 0 , mynn_message )
         ENDIF
       ENDIF


       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq =      cov(k)*abk+cov(k-1)*afk


          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )

          vtt  = 1.0 +vt(k)*abk +vt(k-1)*afk
          vqq  = tv0 +vq(k)*abk +vq(k-1)*afk
          t2sq = vtt*t2sq +vqq*c2sq
          r2sq = vtt*c2sq +vqq*r2sq
          c2sq = MAX( vtt*t2sq+vqq*r2sq, 0.0d0 )
          t3sq = vtt*t3sq +vqq*c3sq
          r3sq = vtt*c3sq +vqq*r3sq
          c3sq = MAX( vtt*t3sq+vqq*r3sq, 0.0d0 )

          cw25 = e1*( e2 + 3.0*c1*e5c*gmel*qdiv**2 )/( 3.0*eden )


          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)



          
          auh = 27.*a1*((a2/a2den)**2)*b2*(g/tref)**2
          aum = 54.*(a1**2)*(a2/a2den)*b2*c1*(g/tref)
          adh = 9.*a1*((a2/a2den)**2)*(12.*a1 + 3.*b2)*(g/tref)**2
          adm = 18.*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))*(g/tref)

          aeh = (9.*a1*((a2/a2den)**2)*b1 +9.*a1*((a2/a2den)**2)* &
                (12.*a1 + 3.*b2))*(g/tref)
          aem = 3.*a1*(a2/a2den)*b1*(3.*(a2/a2den) + 3.*b2*c1 + &
                (18.*a1*c1 - b2)) + &
                (18.)*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))

          Req = -aeh/aem
          Rsl = (auh + aum*Req)/(3.*adh + 3.*adm*Req)
          
          Rsl = .12             
          Rsl2= 1.0 - 2.*Rsl    
          
          


          

          
          
          

          
          
          
          
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = q3sq + e3c*ghel/(a2den**2) * qdiv**2
          e4   = q3sq - e4c*ghel/a2den * qdiv**2
          eden = e2*e4  + e3 *e5c*gmel * qdiv**2

          
          
          
          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c/a2den - e3c*e5c*gmel/(a2den**2) * qdiv**2 )

          IF ( wden .NE. 0.0 ) THEN
             
             
             
             clow = q3sq*( Rsl -cw25 )*eden/wden
             cupp = q3sq*( Rsl2-cw25 )*eden/wden

             IF ( wden .GT. 0.0 ) THEN
                c3sq  = MIN( MAX( c3sq, c2sq+clow ), c2sq+cupp )
             ELSE
                c3sq  = MAX( MIN( c3sq, c2sq+clow ), c2sq+cupp )
             END IF
          END IF

          e1   = e2 + e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )


          
          
          e6c  = 3.0*(a2/a2den)*cc3*gtr * dlsq/elsq

          
          
          
          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF
          gamt =-e1  *enum    /eden

          
          
          
          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF
          gamq =-e1  *enum    /eden

          

          
          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)

          
          
          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c/(a2den**2) + &
               & e4c/a2den)*a1/(a2/a2den)

          gamv = e1  *enum*gtr/eden
          sm(k) = sm(k) +smd

          
          
          qdiv = 1.0

          
          IF ( wrf_at_debug_level(3000) ) THEN
            IF (sh(k)<-0.3 .OR. sm(k)<-0.3 .OR. &
              qke(k) < -0.1 .or. ABS(smd) .gt. 2.0) THEN
              WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
              " MYNN; mym_turbulence3.0; sh=",sh(k)," k=",k
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " qke=",qke(k)," el=",el(k)," ri=",ri
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " PBLH=",zi," u=",u(k)," v=",v(k)
              CALL wrf_debug ( 0 , mynn_message )
            ENDIF
          ENDIF



       ELSE

          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF


       if (spp_pbl==1) then
          prlimit = MIN(MAX(1.,2.5 + 5.0*rstoch_col(k)), 10.)
          IF(sm(k) > sh(k)*Prlimit) THEN
             sm(k) = sh(k)*Prlimit
          ENDIF
       ENDIF

       elq = el(k)*qkw(k)
       elh = elq*qdiv

       
       
       pdk(k) = elq*( sm(k)*gm(k) &
            &                    +sh(k)*gh(k)+gamv ) 
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5

       
       tcd(k) = elq*gamt
       qcd(k) = elq*gamq

       
       dfm(k) = elq*sm(k) / dzk
       dfh(k) = elq*sh(k) / dzk



       dfq(k) =     dfm(k)


   IF ( bl_mynn_tkebudget == 1) THEN
       
       dudz = ( u(k)-u(k-1) )/dzk
       dvdz = ( v(k)-v(k-1) )/dzk
       dTdz = ( thl(k)-thl(k-1) )/dzk

       upwp = -elq*sm(k)*dudz
       vpwp = -elq*sm(k)*dvdz
       Tpwp = -elq*sh(k)*dTdz
       Tpwp = SIGN(MAX(ABS(Tpwp),1.E-6),Tpwp)

       IF ( k .EQ. kts+1 ) THEN
          qWT1D(kts)=0.
          q3sq_old =0.
          qWTP_old =0.
          
          dlsq1 = MAX(el(kts)**2,1.0)
          IF ( q3sq_old/dlsq1 .LT. -gh(k) ) q3sq_old = -dlsq1*gh(k)
       ENDIF

       
       qWTP_new = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       qWT1D(k) = 0.5*(qWTP_new - qWTP_old)/dzk
       qWTP_old = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       q3sq_old = q3sq

       
       
       qSHEAR1D(k) = elq*sm(k)*gm(k)

       
       
       
       qBUOY1D(k) = elq*(sh(k)*(-dTdz*g/thl(k)) + gamv)

       
       qDISS1D(k) = (q3sq**(3./2.))/(b1*MAX(el(k),1.))
    ENDIF

    END DO


    dfm(kts) = 0.0
    dfh(kts) = 0.0
    dfq(kts) = 0.0
    tcd(kts) = 0.0
    qcd(kts) = 0.0

    tcd(kte) = 0.0
    qcd(kte) = 0.0


    DO k = kts,kte-1
       dzk = dz(k)
       tcd(k) = ( tcd(k+1)-tcd(k) )/( dzk )
       qcd(k) = ( qcd(k+1)-qcd(k) )/( dzk )
    END DO


   IF ( bl_mynn_tkebudget == 1) THEN
      
      qWT1D(kts)=0.
      qSHEAR1D(kts)=qSHEAR1D(kts+1)
      qBUOY1D(kts)=qBUOY1D(kts+1)
      qDISS1D(kts)=qDISS1D(kts+1)
   ENDIF

    RETURN

  END SUBROUTINE mym_turbulence













































  SUBROUTINE  mym_predict (kts,kte,&
       &            levflag,  &
       &            delt,&
       &            dz, &
       &            ust, flt, flq, pmz, phh, &
       &            el, dfq, &
       &            pdk, pdt, pdq, pdc,&
       &            qke, tsq, qsq, cov, &
       &            s_aw,s_awqke,bl_mynn_edmf_tke &
       &)


    INTEGER, INTENT(IN) :: kts,kte    
    INTEGER, INTENT(IN) :: levflag
    INTEGER, INTENT(IN) :: bl_mynn_edmf_tke
    REAL, INTENT(IN)    :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN)    ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov

    REAL, DIMENSION(kts:kte+1), INTENT(INOUT) :: s_awqke,s_aw

    INTEGER :: k,nz
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l,onoff
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    nz=kte-kts+1

    
    IF (bl_mynn_edmf_tke == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF


    vkz = vk*0.5*dz(kts)



    DO k = kts,kte

       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       df3q(k)=Sqfac*dfq(k)
       dtz(k)=delt/dz(k)
    END DO

    pdk1 = 2.0*ust**3*pmz/( vkz )
    phm  = 2.0/ust   *phh/( vkz )
    pdt1 = phm*flt**2
    pdq1 = phm*flq**2
    pdc1 = phm*flt*flq


    pdk(kts) = pdk1 -pdk(kts+1)




    pdt(kts) = pdt(kts+1)
    pdq(kts) = pdq(kts+1)
    pdc(kts) = pdc(kts+1)



    DO k = kts,kte-1
       b1l = b1*0.5*( el(k+1)+el(k) )
       bp(k) = 2.*qkw(k) / b1l
       rp(k) = pdk(k+1) + pdk(k)
    END DO







    DO k=kts,kte-1





       a(k-kts+1)=-dtz(k)*df3q(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(k-kts+1)=1. + dtz(k)*(df3q(k)+df3q(k+1)) &
                     + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff + bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k-kts+1)=rp(k)*delt + qke(k) + dtz(k)*(s_awqke(k)-s_awqke(k+1))*onoff
    ENDDO








    a(nz)=-1. 
    b(nz)=1.
    c(nz)=0.
    d(nz)=0.

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte   
       qke(k)=d(k-kts+1)
    ENDDO
      

    IF ( levflag .EQ. 3 ) THEN








       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdt(k+1) + pdt(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + tsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          tsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) +pdq(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + qsq(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          qsq(k)=d(k-kts+1)
       ENDDO
       


       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdc(k+1) + pdc(k) 
       END DO
       

       






       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + cov(k)
       ENDDO








       a(nz)=-1. 
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          cov(k)=d(k-kts+1)
       ENDDO
       
    ELSE

       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       




       tsq(kte)=tsq(kte-1)
       qsq(kte)=qsq(kte-1)
       cov(kte)=cov(kte-1)
      
    END IF

  END SUBROUTINE mym_predict
  





























  SUBROUTINE  mym_condensation (kts,kte,  &
    &            dx, dz,                  &
    &            thl, qw,                 &
    &            p,exner,                 &
    &            tsq, qsq, cov,           &
    &            Sh, el, bl_mynn_cloudpdf,&
    &            qc_bl1D, cldfra_bl1D,    &
    &            PBLH1,HFX1,              &
    &            Vt, Vq, th, sgm)



    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf
    REAL, INTENT(IN)      :: dx,PBLH1,HFX1
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov, th

    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: vt,vq,sgm

    REAL, DIMENSION(kts:kte) :: qmq,alp,a,bet,b,ql,q1,cld,RH
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qc_bl1D,cldfra_bl1D
    DOUBLE PRECISION :: t3sq, r3sq, c3sq

    REAL :: p2a,qsl,esat,qsat,tlk,qsat_tl,dqsl,cld0,q1k,eq1,qll,&
         &q2p,pt,rac,qt,t,xl,rsl,cpm,cdhdz,Fng,qww,alpha,beta,bb,ls_min,ls,wt
    INTEGER :: i,j,k

    REAL :: erf

    
    REAL::dth,dtl,dqw,dzk
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el

    
    REAL::zagl,cld9,damp,edown,RHcrit,RHmean,RHsum,RHnum,Hshcu,PBLH2,ql_limit
    REAL, PARAMETER :: Hfac = 3.0     
    REAL, PARAMETER :: HFXmin = 50.0  
    REAL            :: RH_00L, RH_00O, phi_dz, lfac
    REAL, PARAMETER :: cdz = 2.0
    REAL, PARAMETER :: mdz = 1.5

    
    REAL            :: theta1, theta2, ht1, ht2
    INTEGER         :: k_tropo






      DO k = kte-3, kts, -1
         theta1 = th(k)
         theta2 = th(k+2)
         ht1 = 44307.692 * (1.0 - (p(k)/101325.)**0.190)
         ht2 = 44307.692 * (1.0 - (p(k+2)/101325.)**0.190)
         if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND.       &
     &                       (ht1.lt.19000.) .and. (ht1.gt.4000.) ) then 
            goto 86
         endif
      ENDDO
 86   continue
      k_tropo = MAX(kts+2, k+2)


 
 

    zagl = 0.

    DO k = kts,kte-1
       t  = th(k)*exner(k)











       
       esat = esat_blend(t)
       
       qsl=ep_2*esat/(p(k)-ep_3*esat)
       
       dqsl = qsl*ep_2*ev/( rd*t**2 )
       
       RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)

       alp(k) = 1.0/( 1.0+dqsl*xlvcp )
       bet(k) = dqsl*p2a

       IF (bl_mynn_cloudpdf == 0) THEN
          
          
          
          
          t3sq = MAX( tsq(k), 0.0 )
          r3sq = MAX( qsq(k), 0.0 )
          c3sq =      cov(k)
          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
          r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
          
          qmq(k) = qw(k) -qsl
          
          
          sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
          
          q1(k)   = qmq(k) / sgm(k)
          
          cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

       ELSE IF (bl_mynn_cloudpdf == 1 .OR. bl_mynn_cloudpdf == -1) THEN
          
          
          if (k .eq. kts) then 
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if
          dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
          sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,0.1) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
          qmq(k) = qw(k) -qsl
          q1(k)   = qmq(k) / sgm(k)
          cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

       ELSE IF (bl_mynn_cloudpdf == 2 .OR. bl_mynn_cloudpdf == -2) THEN
          
          
          xl = xl_blend(t)                    

          tlk = thl(k)*(p(k)/p1000mb)**rcp    

          qsat_tl = qsat_blend(tlk,p(k))      
                                              

          rsl = xl*qsat_tl / (r_v*tlk**2)     
                                              
 
          cpm = cp + qw(k)*cpv                
     
          a(k) = 1./(1. + xl*rsl/cpm)         

          qmq(k) = a(k) * (qw(k) - qsat_tl) 
                                              

          b(k) = a(k)*rsl                     

          dtl =    0.5*(thl(k+1)*(p(k+1)/p1000mb)**rcp + tlk) &
               & - 0.5*(tlk + thl(MAX(k-1,kts))*(p(MAX(k-1,kts))/p1000mb)**rcp)

          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))

          if (k .eq. kts) then
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if

          cdhdz = dtl/dzk + (g/cpm)*(1.+qw(k))  
                                                

          zagl = zagl + dz(k)
          ls_min = MIN(MAX(zagl,25.),300.) 
                                       
          lfac=MIN(4.25+dx/4000.,6.)   
                                       
                                       
                                       

          ls = MAX(MIN(lfac*el(k),900.),ls_min)  
                  

                  
                  
                  

          sgm(k) = MAX(1.e-10, 0.225*ls*SQRT(MAX(0., & 
                  & (a(k)*dqw/dzk)**2              & 
                  & -2*a(k)*b(k)*cdhdz*dqw/dzk     & 
                  & +b(k)**2 * cdhdz**2)))           
                  
                  

          q1(k) = qmq(k) / sgm(k)  

          cld(k) = MAX(0., MIN(1., 0.5+0.36*ATAN(1.55*q1(k)))) 

       ENDIF
    END DO

    zagl = 0.
    RHsum=0.
    RHnum=0.
    RHmean=0.1 
    damp =0
    PBLH2=MAX(10.,PBLH1)

    DO k = kts,kte-1
         t    = th(k)*exner(k)
         q1k  = q1(k)
         zagl = zagl + dz(k)
         
         

         IF ( bl_mynn_cloudpdf <= 1 .AND. bl_mynn_cloudpdf >= -1) THEN

              
              IF (zagl < PBLH2 .AND. PBLH2 > 400.) THEN
                 RHsum=RHsum+RH(k)
                 RHnum=RHnum+1.0
                 RHmean=RHsum/RHnum
              ENDIF

              RHcrit = 1. - 0.35*(1.0 - (MAX(250.- MAX(HFX1,HFXmin),0.0)/200.)**2)
              if(HFX1 > HFXmin)then
                 cld9=MIN(MAX(0., (rh(k)-RHcrit)/(1.1-RHcrit)), 1.)**2
              else
                 cld9=0.0
              endif
       
              edown=PBLH2*.1
              
              
              Hshcu=200. + (RHmean+0.5)**1.5*MAX(HFX1,0.)*Hfac
              if(zagl < PBLH2-edown)then
                 damp=MIN(1.0,exp(-ABS(((PBLH2-edown)-zagl)/edown)))
              elseif(zagl >= PBLH2-edown .AND. zagl < PBLH2+Hshcu)then
                 damp=1.
              elseif (zagl >= PBLH2+Hshcu)then
                 damp=MIN(1.0,exp(-ABS((zagl-(PBLH2+Hshcu))/500.)))
              endif
              cldfra_bl1D(k)=cld9*damp
              
       
              
              eq1  = rrp*EXP( -0.5*q1k*q1k )
              qll  = MAX( cldfra_bl1D(k)*q1k + eq1, 0.0 )
              
              ql (k) = alp(k)*sgm(k)*qll
              if(cldfra_bl1D(k)>0.01 .and. ql(k)<1.E-6)ql(k)=1.E-6
              qc_bl1D(k)=ql(k)*damp
              
       

              
              
              eq1  = rrp*EXP( -0.5*q1k*q1k )
              qll  = MAX( cld(k)*q1k + eq1, 0.0 )
              
              ql (k) = alp(k)*sgm(k)*qll
       
              q2p = xlvcp/exner(k)
              pt = thl(k) +q2p*ql(k) 
       
              
              qt   = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
              rac  = alp(k)*( cld(k)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )
       
              
              
              
              vt(k) =      qt-1.0 -rac*bet(k)
              vq(k) = p608*pt-tv0 +rac
       
         ELSE IF ( bl_mynn_cloudpdf == 2 .OR.  bl_mynn_cloudpdf == -2) THEN
         
         
         
         
         
         
              IF (q1k < 0.) THEN                 
                ql (k) = sgm(k)*EXP(1.2*q1k-1)
              ELSE IF (q1k > 2.) THEN
                ql (k) = sgm(k)*q1k
              ELSE
                ql (k) = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
              ENDIF
  
              
              
              
              if ((cld(k) .gt. 0.) .or. (ql(k) .gt. 0.))  then
                 if (k .le. k_tropo) then
                   
                   
                   
                    ql_limit = 0.005 * qsat_blend( th(k)*exner(k), p(k) )
                    ql(k) = MIN( ql(k), ql_limit )
                 else
                   
                    cld(k) = 0.
                    ql(k) = 0.
                 endif 
              endif
       
              
              
              
              
              
              
              
              
              
              
              
              
              
              Fng = 1.

              xl    = xl_blend(t)
              bb = b(k)*t/th(k) 
                                
                                
                                
                                
                                
              qww   = 1.+0.61*qw(k)
              alpha = 0.61*th(k)
              beta  = (th(k)/t)*(xl/cp) - 1.61*th(k)

              vt(k) = qww   - cld(k)*beta*bb*Fng   - 1.
              vq(k) = alpha + cld(k)*beta*a(k)*Fng - tv0
              
              
              
              
              

              
              if (zagl .lt. PBLH2+1000.) cld(k) = MIN( 1., 1.8*cld(k) )
              
              cldfra_bl1D(k) = cld(k)
              qc_bl1D(k) = ql(k)
       
         ENDIF 

         
         IF (bl_mynn_cloudpdf .LT. 0) THEN
              cldfra_bl1D(k) = 0.0
              qc_bl1D(k) = 0.0
         ENDIF
         
         
         IF (QC_BL1D(k) < 1E-6 .AND. ABS(CLDFRA_BL1D(k)) > 0.01) QC_BL1D(k)= 1E-6
         IF (CLDFRA_BL1D(k) < 1E-2)THEN
            CLDFRA_BL1D(k)=0.
            QC_BL1D(k)=0.
         ENDIF

    END DO


    cld(kte) = cld(kte-1)
    ql(kte) = ql(kte-1)
    vt(kte) = vt(kte-1)
    vq(kte) = vq(kte-1)
    qc_bl1D(kte)=0.
    cldfra_bl1D(kte)=0.

    RETURN

  END SUBROUTINE mym_condensation


  SUBROUTINE mynn_tendencies(kts,kte,      &
       &levflag,grav_settling,             &
       &delt,dz,                           &
       &u,v,th,tk,qv,qc,qi,qni,qnc,        &
       &p,exner,                           &
       &thl,sqv,sqc,sqi,sqw,               &
       &ust,flt,flq,flqv,flqc,wspd,qcg,    &
       &uoce,voce,                         &
       &tsq,qsq,cov,                       &
       &tcd,qcd,                           &
       &dfm,dfh,dfq,                       &
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqni,        &
       &vdfg1,                             &
       &s_aw,s_awthl,s_awqt,s_awqv,s_awqc, &
       &s_awu,s_awv,                       &
       &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC, &
       &cldfra_bl1d,                       &
       &bl_mynn_cloudmix,                  &
       &bl_mynn_mixqt,                     &
       &bl_mynn_edmf,                      &
       &bl_mynn_edmf_mom                  )


    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag
    INTEGER, INTENT(in) :: bl_mynn_cloudmix,bl_mynn_mixqt,&
                           bl_mynn_edmf,bl_mynn_edmf_mom
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC









    REAL,DIMENSION(kts:kte+1), INTENT(in) :: s_aw,s_awthl,s_awqt,&
                             s_awqv,s_awqc,s_awu,s_awv
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,tk,qv,qc,qi,qni,qnc,&
         &p,exner,dfq,dz,tsq,qsq,cov,tcd,qcd,cldfra_bl1d
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi,&
         &dfm,dfh
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni 
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg






    REAL, DIMENSION(kts:kte) :: dtz,vt,vq,dfhc,dfmc 
    REAL, DIMENSION(kts:kte) :: sqv2,sqc2,sqi2,sqw2,qni2 

    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    REAL :: rhs,gfluxm,gfluxp,dztop,maxdfh,mindfh
    REAL :: grav_settling2,vdfg1    
    REAL :: t,esat,qsl,onoff
    INTEGER :: k,kk,nz

    nz=kte-kts+1

    dztop=.5*(dz(kte)+dz(kte-1))

    
    IF (bl_mynn_edmf_mom == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF
    
    
    maxdfh=maxval(dfh(1:15))
    mindfh=maxdfh*0.01
    DO k=kts,kte
       dtz(k)=delt/dz(k)
       IF (dfm(k) > dfh(k)) THEN 
         
         IF (qc(k) > 1.e-6 .OR. &
             qi(k) > 1.e-6 .OR. &
             cldfra_bl1D(k) > 0.05 ) THEN
             dfh(k)= MAX(dfh(k),dfm(k)*0.5)
         ENDIF
       ENDIF
       
       IF(bl_mynn_edmf==2 .AND. k > 1 .AND. s_aw(k)>0.0) THEN
           dfh(k)=MAX(mindfh,dfh(k))
           dfm(k)=MAX(mindfh,dfm(k))
       ENDIF
    ENDDO





    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
    d(1)=u(k) + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff






    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(kk)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(kk)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(kk)=u(k) + dtz(k)*(s_awu(k)-s_awu(k+1))*onoff
    ENDDO














    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=u(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       du(k)=(d(k-kts+1)-u(k))/delt
    ENDDO





    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff

    d(1)=v(k) + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff






    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(kk)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(kk)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(kk)=v(k) + dtz(k)*(s_awv(k)-s_awv(k+1))*onoff
    ENDDO














    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=v(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       dv(k)=(d(k-kts+1)-v(k))/delt
    ENDDO





    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

    rhs= tcd(k)

    d(1)=thl(k) + dtz(k)*flt + rhs*delt -dtz(1)*s_awthl(kts+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*(dfh(k) - 0.5*s_aw(k))
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1) + 0.5*(s_aw(k)-s_aw(k+1)))
       c(kk)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

       rhs= tcd(k)

       d(kk)=thl(k) + rhs*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1))
    ENDDO















    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=thl(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       thl(k)=d(k-kts+1)
    ENDDO

IF (bl_mynn_mixqt > 0) THEN
 
 
 
 
 
 

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)  - 0.5*dtz(k)*s_aw(k+1)
    c(1)=  -dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

    rhs= qcd(k) 

    d(1)=sqw(k) + dtz(k)*flq + rhs*delt - dtz(k)*s_awqt(k+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*(dfh(k) - 0.5*s_aw(k))
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1) + 0.5*(s_aw(k)-s_aw(k+1)))
       c(kk)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

       rhs= qcd(k)

       d(kk)=sqw(k) + rhs*delt + dtz(k)*(s_awqt(k)-s_awqt(k+1))
    ENDDO













    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqw(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqw2(k)=d(k-kts+1)
    ENDDO
ELSE
    sqw2=sqw
ENDIF

IF (bl_mynn_mixqt == 0) THEN




  IF (bl_mynn_cloudmix > 0 .AND. FLAG_QC) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*dfh(k+1)   - 0.5*dtz(k)*s_aw(k+1)

    rhs   =  qcd(k)
    d(1)=sqc(k) + dtz(k)*flqc + rhs*delt -dtz(k)*s_awqc(k+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(kk)=-dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)

       rhs = qcd(k)
       d(kk)=sqc(k) + rhs*delt + dtz(k)*(s_awqc(k)-s_awqc(k+1))
    ENDDO


    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqc(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqc2(k)=d(k-kts+1)
    ENDDO
  ELSE
    
    sqc2=sqc
  ENDIF
ENDIF

IF (bl_mynn_mixqt == 0) THEN
  
  
  
  

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*dfh(k+1)   - 0.5*dtz(k)*s_aw(k+1)
    d(1)=sqv(k) + dtz(k)*flqv + qcd(k)*delt - dtz(k)*s_awqv(k+1)  

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(kk)=-dtz(k)*dfh(k+1)  - 0.5*dtz(k)*s_aw(k+1)
       d(kk)=sqv(k) + qcd(k)*delt + dtz(k)*(s_awqv(k)-s_awqv(k+1))
    ENDDO















    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqv(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqv2(k)=d(k-kts+1)
    ENDDO
ELSE
    sqv2=sqv
ENDIF




IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    d(1)=sqi(k) 

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)
       d(kk)=sqi(k) 
    ENDDO















    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqi(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqi2(k)=d(k-kts+1)
    ENDDO
ELSE
   sqi2=sqi
ENDIF








































    qni2=qni







    DO k=kts,kte

       IF (bl_mynn_mixqt > 0) THEN
         t  = th(k)*exner(k)
         
         esat=esat_blend(t)
         
         qsl=ep_2*esat/(p(k)-ep_3*esat)

         
         
         
         
         
            IF (FLAG_QI) THEN
              
              sqi2(k) = MAX(0., sqi2(k))
              sqc2(k) = MAX(0., sqw2(k) - sqi2(k) - qsl)      
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k) - sqi2(k))  
            ELSE
              
              sqi2(k) = 0.0
              sqc2(k) = MAX(0., sqw2(k) - qsl)         
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k))     
            ENDIF
         
       ENDIF

       
       
       
       Dqv(k)=(sqv2(k)/(1.-sqv2(k)) - qv(k))/delt
       

       
       
       
       
       
       
       IF (bl_mynn_cloudmix > 0 .AND. FLAG_QC) THEN
         Dqc(k)=(sqc2(k)/(1.-sqc2(k)) - qc(k))/delt
         IF(Dqc(k)*delt + qc(k) < 0.) THEN


           
           Dqc(k)=-qc(k)/delt 
         ENDIF

         
         
         
         
         
         
         
         
       ELSE
         Dqc(k)=0.
         
       ENDIF

       
       
       
       IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN
         Dqi(k)=(sqi2(k)/(1.-sqi2(k)) - qi(k))/delt
         IF(Dqi(k)*delt + qi(k) < 0.) THEN


           
           Dqi(k)=-qi(k)/delt
         ENDIF

         
         
         IF (FLAG_QNI) THEN
           Dqni(k)=(qni2(k)-qni(k))/delt
           IF(Dqni(k)*delt + qni(k) < 0.)Dqni(k)=-qni(k)/delt
         ELSE
           Dqni(k)=0.
         ENDIF
       ELSE
         Dqi(k)=0.
         Dqni(k)=0.
       ENDIF

       
       
       
       IF (FLAG_QI) THEN
         Dth(k)=(thl(k) + xlvcp/exner(k)*sqc(k) &
           &            + xlscp/exner(k)*sqi(k) &
           &            - th(k))/delt
         
         
         
         
         
       ELSE
         Dth(k)=(thl(k)+xlvcp/exner(k)*sqc2(k) - th(k))/delt
         
         
         
         
       ENDIF

    ENDDO

  END SUBROUTINE mynn_tendencies




  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dfq,dz,&
       &K_m,K_h,K_q)



    INTEGER , INTENT(in) :: kts,kte

    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh,dfq

    REAL, DIMENSION(KtS:KtE), INTENT(out) :: &
         &K_m, K_h, K_q


    INTEGER :: k
    REAL :: dzk

    K_m(kts)=0.
    K_h(kts)=0.
    K_q(kts)=0.

    DO k=kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
       K_q(k)=Sqfac*dfq(k)*dzk
    ENDDO

  END SUBROUTINE retrieve_exchange_coeffs


  SUBROUTINE tridiag(n,a,b,c,d)






    


    INTEGER, INTENT(in):: n
    REAL, DIMENSION(n), INTENT(in) :: a,b
    REAL, DIMENSION(n), INTENT(inout) :: c,d
    
    INTEGER :: i
    REAL :: p
    REAL, DIMENSION(n) :: q
    
    c(n)=0.
    q(1)=-c(1)/b(1)
    d(1)=d(1)/b(1)
    
    DO i=2,n
       p=1./(b(i)+a(i)*q(i-1))
       q(i)=-c(i)*p
       d(i)=(d(i)-a(i)*d(i-1))*p
    ENDDO
    
    DO i=n-1,1,-1
       d(i)=d(i)+q(i)*d(i+1)
    ENDDO

  END SUBROUTINE tridiag


  SUBROUTINE mynn_bl_driver(            &
       &initflag,grav_settling,         &
       &delt,dz,dx,znt,                 &
       &u,v,w,th,qv,qc,qi,qni,qnc,      &
       &p,exner,rho,T3D,                &
       &xland,ts,qsfc,qcg,ps,           &
       &ust,ch,hfx,qfx,rmol,wspd,       &
       &uoce,voce,                      & 
       &vdfg,                           & 
       &Qke,tke_pbl,                    &
       &qke_adv,bl_mynn_tkeadvect,      & 
       &Tsq,Qsq,Cov,                    &
       &RUBLTEN,RVBLTEN,RTHBLTEN,       &
       &RQVBLTEN,RQCBLTEN,RQIBLTEN,     &
       &RQNIBLTEN,                      &
       &exch_h,exch_m,                  &
       &Pblh,kpbl,                      & 
       &el_pbl,                         &
       &dqke,qWT,qSHEAR,qBUOY,qDISS,    & 
       &wstar,delta,                    & 
       &bl_mynn_tkebudget,              &
       &bl_mynn_cloudpdf,Sh3D,          &
       &bl_mynn_mixlength,              &
       &icloud_bl,qc_bl,cldfra_bl,      &
       &bl_mynn_edmf,                   &
       &bl_mynn_edmf_mom,bl_mynn_edmf_tke, &
       &bl_mynn_edmf_part,              &
       &bl_mynn_cloudmix,bl_mynn_mixqt, &
       &edmf_a,edmf_w,edmf_qt,          &
       &edmf_thl,edmf_ent,edmf_qc,      &
       &spp_pbl,pattern_spp_pbl,        &
       &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC &
       &,IDS,IDE,JDS,JDE,KDS,KDE        &
       &,IMS,IME,JMS,JME,KMS,KME        &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    


    INTEGER, INTENT(in) :: initflag
    
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    INTEGER, INTENT(in) :: bl_mynn_mixlength
    INTEGER, INTENT(in) :: bl_mynn_edmf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
    INTEGER, INTENT(in) :: bl_mynn_edmf_mom
    INTEGER, INTENT(in) :: bl_mynn_edmf_tke
    INTEGER, INTENT(in) :: bl_mynn_edmf_part
    INTEGER, INTENT(in) :: bl_mynn_cloudmix
    INTEGER, INTENT(in) :: bl_mynn_mixqt
    INTEGER, INTENT(in) :: icloud_bl

    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC
    
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE
    







    
    REAL, INTENT(in) :: delt,dx
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,w,th,qv,p,exner,rho,T3D
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(in)::&
         &qc,qi,qni,qnc
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce, vdfg,znt

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
         &tke_pbl, & 
         &qke_adv    

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
         &RQIBLTEN,RQNIBLTEN 

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &exch_h,exch_m

   REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(inout) :: &
         & edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh,wstar,delta  

    REAL, DIMENSION(IMS:IME,JMS:JME) :: &
         &Psig_bl,Psig_shcu

    INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: & 
         &KPBL

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    
    
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: K_q,Sh3D

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &qc_bl,cldfra_bl
    REAL, DIMENSION(KTS:KTE) :: qc_bl1D,cldfra_bl1D,&
                            qc_bl1D_old,cldfra_bl1D_old




    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,tl,sqv,sqc,sqi,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
         &Vt, Vq, sgm

    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,w1,p1,ex1,dz1,th1,tk1,rho1,&
           & qke1,tsq1,qsq1,cov1,qv1,qi1,qc1,du1,dv1,dth1,dqv1,dqc1,dqi1, &
           & k_m1,k_h1,k_q1,qni1,dqni1,qnc1 


    REAL, DIMENSION(KTS:KTE) :: dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf
    REAL, DIMENSION(KTS:KTE) :: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,&
                                edmf_ent1,edmf_qc1
    REAL,DIMENSION(KTS:KTE+1) :: s_aw1,s_awthl1,s_awqt1,&
                  s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    REAL :: cpm,sqcg,flt,flq,flqv,flqc,pmz,phh,exnerg,zet,& 
              &afk,abk,ts_decay,th_sfc


   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   REAL :: govrth, sflux, bfx0, wstar3, wm2, wm3, delb

    INTEGER, SAVE :: levflag


     INTEGER,  INTENT(IN)                                               ::spp_pbl
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN),OPTIONAL  ::pattern_spp_pbl
     REAL, DIMENSION(KTS:KTE)                         ::    rstoch_col


    IF ( wrf_at_debug_level(3000) ) THEN
        WRITE ( mynn_message , FMT='(A)' ) &
           'in MYNN driver; at beginning'
        CALL wrf_debug ( 0 , mynn_message )
    ENDIF


    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2


    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)

    levflag=mynn_level

    IF (bl_mynn_edmf > 0) THEN
      
      

      edmf_a(its:ite,kts:kte,jts:jte)=0.
      edmf_w(its:ite,kts:kte,jts:jte)=0.
      edmf_qt(its:ite,kts:kte,jts:jte)=0.
      edmf_thl(its:ite,kts:kte,jts:jte)=0.
      edmf_ent(its:ite,kts:kte,jts:jte)=0.
      edmf_qc(its:ite,kts:kte,jts:jte)=0.
    ENDIF

    IF (initflag > 0) THEN
 
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.
       dqc1(kts:kte)=0.0
       dqi1(kts:kte)=0.0
       dqni1(kts:kte)=0.0
       
       qc_bl1D(kts:kte)=0.0
       cldfra_bl1D(kts:kte)=0.0
       qc_bl1D_old(kts:kte)=0.0
       cldfra_bl1D_old(kts:kte)=0.0
       edmf_a1(kts:kte)=0.0
       edmf_qc1(kts:kte)=0.0
       sgm(kts:kte)=0.0
       vt(kts:kte)=0.0
       vq(kts:kte)=0.0

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTF
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                w1(k) = w(i,k,j)
                th1(k)=th(i,k,j)
                tk1(k)=T3D(i,k,j)
                rho1(k)=rho(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qc(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                IF (PRESENT(qi) .AND. FLAG_QI ) THEN
                   sqi(k)=qi(i,k,j)/(1.+qi(i,k,j))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc(k) &
                       &           - xlscp/exner(i,k,j)*sqi(k)
                   
                   
                   
                   
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                   
                   
                   
                ENDIF

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF

                exch_m(i,k,j)=0.
                exch_h(i,k,j)=0.
                K_q(i,k,j)=0.
                qke(i,k,j)=0.1-MIN(zw(k)*0.001, 0.0) 
                qke1(k)=qke(i,k,j)
                el(k)=el_pbl(i,k,j)
                sh(k)=Sh3D(i,k,j)
                tsq1(k)=tsq(i,k,j)
                qsq1(k)=qsq(i,k,j)
                cov1(k)=cov(i,k,j)
                if (spp_pbl==1) then
                    rstoch_col(k)=pattern_spp_pbl(i,k,j)
                else
                    rstoch_col(k)=0.0
                endif


                IF ( bl_mynn_tkebudget == 1) THEN
                   
                   qWT(i,k,j)=0.
                   qSHEAR(i,k,j)=0.
                   qBUOY(i,k,j)=0.
                   qDISS(i,k,j)=0.
                   dqke(i,k,j)=0.
                ENDIF
             ENDDO

             zw(kte+1)=zw(kte)+dz(i,kte,j)

             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
               &  Qke1,zw,dz1,xland(i,j),KPBL(i,j))
             
             IF (scaleaware > 0.) THEN
                CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
             ELSE
                Psig_bl(i,j)=1.0
                Psig_shcu(i,j)=1.0
             ENDIF

             CALL mym_initialize (             & 
                  &kts,kte,                    &
                  &dz1, zw, u1, v1, thl, sqv,  &
                  &PBLH(i,j), th1, sh,         &
                  &ust(i,j), rmol(i,j),        &
                  &el, Qke1, Tsq1, Qsq1, Cov1, &
                  &Psig_bl(i,j), cldfra_bl1D,  &
                  &bl_mynn_mixlength,          &
                  &edmf_a1,edmf_qc1,bl_mynn_edmf,&
                  &spp_pbl,rstoch_col )

             
             DO k=KTS,KTE 
                el_pbl(i,k,j)=el(k)
                sh3d(i,k,j)=sh(k)
                qke(i,k,j)=qke1(k)
                tsq(i,k,j)=tsq1(k)
                qsq(i,k,j)=qsq1(k)
                cov(i,k,j)=cov1(k)
                
                IF (bl_mynn_tkeadvect) THEN
                   qke_adv(i,k,j)=qke1(k)
                ENDIF
             ENDDO












          ENDDO
       ENDDO

    ENDIF 

    
    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF

    DO j=JTS,JTF
       DO i=ITS,ITF
          DO k=KTS,KTF
             
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)=qke(i,k,j)
             END IF
             dz1(k)= dz(i,k,j)
             u1(k) = u(i,k,j)
             v1(k) = v(i,k,j)
             w1(k) = w(i,k,j)
             th1(k)= th(i,k,j)
             tk1(k)=T3D(i,k,j)
             rho1(k)=rho(i,k,j)
             qv1(k)= qv(i,k,j)
             qc1(k)= qc(i,k,j)
             sqv(k)= qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)= qc(i,k,j)/(1.+qc(i,k,j))
             IF(icloud_bl > 0)cldfra_bl1D_old(k)=cldfra_bl(i,k,j)
             IF(icloud_bl > 0)qc_bl1D_old(k)=qc_bl(i,k,j)
             dqc1(k)=0.0
             dqi1(k)=0.0
             dqni1(k)=0.0
             
             IF(PRESENT(qi) .AND. FLAG_QI)THEN
                qi1(k)= qi(i,k,j)
                sqi(k)= qi(i,k,j)/(1.+qi(i,k,j))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th(i,k,j) - xlvcp/exner(i,k,j)*sqc(k) &
                     &            - xlscp/exner(i,k,j)*sqi(k)
                
                
                
                
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                
                
                
             ENDIF

             IF (PRESENT(qni) .AND. FLAG_QNI ) THEN
                qni1(k)=qni(i,k,j)
                
             ELSE
                qni1(k)=0.0
             ENDIF
             IF (PRESENT(qnc) .AND. FLAG_QNC ) THEN
                qnc1(k)=qnc(i,k,j)
                
             ELSE
                qnc1(k)=0.0
             ENDIF
             thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)
             if (spp_pbl==1) then
                rstoch_col(k)=pattern_spp_pbl(i,k,j)
             else
                rstoch_col(k)=0.0
             endif


             
             edmf_a1(k)=0.0
             edmf_qc1(k)=0.0
             s_aw1(k)=0.
             s_awthl1(k)=0.
             s_awqt1(k)=0.
             s_awqv1(k)=0.
             s_awqc1(k)=0.
             s_awu1(k)=0.
             s_awv1(k)=0.
             s_awqke1(k)=0.


             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO

          zw(kte+1)=zw(kte)+dz(i,kte,j)
          
          s_aw1(kte+1)=0.
          s_awthl1(kte+1)=0.
          s_awqt1(kte+1)=0.
          s_awqv1(kte+1)=0.
          s_awqc1(kte+1)=0.
          s_awu1(kte+1)=0.
          s_awv1(kte+1)=0.
          s_awqke1(kte+1)=0.

          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
          & Qke1,zw,dz1,xland(i,j),KPBL(i,j))

          IF (scaleaware > 0.) THEN
             CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
          ELSE
             Psig_bl(i,j)=1.0
             Psig_shcu(i,j)=1.0
          ENDIF

          sqcg= 0.0   
          cpm=cp*(1.+0.84*qv(i,kts,j))
          exnerg=(ps(i,j)/p1000mb)**rcp

          
          
          
          
          
          
          
          
          
          flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
            & +xlvcp*vdfg(i,j)*(sqc(kts)/exner(i,kts,j)- sqcg/exnerg)
          flq = qfx(i,j)/  rho(i,kts,j)       &
            & -vdfg(i,j)*(sqc(kts) - sqcg )
          flqv = qfx(i,j)/rho(i,kts,j)
          flqc = -vdfg(i,j)*(sqc(kts) - sqcg )
          th_sfc = ts(i,j)/ex1(kts)

          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 +  cphh_st      * zet
          else
            pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if

          
          govrth = g/th1(kts)
          sflux = hfx(i,j)/rho(i,kts,j)/cpm + &
                  qfx(i,j)/rho(i,kts,j)*ep_1*th1(kts)
          bfx0 = max(sflux,0.)
          wstar3     = (govrth*bfx0*pblh(i,j))
          wstar(i,j) = wstar3**h1
          wm3        = wstar3 + 5.*ust(i,j)**3.
          wm2        = wm3**h2
          delb       = govrth*d3*pblh(i,j)
          delta(i,j) = min(d1*pblh(i,j) + d2*wm2/delb, 100.)
          

          CALL  mym_condensation ( kts,kte,      &
               &dx,dz1,thl,sqw,p1,ex1,           &
               &tsq1, qsq1, cov1,                &
               &Sh,el,bl_mynn_cloudpdf,          &
               &qc_bl1D,cldfra_bl1D,             &
               &PBLH(i,j),HFX(i,j),              &
               &Vt, Vq, th1, sgm )

          IF (bl_mynn_edmf == 1) THEN
            
            CALL StEM_mf(                         &
               &kts,kte,delt,zw,dz1,p1,           &
               &bl_mynn_edmf_mom,                 &
               &bl_mynn_edmf_tke,                 &
               &u1,v1,w1,th1,thl,thetav,tk1,      &
               &sqw,sqv,sqc,qke1,                 &
               &ex1,Vt,Vq,sgm,                    &
               &ust(i,j),flt,flq,flqv,flqc,       &
               &PBLH(i,j),KPBL(i,j),DX,           &
               &xland(i,j),th_sfc,                &
            
            
            
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               & s_awqke1,                        &
               & qc_bl1D,cldfra_bl1D,             &
               & FLAG_QI,FLAG_QC,                 &
               & Psig_shcu(i,j),                  &
               & spp_pbl,rstoch_col               &
            )

          ELSEIF (bl_mynn_edmf == 2) THEN
            CALL temf_mf(                         &
               &kts,kte,delt,zw,p1,ex1,           &
               &u1,v1,w1,th1,thl,thetav,          &
               &sqw,sqv,sqc,qke1,                 &
               &ust(i,j),flt,flq,flqv,flqc,       &
               &hfx(i,j),qfx(i,j),ts(i,j),        &
               &pblh(i,j),rho1,dfh,dx,znt(i,j),ep_2,   &
            
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,                 &
               & s_awu1,s_awv1,s_awqke1,          &
               & qc_bl1D,cldfra_bl1D              &
               &,FLAG_QI,FLAG_QC                  &
               &,Psig_shcu(i,j)                   &
               &,spp_pbl,rstoch_col               &
               &,i,j,ids,ide,jds,jde              &
             )
          ENDIF

          CALL mym_turbulence (                  & 
               &kts,kte,levflag,                 &
               &dz1, zw, u1, v1, thl, sqc, sqw,  &
               &qke1, tsq1, qsq1, cov1,          &
               &vt, vq,                          &
               &rmol(i,j), flt, flq,             &
               &PBLH(i,j),th1,                   &
               &Sh,el,                           &
               &Dfm,Dfh,Dfq,                     &
               &Tcd,Qcd,Pdk,                     &
               &Pdt,Pdq,Pdc,                     &
               &qWT1,qSHEAR1,qBUOY1,qDISS1,      &
               &bl_mynn_tkebudget,               &
               &Psig_bl(i,j),Psig_shcu(i,j),     &     
               &cldfra_bl1D,bl_mynn_mixlength,   &
               &edmf_a1,edmf_qc1,bl_mynn_edmf,   &
               &spp_pbl,rstoch_col)













          IF (bl_mynn_edmf_part > 0 .AND. bl_mynn_edmf > 0) THEN
             
             

             DO k=kts,kte
                dfm(k)=dfm(k) * (1. - 0.5*edmf_a1(k))
                dfh(k)=dfh(k) * (1. - 0.5*edmf_a1(k))
                dfq(k)=dfq(k) * (1. - 0.5*edmf_a1(k))
             ENDDO
          ENDIF

          CALL mym_predict (kts,kte,levflag,     &
               &delt, dz1,                       &
               &ust(i,j), flt, flq, pmz, phh,    &
               &el, dfq, pdk, pdt, pdq, pdc,     &
               &Qke1, Tsq1, Qsq1, Cov1,          &
               &s_aw1, s_awqke1, bl_mynn_edmf_tke)

          CALL mynn_tendencies(kts,kte,          &
               &levflag,grav_settling,           &
               &delt, dz1,                       &
               &u1, v1, th1, tk1, qv1, qc1, qi1, &
               &qni1,qnc1,                       &
               &p1, ex1, thl, sqv, sqc, sqi, sqw,&
               &ust(i,j),flt,flq,flqv,flqc,      &
               &wspd(i,j),qcg(i,j),              &
               &uoce(i,j),voce(i,j),             &
               &tsq1, qsq1, cov1,                &
               &tcd, qcd,                        &
               &dfm, dfh, dfq,                   &
               &Du1, Dv1, Dth1, Dqv1,            &
               &Dqc1, Dqi1, Dqni1,               & 
               &vdfg(i,j),                       & 
               
               &s_aw1,s_awthl1,s_awqt1,          &
               &s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
               &cldfra_bl1d,                     &
               &bl_mynn_cloudmix,                &
               &bl_mynn_mixqt,                   &
               &bl_mynn_edmf,                    &
               &bl_mynn_edmf_mom)













 

          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dfq, dz1,&
               &K_m1, K_h1, K_q1)

          
          DO k=KTS,KTF
             exch_m(i,k,j)=K_m1(k)
             exch_h(i,k,j)=K_h1(k)
             K_q(i,k,j)=K_q1(k)
             RUBLTEN(i,k,j)=du1(k)
             RVBLTEN(i,k,j)=dv1(k)
             RTHBLTEN(i,k,j)=dth1(k)
             RQVBLTEN(i,k,j)=dqv1(k)
             IF(bl_mynn_cloudmix > 0)THEN
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=dqc1(k)
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=dqi1(k)
               
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=dqni1(k)
             ELSE
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=0.
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=0.
               
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=0.
             ENDIF

             IF(icloud_bl > 0)THEN
               
               qc_bl(i,k,j)=qc_bl1D(k) 
               cldfra_bl(i,k,j)=cldfra_bl1D(k) 

               
               IF (CLDFRA_BL(i,k,j) > cldfra_bl1D_old(k)) THEN
                  
               ELSE
                  
                  
                  ts_decay = MIN( 1800., 3.*dx/MAX(SQRT(u1(k)**2 + v1(k)**2), 1.0) )
                  cldfra_bl(i,k,j)= MAX(cldfra_bl1D(k), cldfra_bl1D_old(k)-(0.25*delt/ts_decay))
                  IF (cldfra_bl(i,k,j) > 0.1) THEN
                     IF (QC_BL(i,k,j) < 1E-5)QC_BL(i,k,j)= MAX(qc_bl1D_old(k), 1E-5)
                  ELSE
                     CLDFRA_BL(i,k,j)= 0.
                     QC_BL(i,k,j)    = 0.
                  ENDIF
               ENDIF

               
               if (spp_pbl==1) then
                   cldfra_bl(i,k,j)= cldfra_bl(i,k,j)*(1.0-rstoch_col(k))
                  IF ((cldfra_bl(i,k,j) > 1.0) .or. (cldfra_bl(i,k,j) < 0.0)) then
                     cldfra_bl(i,k,j)=MAX(MIN(cldfra_bl(i,k,j),1.0),0.0)
                  ENDIF
               ENDIF

               
               
               
               IF (icloud_bl > 0) THEN
                 IF (QC_BL(i,k,j) < 1E-6 .AND. ABS(CLDFRA_BL(i,k,j)) > 0.1)QC_BL(i,k,j)= 1E-6
                 IF (CLDFRA_BL(i,k,j) < 1E-2)CLDFRA_BL(i,k,j)= 0.
               ENDIF
             ENDIF
             el_pbl(i,k,j)=el(k)
             qke(i,k,j)=qke1(k)
             tsq(i,k,j)=tsq1(k)
             qsq(i,k,j)=qsq1(k)
             cov(i,k,j)=cov1(k)
             sh3d(i,k,j)=sh(k)

             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)  = (qke1(k)-dqke(i,k,j))*0.5  
                qWT(i,k,j)   = qWT1(k)*delt
                qSHEAR(i,k,j)= qSHEAR1(k)*delt
                qBUOY(i,k,j) = qBUOY1(k)*delt
                qDISS(i,k,j) = qDISS1(k)*delt
             ENDIF

             
             IF (bl_mynn_edmf > 0) THEN
               edmf_a(i,k,j)=edmf_a1(k)
               edmf_w(i,k,j)=edmf_w1(k)
               edmf_qt(i,k,j)=edmf_qt1(k)
               edmf_thl(i,k,j)=edmf_thl1(k)
               edmf_ent(i,k,j)=edmf_ent1(k)
               edmf_qc(i,k,j)=edmf_qc1(k)
             ENDIF

             
             IF ( wrf_at_debug_level(3000) ) THEN
               IF ( sh(k) < 0. .OR. sh(k)> 200.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," sh=",sh(k)
               IF ( qke(i,k,j) < -1. .OR. qke(i,k,j)> 200.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," qke=",qke(i,k,j)
               IF ( el_pbl(i,k,j) < 0. .OR. el_pbl(i,k,j)> 2000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," el_pbl=",el_pbl(i,k,j)
               IF ( ABS(vt(k)) > 0.8 )print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vt=",vt(k)
               IF ( ABS(vq(k)) > 6000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vq=",vq(k) 
               IF ( exch_m(i,k,j) < 0. .OR. exch_m(i,k,j)> 2000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," exxch_m=",exch_m(i,k,j)
               IF ( vdfg(i,j) < 0. .OR. vdfg(i,j)>5. )print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vdfg=",vdfg(i,j)
               IF ( ABS(QFX(i,j))>.001)print*, "**SUSPICIOUS VALUES AT: i,j=",i,j," QFX=",QFX(i,j)
               IF ( ABS(HFX(i,j))>1000.)print*, "**SUSPICIOUS VALUES AT: i,j=",i,j," HFX=",HFX(i,j)
               IF (icloud_bl > 0) then
                  IF( cldfra_bl(i,k,j) < 0.0 .OR. cldfra_bl(i,k,j)> 1.)THEN
                  PRINT*,"**SUSPICIOUS VALUES: CLDFRA_BL=",cldfra_bl(i,k,j)," qc_bl=",QC_BL(i,k,j)
                  ENDIF
               ENDIF
             ENDIF
             
          ENDDO

          
          
          tke_pbl(i,kts,j) = 0.5*MAX(qke(i,kts,j),1.0e-10)
          DO k = kts+1,kte
             afk = dz1(k)/( dz1(k)+dz1(k-1) )
             abk = 1.0 -afk
             tke_pbl(i,k,j) = 0.5*MAX(qke(i,k,j)*abk+qke(i,k-1,j)*afk,1.0e-3)
          ENDDO













       ENDDO
    ENDDO


    IF (bl_mynn_tkeadvect) THEN
       qke_adv=qke
    ENDIF

    
  END SUBROUTINE mynn_bl_driver


  SUBROUTINE mynn_bl_init_driver(                   &
       &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,          &
       &RQCBLTEN,RQIBLTEN & 
       &,QKE,TKE_PBL,EXCH_H                         &

       &,RESTART,ALLOWED_TO_READ,LEVEL              &
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)

    
    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL 

    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
         &                IMS,IME,JMS,JME,KMS,KME,                    &
         &                ITS,ITE,JTS,JTE,KTS,KTE
    
    
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,                 &
         &RQCBLTEN,RQIBLTEN,& 
         &QKE,TKE_PBL,EXCH_H




    INTEGER :: I,J,K,ITF,JTF,KTF
    
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                RUBLTEN(i,k,j)=0.
                RVBLTEN(i,k,j)=0.
                RTHBLTEN(i,k,j)=0.
                RQVBLTEN(i,k,j)=0.
                if( p_qc >= param_first_scalar ) RQCBLTEN(i,k,j)=0.
                if( p_qi >= param_first_scalar ) RQIBLTEN(i,k,j)=0.
                
                
                
                TKE_PBL(i,k,j)=0.
                EXCH_H(i,k,j)=0.


             ENDDO
          ENDDO
       ENDDO
    ENDIF

    mynn_level=level

  END SUBROUTINE mynn_bl_init_driver



  SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea,kzi)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    INTEGER,INTENT(IN) :: KTS,KTE
    REAL, INTENT(OUT) :: zi
    REAL, INTENT(IN) :: landsea
    REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
    REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
    
    REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
    REAL :: delt_thv   
    REAL, PARAMETER :: sbl_lim  = 250. 
    REAL, PARAMETER :: sbl_damp = 500. 
    INTEGER :: I,J,K,kthv,ktke,kzi,kzi2

    
    
    kzi = 2
    kzi2= 2

    
    k = kts+1
    kthv = 1
    ktke = 1
    maxqke = 0.
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. sbl_lim)
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
    TKEeps = MAX(TKEeps,0.02) 

    
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN
        
        delt_thv = 0.75
    ELSE
        
        delt_thv = 1.25
    ENDIF

    zi=0.
    k = kthv+1
    DO WHILE (zi .EQ. 0.) 
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
          kzi = MAX(k-1,1)
          zi = zw1D(k) - dz1D(k-1)* &
             & MIN((thetav1D(k)-(minthv + delt_thv))/ &
             & MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
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
           kzi2 = MAX(k-1,1)
           PBLH_TKE = zw1D(k) - dz1D(k-1)* &
             & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
           
           PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
           
       ENDIF
       k = k+1
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) 
    ENDDO

    
    
    
    
    
    
    PBLH_TKE = MIN(PBLH_TKE,zi+500.)
    PBLH_TKE = MAX(PBLH_TKE,MAX(zi-500.,10.))

    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    IF (maxqke <= 0.05) THEN
       
    ELSE
       
       zi=PBLH_TKE*(1.-wt) + zi*wt
    ENDIF

    
     kzi = MAX(INT(kzi2*(1.-wt) + kzi*wt),1)

  END SUBROUTINE GET_PBLH
  










  SUBROUTINE StEM_mf(                       &
                 & kts,kte,dt,zw,dz,p,      &
                 & momentum_opt,            &
                 & tke_opt,                 &
                 & u,v,w,th,thl,thv,tk,     &
                 & qt,qv,qc,qke,            &
                 & exner,vt,vq,sgm,         &
                 & ust,flt,flq,flqv,flqc,   &
                 & pblh,kpbl,DX,landsea,ts, &
            
            
            
                 & edmf_a,edmf_w,           &
                 & edmf_qt,edmf_thl,        &
                 & edmf_ent,edmf_qc,        &
            
                 & s_aw,s_awthl,s_awqt,     &
                 & s_awqv,s_awqc,           &
                 & s_awu,s_awv,s_awqke,     &
            
                 & qc_bl1d,cldfra_bl1d,     &
            
                 &F_QC,F_QI,                &
                 &Psig_shcu,                &
            
                 &spp_pbl,rstoch_col) 


     INTEGER,  INTENT(IN)          :: spp_pbl
     REAL, DIMENSION(KTS:KTE)      :: rstoch_col
  
     INTEGER, INTENT(IN) :: KTS,KTE,momentum_opt,tke_opt,kpbl
     REAL,DIMENSION(KTS:KTE), INTENT(IN) :: U,V,W,TH,THL,TK,QT,QV,QC,&
                                            THV,P,qke,exner,dz
     REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: ZW  
     REAL, INTENT(IN) :: DT,UST,FLT,FLQ,FLQV,FLQC,PBLH,&
                         DX,Psig_shcu,landsea,ts
     LOGICAL, OPTIONAL :: F_QC,F_QI

  
  
  
     REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a,edmf_w,        &
                      & edmf_qt,edmf_thl, edmf_ent,edmf_qc
  
     INTEGER :: nup2,ktop
     REAL    :: maxmf
  
     REAL,DIMENSION(KTS:KTE+1) :: s_aw,      & 
                               s_awthl,      & 
                                s_awqt,      &
                                s_awqv,      &
                                s_awqc,      &
                                 s_awu,      &
                                 s_awv,      &
                               s_awqke

     REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d

    INTEGER, PARAMETER :: NUP=10
  
  
     REAL,DIMENSION(KTS:KTE+1,1:NUP) :: UPW,UPTHL,UPQT,UPQC,UPQV,  &
                                        UPA,UPU,UPV,UPTHV,UPQKE
  
     REAL,DIMENSION(KTS:KTE,1:NUP) :: ENT,ENTf
     INTEGER,DIMENSION(KTS:KTE,1:NUP) :: ENTi
  
     INTEGER :: K,I,k50
     REAL :: fltv,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,    &
             pwmin,pwmax,wmin,wmax,wlv,wtv,Psig_w,maxw,maxqc,wpbl
     REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,Wn2,EntEXP,EntW

  
     REAL,PARAMETER :: &
          &Wa=2./3., &
          &Wb=0.002,&
          &Wc=1.5 
        
  
  
     REAL,PARAMETER :: &
         & L0=100.,&
         & ENT0=0.1

  
     REAL, PARAMETER :: Atot = 0.1  
     REAL, PARAMETER :: lmax = 1000.
     REAL, PARAMETER :: dl   = 100. 
     REAL, PARAMETER :: dcut = 1.0  
     REAL ::  d            
          
          
     REAL :: cn,c,l,n,an2,hux,maxwidth,wspd_pbl,cloud_base,width_flx


  
   REAL :: ERF

  
   INTEGER, PARAMETER :: cldfra_opt = 0  
                                         
  
   REAL :: xcldfra, UMF_new, dcf, ktop_dcf, kbcon_dcf
  
   REAL, PARAMETER  ::  coef_p = 0.25, coef_gamm = 0.49, coef_alph = 100.
   REAL :: satvp,rhgrid,h2oliq
   LOGICAL :: superadiabatic

  
   REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: vt, vq, sgm
   REAL :: sigq,xl,tlk,qsat_tl,rsl,cpm,a,qmq,mf_cf,diffqt,&
           Fng,qww,alpha,beta,bb,f,pt,t,q2p,b9

   
   REAL :: csigma,acfac

   
   INTEGER :: overshoot
   REAL :: bvf, Frz














  UPW=0.
  UPTHL=0.
  UPTHV=0.
  UPQT=0.
  UPA=0.
  UPU=0.
  UPV=0.
  UPQC=0.
  UPQV=0.
  UPQKE=0.
  ENT=0.001

  edmf_a  =0.
  edmf_w  =0.
  edmf_qt =0.
  edmf_thl=0.
  edmf_ent=0.
  edmf_qc =0.

  s_aw=0.
  s_awthl=0.
  s_awqt=0.
  s_awqv=0.
  s_awqc=0.
  s_awu=0.
  s_awv=0.
  s_awqke=0.


  
  
  k      = 1
  maxw   = 0.0
  cloud_base  = 9000.0
  DO WHILE (ZW(k) < pblh + 500.)
     wpbl = w(k)
     IF(w(k) < 0.)wpbl = 2.*w(k)
     maxw = MAX(maxw,ABS(wpbl))

     
     IF(ZW(k)<=50.)k50=k

     
     IF(qc(k)>1E-5 .AND. cloud_base == 9000.0)THEN
       cloud_base = 0.5*(ZW(k)+ZW(k+1))
     ENDIF

     k = k + 1
  ENDDO
  
  maxw = MAX(0.,maxw - 0.5)         
  Psig_w = MAX(0.0, 1.0 - maxw/0.5) 
  Psig_w = MIN(Psig_w, Psig_shcu)
  

  fltv = flt + svp1*flq
  

  
  IF(Psig_w == 0.0 .and. fltv > 0.0) fltv = -1.*fltv




  superadiabatic = .false.
  IF((landsea-1.5).GE.0)THEN
     hux = -0.002   
  ELSE
     hux = -0.005  
  ENDIF
  DO k=1,MAX(1,k50-1)
    IF (k == 1) then
      IF ((th(k)-ts)/(0.5*dz(k)) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ELSE
      IF ((th(k)-th(k-1))/(0.5*(dz(k)+dz(k-1))) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ENDIF
  ENDDO

  
  
  
  
  
  
  
  
  NUP2 = max(1,min(NUP,INT(dx*dcut/dl)))
  
  wspd_pbl=SQRT(MAX(u(kpbl)**2 + v(kpbl)**2, 0.01))
  maxwidth = 0.9*PBLH - MIN(15.*MAX(wspd_pbl - 7.5, 0.), 0.3*PBLH)
  
  maxwidth = MIN(maxwidth,0.5*cloud_base)
  
  IF((landsea-1.5).LT.0)THEN
    IF (cloud_base .LT. 2000.) THEN
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.120)/0.03) + .5),1000.), 0.)
    ELSE
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.085)/0.04) + .5),1000.), 0.)
    ENDIF
    maxwidth = MIN(maxwidth,width_flx)
  ENDIF
  
  NUP2 = MIN(MAX(INT((maxwidth - MOD(maxwidth,100.))/100), 0), NUP2)

  
  ktop = 0
  maxmf= 0.0

  IF ( fltv > 0.002 .AND. NUP2 .GE. 1 .AND. superadiabatic) then
    

    
    cn = 0.
    d=-1.9  
    
    do I=1,NUP2
       l  = dl*I                            
       cn = cn + l**d * (l*l)/(dx*dx) * dl  
    enddo
    C = Atot/cn   

    
    An2 = 0.
    do I=1,NUP2
       l  = dl*I                            
       N = C*l**d                           
       UPA(1,I) = N*l*l/(dx*dx) * dl        
       

       acfac = .5*tanh((fltv - 0.07)/0.09) + .5 
       UPA(1,I)=UPA(1,I)*acfac
       An2 = An2 + UPA(1,I)                 
       
    end do

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    z0=50.
    pwmin=0.1       
    pwmax=0.5       

    wstar=max(1.E-2,(g/thv(1)*fltv*pblh)**(1./3.))
    qstar=max(flq,1.0E-5)/wstar
    thstar=flt/wstar

    IF((landsea-1.5).GE.0)THEN
       csigma = 1.34   
    ELSE
       csigma = 1.34   
    ENDIF
    sigmaW =1.34*wstar*(z0/pblh)**(1./3.)*(1 - 0.8*z0/pblh)
    sigmaQT=csigma*qstar*(z0/pblh)**(-1./3.)
    sigmaTH=csigma*thstar*(z0/pblh)**(-1./3.)

    wmin=sigmaW*pwmin
    wmax=sigmaW*pwmax

    
    acfac = .5*tanh((fltv - 0.08)/0.07) + .5

    
    DO I=1,NUP2
       wlv=wmin+(wmax-wmin)/NUP2*(i-1)
       wtv=wmin+(wmax-wmin)/NUP2*i

       
       
       UPW(1,I)=wmin + REAL(i)/REAL(NUP)*(wmax-wmin)
       

       
       
       

       UPU(1,I)=U(1)
       UPV(1,I)=V(1)
       UPQC(1,I)=0
       
       
       
       UPQT(1,I) = 0.
       UPTHV(1,I)= 0.
       UPTHL(1,I)= 0.
       k50=1 
       DO k=1,k50
         UPQT(1,I) = UPQT(1,I) +QT(k) +0.58*UPW(1,I)*sigmaQT/sigmaW *acfac
         UPTHV(1,I)= UPTHV(1,I)+THV(k)+0.58*UPW(1,I)*sigmaTH/sigmaW *acfac
         UPTHL(1,I)= UPTHL(1,I)+THL(k)+0.58*UPW(1,I)*sigmaTH/sigmaW *acfac
       ENDDO
       UPQT(1,I) = UPQT(1,I)/REAL(k50)
       UPTHV(1,I)= UPTHV(1,I)/REAL(k50)

       UPTHL(1,I)= UPTHL(1,I)/REAL(k50)             
       UPQKE(1,I)= QKE(1)









    ENDDO

  
  
    DO I=1,NUP2
       QCn = 0.
       overshoot = 0
       l  = dl*I                            
       DO k=KTS+1,KTE

          
          ENT(k,i) = 0.75/(MIN(MAX(UPW(K-1,I),0.75),1.25)*l)
          
          
          
          ENT(k,i) = max(ENT(k,i),0.0006)
          ENT(k,i) = max(ENT(k,i),0.05/ZW(k))
          
          IF(ZW(k) >= pblh+1000.) ENT(k,i) =ENT(k,i) + (ZW(k)-(pblh+1000.)) * 5.0E-6
          IF(UPW(K-1,I) > 1.5) ENT(k,i) = ENT(k,i) + 0.004*(UPW(K-1,I) - 1.5)
          ENT(k,i) = min(ENT(k,i),0.9/(ZW(k)-ZW(k-1)))

          
          EntExp= ENT(K,I)*(ZW(k)-ZW(k-1))
          QTn =UPQT(k-1,I) *(1.-EntExp) + QT(k-1)*EntExp
          THLn=UPTHL(k-1,I)*(1.-EntExp) + THL(k-1)*EntExp
          Un  =UPU(k-1,I)  *(1.-EntExp) + U(k-1)*EntExp
          Vn  =UPV(k-1,I)  *(1.-EntExp) + V(k-1)*EntExp
          QKEn=UPQKE(k-1,I)*(1.-EntExp) + QKE(k-1)*EntExp

          
          
          
          
          
          
          


          
          call condensation_edmf(QTn,THLn,(P(K)+P(K-1))/2.,ZW(k),THVn,QCn)

          B=g*(0.5*(THVn+UPTHV(k-1,I))/THV(k-1) - 1.0)

          EntW=exp(-2.*(Wb+Wc*ENT(K,I))*(ZW(k)-ZW(k-1)))
          Wn2=UPW(K-1,I)**2*EntW + (1.-EntW)*0.5*Wa*B/(Wb+Wc*ENT(K,I))
          Wn2=MAX(Wn2,0.0)

          
          IF (fltv > 0.05 .AND. Wn2 <= 0 .AND. overshoot == 0) THEN
             overshoot = 1
             IF ( THV(k)-THV(k-1) .GT. 0.0 ) THEN
                bvf = SQRT( gtr*(THV(k)-THV(k-1))/(0.5*(dz(k)+dz(k-1))) )
                
                Frz = UPW(K-1,I)/(bvf*0.5*(dz(k)+dz(k-1)))
                IF ( Frz >= 0.5 ) Wn2 =  MIN(Frz,1.0)*(UPW(K-1,I)**2) 
             ENDIF
          ELSEIF (fltv > 0.05 .AND. overshoot == 1) THEN
             
             Wn2 = 0.0
          ENDIF

          
          Wn2=Wn2*EXP(-MAX(ZW(k)-(pblh+1500.),0.0)/1000.)
          IF(ZW(k) >= pblh+3000.)Wn2=0.
 
          
          IF (fltv < 0.06) THEN
             IF(ZW(k) >= pblh-200. .AND. qc(k) > 1e-5 .AND. I > 2) Wn2=0.
          ENDIF

          IF (Wn2 > 0.) THEN
             UPW(K,I)=sqrt(Wn2)
             UPTHV(K,I)=THVn
             UPTHL(K,I)=THLn
             UPQT(K,I)=QTn
             UPQC(K,I)=QCn
             UPU(K,I)=Un
             UPV(K,I)=Vn
             UPQKE(K,I)=QKEn
             UPA(K,I)=UPA(K-1,I)
             ktop = MAX(ktop,k)
          ELSE
             exit
          END IF
       ENDDO
    ENDDO
  ELSE
    
    NUP2=0. 
  END IF 

  ktop=MIN(ktop,KTE-1)  



  IF(nup2 > 0) THEN

    DO k=KTS,ktop 
      DO I=1,NUP2
        edmf_a(K)=edmf_a(K)+UPA(K+1,I)
        edmf_w(K)=edmf_w(K)+UPA(K+1,I)*UPW(K+1,I)
        edmf_qt(K)=edmf_qt(K)+UPA(K+1,I)*UPQT(K+1,I)
        edmf_thl(K)=edmf_thl(K)+UPA(K+1,I)*UPTHL(K+1,I)
        edmf_ent(K)=edmf_ent(K)+UPA(K+1,I)*ENT(K+1,I)
        edmf_qc(K)=edmf_qc(K)+UPA(K+1,I)*UPQC(K+1,I)
      ENDDO 

      IF (edmf_a(k)>0.) THEN
        edmf_w(k)=edmf_w(k)/edmf_a(k)
        edmf_qt(k)=edmf_qt(k)/edmf_a(k)
        edmf_thl(k)=edmf_thl(k)/edmf_a(k)
        edmf_ent(k)=edmf_ent(k)/edmf_a(k)
        edmf_qc(k)=edmf_qc(k)/edmf_a(k)
        edmf_a(k)=edmf_a(k)*Psig_w

        
        IF(edmf_a(k)*edmf_w(k) > maxmf) maxmf = edmf_a(k)*edmf_w(k)
      ENDIF
    ENDDO

    DO k=KTS,ktop 
      DO I=1,NUP2
        s_aw(k)   = s_aw(K)    + UPA(K,I)*UPW(K,I)*Psig_w * (1.0+rstoch_col(k))
        s_awthl(k)= s_awthl(K) + UPA(K,i)*UPW(K,I)*UPTHL(K,I)*Psig_w * (1.0+rstoch_col(k))
        s_awqt(k) = s_awqt(K)  + UPA(K,i)*UPW(K,I)*UPQT(K,I)*Psig_w * (1.0+rstoch_col(k))
        s_awqc(k) = s_awqc(K)  + UPA(K,i)*UPW(K,I)*UPQC(K,I)*Psig_w * (1.0+rstoch_col(k))
        IF (momentum_opt > 0) THEN
          s_awu(k)  = s_awu(K)   + UPA(K,i)*UPW(K,I)*UPU(K,I)*Psig_w * (1.0+rstoch_col(k))
          s_awv(k)  = s_awv(K)   + UPA(K,i)*UPW(K,I)*UPV(K,I)*Psig_w * (1.0+rstoch_col(k))
        ENDIF
        IF (tke_opt > 0) THEN
          s_awqke(k)= s_awqke(K) + UPA(K,i)*UPW(K,I)*UPQKE(K,I)*Psig_w * (1.0+rstoch_col(k))
        ENDIF
      ENDDO
      s_awqv(k) = s_awqt(k)  - s_awqc(k)
    ENDDO



     DO K=KTS,ktop 

       IF (cldfra_opt == 0) THEN
         IF(edmf_qc(k)>0.0)THEN
            satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
            rhgrid = max(.01,MIN( 1., qv(k) /satvp))

            

            xl = xl_blend(tk(k))                
            tlk = thl(k)*(p(k)/p1000mb)**rcp    
            qsat_tl = qsat_blend(tlk,p(k))      
                                                
            rsl = xl*qsat_tl / (r_v*tlk**2)     
                                                
            cpm = cp + qt(k)*cpv                
            a   = 1./(1. + xl*rsl/cpm)          
            b9  = a*rsl                         

            q2p  = xlvcp/exner(k)
            pt = thl(k) +q2p*edmf_qc(k) 
            bb = b9*tk(k)/pt 
                           
                           
                           
                           
                           
            qww   = 1.+0.61*qt(k)
            alpha = 0.61*pt
            t     = th(k)*exner(k)
            beta  = pt*xl/(t*cp) - 1.61*pt
            

            
            if (a > 0.0) then
               f = MIN(1.0/a, 4.0)              
            else
               f = 1.0
            endif
            sigq = 6.E-3 * edmf_a(k) * edmf_w(k) * f 
            
            sigq = SQRT(sigq**2 + sgm(k)**2)    

            qmq = a * (qt(k) - qsat_tl)         
                                                
            mf_cf = min(max(0.5 + 0.36 * atan(1.55*(qmq/sigq)),0.01),1.0)
            
            
            
            
            
            
            IF (rhgrid >= .93) THEN
               
               cldfra_bl1d(k) = MAX(mf_cf, cldfra_bl1d(k))
               IF (cldfra_bl1d(k) > edmf_a(k)) THEN
                  qc_bl1d(k) = edmf_qc(k)*edmf_a(k)/cldfra_bl1d(k)
               ELSE
                 cldfra_bl1d(k)=edmf_a(k)
                 qc_bl1d(k) = edmf_qc(k)
               ENDIF
            ELSE
               IF (mf_cf > edmf_a(k)) THEN
                  cldfra_bl1d(k) = mf_cf
                  qc_bl1d(k) = edmf_qc(k)*edmf_a(k)/mf_cf
               ELSE
                  cldfra_bl1d(k)=edmf_a(k)
                  qc_bl1d(k) = edmf_qc(k)
               ENDIF
            ENDIF
            
            
            
            
              Fng = 2.05 
            vt(k) = qww   - MIN(0.3,cldfra_bl1D(k))*beta*bb*Fng - 1.
            vq(k) = alpha + MIN(0.3,cldfra_bl1D(k))*beta*a*Fng  - tv0
          ENDIF
        ELSEIF(cldfra_opt == 1) THEN
          
          qc_bl1d(k) = MAX(qc_bl1d(k), edmf_qc(k))
          if(F_qc .and. .not. F_qi)then
            satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
            rhgrid = max(.1,MIN( .95, qv(k) /satvp))
            h2oliq=1000.*qc_bl1d(k)
            satvp=1000.*satvp
            cldfra_bl1d(k)=(1.-exp(-coef_alph*h2oliq/&
                   ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
            cldfra_bl1d(k)=max(0.0,MIN(1.,cldfra_bl1d(k)))
          elseif(F_qc .and. F_qi)then
            satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
            rhgrid = max(.1,MIN( .95, qv(k) /satvp))
            h2oliq=1000.*qc_bl1d(k)
            satvp=1000.*satvp
            cldfra_bl1d(k)=(1.-exp(-coef_alph*h2oliq/&
                    ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
            cldfra_bl1d(k)=max(0.0,MIN(1.,cldfra_bl1d(k)))
          endif
        ENDIF
      ENDDO

    ENDIF  

    
    IF (ktop > 0) THEN
      maxqc = maxval(edmf_qc(1:ktop)) 
      IF ( maxqc < 1.E-8) maxmf = -1.0*maxmf
    ENDIF
       



IF (edmf_w(1) > 4.0) THEN 

    print *,'flq:',flq,' fltv:',fltv
    print *,'pblh:',pblh,' wstar:',wstar
    print *,'sigmaW=',sigmaW,' sigmaTH=',sigmaTH,' sigmaQT=',sigmaQT







 

















 

   print *,' edmf_a',edmf_a(1:14)
   print *,' edmf_w',edmf_w(1:14)
   print *,' edmf_qt:',edmf_qt(1:14)
   print *,' edmf_thl:',edmf_thl(1:14)
 
ENDIF 










END SUBROUTINE StEM_MF

subroutine Poisson(istart,iend,jstart,jend,mu,POI)

  integer, intent(in) :: istart,iend,jstart,jend 
  real,dimension(istart:iend,jstart:jend),intent(in) :: MU
  integer, dimension(istart:iend,jstart:jend), intent(out) :: POI
  integer :: i,j
  
  
  

  do i=istart,iend
    do j=jstart,jend
      call   random_Poisson(mu(i,j),.true.,POI(i,j))
    enddo
  enddo

end subroutine Poisson

subroutine init_random_seed()
   
   
   implicit none
   integer, allocatable :: seed(:)
   integer :: i, n, un, istat, dt(8), pid
   
   integer :: t

   call random_seed(size = n)
   allocate(seed(n))

   
   
   un=191
   open(unit=un, file="/dev/urandom", access="stream", &
   form="unformatted", action="read", status="old", iostat=istat)

   if (istat == 0) then
      read(un) seed
      close(un)
   else
      
      
      
      call system_clock(t)
      if (t == 0) then
         call date_and_time(values=dt)
         
         
         
         
         
         
         t = dt(6) * 60 &  
           + dt(7)
      end if

      
      
      
      pid = 666 + MOD(t,10)  
 
      t = ieor(t, int(pid, kind(t)))
      do i = 1, n
         seed(i) = lcg(t)
      end do
   end if
   call random_seed(put=seed)

  contains

  
  
  
  function lcg(s)

   integer :: lcg
   
   integer :: s

   if (s == 0) then
      
      s = 1047
   else
      
      s = mod(s, 71)
   end if
   
   s = mod(s * 23, 17)
   
   lcg = int(mod(s, int(s/3.5)))

  end function lcg

  end subroutine init_random_seed


subroutine random_Poisson(mu,first,ival) 

















	REAL, INTENT(IN)    :: mu  
                                   
	LOGICAL, INTENT(IN) :: first
        INTEGER             :: ival










	REAL          :: b1_s, b2_s, c, c0, c1_s, c2_s, c3_s, del, difmuk, e, fk, fx, fy, g_s,  &
                    omega, px, py, t, u, v, x, xx
	REAL, SAVE    :: s, d, p, q, p0
        INTEGER       :: j, k, kflag
	LOGICAL, SAVE :: full_init
        INTEGER, SAVE :: l, m


	REAL, SAVE    :: pp(35)





	REAL, PARAMETER :: a0 = -.5, a1_s = .3333333, a2_s = -.2500068, a3 = .2000118,  &
                           a4 = -.1661269, a5 = .1421878, a6 = -0.1384794,  &
                           a7 = .1250060

	REAL, PARAMETER :: fact(10) = (/ 1., 1., 2., 6., 24., 120., 720., 5040.,  &
                 40320., 362880. /)


   difmuk = 0.
   fk = 1.0
   u = 0.



   IF (mu > 10.0) THEN


      IF (first) THEN
         s = SQRT(mu)
         d = 6.0*mu*mu





         l = mu - 1.1484
         full_init = .false.
      END IF


      g_s = mu + s*random_normal()
      IF (g_s > 0.0) THEN
         ival = g_s

 	 
         IF (ival>=l) RETURN

	 
		fk = ival
		difmuk = mu - fk
		CALL RANDOM_NUMBER(u)
		IF (d*u >= difmuk*difmuk*difmuk) RETURN
      END IF

      
      
      
      
      


      IF (.NOT. full_init) THEN
         omega = .3989423/s
		b1_s = .4166667E-1/mu
		b2_s = .3*b1_s*b1_s
		c3_s = .1428571*b1_s*b2_s
		c2_s = b2_s - 15.*c3_s
		c1_s = b1_s - 6.*b2_s + 45.*c3_s
		c0 = 1. - b1_s + 3.*b2_s - 15.*c3_s
		c = .1069/mu
		full_init = .true.
      END IF

	  IF (g_s < 0.0) GO TO 50

	

	  kflag = 0
	  GO TO 70

	

	  40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN

	
	
	

	  50 e = random_exponential()
	  CALL RANDOM_NUMBER(u)
	  u = u + u - one
	  t = 1.8 + SIGN(e, u)
	  IF (t <= (-.6744)) GO TO 50
	  ival = mu + s*t
	  fk = ival
	  difmuk = mu - fk

	

	  kflag = 1
	  GO TO 70

	

	  60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
	  RETURN

	
	

	  70 IF (ival>=10) GO TO 80
	  px = -mu

         
	  py = mu**ival/fact(MAX(ival+1,1))
	  GO TO 110

	
	
	

	  80 del = .8333333E-1/fk
	  del = del - 4.8*del*del*del
	  v = difmuk/fk
	  IF (ABS(v)>0.25) THEN
		px = fk*LOG(one + v) - difmuk - del
	  ELSE
		px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2_s)*v+a1_s)*v+a0) - del
	  END IF
	  py = .3989423/SQRT(fk)
	  110 x = (half - difmuk)/s
	  xx = x*x
	  fx = -half*xx
	  fy = omega* (((c3_s*xx + c2_s)*xx + c1_s)*xx + c0)
	  IF (kflag <= 0) GO TO 40
	  GO TO 60

	
	
	
      ELSE

	  IF (first) THEN
		m = MAX(1, INT(mu))
		l = 0
                
                
		p = EXP(-mu)
		q = p
		p0 = p
	  END IF

	

	  DO
		CALL RANDOM_NUMBER(u)
		ival = 0
		IF (u <= p0) RETURN

	
	
	

		IF (l == 0) GO TO 150
		j = 1
		IF (u > 0.458) j = MIN(l, m)
		DO k = j, l
		  IF (u <= pp(k)) GO TO 180
		END DO
		IF (l == 35) CYCLE

	
	

		150 l = l + 1
		DO k = l, 35
		  p = p*mu / k
		  q = q + p
		  pp(k) = q
		  IF (u <= q) GO TO 170
		END DO
		l = 35
	  END DO

	  170 l = k
	  180 ival = k
	  RETURN
	END IF

	RETURN
	END subroutine random_Poisson



	FUNCTION random_normal() RESULT(fn_val)

	
	
	
	

	
	

	
	

	REAL :: fn_val

	
	REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
				r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

	

	DO
	  CALL RANDOM_NUMBER(u)
	  CALL RANDOM_NUMBER(v)
	  v = 1.7156 * (v - half)

	
	  x = u - s
	  y = ABS(v) - t
	  q = x**2 + y*(a*y - b*x)

	
	  IF (q < r1) EXIT
	
	  IF (q > r2) CYCLE
	
	  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
	END DO

	
	fn_val = v/u
	RETURN

	END FUNCTION random_normal



	FUNCTION random_exponential() RESULT(fn_val)

	
	
	

	
	
	

	REAL  :: fn_val

	
	REAL  :: r

	DO
	  CALL RANDOM_NUMBER(r)
	  IF (r > zero) EXIT
	END DO

	fn_val = -LOG(r)
	RETURN

	END FUNCTION random_exponential



subroutine condensation_edmf(QT,THL,P,zagl,THV,QC)



real,intent(in)   :: QT,THL,P,zagl
real,intent(out)  :: THV
real,intent(inout):: QC

integer :: niter,i
real :: diff,exn,t,th,qs,qcold










  niter=50

  diff=2.e-5

  EXN=(P/p1000mb)**rcp
  
  do i=1,NITER
     T=EXN*THL + xlv/cp*QC        
     QS=qsat_blend(T,P)
     QCOLD=QC
     QC=0.5*QC + 0.5*MAX((QT-QS),0.)
     if (abs(QC-QCOLD)<Diff) exit
  enddo

  T=EXN*THL + xlv/cp*QC
  QS=qsat_blend(T,P)
  QC=max(QT-QS,0.)

  
  if(zagl < 100.)QC=0.

  
  THV=(THL+xlv/cp*QC)*(1.+QT*(rvovrd-1.)-rvovrd*QC)
  
  
  


  


end subroutine condensation_edmf



SUBROUTINE SCALE_AWARE(dx,PBL1,Psig_bl,Psig_shcu)

    
    
    
    
    
    
    
    

    REAL,INTENT(IN) :: dx,PBL1
    REAL, INTENT(OUT) :: Psig_bl,Psig_shcu
    REAL :: dxdh

    Psig_bl=1.0
    Psig_shcu=1.0
    dxdh=MAX(dx,10.)/MIN(PBL1,3000.)
    
    
    
    
    
     
    
     Psig_bl= ((dxdh**2) + 0.106*(dxdh**0.667))/((dxdh**2) +0.066*(dxdh**0.667) + 0.071)

    
    dxdh=MAX(dx,10.)/MIN(PBL1+500.,3500.)
    
    
    

    
    
    


    
    

    
    


    
    

    
    


    
    
    
    Psig_shcu= ((dxdh**2) + 0.145*(dxdh**0.667))/((dxdh**2) +0.172*(dxdh**0.667) + 0.170)


    

    
    

    
    
    
    If(Psig_bl > 1.0) Psig_bl=1.0
    If(Psig_bl < 0.0) Psig_bl=0.0

    If(Psig_shcu > 1.0) Psig_shcu=1.0
    If(Psig_shcu < 0.0) Psig_shcu=0.0

  END SUBROUTINE SCALE_AWARE



  FUNCTION esat_blend(t) 








      IMPLICIT NONE
      
      REAL, INTENT(IN):: t
      REAL :: esat_blend,XC,ESL,ESI,chi

      XC=MAX(-80.,t-273.16)




      IF (t .GE. 273.16) THEN
          esat_blend = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
      ELSE IF (t .LE. 253.) THEN
          esat_blend = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          chi  = (273.16-t)/20.16
          esat_blend = (1.-chi)*ESL  + chi*ESI
      END IF

  END FUNCTION esat_blend



  FUNCTION qsat_blend(t, P)



      IMPLICIT NONE

      REAL, INTENT(IN):: t, P
      REAL :: qsat_blend,XC,ESL,ESI,RSLF,RSIF,chi

      XC=MAX(-80.,t-273.16)

      IF (t .GE. 273.16) THEN
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
          qsat_blend = 0.622*ESL/(P-ESL) 
      ELSE IF (t .LE. 253.) THEN
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          qsat_blend = 0.622*ESI/(P-ESI)
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          RSLF = 0.622*ESL/(P-ESL)
          RSIF = 0.622*ESI/(P-ESI)
          chi  = (273.16-t)/20.16
          qsat_blend = (1.-chi)*RSLF + chi*RSIF
      END IF

  END FUNCTION qsat_blend



  FUNCTION xl_blend(t)




      IMPLICIT NONE

      REAL, INTENT(IN):: t
      REAL :: xl_blend,xlvt,xlst,chi

      IF (t .GE. 273.16) THEN
          xl_blend = xlv + (cpv-cliq)*(t-273.16)  
      ELSE IF (t .LE. 253.) THEN
          xl_blend = xls + (cpv-cice)*(t-273.16)  
      ELSE
          xlvt = xlv + (cpv-cliq)*(t-273.16)  
          xlst = xls + (cpv-cice)*(t-273.16)  
          chi  = (273.16-t)/20.16
          xl_blend = (1.-chi)*xlvt + chi*xlst     
      END IF

  END FUNCTION xl_blend




















  SUBROUTINE temf_mf(                        &
                 & kts,kte,dt,zw,p,pi1d,     &
                 & u,v,w,th,thl,thv,qt,qv,qc,&
                 & qke,ust,flt,flq,flqv,flqc,&
                 & hfx,qfx,tsk,              &
                 & pblh,rho,dfh,dx,znt,ep_2, &
            
                 & edmf_a,edmf_w,edmf_qt,    &
                 & edmf_thl,edmf_ent,edmf_qc,&
            
                 & s_aw,s_awthl,s_awqt,      &
                 & s_awqv,s_awqc,            &
                 & s_awu,s_awv,s_awqke,      &
            
                 & qc_bl1d,cldfra_bl1d,      &
            
                 &F_QC,F_QI,psig,            &
                 &spp_pbl,rstoch_col,        &
                 &ii,jj,ids,ide,jds,jde)

  
     INTEGER, INTENT(IN)   :: spp_pbl
     REAL, DIMENSION(kts:kte), INTENT(in)   :: rstoch_col 
  
     INTEGER, INTENT(IN) :: kts,kte,ii,jj,ids,ide,jds,jde
     REAL,DIMENSION(kts:kte), INTENT(IN) :: u,v,w,th,thl,qt,qv,qc,thv,p,pi1d
     REAL,DIMENSION(kts:kte), INTENT(IN) :: qke
     REAL,DIMENSION(kts:kte+1), INTENT(IN) :: zw  
     REAL,DIMENSION(kts:kte), INTENT(IN) :: rho  
     REAL,DIMENSION(kts:kte), INTENT(IN) :: dfh  
     REAL, INTENT(IN) :: dt,ust,flt,flq,flqv,flqc,hfx,qfx,tsk,pblh,dx,znt,ep_2,psig
     LOGICAL, OPTIONAL :: f_qc,f_qi

  
     REAL,DIMENSION(kts:kte), INTENT(OUT) :: &
                 & edmf_a,edmf_w,edmf_qt,    &
                 & edmf_thl,edmf_ent,edmf_qc

  
     REAL,DIMENSION(kts:kte+1) :: s_aw,      & 
                               s_awthl,      & 
                                s_awqt,      &
                                s_awqv,      &
                                s_awqc,      &
                                 s_awu,      &
                                 s_awv,      &
                                 s_awqke

     REAL,DIMENSION(kts:kte), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d




   real, parameter :: CM = 0.03      
   real, parameter :: Cdelt = 0.006  
   real, parameter :: Cw = 0.5       
   real, parameter :: Cc = 3.0       
   real, parameter :: lasymp = 200.0 
   real, parameter :: hmax = 4000.0  
   integer, parameter :: Nupd = 8    

   integer :: i, k, kt, nu   
   integer::  h0idx
   real::  h0
   real::  wstr, ang, wm
   real, dimension( Nupd) ::  hd,lcl,hct,ht
   real::  convection_TKE_surface_src, sfcFTE
   real::  sfcTHVF
   real::  z0t
   integer, dimension( Nupd) ::  hdidx,lclidx,hctidx,htidx
   integer::  hmax_idx
   integer::  tval
   real, dimension( kts:kte) :: zm, zt, dzm, dzt
   real, dimension( kts:kte) :: thetal, qtot
   real, dimension( kts:kte) :: u_temf, v_temf
   real, dimension( kts:kte) :: rv, rl, rt
   real, dimension( kts:kte) :: chi_poisson, gam
   real, dimension( kts:kte) :: dthdz
   real, dimension( kts:kte) :: lepsmin
   real, dimension( kts:kte) :: thetav
   real, dimension( kts:kte) :: dmoist_qtdz
   real, dimension( kts:kte) :: B, Bmoist
   real, dimension( kts:kte, Nupd) :: epsmf, deltmf, dMdz
   real, dimension( kts:kte, Nupd) :: UUPD, VUPD
   real, dimension( kts:kte, Nupd) :: thetavUPD, qlUPD, TEUPD
   real, dimension( kts:kte, Nupd) :: thetavUPDmoist, wUPD_dry
   real, dimension( kts:kte, Nupd) :: dthUPDdz, dwUPDdz
   real, dimension( kts:kte, Nupd) :: dwUPDmoistdz
   real, dimension( kts:kte, Nupd) :: dUUPDdz, dVUPDdz, dTEUPDdz
   real, dimension( kts:kte, Nupd) :: TUPD, rstUPD, rUPD, rlUPD, qstUPD
   real, dimension( kts:kte, Nupd) :: MUPD, wUPD, qtUPD, thlUPD, qcUPD
   real, dimension( kts:kte, Nupd) :: aUPD, cldfraUPD, aUPDt
   real, dimension( kts:kte) :: N2, S, Ri, beta, ftau, fth, ratio
   real, dimension( kts:kte) :: TKE, TE2
   real, dimension( kts:kte) :: ustrtilde, linv, leps
   real, dimension( kts:kte) :: km, kh
   real, dimension( kts:kte) :: Fz, QFK, uwk, vwk
   real, dimension( kts:kte) :: km_conv, kh_conv, lconv
   real, dimension( kts:kte) :: alpha2, beta2   
   real, dimension( kts:kte) :: THVF, buoy_src, srcs
   real, dimension( kts:kte) :: beta1 
   real, dimension( kts:kte) :: MFCth
   real Cepsmf    
   real red_fact  
   real, dimension( kts:kte) :: edmf_u, edmf_v, edmf_qke 
   integer:: bdy_dist,taper_dist
   real:: taper


   
   real, dimension( kts:kte, Nupd) :: &
             shf_temfx, qf_temfx, uw_temfx, vw_temfx , &
             mf_temfx
   real, dimension( Nupd) :: hd_temfx, lcl_temfx, hct_temfx, cfm_temfx
   logical is_convective
   
   real, dimension( kts:kte) :: sigq, qst, satdef
   real :: sigq2, rst, cldfra_sum, psig_w, maxw


































      IF ( wrf_at_debug_level(3000) ) THEN
           WRITE ( mynn_message , FMT='(A)' ) &
           ' MYNN; in TEMF_MF, beginning'
           CALL wrf_debug ( 0 , mynn_message )
      ENDIF

      
      s_aw   = 0.
      s_awthl= 0.
      s_awqt = 0.
      s_awqv = 0.
      s_awqc = 0.
      s_awu  = 0.
      s_awv  = 0.
      s_awqke= 0.
      edmf_a = 0.
      edmf_w = 0.
      edmf_qt= 0. 
      edmf_thl=0. 
      edmf_ent=0.
      edmf_qc= 0. 
      edmf_u=0.
      edmf_v=0.
      edmf_qke=0.
 
      z0t = znt

      do k = kts,kte
         rv(k) = qv(k) / (1.-qv(k))   
         rl(k) = qc(k) / (1.-qc(k))   
         rt(k) = qt(k)                
         thetal(k) = thl(k)
         qtot(k) = qt(k)
         thetav(k) = thv(k)
      end do

      do k = kts,kte
         u_temf(k) = u(k)
         v_temf(k) = v(k)
      end do

      
      
      k      = 1
      maxw   = 0.0
      DO WHILE (ZW(k) < pblh + 500.)
         maxw = MAX(maxw,ABS(W(k)))
         k = k+1
      ENDDO
      maxw = MAX(0.,maxw - 0.5)         
      Psig_w = MAX(0.0, 1.0 - maxw/0.5) 
      Psig_w = MIN(Psig_w, Psig)
      

      
      zm(1) = znt
      dzt(1) = zw(2) - zm(1)
      
      zt(1) = (zw(2) - znt) / 2.
      do kt = kts+1,kte
         zm(kt) = zw(kt) 
         zt(kt) = (zm(kt) + zw(kt+1)) / 2.
         dzm(kt) = zt(kt) - zt(kt-1)
         dzt(kt) = zw(kt+1) - zw(kt)
      end do
      dzm(1) = dzm(2)

      
      
      
      
      

      
      dthdz(1) = (thetal(2)-thetal(1)) / (zt(1) * log10(zm(2)/z0t))

      
      

      
      sfcTHVF = hfx/(rho(1)*cp) * (1.+0.608*(qv(1)+qc(1))) + 0.608*thetav(1)*qfx

      
      
      h0idx = 1
      h0 = zm(1)

      lepsmin(kts) = 0.

      
      hmax_idx = kte-1

      do k = kts+1,kte-1
         lepsmin(k) = 0.

         
         dthdz(k) = (thetal(k+1) - thetal(k)) / dzt(k)

         
         if (thetav(k) > thetav(1) .AND. h0idx .EQ. 1) then
         
            if (zm(k) < hmax) then
               h0idx = k
               h0 = zm(k)
            else
               h0idx = k
               h0 = hmax
            end if
         end if
         
         if (zm(k) > hmax) then
            hmax_idx = min(hmax_idx,k)
         end if
      end do

      

      dthdz(kte) = dthdz(kte-1)

      if ( hfx > 0.) then
         wstr = (g * h0 / thetav(2) * hfx/(rho(1)*cp) ) ** (1./3.)
         bdy_dist = min( min((ii-ids),(ide-ii)) , min((jj-jds),(jde-jj)) )
         taper_dist = 5
         
         if (bdy_dist .LE. taper_dist) then
            taper = max(0., min( 1., real(bdy_dist) / real(taper_dist) ) )
            wstr  = wstr * taper
         end if
      else
         wstr = 0.
      end if

      
      IF ( wrf_at_debug_level(3000) ) THEN
         WRITE ( mynn_message , FMT='(A,F5.1,F6.1,F5.1,F5.1,F5.1)' ) &
         ' MYNN; in TEMF_MF: wstr,hfx,dtdz1,dtdz2,h0:', wstr,hfx,dthdz(1),dthdz(2),h0
         CALL wrf_debug ( 0 , mynn_message )
      ENDIF

      
      is_convective = wstr > 0. .AND. dthdz(1)<0. .AND. dthdz(2)<0.  
      

      
      

      if ( is_convective) then

         
         IF ( wrf_at_debug_level(3000) ) THEN
            WRITE ( mynn_message , FMT='(A)' ) &
            ' MYNN; in TEMF_MF: inconvective branch'
            CALL wrf_debug ( 0 , mynn_message )
         ENDIF

         
         Cepsmf = 1.0 / max(200.,h0) 
         
         
         
         Cepsmf = max(Cepsmf,0.0010)  

         do nu = 1,Nupd
            do k = kts,kte
               
               
               
               
               
               
               epsmf(k,nu) = Cepsmf * (1+0.25*(nu-1)) 
            end do
            
            


            
            
            

            
            thlUPD(1,nu) = thetal(1) + Cw*wstr
            qtUPD(1,nu) = qtot(1) + 0.0*qfx/wstr
            rUPD(1,nu) = qtUPD(1,nu) / (1. - qtUPD(1,nu))
            wUPD(1,nu) = Cw * wstr
            wUPD_dry(1,nu) = Cw * wstr
            UUPD(1,nu) = u_temf(1)
            VUPD(1,nu) = v_temf(1)
            thetavUPD(1,nu) = thlUPD(1,nu) * (1. + 0.608*qtUPD(1,nu))  
            thetavUPDmoist(1,nu) = thetavUPD(1,nu)
            TEUPD(1,nu) = qke(1) + g / thetav(1) * sfcTHVF
            qlUPD(1,nu) = qc(1)  
            TUPD(1,nu) = thlUPD(1,nu) * pi1d(1)
            
            rstUPD(1,nu) = qsat_blend(TUPD(1,nu),p(1)) 
            rlUPD(1,nu) = 0.

            
            do k = 2,kte
               
               if ( k < hmax_idx) then
                  dthUPDdz(k-1,nu) = -epsmf(k,nu) * (thlUPD(k-1,nu) - thetal(k-1))
                  thlUPD(k,nu) = thlUPD(k-1,nu) + dthUPDdz(k-1,nu) * dzm(k-1)
                  dmoist_qtdz(k-1) = -epsmf(k,nu) * (qtUPD(k-1,nu) - qtot(k-1))
                  qtUPD(k,nu) = qtUPD(k-1,nu) + dmoist_qtdz(k-1) * dzm(k-1)
                  thetavUPD(k,nu) = thlUPD(k,nu) * (1. + 0.608*qtUPD(k,nu))  
                  B(k-1) = g * (thetavUPD(k,nu) - thetav(k)) / thetav(k)
                  if ( wUPD_dry(k-1,nu) < 1e-15 ) then
                     wUPD_dry(k,nu) = 0.
                  else
                     dwUPDdz(k-1,nu) = -2. *epsmf(k,nu)*wUPD_dry(k-1,nu) + 0.33*B(k-1)/wUPD_dry(k-1,nu)
                     wUPD_dry(k,nu) = wUPD_dry(k-1,nu) + dwUPDdz(k-1,nu) * dzm(k-1)
                  end if
                  dUUPDdz(k-1,nu) = -epsmf(k,nu) * (UUPD(k-1,nu) - u_temf(k-1))
                  UUPD(k,nu) = UUPD(k-1,nu) + dUUPDdz(k-1,nu) * dzm(k-1)
                  dVUPDdz(k-1,nu) = -epsmf(k,nu) * (VUPD(k-1,nu) - v_temf(k-1))
                  VUPD(k,nu) = VUPD(k-1,nu) + dVUPDdz(k-1,nu) * dzm(k-1)
                  dTEUPDdz(k-1,nu) = -epsmf(k,nu) * (TEUPD(k-1,nu) - qke(k-1))
                  TEUPD(k,nu) = TEUPD(k-1,nu) + dTEUPDdz(k-1,nu) * dzm(k-1)
                  
                  
                  rUPD(k,nu) = qtUPD(k,nu) / (1. - qtUPD(k,nu))
                  
                  TUPD(k,nu) = thlUPD(k,nu) * pi1d(k)
                  
                  
                  rstUPD(k,nu) = qsat_blend(TUPD(k,nu),p(k-1))
                  
                  beta1(k) = 0.622 * (xlv/(r_d*TUPD(k,nu))) * (xlv/(cp*TUPD(k,nu)))
                  rstUPD(k,nu) = rstUPD(k,nu) * (1.0+beta1(k)*rUPD(k,nu)) / (1.0+beta1(k)*rstUPD(k,nu))
                  qstUPD(k,nu) = rstUPD(k,nu) / (1. + rstUPD(k,nu))
                  if (rUPD(k,nu) > rstUPD(k,nu)) then
                     rlUPD(k,nu) = rUPD(k,nu) - rstUPD(k,nu)
                     qlUPD(k,nu) = rlUPD(k,nu) / (1. + rlUPD(k,nu))
                     thetavUPDmoist(k,nu) = (thlUPD(k,nu) + ((xlv/cp)*qlUPD(k,nu)/pi1d(k))) * &
                                        (1. + 0.608*qstUPD(k,nu) - qlUPD(k,nu))
                  else
                     rlUPD(k,nu) = 0.
                     qlUPD(k,nu) = qc(k-1)   
                     thetavUPDmoist(k,nu) = thlUPD(k,nu) * (1. + 0.608*qtUPD(k,nu))
                  end if
                  Bmoist(k-1) = g * (thetavUPDmoist(k,nu) - thetav(k)) / thetav(k)
                  if ( wUPD(k-1,nu) < 1e-15 ) then
                     wUPD(k,nu) = 0.
                  else
                     dwUPDmoistdz(k-1,nu) = -2. *epsmf(k,nu)*wUPD(k-1,nu) + 0.33*Bmoist(k-1)/wUPD(k-1,nu)
                     wUPD(k,nu) = wUPD(k-1,nu) + dwUPDmoistdz(k-1,nu) * dzm(k-1)
                  end if
               else 
                  thlUPD(k,nu) = thetal(k)
                  qtUPD(k,nu) = qtot(k)
                  wUPD_dry(k,nu) = 0.
                  UUPD(k,nu) = u_temf(k)
                  VUPD(k,nu) = v_temf(k)
                  TEUPD(k,nu) = qke(k)
                  qlUPD(k,nu) = qc(k-1)
                  wUPD(k,nu) = 0.
               end if

               IF ( wrf_at_debug_level(3000) ) THEN
                 IF ( ABS(wUPD(k,nu))>10. ) THEN
                   WRITE ( mynn_message , FMT='(A,2I3)' ) &
                   ' MYNN, in TEMF_MF, huge w at (nu,k):', nu,k
                   CALL wrf_debug ( 0 , mynn_message )
                   print *,"     thlUPD(1:k,nu) = ", thlUPD(1:k,nu)
                   print *,"     wUPD(1:k,nu)   = ", wUPD(1:k,nu)
                   print *,"     Bmoist(1:k-1)  = ", Bmoist(1:k-1)
                   print *,"     epsmf(1:k,nu)  = ", epsmf(1:k,nu)
                 ENDIF
               ENDIF

            ENDDO 

            
            if (wUPD_dry(1,nu) == 0.) then
               hdidx(nu) = 1
            else
               hdidx(nu) = kte  
               do k = 2,kte
                  if (wUPD_dry(k,nu) <= 0. .OR. zm(k) > hmax) then
                     hdidx(nu) = k
                     
                     exit
                  end if
               end do
            end if
 100                   hd(nu) = zm(hdidx(nu))

            
            lclidx(nu) = kte   
            do k = kts,kte
               if ( k < hmax_idx .AND. rUPD(k,nu) > rstUPD(k,nu)) then
                  lclidx(nu) = k
                  
                  exit
               end if
            end do
 200                   lcl(nu) = zm(lclidx(nu))

            if (hd(nu) > lcl(nu)) then   
               
               if (wUPD(1,nu) == 0.) then
                  hctidx(nu) = 1
               else
                  hctidx(nu) = kte  
                  do k = 2,kte
                     if (wUPD(k,nu) <= 0. .OR. zm(k) > hmax) then
                        hctidx(nu) = k
                        
                        exit
                     end if
                  end do
               end if
 300                         hct(nu) = zm(hctidx(nu))
               if (hctidx(nu) <= hdidx(nu)+1) then   
                  hct(nu) = hd(nu)
                  hctidx(nu) = hdidx(nu)
               else
               end if
            else   
               hct(nu) = hd(nu)
               hctidx(nu) = hdidx(nu)
            end if
            ht(nu) = max(hd(nu),hct(nu))
            htidx(nu) = max(hdidx(nu),hctidx(nu))

            
            do k = 1,kte
               if (zm(k) < 0.9*ht(nu)) then  
                   tval = 1
               else if (zm(k) >= 0.9*ht(nu) .AND. zm(k) <= 1.0*ht(nu)) then
                  
                  tval = 1. - ((zm(k) - 0.9*ht(nu)) / (1.0*ht(nu) - 0.9*ht(nu)))
               else  
                  tval = 0.
               end if
               thlUPD(k,nu) = tval * thlUPD(k,nu) + (1-tval)*thetal(k)
               thetavUPD(k,nu) = tval * thetavUPD(k,nu) + (1-tval)*thetav(k)
               qtUPD(k,nu) = tval * qtUPD(k,nu) + (1-tval) * qtot(k)
               if (k > 1) then
                  qlUPD(k,nu) = tval * qlUPD(k,nu) + (1-tval) * qc(k-1)
               end if
               UUPD(k,nu) = tval * UUPD(k,nu) + (1-tval) * u_temf(k)
               VUPD(k,nu) = tval * VUPD(k,nu) + (1-tval) * v_temf(k)
               TEUPD(k,nu) = tval * TEUPD(k,nu) + (1-tval) * qke(k)
               if (zm(k) > ht(nu)) then  
                  wUPD(k,nu) = 0.
                  dwUPDmoistdz(k,nu) = 0.
                  wUPD_dry(k,nu) = 0.
                  dwUPDdz(k,nu) = 0.
               end if
            end do

            
            
            
            
            
            
            
            deltmf(:,nu) = epsmf(:,nu)  

            
            mf_temfx(1,nu) = CM * wstr / Nupd
            
            
            mf_temfx(1,nu) = min(mf_temfx(1,nu),0.2/Nupd)
            do kt = 2,kte-1
               dMdz(kt,nu) = (epsmf(kt,nu) - deltmf(kt,nu)) * mf_temfx(kt-1,nu) * dzt(kt)
               mf_temfx(kt,nu) = mf_temfx(kt-1,nu) + dMdz(kt,nu)

               mf_temfx(kt,nu) = max(mf_temfx(kt,nu),0.0)
               IF ( wrf_at_debug_level(3000) ) THEN
                  IF ( mf_temfx(kt,nu)>=0.2/NUPD ) THEN
                     WRITE ( mynn_message , FMT='(A,2I3)' ) &
                     ' MYNN, in TEMF_MF, huge MF at (nu,k):', nu,kt
                     CALL wrf_debug ( 0 , mynn_message )
                     print *,"     mf_temfx(1:kt,nu)  = ", mf_temfx(1:kt,nu)
                  ENDIF
               ENDIF
            end do
            mf_temfx(kte,nu) = 0.

            
            
            
            
            aUPD(1,nu) = 0.06 / Nupd
            do k = 2,kte
               
               
               if (wUPD(k-1,nu) >= 1.0e-5 .AND. wUPD(k,nu) >= 1.0e-5) then
                  aUPD(k,nu) = ((mf_temfx(k-1,nu)+mf_temfx(k,nu))/2.0) / &
                         ((wUPD(k-1,nu)+wUPD(k,nu))/2.0)  
               else
                  aUPD(k,nu) = 0.0
               end if
               sigq2 = aUPD(k,nu) * (qtUPD(k,nu)-qtot(k))
               if (sigq2 > 0.0) then
                  sigq(k) = sqrt(sigq2)
               else
                  sigq(k) = 0.0
               end if
               
               rst = qsat_blend(th(k-1)*pi1d(k-1),p(k-1))
               qst(k) = rst / (1. + rst)
               satdef(k) = qtot(k) - qst(k)
               if (satdef(k) <= 0.0) then
                  if (sigq(k) > 1.0e-15) then
                     cldfraUPD(k,nu) = max(0.5 + 0.36 * atan(1.55*(satdef(k)/sigq(k))),0.0) / Nupd
                  else
                     cldfraUPD(k,nu) = 0.0
                  end if
               else
                  cldfraUPD(k,nu) = 1.0 / Nupd
               end if
               if (zm(k) < lcl(nu)) then
                  cldfraUPD(k,nu) = 0.0
               end if
            end do

         end do  

         
         
         
         cfm_temfx = 0.0
         do k = 2,kte
            
            cldfra_sum = 0.0
            edmf_a(k) = 0.0
            edmf_w(k) = 0.0
            edmf_thl(k) = 0.0
            edmf_qt(k) = 0.0
            edmf_qc(k) = 0.0
            edmf_u(k) = 0.0
            edmf_v(k) = 0.0
            edmf_qke(k) = 0.0
            edmf_ent(k) = 0.0
            do nu = 1,Nupd
               
               aUPDt(k,nu) = mf_temfx(k,nu) / wUPD(k,nu)
               if (aUPDt(k,nu) >= 1.0e-3 .AND. wUPD(k,nu) >= 1.0e-5) then
                  edmf_a(k) = edmf_a(k) + aUPDt(k,nu)
                  edmf_w(k) = edmf_w(k) + aUPDt(k,nu)*wUPD(k,nu)
                  edmf_thl(k) = edmf_thl(k) + aUPDt(k,nu)*thlUPD(k,nu)
                  edmf_qt(k) = edmf_qt(k) + aUPDt(k,nu)*qtUPD(k,nu)
                  edmf_qc(k) = edmf_qc(k) + aUPDt(k,nu)*qlUPD(k,nu)
                  edmf_u(k) = edmf_u(k) + aUPDt(k,nu)*UUPD(k,nu)
                  edmf_v(k) = edmf_v(k) + aUPDt(k,nu)*VUPD(k,nu)
                  edmf_qke(k) = edmf_qke(k) + aUPDt(k,nu)*TEUPD(k,nu)
                  edmf_ent(k) = edmf_ent(k) + aUPDt(k,nu)*epsmf(k,nu)
                  cldfra_sum = cldfra_sum + cldfraUPD(k,nu)
               end if
            end do

            IF ( wrf_at_debug_level(3000) ) THEN
            
            
            
            
            
            
            ENDIF


            
            if (edmf_a(k)>1.e-3) then
               edmf_w(k)=edmf_w(k)/edmf_a(k)
               edmf_qt(k)=edmf_qt(k)/edmf_a(k)
               edmf_thl(k)=edmf_thl(k)/edmf_a(k)
               edmf_ent(k)=edmf_ent(k)/edmf_a(k)
               edmf_qc(k)=edmf_qc(k)/edmf_a(k)
               edmf_u(k)=edmf_u(k)/edmf_a(k)
               edmf_v(k)=edmf_v(k)/edmf_a(k)
               edmf_qke(k)=edmf_qke(k)/edmf_a(k)

               if (edmf_qc(k) > 0.0) then
                 IF (cldfra_sum > edmf_a(k)) THEN
                   cldfra_bl1d(k) = cldfra_sum
                   qc_bl1d(k) = edmf_qc(k)*edmf_a(k)/cldfra_sum
                 ELSE
                   cldfra_bl1d(k)=edmf_a(k)
                   qc_bl1d(k) = edmf_qc(k)
                 ENDIF
               endif
            endif

            
            if (zt(k) <= hmax) then
               cfm_temfx = max(cldfra_bl1d(k),cfm_temfx)
            end if
         end do

         

         

         do k=kts,kte  

            
            if (edmf_a(k)>1.0e-3) then
            s_aw(k)   = edmf_a(k)*edmf_w(k)*psig_w * (1.0+rstoch_col(k))
            s_awthl(k)= edmf_a(k)*edmf_w(k)*edmf_thl(k)*psig_w * (1.0+rstoch_col(k)) 
            s_awqt(k) = edmf_a(k)*edmf_w(k)*edmf_qt(k)*psig_w * (1.0+rstoch_col(k))
            s_awqc(k) = edmf_a(k)*edmf_w(k)*edmf_qc(k)*psig_w * (1.0+rstoch_col(k))
            s_awqv(k) = s_awqt(k) - s_awqc(k)
            s_awu(k)  = edmf_a(k)*edmf_w(k)*edmf_u(k)*psig_w * (1.0+rstoch_col(k)) 
            s_awv(k)  = edmf_a(k)*edmf_w(k)*edmf_v(k)*psig_w * (1.0+rstoch_col(k))
            s_awqke(k) = edmf_a(k)*edmf_w(k)*edmf_qke(k)*psig_w * (1.0+rstoch_col(k))
            endif
            
             edmf_a(k)=edmf_a(k)*psig_w 
         enddo

      
      
      else
         edmf_a = 0.
         edmf_w = 0.
         edmf_qt = 0.
         edmf_thl = 0.
         edmf_ent = 0.
         edmf_u = 0.
         edmf_v = 0.
         edmf_qke = 0.
         s_aw   = 0.
         s_awthl= 0.
         s_awqt = 0.
         s_awqv = 0.
         s_awqc = 0.
         s_awu  = 0.
         s_awv  = 0.
         s_awqke= 0.
         edmf_qc(1) = qc(1)
         
         do k = kts+1,kte-1
            edmf_qc(k) = qc(k-1)
            
         end do
      end if
      
      

      
      
      
      
      
      
      
      
      
      

END SUBROUTINE temf_mf



   real function rsat_temf(p,T,ep2)






implicit none
real p, T, ep2
real temp, x
real, parameter :: c0 = 0.6105851e+3
real, parameter :: c1 = 0.4440316e+2
real, parameter :: c2 = 0.1430341e+1
real, parameter :: c3 = 0.2641412e-1
real, parameter :: c4 = 0.2995057e-3
real, parameter :: c5 = 0.2031998e-5
real, parameter :: c6 = 0.6936113e-8
real, parameter :: c7 = 0.2564861e-11
real, parameter :: c8 = -0.3704404e-13

temp = T - 273.15

x =c0+temp*(c1+temp*(c2+temp*(c3+temp*(c4+temp*(c5+temp*(c6+temp*(c7+temp*c8)))))))
rsat_temf = ep2*x/(p-x)

return
end function rsat_temf



END MODULE module_bl_mynn
