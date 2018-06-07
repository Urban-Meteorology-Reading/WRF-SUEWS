













MODULE AnOHM_module

  IMPLICIT NONE
CONTAINS

  
  
  
  
  
  
  
  
  SUBROUTINE AnOHM(&
       qn1,qn1_store,qn1_av_store,qf,&
       MetForcingData_grid,moist_surf,&
       alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
       sfr,nsurf,nsh,EmissionsMethod,id,Gridiv,&
       a1,a2,a3,qs,deltaQi)

    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:,:)::MetForcingData_grid 

    REAL(KIND(1d0)),INTENT(in):: qn1               
    REAL(KIND(1d0)),INTENT(in):: qf                
    REAL(KIND(1d0)),INTENT(in):: sfr(nsurf)        
    REAL(KIND(1d0)),INTENT(in):: moist_surf(nsurf) 

    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)::alb  
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)::emis 
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)::cpAnOHM   
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)::kkAnOHM   
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)::chAnOHM   

    INTEGER,INTENT(in):: id                
    INTEGER,INTENT(in):: Gridiv            
    INTEGER,INTENT(in):: EmissionsMethod 
    INTEGER,INTENT(in):: nsurf             
    INTEGER,INTENT(in):: nsh               

    REAL(KIND(1d0)),INTENT(inout)::qn1_store(nsh) 
    REAL(KIND(1d0)),INTENT(inout)::qn1_av_store(2*nsh+1) 

    REAL(KIND(1d0)),INTENT(out):: a1 
    REAL(KIND(1d0)),INTENT(out):: a2 
    REAL(KIND(1d0)),INTENT(out):: a3 
    REAL(KIND(1d0)),INTENT(out):: qs 
    REAL(KIND(1d0)),INTENT(out):: deltaQi(nsurf) 

    INTEGER :: is,xid 
    INTEGER,SAVE :: id_save 
    REAL(KIND(1d0)),PARAMETER::NotUsed=-55.5
    INTEGER,PARAMETER::notUsedI=-55
    LOGICAL :: idQ 

    REAL(KIND(1d0))                  :: dqndt       
    
    REAL(KIND(1d0)),DIMENSION(nsurf) :: xa1,xa2,xa3 
    
    

    
    xa1     = 0.1
    xa2     = 0.2
    xa3     = 10
    qs      = -999
    deltaQi = 0 

    
    
    
    idQ=COUNT(MetForcingData_grid(:,2)==id .AND. & 
         MetForcingData_grid(:,4)==0 .AND. & 
         MetForcingData_grid(:,15)>0) & 
         .GE. 6

    
    IF ( idQ ) THEN
       
       xid=id
       id_save=id 
    ELSE
       
       xid=id_save
    END IF

    DO is=1,nsurf
       
       CALL AnOHM_coef(is,xid,Gridiv,MetForcingData_grid,moist_surf,EmissionsMethod,qf,& 
            alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
            xa1(is),xa2(is),xa3(is))                         

    ENDDO

    
    a1=DOT_PRODUCT(xa1,sfr)
    a2=DOT_PRODUCT(xa2,sfr)
    a3=DOT_PRODUCT(xa3,sfr)


    
    qs=-999          
    IF(qn1>-999) THEN   

       
       CALL OHM_dqndt_cal(nsh,qn1,qn1_store,qn1_av_store,dqndt)

       
       CALL OHM_QS_cal(qn1,dqndt,a1,a2,a3,qs)

    ELSE
       CALL ErrorHint(21,'SUEWS_AnOHM.f95: bad value for qn found during qs calculation.',qn1,NotUsed,notUsedI)
    ENDIF

  END SUBROUTINE AnOHM
  

  
  
  
  
  
  
  SUBROUTINE AnOHM_coef(&
       sfc_typ,xid,xgrid,&
       MetForcingData_grid,moist,EmissionsMethod,qf,& 
       alb, emis, cpAnOHM, kkAnOHM, chAnOHM,&
       xa1,xa2,xa3)                         

    IMPLICIT NONE

    
    INTEGER,INTENT(in):: sfc_typ           
    INTEGER,INTENT(in):: xid               
    INTEGER,INTENT(in):: xgrid             
    INTEGER,INTENT(in):: EmissionsMethod 

    REAL(KIND(1d0)),INTENT(in):: qf                
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: alb                 
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: emis                
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: cpAnOHM                  
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: kkAnOHM                  
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: chAnOHM                  
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:)   :: moist               
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:,:) :: MetForcingData_grid 

    
    REAL(KIND(1d0)),INTENT(out) :: xa1 
    REAL(KIND(1d0)),INTENT(out) :: xa2 
    REAL(KIND(1d0)),INTENT(out) :: xa3 

    
    REAL(KIND(1d0)):: ATs   
    REAL(KIND(1d0)):: mTs   
    REAL(KIND(1d0)):: gamma 

    
    REAL(KIND(1d0))::ASd,mSd,tSd 
    REAL(KIND(1d0))::ATa,mTa,tTa 
    REAL(KIND(1d0))::tau         
    REAL(KIND(1d0))::mWS,mWF,mAH 


    
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::Sd   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::Ta   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::RH   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::pres 
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::WS   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::WF   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::AH   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE::tHr  

    
    REAL(KIND(1d0)) ::xalb   
    REAL(KIND(1d0)) ::xemis  
    REAL(KIND(1d0)) ::xcp    
    REAL(KIND(1d0)) ::xk     
    REAL(KIND(1d0)) ::xch    
    REAL(KIND(1d0)) ::xBo    
    REAL(KIND(1d0)) ::xeta   
    REAL(KIND(1d0)) ::xmu    
    REAL(KIND(1d0)) ::xmoist 



    
    
    
    INTEGER,SAVE :: id_save,grid_save
    REAL(KIND(1d0)), SAVE:: coeff_grid_day(7,3)=-999.






    IF ( xid==id_save .AND. xgrid ==grid_save) THEN
       

       xa1=coeff_grid_day(sfc_typ,1)
       xa2=coeff_grid_day(sfc_typ,2)
       xa3=coeff_grid_day(sfc_typ,3)
    ELSE


       
       CALL AnOHM_Fc(&
            xid,MetForcingData_grid,EmissionsMethod,qf,& 
            ASd,mSd,tSd,ATa,mTa,tTa,tau,mWS,mWF,mAH)    

       
       CALL AnOHM_FcLoad(&
            xid,MetForcingData_grid,EmissionsMethod,qf,& 
            Sd,Ta,RH,pres,WS,WF,AH,tHr)                 

       
       xalb   = alb(sfc_typ)
       xemis  = emis(sfc_typ)
       xcp    = cpAnOHM(sfc_typ)
       xk     = kkAnOHM(sfc_typ)
       xch    = chAnOHM(sfc_typ)
       xmoist = moist(sfc_typ)


       
       CALL AnOHM_Bo_cal(&
            sfc_typ,&
            Sd,Ta,RH,pres,tHr,               & 
            ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH, & 
            xalb,xemis,xcp,xk,xch,xmoist,    & 
            tSd,                             & 
            xBo)                               


       
       SELECT CASE (sfc_typ)
       CASE (1:6) 
          CALL  AnOHM_coef_land_cal(&
               ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,& 
               xalb,xemis,xcp,xk,xch,xBo,      & 
               xa1,xa2,xa3,ATs,mTs,gamma)                    

       CASE (7) 
          
          xeta = 0.3
          xmu  = 0.2
          CALL  AnOHM_coef_water_cal(&
               ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
               xalb,xemis,xcp,xk,xch,xBo,xeta,xmu,&   
               xa1,xa2,xa3,ATs,mTs,gamma)            

          
          id_save =xid
          grid_save=xgrid

       END SELECT
       
       coeff_grid_day(sfc_typ,:)=(/xa1,xa2,xa3/)
    END IF

  END SUBROUTINE AnOHM_coef
  

  
  
  
  
  
  SUBROUTINE AnOHM_xTs(&
       sfc_typ,& 
       ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
       xalb,xemis,xcp,xk,xch,xBo,&   
       tSd,& 
       xTHr,&
       xTs)

    IMPLICIT NONE
    
    INTEGER,INTENT(in):: sfc_typ 

    
    REAL(KIND(1d0)),INTENT(in):: ASd 
    REAL(KIND(1d0)),INTENT(in):: mSd 
    REAL(KIND(1d0)),INTENT(in):: ATa 
    REAL(KIND(1d0)),INTENT(in):: mTa 
    REAL(KIND(1d0)),INTENT(in):: tau 
    REAL(KIND(1d0)),INTENT(in):: mWS 
    REAL(KIND(1d0)),INTENT(in):: mWF 
    REAL(KIND(1d0)),INTENT(in):: mAH 

    
    REAL(KIND(1d0)),INTENT(in):: xalb  
    REAL(KIND(1d0)),INTENT(in):: xemis 
    REAL(KIND(1d0)),INTENT(in):: xcp   
    REAL(KIND(1d0)),INTENT(in):: xk    
    REAL(KIND(1d0)),INTENT(in):: xch   
    REAL(KIND(1d0)),INTENT(in):: xBo   
    REAL(KIND(1d0)):: xeta  
    REAL(KIND(1d0)):: xmu   

    
    REAL(KIND(1d0)),INTENT(in):: tSd  
    REAL(KIND(1d0)),INTENT(in):: xTHr 

    
    REAL(KIND(1d0)),INTENT(out) :: xTs 

    
    REAL(KIND(1d0)) :: &
         xa1,xa2,xa3,&
         ATs,mTs,gamma 

    
    REAL(KIND(1d0)), PARAMETER :: PI    = ATAN(1.0)*4      
    REAL(KIND(1d0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60)  

    SELECT CASE (sfc_typ)
    CASE (1:6) 
       CALL  AnOHM_coef_land_cal(&
            ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,& 
            xalb,xemis,xcp,xk,xch,xBo,      & 
            xa1,xa2,xa3,ATs,mTs,gamma)                    


    CASE (7) 
       
       xeta  = 0.3
       xmu   = 0.2
       CALL  AnOHM_coef_water_cal(&
            ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
            xalb,xemis,xcp,xk,xch,xBo,xeta,xmu,&   
            xa1,xa2,xa3,ATs,mTs,gamma)            

    END SELECT

    
    xTs=ATs*SIN(OMEGA*(xTHr-tSd+6)*3600-gamma)+mTs

  END SUBROUTINE AnOHM_xTs
  

  
  SUBROUTINE AnOHM_coef_land_cal(&
       ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
       xalb,xemis,xcp,xk,xch,xBo,&   
       xa1,xa2,xa3,ATs,mTs,gamma)            

    IMPLICIT NONE

    
    REAL(KIND(1d0)),INTENT(in):: ASd 
    REAL(KIND(1d0)),INTENT(in):: mSd 
    REAL(KIND(1d0)),INTENT(in):: ATa 
    REAL(KIND(1d0)),INTENT(in):: mTa 
    REAL(KIND(1d0)),INTENT(in):: tau 
    REAL(KIND(1d0)),INTENT(in):: mWS 
    REAL(KIND(1d0)),INTENT(in):: mWF 
    REAL(KIND(1d0)),INTENT(in):: mAH 
    
    REAL(KIND(1d0)),INTENT(in):: xalb  
    REAL(KIND(1d0)),INTENT(in):: xemis 
    REAL(KIND(1d0)),INTENT(in):: xcp   
    REAL(KIND(1d0)),INTENT(in):: xk    
    REAL(KIND(1d0)),INTENT(in):: xch   
    REAL(KIND(1d0)),INTENT(in):: xBo   

    
    REAL(KIND(1d0)),INTENT(out) :: xa1   
    REAL(KIND(1d0)),INTENT(out) :: xa2   
    REAL(KIND(1d0)),INTENT(out) :: xa3   
    REAL(KIND(1d0)),INTENT(out) :: ATs   
    REAL(KIND(1d0)),INTENT(out) :: mTs   
    REAL(KIND(1d0)),INTENT(out) :: gamma 


    
    REAL(KIND(1d0)), PARAMETER :: SIGMA = 5.67e-8          
    REAL(KIND(1d0)), PARAMETER :: PI    = ATAN(1.0)*4      
    REAL(KIND(1d0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60)  

    
    REAL(KIND(1d0)) :: beta              
    REAL(KIND(1d0)) :: f,fL,fT           
    REAL(KIND(1d0)) :: lambda            
    REAL(KIND(1d0)) :: delta,m,n         
    REAL(KIND(1d0)) :: ms,ns             
    REAL(KIND(1d0)) :: ceta,cphi         
    REAL(KIND(1d0)) :: eta,phi,xlag      
    REAL(KIND(1d0)) :: xx1,xx2,xx3,xchWS 
    



    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


    
    
    

    
    xchWS = xch*mWS

    
    IF ( ABS(xBo)<0.001 ) THEN
       beta   = 1/0.001
    ELSE
       beta   = 1/xBo
    END IF


    f      = ((1+beta)*xchWS+4*SIGMA*xemis*mTa**3)

    fL     = 4*SIGMA*xemis*mTa**3

    fT     = (1+beta)*xchWS

    lambda = xk/xcp

    delta  = SQRT(.5*(mWF**2+SQRT(mWF**4+16*lambda**2*OMEGA**2)))

    m      = (2*lambda)/(delta+mWF)
    n      = delta/OMEGA



    
    mTs   = (mSd*(1-xalb)/f)+mTa
    ms    = 1+xk/(f*m)
    ns    = xk/(f*n)

    xx1   = f*SIN(tau)*ATa

    xx2   = (1-xalb)*ASd+f*COS(tau)*ATa

    gamma = ATAN(ns/ms)+ATAN(xx1/xx2)

    ATs   = -(SIN(tau)*ATa)/(ns*COS(gamma)-ms*SIN(gamma))


    
    xx1  = (ns*COS(gamma)+SIN(gamma)-ms*SIN(gamma))*SIN(tau)*ATa*fL
    xx2  = (xalb-1)*(ns*COS(gamma)-ms*SIN(gamma))*ASd
    xx3  = (-ms*COS(tau)*SIN(tau)+COS(gamma)*(ns*COS(tau)+SIN(tau)))*ATa*fL
    xx2  = xx2-xx3
    phi  = ATAN(xx1/xx2)
    xx3  = (ns*COS(gamma)-ms*SIN(gamma))
    xx1  = (1+SIN(gamma)/xx3)*SIN(tau)*ATa*fL
    xx2  = (xalb-1)*ASd-(COS(tau)+COS(gamma)*SIN(tau)/xx3)*ATa*fL
    cphi = SQRT(xx1**2+xx2**2)

    
    xx1  = m*COS(gamma)-n*SIN(gamma)
    xx2  = m*SIN(gamma)+n*COS(gamma)
    eta  = ATAN(xx1/xx2)

    xx1  = xk**2*(m**2+n**2)*ATs**2
    xx2  = m**2*n**2
    ceta = SQRT(xx1/xx2)

    
    xlag = eta-phi

    
    
    xa1 = (ceta*COS(xlag))/cphi

    
    xa2 = (ceta*SIN(xlag))/(OMEGA*cphi)
    xa2 = xa2/3600 

    
    xa3  = -xa1*(fT/f)*(mSd*(1-xalb))-mAH







  END SUBROUTINE AnOHM_coef_land_cal
  

  
  
  
  

  SUBROUTINE AnOHM_coef_water_cal(&
       ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
       xalb,xemis,xcp,xk,xch,xBo,xeta,xmu,&   
       xa1,xa2,xa3,ATs,mTs,gamma)            


    IMPLICIT NONE

    
    REAL(KIND(1d0)),INTENT(in):: ASd 
    REAL(KIND(1d0)),INTENT(in):: mSd 
    REAL(KIND(1d0)),INTENT(in):: ATa 
    REAL(KIND(1d0)),INTENT(in):: mTa 
    REAL(KIND(1d0)),INTENT(in):: tau 
    REAL(KIND(1d0)),INTENT(in):: mWS 
    REAL(KIND(1d0)),INTENT(in):: mWF 
    REAL(KIND(1d0)),INTENT(in):: mAH 

    
    REAL(KIND(1d0)),INTENT(in):: xalb  
    REAL(KIND(1d0)),INTENT(in):: xemis 
    REAL(KIND(1d0)),INTENT(in):: xcp   
    REAL(KIND(1d0)),INTENT(in):: xk    
    REAL(KIND(1d0)),INTENT(in):: xch   
    REAL(KIND(1d0)),INTENT(in):: xBo   
    REAL(KIND(1d0)),INTENT(in):: xeta  
    REAL(KIND(1d0)),INTENT(in):: xmu   

    
    REAL(KIND(1d0)),INTENT(out) :: xa1   
    REAL(KIND(1d0)),INTENT(out) :: xa2   
    REAL(KIND(1d0)),INTENT(out) :: xa3   
    REAL(KIND(1d0)),INTENT(out) :: ATs   
    REAL(KIND(1d0)),INTENT(out) :: mTs   
    REAL(KIND(1d0)),INTENT(out) :: gamma 

    
    REAL(KIND(1d0)), PARAMETER :: SIGMA = 5.67e-8          
    REAL(KIND(1d0)), PARAMETER :: PI    = ATAN(1.0)*4      
    REAL(KIND(1d0)), PARAMETER :: OMEGA = 2*Pi/(24*60*60)  

    
    REAL(KIND(1d0)) :: beta                   
    REAL(KIND(1d0)) :: f,fL,fT                
    REAL(KIND(1d0)) :: lambda,calb            
    REAL(KIND(1d0)) :: delta             
    
    REAL(KIND(1d0)) :: xm,xn                  
    
    REAL(KIND(1d0)) :: phi              
    
    REAL(KIND(1d0)) :: czeta,ctheta           
    REAL(KIND(1d0)) :: zeta,theta,xlag           
    REAL(KIND(1d0)) :: xx1,xx2,xx3            
    REAL(KIND(1d0)) :: kappa                  
    REAL(KIND(1d0)) :: dtau,dpsi,dphi         
    REAL(KIND(1d0)) :: cdtau,cdpsi,cdphi      
    REAL(KIND(1d0)) :: xxT,xxkappa,xxdltphi,xchWS   
    

    
    REAL(KIND(1d0)) :: dummy
    dummy=mah+mwf
    

    
    xm     = xk*xmu**2
    xn     = xcp*OMEGA
    phi    = ATAN(xn/xm)
    kappa  = SQRT(xcp*OMEGA/(2*xk))

    
    xchWS    = xch*mWS
    beta   = 1/xBo
    f      = ((1+beta)*xchWS+4*SIGMA*xemis*mTa**3)
    fL     = 4*SIGMA*xemis*mTa**3
    fT     = (1+beta)*xchWS

    calb=1-xalb


    lambda= SQRT(xm**2+xn**2)

    dtau=ATAN(xk*kappa/(f+xk*kappa))
    dpsi=ATAN((xk-xmu)/kappa)
    dphi=ATAN(kappa*(f+xk*xmu)/(f*(kappa-xmu)+xk*kappa*(2*kappa-xmu)))
    cdtau=SQRT((xk*kappa)**2+(f+xk*kappa)**2)
    cdpsi=SQRT((xk-xmu)**2+kappa**2)
    cdphi=cdtau*cdpsi

    
    
    mTs   = (mSd*(1-xalb+xeta)/f)+mTa
    
    xx1 = (xk*xeta*xmu*calb*ASd*cdpsi)**2
    xx2 = 2*lambda*SQRT(xx1)*(calb*ASd*SIN(phi-dpsi)+f*ATa*SIN(tau+phi-dpsi))
    xx3 = lambda**2*((calb*ASd+COS(tau)*f*ATa)**2+(SIN(tau)*f*ATa)**2)
    ATs = 1/(cdtau*lambda)*SQRT(xx1+xx2+xx3)
    
    xx1 = (xk*kappa*calb*ASd+cdtau*f*ATa*SIN(tau+dtau))*lambda    &
         +(xk*xeta*xmu*calb*ASd*cdphi*SIN(phi+dphi))
    xx2 = ((f+xk*kappa)*calb*ASd-cdtau*f*ATa*COS(tau+dtau))*lambda&
         -xk*xeta*xmu*calb*ASd*cdphi*COS(phi+dphi)
    delta = ATAN(xx1/xx2)
    gamma = delta

    
    
    xx1    = fL*(ATs*SIN(delta)-ATa*SIN(tau))
    xx2    = calb*ASd-fL*(ATs*COS(delta)-ATa*COS(tau))
    theta  = ATAN(xx1/xx2)
    
    ctheta = SQRT(xx1**2+xx2**2)

    
    
    xxT      = SQRT(2.)*kappa*lambda*ATs
    xxkappa  = cdpsi*xeta*xmu*ASd
    xxdltphi = COS(delta)*SIN(dpsi)*COS(phi)-SIN(delta)*COS(dpsi)*SIN(phi)
    
    xx1  = xxT*SIN(PI/4-delta)+xxkappa*SIN(phi+dpsi)
    xx2  = xxT*SIN(PI/4+delta)-xxkappa*SIN(PHI-dpsi)
    zeta = ATAN(xx1/xx2)
    
    xx1   = 2*SQRT(2.)*xxkappa*xxT*xxdltphi
    xx2   = (1-COS(2*dpsi)*COS(2*phi))*xxkappa**2
    xx3   = xxT**2
    czeta = xk/lambda*SQRT(xx1+xx2+xx3)


    
    xlag = zeta-theta
    
    xa1  = (czeta*COS(xlag))/ctheta


    
    xa2  = (czeta*SIN(xlag))/(OMEGA*ctheta)
    xa2  = xa2/3600 

    
    xa3  = mSd*(xalb-1)*(xeta+(fT-fL*xeta)/f*xa1)






  END SUBROUTINE AnOHM_coef_water_cal
  

  
  SUBROUTINE AnOHM_Fc(&
       xid,MetForcingData_grid,EmissionsMethod,qf,& 
       ASd,mSd,tSd,ATa,mTa,tTa,tau,mWS,mWF,mAH)    

    IMPLICIT NONE

    
    INTEGER,INTENT(in):: xid
    INTEGER,INTENT(in):: EmissionsMethod
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:,:) ::MetForcingData_grid
    REAL(KIND(1d0)),INTENT(in):: qf                

    
    REAL(KIND(1d0)),INTENT(out):: ASd 
    REAL(KIND(1d0)),INTENT(out):: mSd 
    REAL(KIND(1d0)),INTENT(out):: tSd 
    REAL(KIND(1d0)),INTENT(out):: ATa 
    REAL(KIND(1d0)),INTENT(out):: mTa 
    REAL(KIND(1d0)),INTENT(out):: tTa 
    REAL(KIND(1d0)),INTENT(out):: tau 
    REAL(KIND(1d0)),INTENT(out):: mWS 
    REAL(KIND(1d0)),INTENT(out):: mWF 
    REAL(KIND(1d0)),INTENT(out):: mAH 

    
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: Sd   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: Ta   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: RH   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: pres 
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: WS   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: WF   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: AH   
    REAL(KIND(1d0)), DIMENSION(:),ALLOCATABLE:: tHr  


    
    CALL AnOHM_FcLoad(xid,MetForcingData_grid,EmissionsMethod,qf,Sd,Ta,RH,pres,WS,WF,AH,tHr)
    
    CALL AnOHM_FcCal(Sd,Ta,WS,WF,AH,tHr,ASd,mSd,tSd,ATa,mTa,tTa,tau,mWS,mWF,mAH)


    


    

  END SUBROUTINE AnOHM_Fc
  

  
  
  SUBROUTINE AnOHM_FcLoad(&
       xid,MetForcingData_grid,EmissionsMethod,qf,& 
       Sd,Ta,RH,pres,WS,WF,AH,tHr) 

    IMPLICIT NONE

    
    INTEGER,INTENT(in):: xid 
    INTEGER,INTENT(in):: EmissionsMethod 
    REAL(KIND(1d0)),INTENT(in),DIMENSION(:,:) ::MetForcingData_grid 
    REAL(KIND(1d0)),INTENT(in):: qf                

    
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: Sd  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: Ta  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: RH  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: pres
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: WS  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: WF  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: AH  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(out), ALLOCATABLE:: tHr 


    
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE :: subMet 

    INTEGER :: err
    INTEGER :: lenMetData,nVar


    LOGICAL, ALLOCATABLE :: metMask(:)

    
    IF (ALLOCATED(metMask)) DEALLOCATE(metMask, stat=err)
    ALLOCATE(metMask(SIZE(MetForcingData_grid, dim=1)))
    metMask=(MetForcingData_grid(:,2)==xid & 
         .AND. MetForcingData_grid(:,4)==0)

    
    lenMetData = COUNT(metMask)

    
    nVar=8

    
    IF (ALLOCATED(subMet)) DEALLOCATE(subMet, stat=err)
    ALLOCATE(subMet(lenMetData,nVar))
    subMet=RESHAPE(PACK(MetForcingData_grid(:,(/3,& 
         15,12,11,13,10,12,9/)),&
                                
         SPREAD(metMask, dim=2, ncopies=nVar)),& 
         (/lenMetData,nVar/)) 


    
    IF (ALLOCATED(tHr)) DEALLOCATE(tHr, stat=err)
    ALLOCATE(tHr(lenMetData))
    IF (ALLOCATED(Sd)) DEALLOCATE(Sd, stat=err)
    ALLOCATE(Sd(lenMetData))
    IF (ALLOCATED(Ta)) DEALLOCATE(Ta, stat=err)
    ALLOCATE(Ta(lenMetData))
    IF (ALLOCATED(RH)) DEALLOCATE(RH, stat=err)
    ALLOCATE(RH(lenMetData))
    IF (ALLOCATED(pres)) DEALLOCATE(pres, stat=err)
    ALLOCATE(pres(lenMetData))
    IF (ALLOCATED(WS)) DEALLOCATE(WS, stat=err)
    ALLOCATE(WS(lenMetData))
    IF (ALLOCATED(WF)) DEALLOCATE(WF, stat=err)
    ALLOCATE(WF(lenMetData))
    IF (ALLOCATED(AH)) DEALLOCATE(AH, stat=err)
    ALLOCATE(AH(lenMetData))

    
    tHr  = subMet(:,1)
    Sd   = subMet(:,2)
    Ta   = subMet(:,3)
    RH   = subMet(:,4)
    pres = subMet(:,5)
    WS   = subMet(:,6)
    WF   = 0          
    IF ( EmissionsMethod == 0 ) THEN
       AH = subMet(:,8)    
    ELSE
       
       AH =  qf 
       
    END IF


  END SUBROUTINE AnOHM_FcLoad
  

  
  
  
  SUBROUTINE AnOHM_FcCal(&
       Sd,Ta,WS,WF,AH,tHr,&                     
       ASd,mSd,tSd,ATa,mTa,tTa,tau,mWS,mWF,mAH) 
    IMPLICIT NONE

    
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: Sd  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: Ta  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: WS  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: WF  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: AH  
    REAL(KIND(1d0)),DIMENSION(:),INTENT(in):: tHr 

    
    REAL(KIND(1d0)),INTENT(out):: ASd 
    REAL(KIND(1d0)),INTENT(out):: mSd 
    REAL(KIND(1d0)),INTENT(out):: tSd 
    REAL(KIND(1d0)),INTENT(out):: ATa 
    REAL(KIND(1d0)),INTENT(out):: mTa 
    REAL(KIND(1d0)),INTENT(out):: tTa 
    REAL(KIND(1d0)),INTENT(out):: tau 
    REAL(KIND(1d0)),INTENT(out):: mWS 
    REAL(KIND(1d0)),INTENT(out):: mWF 
    REAL(KIND(1d0)),INTENT(out):: mAH 

    
    REAL(KIND(1d0)), PARAMETER :: PI  = ATAN(1.0)*4 
    REAL(KIND(1d0)), PARAMETER :: C2K = 273.15      

    
    REAL(KIND(1d0)),ALLOCATABLE :: tHrDay(:) 
    REAL(KIND(1d0)),ALLOCATABLE :: selX(:)   


    
    INTEGER :: err,lenDay
    LOGICAL,DIMENSION(:),ALLOCATABLE::SdMask



    ALLOCATE(SdMask(SIZE(Sd, dim=1)), stat=err)
    IF ( err/= 0) PRINT *, "SdMask: Allocation request denied"
    SdMask=Sd>5
    lenDay=COUNT(SdMask)



    ALLOCATE(tHrDay(lenDay), stat=err)
    IF ( err/= 0) PRINT *, "tHrDay: Allocation request denied"
    tHrDay=PACK(tHr, mask=SdMask)


    ALLOCATE(selX(lenDay), stat=err)
    IF ( err/= 0) PRINT *, "selX: Allocation request denied"

    

    selX=PACK(Sd, mask=SdMask)
    ASd=(MAXVAL(selX)-MINVAL(selX))/2
    mSd=SUM(selX)/lenDay
    tSd=12
    CALL AnOHM_ShapeFit(tHrDay,selX,ASd,mSd,tSd)

    
    
    
    





    


    
    IF ( mSd+ASd<MAXVAL(selX) ) THEN



       mSd=MAXVAL(selX)-ASd


    END IF

    

    selX=PACK(Ta, mask=SdMask)
    ATa=(MAXVAL(selX)-MINVAL(selX))/2
    mTa=SUM(selX)/lenDay
    tTa=12
    CALL AnOHM_ShapeFit(tHrDay,selX,ATa,mTa,tTa)

    IF ( mTa < 60 ) mTa = mTa+C2K 
    
    IF ( ATa < 0 ) THEN
       
       
       CALL r8vec_print(lenDay,selX,'Ta Day:')
       PRINT*, 'ATa:', ATa
       PRINT*, 'mTa:', mTa
       PRINT*, 'tTa:', tTa
    END IF



    
    tau = (tTa-tSd)/24*2*PI


    
    selX=PACK(WS, mask=SdMask)
    mWS = SUM(selX)/lenDay  

    selX=PACK(WF, mask=SdMask)
    mWF = SUM(selX)/lenDay  

    selX=PACK(AH, mask=SdMask)
    mAH = SUM(selX)/lenDay  



    IF (ALLOCATED(SdMask)) DEALLOCATE(SdMask, stat=err)
    IF ( err/= 0) PRINT *, "SdMask: Deallocation request denied"

    IF (ALLOCATED(tHrDay)) DEALLOCATE(tHrDay, stat=err)
    IF ( err/= 0) PRINT *, "tHrDay: Deallocation request denied"

    IF (ALLOCATED(selX)) DEALLOCATE(selX, stat=err)
    IF ( err/= 0) PRINT *, "selX: Deallocation request denied"

  END SUBROUTINE AnOHM_FcCal
  

  
  
  
  SUBROUTINE AnOHM_ShapeFit(&
       tHr,obs,&      
       amp,mean,tpeak)
    IMPLICIT NONE

    
    REAL(KIND(1d0)),INTENT(in) :: tHr(:)  
    REAL(KIND(1d0)),INTENT(in) :: obs(:)  

    
    REAL(KIND(1d0)),INTENT(out) :: amp     
    REAL(KIND(1d0)),INTENT(out) :: mean    
    REAL(KIND(1d0)),INTENT(out) :: tpeak   

    INTEGER :: m,n,info,err         

    
    REAL ( KIND(1d0) ),ALLOCATABLE:: fvec(:),x(:)

    REAL ( KIND(1d0) ):: tol= 0.00001D+00 

    n=3 
    m=SIZE(tHr, dim=1) 



    ALLOCATE(fvec(m), stat=err)
    IF ( err/= 0) PRINT *, "fvec: Allocation request denied"

    ALLOCATE(x(n), stat=err)
    IF ( err/= 0) PRINT *, "x: Allocation request denied"


    
    x=(/mean,amp,tpeak/)



    CALL lmdif1( fSin, m, n, x, tHr, obs, fvec, tol, info )


    mean  = x(1)
    amp   = x(2)
    tpeak = x(3)+6 

    
    
    IF ( amp<0 ) THEN


       


       amp=ABS(amp)
       tpeak=x(3)-12+6+24
       tpeak=MOD(tpeak,24.)

       
    END IF
    tpeak=MOD(tpeak,24.)

  END SUBROUTINE AnOHM_ShapeFit
  

  
  
  
  
  SUBROUTINE fSin(m, n, x, xdat, ydat, fvec, iflag)

    IMPLICIT NONE
    INTEGER ( kind = 4 ) m
    INTEGER ( kind = 4 ) n

    
    REAL ( kind = 8 ) fvec(m),& 
         xdat(m),ydat(m) 

    INTEGER ( kind = 4 ) iflag,i
    REAL ( kind = 8 ) x(n)
    REAL(KIND(1d0)), PARAMETER :: PI  = ATAN(1.0)*4      

    IF ( iflag == 0 ) THEN

       WRITE ( *, '(a)' ) ''
       DO i = 1, n
          WRITE ( *, '(g14.6)' ) x(i)
       END DO

    ELSE  IF ( iflag == 1 ) THEN
       fvec(1:m) = x(1) + x(2) * SIN(2*PI/24*(xdat(1:m)-x(3))) - ydat(1:m)

    END IF
    RETURN

  END SUBROUTINE fSin
  

  
  
  SUBROUTINE AnOHM_Bo_cal(&
       sfc_typ,& 
       Sd,Ta,RH,pres,tHr,              & 
       ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,& 
       xalb,xemis,xcp,xk,xch,xSM,      & 
       tSd,                            & 
       xBo)                              


    IMPLICIT NONE

    
    INTEGER, INTENT(in) :: sfc_typ 

    
    REAL(kind = 8), INTENT(in), DIMENSION(:) ::Sd  
    REAL(kind = 8), INTENT(in), DIMENSION(:) ::Ta  
    REAL(kind = 8), INTENT(in), DIMENSION(:) ::RH  
    REAL(kind = 8), INTENT(in), DIMENSION(:) ::pres
    REAL(kind = 8), INTENT(in), DIMENSION(:) ::tHr 

    
    REAL(KIND(1d0)),INTENT(in):: ASd 
    REAL(KIND(1d0)),INTENT(in):: mSd 
    REAL(KIND(1d0)),INTENT(in):: tSd 
    REAL(KIND(1d0)),INTENT(in):: ATa 
    REAL(KIND(1d0)),INTENT(in):: mTa 
    REAL(KIND(1d0)),INTENT(in):: tau 
    REAL(KIND(1d0)),INTENT(in):: mWS 
    REAL(KIND(1d0)),INTENT(in):: mWF 
    REAL(KIND(1d0)),INTENT(in):: mAH 

    
    REAL(KIND(1d0)),INTENT(in) :: xalb  
    REAL(KIND(1d0)),INTENT(in) :: xemis 
    REAL(KIND(1d0)),INTENT(in) :: xcp   
    REAL(KIND(1d0)),INTENT(in) :: xk    
    REAL(KIND(1d0)),INTENT(in) :: xch   
    REAL(KIND(1d0)),INTENT(in) :: xSM   

    
    REAL(kind=8), INTENT(out) :: xBo 

    

    REAL(kind=8), ALLOCATABLE :: x(:),fvec(:),prms(:)

    INTEGER :: lenDay,n,m,info,err,nVar,nPrm
    LOGICAL, DIMENSION(:), ALLOCATABLE :: maskDay
    REAL(kind=8) :: tol=1E-20

    

    
    ALLOCATE(maskDay(SIZE(sd)),stat=err)
    maskDay=sd>5
    
    lenDay=SIZE(PACK(sd, mask=maskDay), dim=1)
    
    
    
    


    
    n=1
    ALLOCATE(x(n), stat=err)
    IF ( err/= 0) PRINT *, "x: Allocation request denied"
    ALLOCATE(fvec(n), stat=err)
    IF ( err/= 0) PRINT *, "fvec: Allocation request denied"

    
    xBo=10
    x(1)= xBo

    
    
    nPrm=16
    
    nVar=5
    
    m=nPrm+nVar*lenDay

    ALLOCATE(prms(m), stat=err)
    IF ( err/= 0) PRINT *, "prms: Allocation request denied"

    
    prms(1)=ASd
    prms(2)=mSd
    prms(3)=ATa
    prms(4)=mTa
    prms(5)=tau
    prms(6)=mWS
    prms(7)=mWF
    prms(8)=mAH

    
    prms(9)=xalb
    prms(10)=xemis
    prms(11)=xcp
    prms(12)=xk
    prms(13)=xch
    prms(14)=xSM

    
    prms(15)=tSd

    
    prms(16)=sfc_typ*1.0


    
    prms(nPrm+1:m)=PACK((/Sd,Ta,RH,pres,tHr/), &
         mask=PACK(SPREAD(maskDay, dim=2, ncopies=nVar),.TRUE.))




    
    CALL hybrd1(fcnBo,n,x,fvec,tol,info,m,prms)
    xBo=x(1)



    IF (ALLOCATED(x)) DEALLOCATE(x, stat=err)
    IF ( err/= 0) PRINT *, "x: Deallocation request denied"
    IF (ALLOCATED(fvec)) DEALLOCATE(fvec, stat=err)
    IF ( err/= 0) PRINT *, "fvec: Deallocation request denied"
    IF (ALLOCATED(prms)) DEALLOCATE(prms, stat=err)
    IF ( err/= 0) PRINT *, "prms: Deallocation request denied"

  END SUBROUTINE AnOHM_Bo_cal
  

  
  
  SUBROUTINE fcnBo( n, x, fvec, iflag, m, prms )

    IMPLICIT NONE
    
    
    
    INTEGER ( kind = 4 ):: n
    INTEGER ( kind = 4 ):: m
    INTEGER ( kind = 4 ):: iflag
    REAL ( kind = 8 ):: fvec(n)
    REAL ( kind = 8 ):: x(n) 
    REAL ( kind = 8 ):: prms(m) 

    
    REAL(kind = 8):: xBo

    
    REAL(kind = 8):: ASd
    REAL(kind = 8):: mSd
    REAL(kind = 8):: ATa
    REAL(kind = 8):: mTa
    REAL(kind = 8):: tau
    REAL(kind = 8):: mWS
    REAL(kind = 8):: mWF
    REAL(kind = 8):: mAH

    
    REAL(kind = 8):: xalb
    REAL(kind = 8):: xemis
    REAL(kind = 8):: xcp
    REAL(kind = 8):: xk
    REAL(kind = 8):: xch

    
    REAL(kind = 8)::tSd

    
    REAL(kind = 8)::xSM

    
    INTEGER :: sfc_typ


    
    REAL(kind=8), DIMENSION(:,:),ALLOCATABLE :: dayArray

    
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: Sd  
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: Ta  
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: RH  
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: pres
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: tHr 
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: Ts  
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: qa  
    REAL(kind = 8), DIMENSION(:),ALLOCATABLE :: qs  

    
    REAL(kind = 8),PARAMETER:: C2K=273.15 

    
    REAL(kind = 8),PARAMETER:: cp_air = 1006.38    
    REAL(kind = 8),PARAMETER:: Lv_air = 2264.705E3 

    INTEGER :: lenDay,i,err,nVar,nPrm


    IF ( iflag == 0 ) THEN
       

       WRITE ( *, '(a)' ) ''
       CALL r8vec_print(n,x,'x in fcnBo')

    ELSE  IF ( iflag == 1 ) THEN
       
       
       xBo=x(1)

       
       ASd = prms(1)
       mSd = prms(2)
       ATa = prms(3)
       mTa = prms(4)
       tau = prms(5)
       mWS = prms(6)
       mWF = prms(7)
       mAH = prms(8)

       
       xalb  = prms(9)
       xemis = prms(10)
       xcp   = prms(11)
       xk    = prms(12)
       xch   = prms(13)
       xSM   = MIN(prms(14),1.0)

       
       tSd=prms(15)

       
       sfc_typ=INT(prms(16))

       
       nPrm=16

       
       nVar=5
       
       lenDay=(m-nPrm)/nVar
       
       ALLOCATE(dayArray(nVar,lenDay), stat=err)
       IF ( err/= 0) PRINT *, "dayArray: Allocation request denied"
       dayArray=RESHAPE(prms(nPrm+1:SIZE(prms)), shape=(/nVar,lenDay/),order=(/2,1/))

       
       
       ALLOCATE(Sd(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "Sd: Allocation request denied"
       Sd(:)=dayArray(1,:)
       
       ALLOCATE(Ta(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "Ta: Allocation request denied"
       Ta(:)=dayArray(2,:)
       
       ALLOCATE(RH(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "RH: Allocation request denied"
       RH(:)=dayArray(3,:)
       
       ALLOCATE(pres(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "pres: Allocation request denied"
       pres(:)=dayArray(4,:)
       
       ALLOCATE(tHr(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "tHr: Allocation request denied"
       tHr(:)=dayArray(5,:)








       ALLOCATE(Ts(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "Ts: Allocation request denied"
       ALLOCATE(qs(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "qs: Allocation request denied"
       ALLOCATE(qa(lenDay), stat=err)
       IF ( err/= 0) PRINT *, "qa: Allocation request denied"


       
       IF ( xSM==0 ) THEN
          xBo=1000 
       ELSE

          DO i = 1, lenDay, 1
             
             CALL AnOHM_xTs(&
                  sfc_typ,&
                  ASd,mSd,ATa,mTa,tau,mWS,mWF,mAH,&   
                  xalb,xemis,xcp,xk,xch,xBo,&   
                  tSd,& 
                  tHr(i),&
                  Ts(i))

             
             Ts(i)=MIN(Ts(i)-C2K,-40.)

             
             qs(i)=qsat_fn(Ts(i),pres(i))

             
             qa(i)=qa_fn(Ta(i),RH(i),pres(i))












          END DO

          
          
          






          xBo = SUM(cp_air*(Ts-Ta))/& 
               SUM(xSM*Lv_air*(qs-qa))

       END IF

       
       fvec(1)=x(1)-xBo

       
       IF (ALLOCATED(dayArray)) DEALLOCATE(dayArray, stat=err)
       IF ( err/= 0) PRINT *, "dayArray: Deallocation request denied"
       IF (ALLOCATED(Sd)) DEALLOCATE(Sd, stat=err)
       IF ( err/= 0) PRINT *, "Sd: Deallocation request denied"
       IF (ALLOCATED(Ta)) DEALLOCATE(Ta, stat=err)
       IF ( err/= 0) PRINT *, "Ta: Deallocation request denied"
       IF (ALLOCATED(RH)) DEALLOCATE(RH, stat=err)
       IF ( err/= 0) PRINT *, "RH: Deallocation request denied"
       IF (ALLOCATED(pres)) DEALLOCATE(pres, stat=err)
       IF ( err/= 0) PRINT *, "pres: Deallocation request denied"
       IF (ALLOCATED(tHr)) DEALLOCATE(tHr, stat=err)
       IF ( err/= 0) PRINT *, "tHr: Deallocation request denied"
       IF (ALLOCATED(Ts)) DEALLOCATE(Ts, stat=err)
       IF ( err/= 0) PRINT *, "Ts: Deallocation request denied"
       IF (ALLOCATED(qa)) DEALLOCATE(qa, stat=err)
       IF ( err/= 0) PRINT *, "qa: Deallocation request denied"
       IF (ALLOCATED(qs)) DEALLOCATE(qs, stat=err)
       IF ( err/= 0) PRINT *, "qs: Deallocation request denied"

    END IF
    RETURN

  END SUBROUTINE fcnBo
  

  
  
  
  
  FUNCTION esat_fn(Ta) RESULT(esat)
    REAL (KIND(1D0))::Ta  
    REAL (KIND(1D0))::esat  

    REAL (KIND(1D0)),PARAMETER::A=6.106 
    REAL (KIND(1D0)),PARAMETER::B=17.27 
    REAL (KIND(1D0)),PARAMETER::C=237.3 

    esat = A*EXP(B*Ta/(C+Ta))
  END FUNCTION esat_fn
  

  
  
  
  
  FUNCTION qsat_fn(Ta,pres) RESULT(qsat)
    REAL (KIND(1D0))::Ta  
    REAL (KIND(1D0))::es  
    REAL (KIND(1D0))::qsat
    REAL (KIND(1D0))::pres

    REAL (KIND(1D0)),PARAMETER::molar         = 0.028965  
    REAL (KIND(1D0)),PARAMETER::molar_wat_vap = 0.0180153 

    es = esat_fn(Ta)
    qsat = (molar_wat_vap/molar)*es/pres
  END FUNCTION qsat_fn
  

  
  
  
  FUNCTION qa_fn(Ta,RH,pres) RESULT(qa)

    REAL (KIND(1D0))::Ta  
    REAL (KIND(1D0))::RH  
    REAL (KIND(1D0))::ea  
    REAL (KIND(1D0))::es  
    REAL (KIND(1D0))::pres
    REAL (KIND(1D0))::qa  

    REAL (KIND(1D0)),PARAMETER::molar         = 0.028965  
    REAL (KIND(1D0)),PARAMETER::molar_wat_vap = 0.0180153 

    es = esat_fn(Ta)
    ea = es*RH/100
    qa = (molar_wat_vap/molar)*ea/pres
  END FUNCTION qa_fn
  

END MODULE AnOHM_module
