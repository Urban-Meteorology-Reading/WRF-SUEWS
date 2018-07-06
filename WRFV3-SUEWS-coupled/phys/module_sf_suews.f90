





MODULE module_sf_SUEWS
  USE SuMin_Module,ONLY:SuMin,nsurf,nvegsurf

CONTAINS

  
  SUBROUTINE suewsdrv(year, day, hour, minute, second, xtime, OHMcoef, &
       T3D, QV3D, P3D, U3D, V3D, DZ3d, SWDOWN,  &
       PSFC, PREC, NLCAT, LANDUSEF, ht,         &
       HFX, QFX, LH, TSK, QSFC, chklowq,        &
       qn1_av_SUEWS,   &
       LAI_SUEWS      ,&
       albDecTr_SUEWS ,&
       albEveTr_SUEWS ,&
       albGrass_SUEWS ,&
       DecidCap_SUEWS ,&
       porosity_SUEWS ,&
       GDD_SUEWS      ,&
       HDD_SUEWS      ,&
       HDD_PREV_SUEWS ,&
       state_SUEWS,&
       soilmoist_SUEWS,&
       surf_var_SUEWS,&
       dqndt_SUEWS,&
       xlong, xlat, DT, DT_PREV, DX,            &
       ids, ide, jds, jde, kds, kde,            &
       ims, ime, jms, jme, kms, kme,            &
       its, ite, jts, jte, kts, kte)
    
    
    
    
    
    
    

    
    IMPLICIT NONE
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    INTEGER, INTENT(IN)    ::     year, day, hour, minute, second
    REAL, INTENT(IN   )    ::     xtime

    REAL, DIMENSION( 3 ) , INTENT(IN) :: OHMcoef
    REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)   ::  &
         QV3D, P3D, T3D, U3D, V3D, DZ3D
    
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)    ::  SWDOWN, PSFC, PREC, ht

    INTEGER, INTENT(IN)  :: NLCAT
    REAL, DIMENSION(ims:ime, NLCAT, jms:jme), INTENT(IN)    :: LANDUSEF
    REAL,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) ::   &
         HFX, QFX, LH, TSK, QSFC, chklowq
    REAL , INTENT(IN)    :: DT, dt_prev, DX
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)    ::  XLONG, XLAT
    INTEGER, INTENT(IN)    ::     ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte

    
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: qn1_av_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,nvegsurf),INTENT(INOUT)          :: LAI_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: albDecTr_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: albEveTr_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: albGrass_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: DecidCap_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: porosity_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme,5),INTENT(INOUT)                 :: GDD_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,6),INTENT(INOUT)                 :: HDD_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,6),INTENT(INOUT)                 :: HDD_PREV_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,nsurf),INTENT(INOUT)             :: state_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,nsurf),INTENT(INOUT)             :: soilmoist_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,nsurf),INTENT(INOUT)             :: surf_var_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)                   :: dqndt_SUEWS

    
    REAL, DIMENSION(ims:ime, nsurf, jms:jme)    :: landusef_suews
    REAL, DIMENSION(nsurf) :: landusef_suews1d
    REAL :: QV1D, P1D, T1D, U1D, V1D, DZ1D
    REAL :: SWDOWN1D, PSFC1D, PREC1D, ht1d, XLONG1D, XLAT1D
    REAL(KIND(1d0)) :: qh_out, qe_out, qsfc_out, tsk_out, CHKLOWQ_out
    REAL(KIND(1d0)) :: timezone

    REAL(KIND(1d0)),DIMENSION(nsurf)                 :: state_id
    REAL(KIND(1d0)),DIMENSION(nsurf)                 :: soilmoist_id
    REAL(KIND(1d0)),DIMENSION(nsurf)                 :: surf_var_id
    REAL(KIND(1d0)),DIMENSION(5)                     :: GDD_id      
    REAL(KIND(1d0)),DIMENSION(6)                     :: HDD_id      
    REAL(KIND(1d0)),DIMENSION(6)                     :: HDD_id_prev 
    REAL(KIND(1d0)),DIMENSION(nvegsurf)              :: LAI_id      
    REAL(KIND(1d0))                                  :: DecidCap_id
    REAL(KIND(1d0))                                  :: albDecTr_id
    REAL(KIND(1d0))                                  :: albEveTr_id
    REAL(KIND(1d0))                                  :: albGrass_id
    REAL(KIND(1d0))                                  :: porosity_id
    REAL(KIND(1d0))                                  :: qn1_av_id
    REAL(KIND(1d0))                                  :: dqndt_id

    INTEGER :: dt_since_start 

    INTEGER ::  I,J,K,l

    dt_since_start = INT(xtime * 60)
    PRINT *, 'year = ', year, 'day = ', day, 'hour = ', hour, 'minute = ', minute
    PRINT *, 'minutesSinceStart = ', xtime, 'secondsSinceStart = ', dt_since_start
    PRINT *, 'dt = ', dt, 'dt_prev = ', dt_prev

    CALL MODIScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, landusef_suews)

    DO J=jts,jte

       DO I=its,ite

          T1D = T3D(i,1,j)
          QV1D = QV3D(i,1,j)
          P1D  = P3D(i,1,j)
          U1D = U3D(i,1,j)
          V1D = V3D(i,1,j)
          DZ1D = DZ3D(i,1,j)


          SWDOWN1D = SWDOWN(i,j)
          PSFC1D = PSFC(i,j)
          PREC1D = PREC(i,j)
          ht1d = ht(i,j)
          XLAT1D = XLAT(i,j)
          XLONG1D = XLONG(i,j)
          landusef_suews1d = landusef_suews(i, :, j)

          qn1_av_id       = qn1_av_SUEWS(I,J)
          LAI_id          = LAI_SUEWS(I,J,:)
          albDecTr_id     = albDecTr_SUEWS(I,J)
          albEveTr_id     = albEveTr_SUEWS(I,J)
          albGrass_id     = albGrass_SUEWS(I,J)
          DecidCap_id     = DecidCap_SUEWS(I,J)
          porosity_id     = porosity_SUEWS(I,J)
          GDD_id          = GDD_SUEWS(I,J,:)
          HDD_id          = HDD_SUEWS(I,J,:)
          HDD_id_prev     = HDD_PREV_SUEWS(I,J,:)
          state_id        = state_SUEWS(i,j,:)
          soilmoist_id    = soilmoist_SUEWS(i,j,:)
          surf_var_id     = surf_var_SUEWS(i,j,:)
          dqndt_id        = dqndt_SUEWS(i,j)

          
          
          
          

          timezone=0 


          CALL SUEWS1D(&
                                
               I,J,DT,DT_PREV,year,day,hour,minute,second,dt_since_start,timezone,&
               OHMcoef, &
                                
               SWDOWN1D,QV1D,U1D,V1D,T1D,PSFC1D,PREC1D,&
                                
               landusef_suews1d,ht1d,XLAT1D,XLONG1D,DZ1D,DX,&
                                
               LAI_id,albDecTr_id,albEveTr_id,albGrass_id,DecidCap_id,porosity_id,GDD_id,HDD_id,&
               HDD_id_prev,state_id,soilmoist_id,surf_var_id,dqndt_id,qn1_av_id,&
                                
               qh_out,qe_out,qsfc_out,tsk_out,CHKLOWQ_out,&
                                
               ids,ide, jds,jde, kds,kde,&
               ims,ime, jms,jme, kms,kme,&
               its,ite, jts,jte, kts,kte)

          qn1_av_SUEWS(I,J)      = qn1_av_id
          LAI_SUEWS(I,J,:)       = LAI_id
          albDecTr_SUEWS(I,J)    = albDecTr_id
          albEveTr_SUEWS(I,J)    = albEveTr_id
          albGrass_SUEWS(I,J)    = albGrass_id
          DecidCap_SUEWS(I,J)    = DecidCap_id
          porosity_SUEWS(I,J)    = porosity_id
          GDD_SUEWS(I,J,:)       = GDD_id
          HDD_SUEWS(I,J,:)       = HDD_id
          HDD_PREV_SUEWS(I,J,:)  = HDD_id_prev
          state_SUEWS(i,j,:)     = state_id
          soilmoist_SUEWS(i,j,:) = soilmoist_id
          surf_var_SUEWS(i,j,:)  = surf_var_id
          dqndt_SUEWS(i,j)       = dqndt_id

          tsk(i,j) = tsk_out
          
          PRINT *, 'qh_out = ', qh_out
          PRINT *, 'qe_out = ', qe_out
          
          
          
          HFX(I,J)=qh_out
          LH(I,J)=qe_out
          PRINT *, 'HFX(I,J) = ', HFX(I,J)
          PRINT *, 'LH(I,J) = ', LH(I,J)

          QFX(I,J)=qsfc_out
          chklowq(I,J)=CHKLOWQ_out

       ENDDO
    ENDDO

  END SUBROUTINE suewsdrv

  
  SUBROUTINE SUEWS1D(&
                                
       I,J,DT,DT_PREV,iy,id,it,imin,isec,dt_since_start,timezone,&
       OHMcoef, &
                                
       SWDOWN1D,QV1D,U1D,V1D,T1D,PSFC,PREC1D,&
                                
       landusef_suews,ht,XLAT,XLONG,dz8w,DX,&
                                
       LAI_id,albDecTr_id,albEveTr_id,albGrass_id,DecidCap_id,porosity_id,GDD_id,HDD_id,&
       HDD_id_prev,state_id,soilmoist_id,surf_var_id,dqndt_id,qn1_av_id,&
                                
       qh_out,qe_out,qsfc_out,tsk_out,CHKLOWQ_out,&
                                
       ids,ide, jds,jde, kds,kde,&
       ims,ime, jms,jme, kms,kme,&
       its,ite, jts,jte, kts,kte)
    
    IMPLICIT NONE

    
    
    INTEGER, INTENT(IN):: i, j
    REAL, INTENT(IN ) :: DT 
    REAL, INTENT(IN ) :: DT_PREV 
    

    INTEGER,INTENT(in)::iy 
    INTEGER,INTENT(in)::id 
    INTEGER,INTENT(in)::it 
    INTEGER,INTENT(in)::imin 
    INTEGER,INTENT(in)::isec 
    INTEGER,INTENT(in)::dt_since_start
    REAL(KIND(1d0))::timezone   

    REAL, DIMENSION( 3 ) , INTENT(IN) :: OHMcoef

    REAL,INTENT(in) :: SWDOWN1D
    REAL,INTENT(in) :: QV1D
    REAL,INTENT(in) :: U1D, V1D
    REAL,INTENT(in) :: T1D
    REAL,INTENT(in) :: PSFC 
    REAL,INTENT(in) :: PREC1D 

    REAL, DIMENSION(nsurf) ,INTENT(IN ) :: landusef_suews 
    REAL, INTENT(IN ) :: ht 
    REAL, INTENT(IN ) :: XLAT
    REAL, INTENT(IN ) :: XLONG
    REAL, INTENT(IN ) :: dz8w
    REAL, INTENT(IN ) :: DX 

    REAL(KIND(1d0)),INTENT(INOUT) :: DecidCap_id 
    REAL(KIND(1d0)),INTENT(INOUT) :: albDecTr_id 
    REAL(KIND(1d0)),INTENT(INOUT) :: albEveTr_id 
    REAL(KIND(1d0)),INTENT(INOUT) :: albGrass_id 
    REAL(KIND(1d0)),INTENT(INOUT) :: porosity_id 
    REAL(KIND(1d0)),DIMENSION(5),INTENT(INOUT)       :: GDD_id      
    REAL(KIND(1d0)),DIMENSION(6),INTENT(INOUT)       :: HDD_id      
    REAL(KIND(1d0)),DIMENSION(6),INTENT(INOUT)       :: HDD_id_prev 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),INTENT(INOUT):: LAI_id      
    
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(INOUT) :: state_id          
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(INOUT) :: soilmoist_id      
    REAL(KIND(1d0)),DIMENSION(nsurf),INTENT(INOUT) :: surf_var_id   
    REAL(KIND(1d0)),INTENT(INOUT) :: dqndt_id

    
    
    
    
    
    
    REAL(KIND(1d0)),INTENT(INOUT) ::qn1_av_id


    
    REAL(KIND(1d0)), INTENT(out) ::qh_out 
    REAL(KIND(1d0)), INTENT(out) ::qe_out 
    REAL(KIND(1d0)), INTENT(out) ::qsfc_out 
    REAL(KIND(1d0)), INTENT(out) ::tsk_out 
    REAL(KIND(1d0)), INTENT(out) ::CHKLOWQ_out 


    INTEGER, INTENT(IN)::     &
         ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte



    
    REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: SoilStoreCap=[150., 150., 150., 150., 150., 150., 0.]        

    REAL(KIND(1d0)),PARAMETER:: AlbMin_DecTr=0.12   
    REAL(KIND(1d0)),PARAMETER:: AlbMax_DecTr=0.18   
    REAL(KIND(1d0)),PARAMETER:: AlbMin_EveTr=0.11   
    REAL(KIND(1d0)),PARAMETER:: AlbMax_EveTr=0.12   
    REAL(KIND(1d0)),PARAMETER:: AlbMin_Grass=0.18   
    REAL(KIND(1d0)),PARAMETER:: AlbMax_Grass=0.21    

    REAL(KIND(1d0)),PARAMETER:: CapMin_dec=0.3   
    REAL(KIND(1d0)),PARAMETER:: CapMax_dec=0.8   
    REAL(KIND(1d0)),PARAMETER:: PorMin_dec=0.2   
    REAL(KIND(1d0)),PARAMETER:: PorMax_dec=0.6   

    REAL(KIND(1d0)),PARAMETER:: FAIbldg=0.                   
    REAL(KIND(1d0)),PARAMETER:: FAIEveTree=0.                
    REAL(KIND(1d0)),PARAMETER:: FAIDecTree=0.                

    REAL (KIND(1d0)),PARAMETER :: bldgH =10   
    REAL (KIND(1d0)),PARAMETER :: EveTreeH =10
    REAL (KIND(1d0)),PARAMETER :: DecTreeH =10

    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: BaseT          = [5,5,5]          
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: BaseTe         = [11,11,11]       
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: GDDFull        = [300,300,300]    
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: SDDFull        = [-450,-450,-450] 
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: LaiMin         = [4.,1.,1.6]      
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: LaiMax         = [5.1,5.5,5.9]    
    REAL(KIND(1d0)),DIMENSION(nvegsurf),PARAMETER:: MaxConductance = [7.4,11.7,30.1]  

    REAL(KIND(1d0)),DIMENSION(4,nvegsurf),PARAMETER:: LaiPower=RESHAPE(&       
         [[0.03,0.03,0.03],&
         [0.0005,0.0005,0.0005],&
         [0.03,0.03,0.03],&
         [0.0005,0.0005,0.0005]],&
         [4,nvegsurf])

    INTEGER,DIMENSION(nvegsurf),PARAMETER:: LAIType=1     
    INTEGER,PARAMETER::startDLS=85   
    INTEGER,PARAMETER::endDLS=302   

    REAL (KIND(1D0)),PARAMETER ::DRAINRT       = 0.25 
    REAL (KIND(1D0)),PARAMETER ::RAINCOVER     = 1
    REAL (KIND(1D0)),PARAMETER ::RAINMAXRES    = 10   
    REAL (KIND(1d0)),PARAMETER ::FlowChange    = 0    
    REAL (KIND(1d0)),PARAMETER ::PipeCapacity  = 100  
    REAL (KIND(1d0)),PARAMETER ::RunoffToWater = 0.1  

    REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: StateLimit=[0.48, 0.25, 1.3, 0.8, 1.9, 1.0, 30000.]     

    REAL(KIND(1d0)),DIMENSION(nsurf+1,nsurf-1),PARAMETER::WaterDist=& 
         RESHAPE(&
         [[0.,0.1,0.1,0.1,0.,0.],&
         [0.,0.,0.,0.,0.,0.],&
         [0.,0.,0.,0.,0.,0.],&
         [0.,0.,0.,0.,0.,0.],&
         [0.02,0.,0.,0.,0.,0.],&
         [0.,0.,0.,0.,0.,0.],&
         [0.,0.,0.,0.,0.,0.],&
         [0.98,0.9,0.9,0.9,1.,1.]],&
         [nsurf+1,nsurf-1])
    REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: WetThresh = [0.48, 0.25, 1.3, 0.8, 1.9, 1., 0.5]     

    
    
    
    
    
    
    REAL(KIND(1d0)),DIMENSION(5,nsurf),PARAMETER:: surf_attr=RESHAPE(&   
         [[ 0.48 ,  0.25 ,  1.3  ,  0.3  ,  1.9  ,  0.8  ,  0.5  ],&
         [ 3.   ,  3.   ,  2.   ,  2.   ,  2.   ,  3.   ,  0.   ],&
         [10.   , 10.   ,  0.013,  0.013,  0.013, 10.   ,  0.   ],&
         [ 3.   ,  3.   ,  1.71 ,  1.71 ,  1.71 ,  3.   ,  0.   ],&
         [ 0.48 ,  0.25 ,  1.3  ,  0.8  ,  1.9  ,  0.8  ,  0.5  ]],&
         [5,nsurf])


    
    
    REAL (KIND(1d0)),PARAMETER::th   = 40   
    REAL (KIND(1d0)),PARAMETER::tl   = -10  
    REAL (KIND(1d0)),PARAMETER::Kmax = 1200 
    REAL (KIND(1d0)),PARAMETER::g1   = 3.5  
    REAL (KIND(1d0)),PARAMETER::g2   = 200
    REAL (KIND(1d0)),PARAMETER::g3   = 0.1
    REAL (KIND(1d0)),PARAMETER::g4   = 0.7
    REAL (KIND(1d0)),PARAMETER::g5   = 30
    REAL (KIND(1d0)),PARAMETER::g6   = 0.05 
    REAL (KIND(1d0)),PARAMETER::s1   = 5.56
    REAL (KIND(1d0)),PARAMETER::s2   = 0    


    REAL(KIND(1d0)),DIMENSION(nsurf+1,4,3):: OHM_coef   


    REAL(KIND(1d0)),DIMENSION(nsurf+1):: OHM_threshSW = [10,10,10,10,10,10,10,10]         
    REAL(KIND(1d0)),DIMENSION(nsurf+1):: OHM_threshWD = [0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9] 

    REAL (KIND(1d0)),PARAMETER::  BaseTHDD=18.9  

    INTEGER,PARAMETER::OHMIncQF=0             

    
    
    
    REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: alb0=[0.12,0.15,0.1,0.18,0.138403,0.18,0.1]    
    REAL(KIND(1d0)),DIMENSION(nsurf),PARAMETER:: emis0=[0.95,0.91,0.98,0.98,0.988388,0.94,0.95]   
    REAL(KIND(1d0)),DIMENSION(nsurf)::alb,emis
    
    REAL(KIND(1d0)),DIMENSION(nsurf):: sfr   

    REAL (KIND(1d0)) :: alt         
    REAL (KIND(1d0)) :: lat         
    REAL (KIND(1d0)) :: lng         
    REAL (KIND(1d0)) :: z           
    REAL (KIND(1d0)) :: SurfaceArea 

    
    REAL (KIND(1d0))                :: avkdn     
    REAL (KIND(1d0))                :: avrh      
    REAL (KIND(1d0))                :: avu1      
    REAL (KIND(1d0))                :: Temp_C    
    REAL (KIND(1d0))                :: Press_hPa 
    REAL (KIND(1d0))                :: Precip    

    REAL(KIND(1d0))::dectime 

    INTEGER::tstep    
    INTEGER::tstep_prev    

    
    REAL(KIND(1d0)),DIMENSION(6,nsurf):: surf   
    
    
    
    
    
    
    

    PRINT*, 'OHMcoef = ', OHMcoef
    OHM_coef=RESHAPE(SPREAD(SPREAD(OHMcoef, 2, 4), dim=2, ncopies=8),&
         shape=(/8,4,3/), order=(/3,2,1/))

    
    surf(1:5,:)=surf_attr(:,:)
    surf(6,:)=surf_var_id(:)


    
    lat=XLAT
    lng=XLONG
    
    alt=ht

    
    z= 10 

    
    SurfaceArea=dx*dx

    
    avkdn=SWDOWN1D
    avu1=SQRT(U1D**2+V1D**2)
    Temp_C=T1D-273.15
    Precip=PREC1D

    
    Press_hPa=PSFC/100.

    
    avRh=q2rh(QV1D,T1D,REAL(Press_hPa))*100 
    avRh=MAX(5.,avRh)

    
    tstep=INT(DT)
    tstep_prev=INT(DT_PREV)


    
    
    alb=alb0
    emis=emis0

    dectime=id-1+(it+imin/60.)/24

    sfr=landusef_suews

    CALL SuMin(&
         dt_since_start, isec, &
         alb,albDecTr_id,albEveTr_id,albGrass_id,alBMax_DecTr,&
         alBMax_EveTr,alBMax_Grass,AlbMin_DecTr,AlbMin_EveTr,AlbMin_Grass,&
         alt,avkdn,avRh,avU1,BaseT,BaseTe,&
         BaseTHDD,bldgH,CapMax_dec,CapMin_dec,&
         DecidCap_id,dectime,DecTreeH,DRAINRT,&
         emis,endDLS,EveTreeH,FAIBldg,&
         FAIDecTree,FAIEveTree,FlowChange,&
         G1,G2,G3,G4,G5,G6,GDD_id,&
         GDDFull,HDD_id,HDD_id_prev,&
         id,imin,it,iy,Kmax,LAI_id,LAIMax,LAIMin,&
         LAIPower,LAIType,lat,lng,MaxConductance,&
         OHM_coef,OHMIncQF,OHM_threshSW,&
         OHM_threshWD,PipeCapacity,PorMax_dec,PorMin_dec,porosity_id,&
         Precip,Press_hPa,&
         qn1_av_id,dqndt_id,RAINCOVER,RainMaxRes,&
         RunoffToWater,S1,S2,&
         SDDFull,sfr,&
         soilmoist_id,soilstoreCap,startDLS,state_id,StateLimit,&
         surf,SurfaceArea,&
         Temp_C,TH,&
         timezone,TL,&
         tstep,tstep_prev,&
         WaterDist,WetThresh,&
         Z,&
         qh_out,qe_out,qsfc_out, tsk_out)

    CHKLOWQ_out = 0.02
    tsk_out=tsk_out+273.15

  END SUBROUTINE SUEWS1D

  
  SUBROUTINE suewsinit(TSK, nohm, OHMcoef,                     &
       qn1_av_SUEWS,   &
       LAI_SUEWS      ,&
       albDecTr_SUEWS ,&
       albEveTr_SUEWS ,&
       albGrass_SUEWS ,&
       DecidCap_SUEWS ,&
       porosity_SUEWS ,&
       GDD_SUEWS      ,&
       HDD_SUEWS      ,&
       HDD_PREV_SUEWS ,&
       state_SUEWS,&
       soilmoist_SUEWS,&
       surf_var_SUEWS,&
       dqndt_SUEWS,&
       restart, allowed_to_read,                &
       ids,ide, jds,jde, kds,kde,               &
       ims,ime, jms,jme, kms,kme,               &
       its,ite, jts,jte, kts,kte                )
    
    USE module_wrf_error
    IMPLICIT NONE
    
    INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte

    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         INTENT(IN)    ::                               TSK
    INTEGER                     :: nohm
    REAL,DIMENSION( nohm ),INTENT(INOUT)  :: OHMcoef
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: qn1_av_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,3),INTENT(INOUT)       :: LAI_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: albDecTr_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: albEveTr_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: albGrass_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: DecidCap_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: porosity_SUEWS 
    REAL,DIMENSION(ims:ime,jms:jme,5),INTENT(INOUT)       :: GDD_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,6),INTENT(INOUT)       :: HDD_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,6),INTENT(INOUT)       :: HDD_PREV_SUEWS      
    REAL,DIMENSION(ims:ime,jms:jme,7),INTENT(INOUT)       :: state_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,7),INTENT(INOUT)       :: soilmoist_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme,7),INTENT(INOUT)       :: surf_var_SUEWS
    REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT)         :: dqndt_SUEWS

    LOGICAL , INTENT(IN)        :: restart, allowed_to_read

    

    INTEGER                   ::      L,J,I,itf,jtf
    CHARACTER*1024 message
    INTEGER                   :: OHM_unit
    INTEGER , PARAMETER       :: OPEN_OK = 0
    INTEGER                   :: ierr, Code
    LOGICAL, EXTERNAL         :: wrf_dm_on_monitor

    
    IF(.not.restart)THEN

      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)

      DO j = jts,jtf
        DO i = its,itf
          qn1_av_SUEWS(i,j) = 10.
          LAI_SUEWS(i,j,:) = 2.
          albDecTr_SUEWS(i,j) = 0.2
          albEveTr_SUEWS(i,j) = 0.2
          albGrass_SUEWS(i,j) = 0.2
          DecidCap_SUEWS(i,j) = 5.
          porosity_SUEWS(i,j) = 0.5
          GDD_SUEWS(i,j,:) = 20.
          HDD_SUEWS(i,j,:) = 20.
          HDD_PREV_SUEWS(i,j,:) = 20.
          state_SUEWS(i,j,:) = 20.
          soilmoist_SUEWS(i,j,:) = 150.
          surf_var_SUEWS(i,j,:) = 10.
          dqndt_SUEWS(i,j) = 5.
        ENDDO
      ENDDO

      IF ( allowed_to_read ) THEN
         OHM_unit = 30
         IF ( wrf_dm_on_monitor() ) THEN
            OPEN(OHM_unit, FILE='SUEWS_OHMCoefficients.txt',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
            IF ( ierr .NE. OPEN_OK ) THEN
               WRITE(message,FMT='(A)') &
                    'module_sf_suews.F: suewsinit: open failure for SUEWS_OHMCoefficients.txt'
               CALL wrf_error_fatal3("<stdin>",686,&
message )
            END IF
            READ(OHM_unit, *)
            READ(OHM_unit, *)
            READ(OHM_unit, *)code, OHMcoef
            CLOSE (OHM_unit)
            CALL wrf_dm_bcast_real    ( OHMcoef , nohm )
         ENDIF
      ENDIF

   ENDIF

  END SUBROUTINE suewsinit
  

  
  
  
  
  SUBROUTINE USGScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, landusef_suews)

    IMPLICIT NONE

    
    
    
    INTEGER, INTENT(in)                ::  ims, ime, NLCAT, jms, jme
    REAL, INTENT(in)                   ::  landusef(ims:ime, NLCAT, jms:jme)
    REAL, INTENT(out)                  ::  landusef_suews(ims:ime, nsurf, jms:jme)

    
    
    
    INTEGER                            ::  i, j, k

    DO i = ims, ime
       DO j = jms, jme
          landusef_suews(i, :, j) = 0.
          DO k = 1, NLCAT

             SELECT CASE( k )
             CASE( 1 )
                landusef_suews(i, 1, j) = landusef_suews(i, 1, j) + landusef(i, k, j) * 0.5
                landusef_suews(i, 2, j) = landusef_suews(i, 2, j) + landusef(i, k, j) * 0.5
             CASE( 2:10 )
                landusef_suews(i, 5, j) = landusef_suews(i, 5, j) + landusef(i, k, j)
             CASE( 11, 12 )
                landusef_suews(i, 4, j) = landusef_suews(i, 4, j) + landusef(i, k, j)
             CASE( 13:15 )
                landusef_suews(i, 3, j) = landusef_suews(i, 3, j) + landusef(i, k, j)
             CASE( 16:18 )
                landusef_suews(i, 7, j) = landusef_suews(i, 7, j) + landusef(i, k, j)
             CASE( 19:23 )
                landusef_suews(i, 6, j) = landusef_suews(i, 6, j) + landusef(i, k, j)
             CASE( 24 )
                
             END SELECT

          END DO
       END DO
    END DO

  END SUBROUTINE USGScat2SUEWScat

  
  
  
  
  SUBROUTINE MODIScat2SUEWScat(ims, ime, NLCAT, jms, jme, landusef, landusef_suews)

    IMPLICIT NONE

    
    
    
    INTEGER, INTENT(in)                ::  ims, ime, NLCAT, jms, jme
    REAL, INTENT(in)                   ::  landusef(ims:ime, NLCAT, jms:jme)
    REAL, INTENT(out)                  ::  landusef_suews(ims:ime, nsurf, jms:jme)

    
    
    
    INTEGER                            ::  i, j, k

    DO i = ims, ime
       DO j = jms, jme
          landusef_suews(i, :, j) = 0.
          DO k = 1, NLCAT

             SELECT CASE( k )
             CASE( 1, 2, 5 )
                landusef_suews(i, 3, j) = landusef_suews(i, 3, j) + landusef(i, k, j)
             CASE( 3:4 )
                landusef_suews(i, 4, j) = landusef_suews(i, 4, j) + landusef(i, k, j)
             CASE( 6:10 )
                landusef_suews(i, 5, j) = landusef_suews(i, 5, j) + landusef(i, k, j)
             CASE( 11, 17 )
                landusef_suews(i, 7, j) = landusef_suews(i, 7, j) + landusef(i, k, j)
             CASE( 12, 14 )
                landusef_suews(i, 5, j) = landusef_suews(i, 5, j) + landusef(i, k, j)
             CASE( 13 )
                landusef_suews(i, 1, j) = landusef_suews(i, 1, j) + landusef(i, k, j) * 0.5
                landusef_suews(i, 2, j) = landusef_suews(i, 2, j) + landusef(i, k, j) * 0.5
             CASE( 15 )
                
             CASE( 16 )
                landusef_suews(i, 6, j) = landusef_suews(i, 6, j) + landusef(i, k, j)
             CASE( 18:20 )
                landusef_suews(i, 6, j) = landusef_suews(i, 6, j) + landusef(i, k, j)
             END SELECT

          END DO
       END DO
    END DO

  END SUBROUTINE MODIScat2SUEWScat

  REAL FUNCTION  q2rh(q, Ta, p)
    IMPLICIT NONE
    REAL, INTENT(in)  :: q, Ta ,p
    REAL :: e, es

    es=esat(Ta)
    e=(500*p*q)/(311+189*q)
    q2rh=e/es

  END FUNCTION q2rh


  REAL FUNCTION esat(T)
    IMPLICIT NONE
    REAL, INTENT(in):: T
    REAL :: a, b, c, d, est, f, h, Ts
    a=-7.90298
    b=5.02808
    c=-(1.3816/10**7)
    d=11.344
    est=1013.25
    f=8.1328/10**3
    h=-3.49149
    Ts=373.16

    esat=est*10**(a*(Ts/T-1) &
         + b*LOG10(Ts/T) &
         + c*(10**(d*(1-T/Ts))-1)+ f*(10**(h*(Ts/T-1))-1))

  END FUNCTION esat

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

END MODULE module_sf_suews
