
 module matsize
    
    IMPLICIT NONE
    
    integer    :: sizex,sizey 
    real(kind(1d0)), allocatable, dimension(:,:)  :: a,sh,vbshvegsh,vegsh
    real(kind(1d0)), allocatable, dimension(:,:)  :: bush,vegdem,vegdem2,tempgrid
    real(kind(1d0)), allocatable, dimension(:,:)  :: buildings,svf,svfE,svfS,svfW,svfN
    real(kind(1d0)), allocatable, dimension(:,:)  :: svfveg,svfEveg,svfSveg,svfWveg,svfNveg
    real(kind(1d0)), allocatable, dimension(:,:)  :: svfaveg,svfEaveg,svfSaveg,svfWaveg,svfNaveg,last
    real(kind(1d0)), allocatable, dimension(:,:)  :: Kdown2d,Keast,Knorth,Ksouth,Kup2d,Kwest
    real(kind(1d0)), allocatable, dimension(:,:)  :: Ldown2d,Least,Lnorth,Lsouth,Lup2d,Lwest
    real(kind(1d0)), allocatable, dimension(:,:)  :: gvf,Tmrt,shadow,Sstr,F_sh,sunwall
    real(kind(1d0)), allocatable, dimension(:,:)  :: svfalfa,sos,Tgmap1
    real(kind(1d0)), allocatable, dimension(:,:)  :: viktveg,viktsky,viktrefl,viktwall,savegrid
    
 end module matsize









    
module solweig_module
    IMPLICIT NONE
    
    real(kind(1d0)) :: timestepdec,& 
                       CIlatenight,&
                       timeadd,&
                       firstdaytime,& 
                       Fside,& 
                       Fup,& 
                       scale,&
                       amaxvalue,&
                       trans,&
                       transperLAI,&
                       xllcorner,&
                       yllcorner,&
                       NoData,&
                       cellsize
    integer         :: SolweigCount
    
end module solweig_module


