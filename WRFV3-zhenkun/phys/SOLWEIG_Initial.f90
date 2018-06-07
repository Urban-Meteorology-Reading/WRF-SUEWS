 
 
 subroutine SOLWEIG_Initial
 

  use matsize         
  use InitialCond
  use allocateArray
  use data_in
  use sues_data
  use defaultNotUsed
  use InitialCond
  use solweig_module
  use time

  implicit none
    
 character(len=100)  :: Path,GridFile,GridFolder
 real(kind(1d0))                 :: vegmax
 character(len=100),dimension(5) :: svfname
 character(len=100),dimension(10):: svfvegname
 logical                         :: exist
 integer                         :: firstday

namelist/SOLWEIGinput/Posture,&    
    absL,&            
    absK,&            
    heightgravity,&   
    usevegdem,&       
    DSMPath,&         
    DSMname,&         
    CDSMname,&        
    TDSMname,&        
    TransMin,&        
    TransMax,&        
    SVFPath,&         
    SVFsuffix,&       
    buildingsname,&   
    row,&             
    col,&             
    onlyglobal,&      
    SOLWEIGpoi_out,&  
    Tmrt_out,&        
    Lup2d_out,&       
    Ldown2d_out,&     
    Kup2d_out,&       
    Kdown2d_out,&     
    GVF_out,&         
    SOLWEIG_ldown,&   
    OutInterval,&     
    RunForGrid        


  
    open(52,file=trim(FileInputPath)//'SOLWEIGinput.nml',err=274,status='old')
    read(52,nml=SOLWEIGinput)
    close(52)
    
    SolweigCount=1
    
    if (OutInterval == 60) then
        OutInterval=0
    endif
    
    if (Posture==1) then
        Fside=0.22
        Fup=0.06
    else
        Fside=0.1666666667
        Fup=0.166666667
    endif
   
    
    timestepdec=real(OutInterval)/(real(t_interval)*24.)
 
	
    Path=trim(FileInputPath)//trim(DSMPath)
    call LoadEsriAsciiGrid(Path,DSMName,xllcorner,yllcorner,cellsize,NoData)
    allocate(a(sizey,sizex))
    a=tempgrid
    deallocate(tempgrid)
    
    scale=1/cellsize
    
    GridFolder=trim(FileOutputPath)//'Grids'
    
    
    
    
    

    
    
    
    
    if (usevegdem==1) then 
        
        transperLAI=(TransMax-TransMin)/(LAImax(2)-LAImin(2))
        firstday = int(MetForcingData(1,2,1))
        trans=TransMin+(LAImax(2)-LAI(firstday-1,2))*transperLAI
               	
	    
        Path=trim(FileInputPath)//trim(DSMPath)
        call LoadEsriAsciiGrid(Path,CDSMname,xllcorner,yllcorner,cellsize,NoData)
        allocate(vegdem(sizey,sizex))
        vegdem=tempgrid
        deallocate(tempgrid)

        
        Path=trim(FileInputPath)//trim(DSMPath)
        call LoadEsriAsciiGrid(Path,TDSMname,xllcorner,yllcorner,cellsize,NoData)
        allocate(vegdem2(sizey,sizex))
        vegdem2=tempgrid
        deallocate(tempgrid)        
        
    	
        vegmax=maxval(vegdem)
        amaxvalue=maxval(a)-minval(a)
        amaxvalue=max(amaxvalue,vegmax)
    
    	
        vegdem=vegdem+a
        where (vegdem==a)
            vegdem=0.0
        end where
        vegdem2=vegdem2+a;
        where (vegdem2==a)
            vegdem2=0.0
        end where
        
        allocate(bush(sizex,sizey))
        where ((vegdem>0) .and. (vegdem2==0))
            bush=vegdem
        elsewhere
            bush=0.0
        end where
    else
        trans=1.00;
    endif    
    
    
    Path=trim(FileInputPath)//trim(SVFPath)//trim(SVFsuffix)
    svfname=(/'svf.asc ','svfE.asc','svfN.asc','svfW.asc','svfS.asc'/)
    svfvegname=(/'svfveg.asc  ','svfEveg.asc ','svfNveg.asc ','svfWveg.asc ','svfSveg.asc ',&
        'svfaveg.asc ','svfEaveg.asc','svfNaveg.asc','svfWaveg.asc','svfSaveg.asc'/)
    
    call LoadEsriAsciiGrid(Path,svfname(1),xllcorner,yllcorner,cellsize,NoData)
    allocate(svf(sizey,sizex)); svf=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfname(2),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfE(sizey,sizex)); svfE=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfname(3),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfN(sizey,sizex)); svfN=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfname(4),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfW(sizey,sizex)); svfW=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfname(5),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfS(sizey,sizex)); svfS=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(1),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfveg(sizey,sizex)); svfveg=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(2),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfEveg(sizey,sizex)); svfEveg=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfvegname(3),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfNveg(sizey,sizex)); svfNveg=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfvegname(4),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfWveg(sizey,sizex)); svfWveg=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(5),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfSveg(sizey,sizex)); svfSveg=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(6),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfaveg(sizey,sizex)); svfaveg=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(7),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfEaveg(sizey,sizex)); svfEaveg=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfvegname(8),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfNaveg(sizey,sizex)) ; svfNaveg=tempgrid; deallocate(tempgrid)        
    call LoadEsriAsciiGrid(Path,svfvegname(9),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfWaveg(sizey,sizex)); svfWaveg=tempgrid; deallocate(tempgrid)
    call LoadEsriAsciiGrid(Path,svfvegname(10),xllcorner,yllcorner,cellsize,NoData)
    allocate(svfSaveg(sizey,sizex)); svfSaveg=tempgrid; deallocate(tempgrid)
    
    
    Path=trim(FileInputPath)//trim(DSMPath)
    GridFile=trim(Path)//trim(buildingsname)
    inquire(file=GridFile, exist=exist)
    if (exist) then
        call LoadEsriAsciiGrid(Path,buildingsname,xllcorner,yllcorner,cellsize,NoData)
        allocate(buildings(sizey,sizex))
        buildings=tempgrid
        deallocate(tempgrid)
    else
        
    endif
    
    
    
    timeadd=0.00 
    
    
    allocate(Tgmap1(sizey,sizex))
    Tgmap1=0.0
    
    return
    
274 call ErrorHint(40,trim("SOLWEIGinput.nml FileCode is missing"),notUsed,notUsed,notUsedI) 
    
end subroutine SOLWEIG_Initial
