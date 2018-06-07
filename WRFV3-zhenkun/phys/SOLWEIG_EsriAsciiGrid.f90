 
 
 
 
 subroutine LoadEsriAsciiGrid(GridPath,GridName,xllcornerlo,yllcornerlo,cellsizelo,NoDatalo)
    use matsize
    
    implicit none
    real(kind(1d0))                   :: xllcornerlo,yllcornerlo,cellsizelo,NoDatalo
    integer                           :: col,row
    character(len=100)                :: GridPath,GridName,GridFile,n
    
    
    
    


    GridFile=trim(GridPath)//trim(GridName)
    open(99,File=GridFile,status='old')
    
	
    read(99,*) n,sizex
    read(99,*) n,sizey
    read(99,*) n,xllcornerlo
    read(99,*) n,yllcornerlo
    read(99,*) n,cellsizelo
    read(99,*) n,NoDatalo

    allocate(tempgrid(sizex,sizey))     
    
	
    do row=1,sizey
       read(99,*) (tempgrid(row,col),col=1,sizex)
    end do
    close(99)

    return
    end subroutine LoadEsriAsciiGrid
    
    
    
    subroutine SaveEsriAsciiGrid(GridPath,GridName,xllcornerlo,yllcornerlo,cellsizelo,NoDatalo)
    use matsize
    
    implicit none
    real(kind(1d0))                   :: xllcornerlo,yllcornerlo,cellsizelo,NoDatalo
    integer                           :: col,row
    character(len=100)                :: GridPath,GridName,GridFile
    
    
   
    
    


    GridFile=trim(GridPath)//trim(GridName)
    open(94,File=GridFile,status='unknown') 
    
    
    write(94,"(A5,1x,I0)") 'ncols',sizex
    write(94,"(A5,1x,I0)") 'nrows',sizey
    write(94,"(A9,1x,F0.2)") 'xllcorner',xllcornerlo
    write(94,"(A9,1x,F0.2)") 'yllcorner',yllcornerlo
    write(94,"(A8,1x,F0.2)") 'cellsize',cellsizelo
    write(94,"(A12,1x,F0.2)") 'NODATA_value',NoDatalo

	
    do row=1,sizey
        write(94,100) (savegrid(row,col),col=1,sizex)
    end do
    close(94)
100 format(200(f6.2,1x))
 
    return
 end subroutine SaveEsriAsciiGrid
