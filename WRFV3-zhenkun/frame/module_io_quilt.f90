









MODULE module_wrf_quilt



















































  USE module_internal_header_util
  USE module_timing
  USE module_cpl, ONLY : coupler_on, cpl_set_dm_communicator, cpl_finalize

  INTEGER, PARAMETER :: int_num_handles = 99
  INTEGER, PARAMETER :: max_servers = int_num_handles+1  
  LOGICAL, DIMENSION(0:int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(0:int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER,SAVE :: int_local_output_buffer(:)
  INTEGER,      SAVE :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.
  INTEGER          :: prev_server_for_handle = -1
  INTEGER          :: server_for_handle(int_num_handles)
  INTEGER          :: reduced(2), reduced_dummy(2)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  INTEGER :: mpi_comm_avail,availrank
  LOGICAL :: in_avail=.false., poll_servers=.false.

  INTEGER nio_groups


  CONTAINS


    SUBROUTINE set_server_id ( dhandle, value )
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: dhandle, value
       IF ( dhandle .GE. 1 .AND. dhandle .LE. int_num_handles ) THEN
         server_for_handle(dhandle) = value
       ELSE
         CALL wrf_message('module_io_quilt: set_server_id bad dhandle' )
       ENDIF
    END SUBROUTINE set_server_id

    LOGICAL FUNCTION get_poll_servers() 
      implicit none
      get_poll_servers=poll_servers
    end FUNCTION get_poll_servers


    SUBROUTINE init_module_wrf_quilt
      USE module_wrf_error, only: init_module_wrf_error
      USE module_driver_constants








      RETURN
    END SUBROUTINE init_module_wrf_quilt




END MODULE module_wrf_quilt







SUBROUTINE disable_quilting




  USE module_wrf_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

SUBROUTINE quilting_disabled( reslt )




  USE module_wrf_quilt
  LOGICAL, INTENT(OUT) :: reslt
  reslt = disable_quilt
write(0,*)"module_io_quilt_old.F",2931,disable_quilt
  RETURN
END SUBROUTINE quilting_disabled

LOGICAL FUNCTION  use_output_servers_for(ioform)







  USE module_wrf_quilt
  integer, intent(in) :: ioform
  use_output_servers_for = quilting_enabled
  use_output_servers_for = ( use_output_servers_for .and. ioform<100 )
  RETURN
END FUNCTION use_output_servers_for

LOGICAL FUNCTION  use_output_servers()




  USE module_wrf_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()




  USE module_wrf_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

SUBROUTINE wrf_quilt_open_for_write_begin( FileName , gridid, Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )





  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_begin

SUBROUTINE wrf_quilt_open_for_write_commit( DataHandle , Status )







  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_commit

SUBROUTINE wrf_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )





  RETURN  
END SUBROUTINE wrf_quilt_open_for_read

SUBROUTINE wrf_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )





  RETURN
END SUBROUTINE wrf_quilt_inquire_opened

SUBROUTINE wrf_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )










  RETURN
END SUBROUTINE wrf_quilt_inquire_filename

SUBROUTINE wrf_quilt_iosync ( DataHandle, Status )



















  RETURN
END SUBROUTINE wrf_quilt_iosync

SUBROUTINE wrf_quilt_ioclose ( DataHandle, Status )







  RETURN
END SUBROUTINE wrf_quilt_ioclose

SUBROUTINE wrf_quilt_ioexit( Status )





  RETURN  
END SUBROUTINE wrf_quilt_ioexit

SUBROUTINE wrf_quilt_get_next_time ( DataHandle, DateStr, Status )





  RETURN
END SUBROUTINE wrf_quilt_get_next_time

SUBROUTINE wrf_quilt_get_previous_time ( DataHandle, DateStr, Status )





  RETURN
END SUBROUTINE wrf_quilt_get_previous_time

SUBROUTINE wrf_quilt_set_time ( DataHandle, Data,  Status )





RETURN
END SUBROUTINE wrf_quilt_set_time

SUBROUTINE wrf_quilt_get_next_var ( DataHandle, VarName, Status )






  RETURN
END SUBROUTINE wrf_quilt_get_next_var

SUBROUTINE wrf_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_real 

SUBROUTINE wrf_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )








RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_real 

SUBROUTINE wrf_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_double 

SUBROUTINE wrf_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_double 

SUBROUTINE wrf_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_integer 

SUBROUTINE wrf_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )








RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_integer 

SUBROUTINE wrf_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_logical 

SUBROUTINE wrf_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_logical 

SUBROUTINE wrf_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_char 

SUBROUTINE wrf_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )








RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_char 

SUBROUTINE wrf_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_td_real 

SUBROUTINE wrf_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_td_real 

SUBROUTINE wrf_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











  CALL wrf_error_fatal3("<stdin>",500,&
'wrf_quilt_get_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_double 

SUBROUTINE wrf_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )










  CALL wrf_error_fatal3("<stdin>",516,&
'wrf_quilt_put_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_double 

SUBROUTINE wrf_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_td_integer 

SUBROUTINE wrf_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_td_integer 

SUBROUTINE wrf_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_dom_td_logical 

SUBROUTINE wrf_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_td_logical 

SUBROUTINE wrf_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_get_dom_td_char 

SUBROUTINE wrf_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_dom_td_char 

SUBROUTINE wrf_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_var_ti_real 

SUBROUTINE wrf_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_ti_real 

SUBROUTINE wrf_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











  CALL wrf_error_fatal3("<stdin>",648,&
'wrf_quilt_get_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_double 

SUBROUTINE wrf_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )










  CALL wrf_error_fatal3("<stdin>",664,&
'wrf_quilt_put_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_double 

SUBROUTINE wrf_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_var_ti_integer 

SUBROUTINE wrf_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_ti_integer 

SUBROUTINE wrf_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_var_ti_logical 

SUBROUTINE wrf_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_ti_logical 

SUBROUTINE wrf_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_get_var_ti_char 

SUBROUTINE wrf_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )









RETURN
END SUBROUTINE wrf_quilt_put_var_ti_char 

SUBROUTINE wrf_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_var_td_real 

SUBROUTINE wrf_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_td_real 

SUBROUTINE wrf_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











  CALL wrf_error_fatal3("<stdin>",795,&
'wrf_quilt_get_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_td_double 

SUBROUTINE wrf_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










  CALL wrf_error_fatal3("<stdin>",811,&
'wrf_quilt_put_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_td_double 

SUBROUTINE wrf_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)











RETURN
END SUBROUTINE wrf_quilt_get_var_td_integer 

SUBROUTINE wrf_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_td_integer 

SUBROUTINE wrf_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )











RETURN
END SUBROUTINE wrf_quilt_get_var_td_logical 

SUBROUTINE wrf_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_td_logical 

SUBROUTINE wrf_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_get_var_td_char 

SUBROUTINE wrf_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )










RETURN
END SUBROUTINE wrf_quilt_put_var_td_char 

SUBROUTINE wrf_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )







RETURN
END SUBROUTINE wrf_quilt_read_field

SUBROUTINE wrf_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )


















  RETURN
END SUBROUTINE wrf_quilt_write_field

SUBROUTINE wrf_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )







RETURN
END SUBROUTINE wrf_quilt_get_var_info

subroutine wrf_quilt_find_server(iserver)

  
  
  

  
  
  


end subroutine wrf_quilt_find_server
subroutine wrf_quilt_server_ready()

  
  
  
  

  
  
  
  


end subroutine wrf_quilt_server_ready

SUBROUTINE get_mpi_comm_io_groups( retval, isrvr )





      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( id, retval )





      RETURN
END SUBROUTINE get_nio_tasks_in_group

SUBROUTINE collect_on_comm_debug(file,line, comm_io_group,   &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  IMPLICIT NONE
  CHARACTER*(*) file
  INTEGER line
  INTEGER comm_io_group
  INTEGER sze
  INTEGER hdrbuf(*), outbuf(*)
  INTEGER hdrbufsize, outbufsize 

  CALL collect_on_comm( comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  RETURN
END


SUBROUTINE collect_on_comm_debug2(file,line,var,tag,sz,hdr_rec_size, &
                        comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  IMPLICIT NONE
  CHARACTER*(*) file,var
  INTEGER line,tag,sz,hdr_rec_size
  INTEGER comm_io_group
  INTEGER sze
  INTEGER hdrbuf(*), outbuf(*)
  INTEGER hdrbufsize, outbufsize

  CALL collect_on_comm( comm_io_group,                       &
                        sze,                                 &
                        hdrbuf, hdrbufsize ,                 &
                        outbuf, outbufsize                   )
  RETURN
END

