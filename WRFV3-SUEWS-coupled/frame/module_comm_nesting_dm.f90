
MODULE module_comm_nesting_dm

   IMPLICIT NONE

   PRIVATE module_comm_nesting_dm_dummy


   INTEGER, PRIVATE :: idim1, idim2, idim3, idim4, idim5, idim6, idim7


CONTAINS

   
   SUBROUTINE module_comm_nesting_dm_dummy
     USE module_domain, ONLY:domain
     USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
     USE module_state_description, ONLY:PARAM_FIRST_SCALAR
     USE module_driver_constants
     RETURN
   END SUBROUTINE module_comm_nesting_dm_dummy



END MODULE module_comm_nesting_dm

