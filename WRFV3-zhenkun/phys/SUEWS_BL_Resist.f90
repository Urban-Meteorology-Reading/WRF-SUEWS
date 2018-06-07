SUBROUTINE BoundaryLayerResistance(&
     zzd,& 
     z0M,&     
     avU1,&    
     UStar,&
     rb)

  IMPLICIT NONE

  REAL(KIND(1d0)),INTENT(in)::zzd     
  REAL(KIND(1d0)),INTENT(in)::z0M     
  REAL(KIND(1d0)),INTENT(in)::avU1    

  REAL(KIND(1d0)),INTENT(inout)::UStar

  REAL(KIND(1d0)),INTENT(out)::rb   

  REAL(KIND(1d0)),PARAMETER :: k=0.4

  IF(UStar<0.01) THEN
     UStar=avu1/LOG(zzd/z0m)*k
  END IF

  rb=(1.1/UStar)+(5.6*(UStar**0.333333))

  RETURN
END SUBROUTINE BoundaryLayerResistance
