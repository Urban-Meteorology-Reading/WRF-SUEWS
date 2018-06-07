








MODULE qsort_c_module

  IMPLICIT NONE
  PUBLIC :: QsortC
  PRIVATE :: Partition

CONTAINS

  RECURSIVE SUBROUTINE QsortC(A)
    REAL, INTENT(in out), DIMENSION(:) :: A
    INTEGER :: iq

    IF(SIZE(A) > 1) THEN
       CALL Partition(A, iq)
       CALL QsortC(A(:iq-1))
       CALL QsortC(A(iq:))
    ENDIF
  END SUBROUTINE QsortC

  SUBROUTINE Partition(A, marker)
    REAL, INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(out) :: marker
    INTEGER :: i, j
    REAL :: temp
    REAL :: x      
    x = A(1)
    i= 0
    j= SIZE(A) + 1

    DO
       j = j-1
       DO
          IF (A(j) <= x) EXIT
          j = j-1
       END DO
       i = i+1
       DO
          IF (A(i) >= x) EXIT
          i = i+1
       END DO
       IF (i < j) THEN
          
          temp = A(i)
          A(i) = A(j)
          A(j) = temp
       ELSEIF (i == j) THEN
          marker = i+1
          RETURN
       ELSE
          marker = i
          RETURN
       ENDIF
    END DO

  END SUBROUTINE Partition

END MODULE qsort_c_module
