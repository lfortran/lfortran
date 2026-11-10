C     Fixed form: a deferred-shape array given the ALLOCATABLE attribute
C     by a later ALLOCATABLE statement (#15291)
      PROGRAM ALLOCATABLE_STMT_02
      IMPLICIT NONE
      REAL*8, DIMENSION(:) :: A
      ALLOCATABLE A
      ALLOCATE(A(3))
      A = 1.5D0
      IF (ABS(SUM(A) - 4.5D0) .GT. 1D-12) ERROR STOP
      PRINT *, SUM(A)
      END PROGRAM
