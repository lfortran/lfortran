PROGRAM subroutines_12

IMPLICIT NONE

INTEGER :: nang = 2

CALL my_subroutine

CONTAINS

SUBROUTINE my_subroutine

INTEGER, DIMENSION(nang) :: fxhv

fxhv = [1, 2]

PRINT *, fxhv
if ( sum(fxhv) /= 3 ) error stop

END SUBROUTINE my_subroutine

END PROGRAM subroutines_12
