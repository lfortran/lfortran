! This integration test tests for the comparison of `Var_t` expressions
! at runtime.
!
! The test is framed such that it checks if a variable passed from
! one subroutine to another for setting the dimension of an array works.
!
! The underlying idea is that when a variable is used to set the dimension
! of an array in a subroutine, the passed variable is internally checked for
! equality of expression with the parameter variable. If the variable
! expressions are equal, the program will execute, else it will fail.
!
!
! This means that for a subroutine
!
!    SUBROUTINE subroutine_1 ( arr_1, nx )
!
!       INTEGER, DIMENSION(nx), INTENT(IN) :: arr_1
!
!       INTEGER, INTENT(IN) ::  nx
!
!    END SUBROUTINE subroutine_1
!
! and another subroutine
!
!    SUBROUTINE subroutine_2 ( arr_2, ny )
!
!       INTEGER, DIMENSION(ny):: arr_2
!
!       INTEGER, INTENT(IN) ::  ny
!
!       CALL subroutine_1 ( arr_2, ny )
!
!    END SUBROUTINE subroutine_2
!
!
! the expression type of the argument variable `ny` to `subroutine_1` must be equal to
! the parameter variable `nx`.


MODULE module_interface_16

   INTERFACE sub
      MODULE PROCEDURE subroutine_1
   END INTERFACE sub

CONTAINS

   SUBROUTINE subroutine_1 ( arr_1, nx )

      INTEGER, DIMENSION(nx), INTENT(IN) :: arr_1

      INTEGER, INTENT(IN) ::  nx

      PRINT *, nx
      IF (nx /= 4) ERROR STOP

      PRINT *, arr_1
      IF (all(arr_1 /= [1, 2, 3, 4])) ERROR STOP


   END SUBROUTINE subroutine_1

   SUBROUTINE subroutine_2 ( arr_2, ny )

      INTEGER, DIMENSION(ny):: arr_2

      INTEGER, INTENT(IN) ::  ny

      CALL subroutine_1 ( arr_2, ny )

   END SUBROUTINE subroutine_2

END MODULE module_interface_16


PROGRAM interface_16
   USE module_interface_16
   IMPLICIT NONE

   INTEGER :: n
   INTEGER, DIMENSION(4) :: test_arr

   n = 4
   test_arr = [1, 2, 3, 4]

   CALL subroutine_2( test_arr, n )

END PROGRAM interface_16

