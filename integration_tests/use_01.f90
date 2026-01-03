MODULE one
   IMPLICIT NONE
   INTEGER :: nx = 1

CONTAINS

   SUBROUTINE sub (n)
      INTEGER, INTENT(INOUT) :: n
      n = n + 1
   END SUBROUTINE sub

   INTEGER FUNCTION fun() result(i)
      i = 3
   END FUNCTION

END MODULE one


MODULE two
   USE one, ONLY: nx, sub, fun
   IMPLICIT NONE
END MODULE two


PROGRAM test
   USE two
   USE one, ONLY: nx, sub, fun

   PRINT *, nx
   IF (nx /= 1) ERROR STOP

   CALL sub ( nx )

   PRINT *, nx
   IF (nx /= 2) ERROR STOP

   PRINT *, fun()
   if (fun() /= 3) ERROR STOP

END PROGRAM test
