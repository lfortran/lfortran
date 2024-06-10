MODULE implied_do_loops5
   INTEGER, DIMENSION(10) :: fixup_counter
   INTEGER :: n
END MODULE implied_do_loops5

PROGRAM test
   USE implied_do_loops5
   IMPLICIT NONE


   PRINT *, (fixup_counter(n) + n, n=1, 4)
END PROGRAM test
