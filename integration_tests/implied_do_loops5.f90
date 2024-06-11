MODULE module_implied_do_loops5
   INTEGER, DIMENSION(5) :: fixup_counter
   INTEGER :: n
END MODULE module_implied_do_loops5

PROGRAM implied_do_loops5
   USE module_implied_do_loops5
   IMPLICIT NONE
   INTEGER, DIMENSION(5) :: result_array

   PRINT *, (fixup_counter(n) + n, n = 1, 5)
   result_array = [(fixup_counter(n) + n, n = 1, 5)]

   if (all(result_array /= [1, 2, 3, 4, 5])) ERROR STOP

END PROGRAM implied_do_loops5
