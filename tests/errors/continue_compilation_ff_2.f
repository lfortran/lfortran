      PROGRAM continue_compilation_ff_2
      print *, "This is a test program"
      SUBROUTINE faulty_subroutine(a, b, c)
      INTEGER, INTENT(IN) :: sub_a
      END SUBROUTINE faulty_subroutine
      END PROGRAM continue_compilation_ff_2