c compiles in gfortran with "missing symbols" error -> LFortran will
c replicate this behavior 
      subroutine outer_scope(a)
          integer a
          print *, a
      end subroutine

      program main
          external inner_scope
          integer b, c
          call inner_scope(b, c) ! this should compile but NOT run on its own          
      end program

