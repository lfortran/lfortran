      program main
          external g
          integer a
          a = 1
c this yields a TYPE MISMATCH error (just like in gfortran)
          call g(a)
      end 


      subroutine g(a)
          double precision a
          print *, a
      end subroutine
