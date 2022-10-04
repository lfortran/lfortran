      program main 
          external f
          integer b, c, d
          b = 1
          call f(b)
      end program

      subroutine f(a)
          integer a
          print *, a
      end subroutine
