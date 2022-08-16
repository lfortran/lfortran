      program main 
          external f
          integer a
          a = 1
          call f(a)
      end program

      subroutine f(a)
          integer a
          print *, a
      end subroutine
