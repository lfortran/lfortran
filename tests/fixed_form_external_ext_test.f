      program main
          external f
          integer a
          a = 1
          call f(a)
      end program

      subroutine f(xxx)
          integer xxx
          print *, xxx
      end subroutine
