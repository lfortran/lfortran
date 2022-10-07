      subroutine f(xxx)
          integer xxx
          print *, xxx
      end subroutine

      program main
          integer a
          a = 1
          call f(a)
      end program
