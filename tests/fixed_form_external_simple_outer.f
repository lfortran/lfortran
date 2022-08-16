      program main
          external g
          integer a
          a = 1
          call g(a)
      end 


      subroutine g(a)
          integer a
          print *, a
      end subroutine
