      subroutine f(a)
          integer a
          print *, a
      end subroutine


      program main
          if (0<1) then
              print *, "first branch"
   10 continue
          else
              print *, "second branch"
          endif

50        call f(1)

          if (0<2) return

          if (0>1) goto 50

          if (0<1) call f(2)
      end program

