      subroutine cmdo ()
        common /block/ t
        integer :: t

        integer :: i

        do, i=1, t
          print *, t
        end do

      end subroutine
