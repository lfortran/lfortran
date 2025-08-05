module bspline_sub_module
    contains
    subroutine dbtpcf(work)
      real(4),dimension(*), intent(out)     :: work 
      integer(4) :: iq
      call dbintk(work(iq))
    end subroutine dbtpcf
    subroutine dbintk(q)
      implicit none
      real(4),dimension(*),intent(out) :: q  
    end subroutine dbintk
end module bspline_sub_module
program main
use bspline_sub_module
implicit none
real(4), dimension(10) :: work
work = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
call dbtpcf(work)
print *, work
if (any(work - [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0] > 1e-6)) error stop
end program