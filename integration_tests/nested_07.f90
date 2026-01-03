module nested_07_testmod_der1
private
public fcn
contains

subroutine fcn(x, fvec)
real, intent(in) :: x
real, intent(out) :: fvec

integer :: i

do i = 1, 3
    print *, "x(1) = ", x
    fvec = x+1
    print *, "fvec(i) = ", fvec
end do

end subroutine

end module


program main
use nested_07_testmod_der1, only: fcn
real :: x, fvec
! The following starting values provide a rough fit.
x = 1.0

call check_deriv()
contains
subroutine check_deriv()
call fcn(x, fvec)
end subroutine

end program
