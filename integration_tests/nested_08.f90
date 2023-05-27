module nested_08_testmod_der1
private
public fcn
contains

subroutine fcn(x, fvec)
real, intent(in) :: x(3)
real, intent(out) :: fvec(15)

integer :: i

do i = 1, 3
    fvec(i) = x(1) /( x(2) + x(3) )
end do
do i = 1, 3
    if (fvec(i) /= 0.5) error stop
end do
end subroutine

end module


program example_lmder1
use nested_08_testmod_der1, only: fcn
real :: x(3), fvec(15)
! The following starting values provide a rough fit.
x = [1.0, 1.0, 1.0]

call check_deriv(x, fvec)
contains
subroutine check_deriv(x, fvec)
real, intent(in) :: x(3)
real, intent(out) :: fvec(15)
call fcn(x, fvec)
end subroutine

end program
