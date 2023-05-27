module minpack_02_testmod_der1
implicit none

integer, parameter :: dp=kind(0d0)

contains

subroutine check_deriv(x, fvec, fjac)
real(dp), intent(in) :: x(:)
real(dp), intent(out) :: fvec(:), fjac(:,:)
real(dp) :: xp(size(x)), fvecp(size(fvec)), err(size(fvec))
end subroutine

end module

program minpack_test
use minpack_02_testmod_der1, only: dp, check_deriv
implicit none

real(dp) :: x1(10), fvec1(20), fjac1(30, 40)

call check_deriv(x1, fvec1, fjac1)

end program minpack_test
