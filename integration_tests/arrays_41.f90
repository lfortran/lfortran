program arrays_41
use iso_fortran_env, only: dp => real64
integer, parameter :: n = 1000
integer, parameter :: m = 1000
integer :: i, j
integer, dimension(n,m) :: x
real(dp) :: res

x = 18
res = var_2_iint8_dp(x, 1)
print *, res
if (abs(res - 18000000.000000000) > 1e-16) error stop
contains
function var_2_iint8_dp(x, dim) result(res)
use iso_fortran_env, only: dp => real64
integer, intent(in) :: x(:,:)
integer, intent(in) :: dim
real(dp) :: res

res = sum(sum(real(x, dp), dim))
end function var_2_iint8_dp
end program
