module module_arrays_42
contains
function var_2_iint8_dp(x, dim) result(res)
use iso_fortran_env, only: dp => real64
integer, intent(in) :: x(:,:)
integer, intent(in) :: dim
real(dp) :: res
res = sum(sum(real(x, dp), dim))
end function var_2_iint8_dp
end module

program arrays_42
use module_arrays_42
use iso_fortran_env, only: dp => real64
integer :: x(3,3) = reshape([1,2,3,4,5,6,7,8,9], [3,3])
real(dp) :: res
res = var_2_iint8_dp(x, 1)

print *, res
if (abs(res - 45.0) > 1e-16) error stop
end program
