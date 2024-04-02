program arrays_reshape_17
use iso_fortran_env, only: int8
integer(int8), dimension(8) :: x
x = [1_1, 2_1, 3_1, 4_1, 5_1, 6_1, 7_1, 8_1]
print *, median_all_1_iint8_dp(x)
if (abs(median_all_1_iint8_dp(x) - 36.0) > 1e-16) error stop

contains
function median_all_1_iint8_dp(x) result(res)
use iso_fortran_env, only: int64, int8, dp => real64
integer(int8), intent(inout) :: x(:)
real(dp) :: res
integer(kind = int64) :: n

n = size(x)
x = reshape(x, [n])
res = sum(x)
end function median_all_1_iint8_dp
end program
