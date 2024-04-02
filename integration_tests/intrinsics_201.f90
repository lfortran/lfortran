program intrinsics_201
real, dimension(3) :: x = [1.0, 2.0, 3.0]
logical, dimension(3) :: mask = [.true., .false., .true.]
integer, dimension(3) :: y = [1, 2, 3]

print *, median_all_1_rsp_sp(x)
if (median_all_1_rsp_sp(x)) error stop
print *, median_all_mask_1_iint8_dp(y, mask)
if (median_all_mask_1_iint8_dp(y, mask)) error stop
contains
function median_all_1_rsp_sp (x) result(res)
use ieee_arithmetic
real, intent(in) :: x(:)
logical :: res
res = any(ieee_is_nan(x))
end function median_all_1_rsp_sp

function median_all_mask_1_iint8_dp(x, mask) result(res)
integer, intent(in) :: x(:)
logical, intent(in) :: mask(:)
logical :: res
res = any(shape(x) /= shape(mask))
end function median_all_mask_1_iint8_dp
end program
