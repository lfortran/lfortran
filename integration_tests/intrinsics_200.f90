program intrinsics_200
integer, dimension(5) :: x
logical, dimension(5) :: mask

x = (/1, 2, 3, 4, 5/)
mask = (/ .true., .false., .true., .false., .true./)

print *, median_all_mask_1_iint8_dp(x, mask)
if (abs(median_all_mask_1_iint8_dp(x, mask) - 9.0) > 1e-8) error stop
contains
function median_all_mask_1_iint8_dp(x, mask) result(res)
integer, intent(in) :: x(:)
logical, intent(in) :: mask(:)
real :: res
integer, allocatable :: x_tmp(:)

x_tmp = pack(x, mask)
res = sum(x_tmp)
end function median_all_mask_1_iint8_dp
end program
