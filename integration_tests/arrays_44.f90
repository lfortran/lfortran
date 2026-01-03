program arrays_44

integer, dimension(3, 3) :: x
logical, dimension(3, 3) :: mask

x = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], shape(x))
mask = reshape([.true., .false., .true., .false., .true., .false., .true., .false., .true.], shape(mask))

print *, median_mask_2_iint8_dp(x, 1, mask)
if (abs(median_mask_2_iint8_dp(x, 1, mask) - 12.0) > 1e-8) error stop
contains
function median_mask_2_iint8_dp(x, dim, mask) result(res)
integer, intent(in) :: x(:,:)
integer, intent(in) :: dim
logical, intent(in) :: mask(:,:)
real :: res
integer :: j1
integer, allocatable :: x_tmp(:)

do j1 = 1, size(x, 1)
x_tmp = pack(x(j1, :), mask(j1, :))
end do
res = sum(x_tmp)
end function median_mask_2_iint8_dp
end program
