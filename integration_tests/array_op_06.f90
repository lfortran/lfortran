program array_op_06
implicit none
integer, parameter :: dim = 2
integer, parameter :: n = 3
integer :: x(n, n)
logical :: mask(n, n)
real :: res

x = reshape((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/n, n/))
mask = reshape((/.true., .false., .true., .false., .true., .false., .true., .false., .true./), (/n, n/))

res = var_mask_2_iint8_dp(x, dim, mask)
print *, res
if (abs(res - 5.0) > 1e-8) error stop
contains
function var_mask_2_iint8_dp(x, dim, mask) result(res)
integer, intent(in) :: x(:,:)
integer, intent(in) :: dim
logical, intent(in) :: mask(:,:)
real :: res

real :: n(merge(size(x, 1), size(x, 2), mask=1<dim))

n = count(mask, 1)
res = sum(n)
end function var_mask_2_iint8_dp
end program
