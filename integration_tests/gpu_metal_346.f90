program gpu_metal_346
! A `do concurrent` over `integer(8)` data with values beyond huge(0), which
! Metal kernels compute in its 64-bit `long`.
implicit none
integer, parameter :: n = 16
integer(8) :: x(n), y(n)
integer :: i
do i = 1, n
    x(i) = 3000000000_8 + i
end do
do concurrent (i = 1:n)
    y(i) = 2 * x(i)
end do
do i = 1, n
    if (y(i) /= 6000000000_8 + 2 * i) error stop
end do
print *, y(n)
end program
