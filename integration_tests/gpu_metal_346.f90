program gpu_metal_346
! Metal has a 64-bit integer type, but the Metal kernel lowering has none, so
! a `do concurrent` over `integer(8)` data is a lowering LFortran is missing,
! not something the device cannot run: under --gpu=metal the compilation
! fails instead of running the loop on the CPU. tests.toml checks the error.
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
