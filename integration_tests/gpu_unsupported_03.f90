! A do concurrent that reads a real(8) component of a derived type. The
! component reaches the device, so on Metal the loop is on the unsupported
! list: a compile error, or with --gpu-allow-cpu-fallback a loop that runs on
! the CPU.
module gpu_unsupported_03_m
implicit none
type :: pair_t
    real :: a(8)
    real(8) :: d(8)
end type
end module

program gpu_unsupported_03
use gpu_unsupported_03_m
implicit none
type(pair_t) :: p
real :: y(8)
integer :: i
do i = 1, 8
    p%a(i) = real(i)
    p%d(i) = real(i, 8) / 8.0d0
end do
do concurrent (i = 1:8)
    y(i) = p%a(i) + real(p%d(i))
end do
do i = 1, 8
    if (abs(y(i) - (real(i) + real(i) / 8.0)) > 1.0e-5) error stop
end do
print *, "PASS"
end program
