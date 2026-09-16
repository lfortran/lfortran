! A do concurrent that reads only a real(4) allocatable component of a
! derived type that also has a real(8) component. Only the component read
! reaches the device, as a buffer of its own, so the loop is not on the
! unsupported list and is offloaded, Metal included. The real(8) component
! comes first, so a kernel handed the whole value in a narrower layout would
! read the wrong elements.
module gpu_unsupported_05_m
implicit none
type :: sample_t
    real(8) :: unused
    real, allocatable :: a(:)
end type
end module

program gpu_unsupported_05
use gpu_unsupported_05_m
implicit none
type(sample_t) :: s
real :: y(8)
integer :: i
allocate(s%a(8))
do i = 1, 8
    s%a(i) = real(i)
end do
s%unused = 0.0d0
do concurrent (i = 1:8)
    y(i) = 2.0 * s%a(i)
end do
do i = 1, 8
    if (abs(y(i) - 2.0 * real(i)) > 1.0e-5) error stop
end do
print *, "PASS"
end program
