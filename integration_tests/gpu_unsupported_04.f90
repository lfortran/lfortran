! A do concurrent that passes a derived-type value whole to a pure function.
! The function reads only a real(4) component, but the whole value, with its
! unused real(8) component, reaches the device. On Metal the loop is on the
! unsupported list: a compile error, or with --gpu-allow-cpu-fallback a loop
! that runs on the CPU.
module gpu_unsupported_04_m
implicit none
type :: sample_t
    real :: a
    real(8) :: unused
end type
contains
    pure real function twice(s)
        type(sample_t), intent(in) :: s
        twice = 2.0 * s%a
    end function
end module

program gpu_unsupported_04
use gpu_unsupported_04_m
implicit none
type(sample_t) :: s(8)
real :: y(8)
integer :: i
do i = 1, 8
    s(i)%a = real(i)
    s(i)%unused = 0.0d0
end do
do concurrent (i = 1:8)
    y(i) = twice(s(i))
end do
do i = 1, 8
    if (abs(y(i) - 2.0 * real(i)) > 1.0e-5) error stop
end do
print *, "PASS"
end program
