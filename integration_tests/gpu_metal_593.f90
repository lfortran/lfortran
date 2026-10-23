! Named constant arrays of a derived type read only through designators
! with a compile-time value, in a device procedure and in a block of the
! loop body. Registered with --fast, which puts the values in place of the
! designators, so device code does not read the arrays and the loop is
! offloaded; without it, reading them is declined (see gpu_decline_34).
module gpu_metal_593_m
implicit none
type :: pt
    real :: x
    integer :: k
end type
contains
pure real function h(y)
    real, intent(in) :: y
    type(pt), parameter :: la(2) = [pt(1.0, 2), pt(3.0, 4)]
    h = la(2)%k * y
end function
end module

program gpu_metal_593
use gpu_metal_593_m, only: pt, h
implicit none
integer, parameter :: n = 50
real :: t(n), u(n)
integer :: i
do concurrent (i = 1:n)
    t(i) = h(real(i))
    block
        type(pt), parameter :: ba(2) = [pt(1.0, 2), pt(3.0, 5)]
        u(i) = ba(2)%k * i
    end block
end do
print *, t(1:3), u(1:3)
do i = 1, n
    if (abs(t(i) - 4.0*i) > 1e-3) error stop 1
    if (abs(u(i) - 5.0*i) > 1e-3) error stop 2
end do
end program
