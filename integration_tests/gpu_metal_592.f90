! Named constant arrays of a derived type in a device procedure that device
! code does not read: one only another constant is declared with, one not
! used at all. The loop is offloaded; only reading such an array is
! declined (see gpu_decline_34).
module gpu_metal_592_m
implicit none
type :: pt
    real :: x
    integer :: k
end type
contains
pure real function h(y)
    real, intent(in) :: y
    type(pt), parameter :: la(2) = [pt(1.0, 2), pt(3.0, 4)]
    integer, parameter :: m = la(2)%k
    h = m * y
end function
pure real function g(y)
    real, intent(in) :: y
    type(pt), parameter :: lb(2) = [pt(1.0, 2), pt(3.0, 4)]
    g = 4 * y
end function
end module

program gpu_metal_592
use gpu_metal_592_m, only: h, g
implicit none
integer, parameter :: n = 50
real :: t(n), u(n)
integer :: i
do concurrent (i = 1:n)
    t(i) = h(real(i))
    u(i) = g(real(i))
end do
print *, t(1:3), u(1:3)
do i = 1, n
    if (abs(t(i) - 4.0*i) > 1e-3) error stop 1
    if (abs(u(i) - 4.0*i) > 1e-3) error stop 2
end do
end program
