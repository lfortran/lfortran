! Named constant arrays of a derived type, and a named constant with an
! array component of a derived type, read only through designators with a
! compile-time value: in a device procedure, in a block of the loop body
! and in the loop body. Registered with --fast, which puts the values in
! place of the designators, so device code does not read the constants and
! the loop is offloaded; without it, reading them is declined (see
! gpu_decline_34).
module gpu_metal_593_m
implicit none
type :: pt
    real :: x
    integer :: k
end type
type :: poly
    type(pt) :: v(2)
    real :: s
end type
type(poly), parameter :: pp = poly([pt(1.0, 1), pt(2.0, 2)], 3.0)
contains
pure real function h(y)
    real, intent(in) :: y
    type(pt), parameter :: la(2) = [pt(1.0, 2), pt(3.0, 4)]
    h = la(2)%k * y
end function
end module

program gpu_metal_593
use gpu_metal_593_m, only: pt, pp, h
implicit none
integer, parameter :: n = 50
real :: t(n), u(n), w(n)
integer :: i
do concurrent (i = 1:n)
    t(i) = h(real(i))
    block
        type(pt), parameter :: ba(2) = [pt(1.0, 2), pt(3.0, 5)]
        u(i) = ba(2)%k * i
    end block
    w(i) = pp%v(2)%x * i + pp%s
end do
print *, t(1:3), u(1:3), w(1:3)
do i = 1, n
    if (abs(t(i) - 4.0*i) > 1e-3) error stop 1
    if (abs(u(i) - 5.0*i) > 1e-3) error stop 2
    if (abs(w(i) - (2.0*i + 3)) > 1e-3) error stop 3
end do
end program
