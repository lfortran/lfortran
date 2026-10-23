! Named constant arrays of a derived type, and a named constant with an
! array component of a derived type, read only through designators with a
! compile-time value: in a device procedure, in a block of the loop body
! and in the loop body. Registered with --fast, which puts the values in
! place of the designators, so device code does not read the constants and
! the loop is offloaded; without it, reading them is declined (see
! gpu_decline_34). An element of a constant array whose component is read,
! as the value of an assignment in a block or as a bound of a do loop in
! the loop body or in a device procedure, may itself be put in place of its
! read, and is a value of the device struct then.
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
type(pt), parameter :: carr(2) = [pt(1.0, 1), pt(2.0, 3)]
contains
pure real function dp(v)
    real, intent(in) :: v
    integer :: j
    dp = 0
    do j = 1, carr(2)%k
        dp = dp + v * carr(1)%x
    end do
end function
pure real function h(y)
    real, intent(in) :: y
    type(pt), parameter :: la(2) = [pt(1.0, 2), pt(3.0, 4)]
    h = la(2)%k * y
end function
end module

program gpu_metal_593
use gpu_metal_593_m, only: pt, pp, carr, h, dp
implicit none
integer, parameter :: n = 50
real :: t(n), u(n), w(n), a(n), c(n), d(n)
integer :: i, j
do concurrent (i = 1:n)
    t(i) = h(real(i))
    block
        type(pt), parameter :: ba(2) = [pt(1.0, 2), pt(3.0, 5)]
        u(i) = ba(2)%k * i
    end block
    w(i) = pp%v(2)%x * i + pp%s
    block
        real :: b
        b = carr(2)%x
        a(i) = b * i
    end block
    c(i) = 0
    do j = 1, carr(2)%k
        c(i) = c(i) + i
    end do
    d(i) = dp(real(i))
end do
print *, t(1:3), u(1:3), w(1:3), a(1:3), c(1:3), d(1:3)
do i = 1, n
    if (abs(t(i) - 4.0*i) > 1e-3) error stop 1
    if (abs(u(i) - 5.0*i) > 1e-3) error stop 2
    if (abs(w(i) - (2.0*i + 3)) > 1e-3) error stop 3
    if (abs(a(i) - 2.0*i) > 1e-3) error stop 4
    if (abs(c(i) - 3.0*i) > 1e-3) error stop 5
    if (abs(d(i) - 3.0*i) > 1e-3) error stop 6
end do
end program
