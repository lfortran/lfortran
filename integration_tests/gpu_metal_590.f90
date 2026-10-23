! Derived-type named constants with an array component, passed to device
! procedures: from the loop body as a constant of the module and of the
! program, and from a device procedure as a constant of its module and as
! one of its own. Their values in the kernel name the kernel's copy of the
! derived type, as the constants do. Registered with --fast, which replaces
! a reference to a constant by its value.
module gpu_metal_590_m
implicit none
type :: pt
    real :: w(3)
end type
type :: line
    type(pt) :: a
    integer :: k
end type
type(pt), parameter :: p0 = pt([0.1, 0.2, 0.3])
type(line), parameter :: l0 = line(pt([1.0, 2.0, 3.0]), 4)
contains
pure real function useit(p, y)
    type(pt), intent(in) :: p
    real, intent(in) :: y
    useit = p%w(3) * y
end function
pure real function uselin(l, y)
    type(line), intent(in) :: l
    real, intent(in) :: y
    uselin = l%a%w(2) * y + l%k
end function
pure real function f(y)
    real, intent(in) :: y
    f = useit(p0, y) + uselin(l0, y)
end function
pure real function h(y)
    real, intent(in) :: y
    type(pt), parameter :: lp = pt([1.0, 2.0, 7.0])
    h = useit(lp, y)
end function
end module

program gpu_metal_590
use gpu_metal_590_m, only: pt, p0, l0, useit, uselin, f, h
implicit none
type(pt), parameter :: p1 = pt([1.0, 2.0, 5.0])
integer, parameter :: n = 100
real :: q(n), r(n), s(n), t(n)
integer :: i
do concurrent (i = 1:n)
    q(i) = useit(p0, real(i)) + uselin(l0, real(i))
    r(i) = useit(p1, real(i))
    s(i) = f(real(i))
    t(i) = h(real(i))
end do
print *, q(1:3), r(1:3), s(1:3), t(1:3)
do i = 1, n
    if (abs(q(i) - (0.3*i + 2.0*i + 4)) > 1e-3) error stop 1
    if (abs(r(i) - 5.0*i) > 1e-3) error stop 2
    if (abs(s(i) - (0.3*i + 2.0*i + 4)) > 1e-3) error stop 3
    if (abs(t(i) - 7.0*i) > 1e-3) error stop 4
end do
end program
