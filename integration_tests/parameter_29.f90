! An element of a named constant array passed to an explicit-shape array
! dummy of a type-bound procedure that follows the passed object (sequence
! association, F2018 15.5.2.11 p2), and to a VALUE dummy, also in --fast mode.
module parameter_29_m
implicit none
type :: t
    integer :: k = 1
contains
    procedure :: tb
end type
contains
subroutine tb(self, a, x, r)
class(t), intent(in) :: self
integer, intent(in) :: a(3)
integer, intent(in) :: x
integer, intent(out) :: r
r = self%k * (a(1) + a(2) + a(3)) + x
end subroutine

subroutine sv(x, r)
integer, value :: x
integer, intent(out) :: r
r = x
end subroutine
end module

program parameter_29
use parameter_29_m
implicit none
integer, parameter :: ip(4) = [10, 20, 30, 40]
type(t) :: o
integer :: r

call o%tb(ip(2), ip(1), r)
print *, r
if (r /= 100) error stop 1

call o%tb(ip(1), ip(4), r)
print *, r
if (r /= 100) error stop 2

call sv(ip(3), r)
print *, r
if (r /= 30) error stop 3
end program
