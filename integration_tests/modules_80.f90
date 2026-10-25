! A private specific of a use-associated assignment(=) does not take the
! name of a derived type of the using program.
module modules_80_mod
implicit none
private
public :: b_t, assignment(=)
type :: b_t
    integer :: x = 0
end type
interface assignment(=)
    module procedure set_a
end interface
contains
subroutine set_a(lhs, rhs)
    type(b_t), intent(out) :: lhs
    integer, intent(in) :: rhs
    lhs%x = 2*rhs
end subroutine
end module

program modules_80
use modules_80_mod
implicit none
type :: set_a
    integer :: y
end type
type(b_t) :: b
type(set_a) :: s
b = 3
s = set_a(4)
print *, b%x, s%y
if (b%x /= 6) error stop 1
if (s%y /= 4) error stop 2
end program
