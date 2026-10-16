module modules_79_a
implicit none
private
public :: ta, assignment(=)
type :: ta
    integer :: x = 0
end type
interface assignment(=)
    module procedure set_a
end interface
contains
subroutine set_a(lhs, rhs)
    type(ta), intent(out) :: lhs
    integer, intent(in) :: rhs
    lhs%x = 2*rhs
end subroutine
end module

module modules_79_b
implicit none
private
public :: tb, assignment(=)
type :: tb
    integer :: x = 0
end type
interface assignment(=)
    module procedure set_b
end interface
contains
subroutine set_b(lhs, rhs)
    type(tb), intent(out) :: lhs
    integer, intent(in) :: rhs
    lhs%x = 3*rhs
end subroutine
end module

module modules_79_c
use modules_79_a
use modules_79_b
implicit none
contains
subroutine set_a(lhs, rhs)
    integer, intent(out) :: lhs
    integer, intent(in) :: rhs
    lhs = rhs
end subroutine
end module
