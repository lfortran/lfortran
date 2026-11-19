! An ALLOCATABLE statement gives the attribute to a deferred-shape array
! whether it comes before or after the type declaration, and only to the
! entities of its own scoping unit, not to a component of the same name.
module allocatable_statement_01_m
implicit none
type t
    integer :: a(2)
end type
integer :: a(:)
allocatable :: a
end module

program allocatable_statement_01
use allocatable_statement_01_m
implicit none
allocatable :: b
integer :: b(:)
type(t) :: x
x%a = 3
allocate(a(2), b(3))
a = 1
b = 2
if (any(x%a /= 3)) error stop 1
if (sum(a) /= 2 .or. sum(b) /= 6) error stop 2
call s
if (size(f(4)) /= 4 .or. sum(f(4)) /= 10) error stop 3
contains
subroutine s
    integer :: c(:)
    allocatable c
    allocate(c(2))
    c = 1
    if (c(1) /= 1 .or. c(2) /= 1) error stop 4
end subroutine
function f(n) result(r)
    integer, intent(in) :: n
    integer :: r(:)
    allocatable :: r
    integer :: i
    allocate(r(n))
    r = [(i, i = 1, n)]
end function
end program
