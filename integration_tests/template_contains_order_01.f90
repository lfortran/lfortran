! Instantiating a templated procedure of the host's `contains` before its
! definition (from the host body or an earlier contained procedure) must
! use its full body (#13451).
module template_contains_order_01_m
implicit none
contains
subroutine set_mod(i)
integer, intent(inout) :: i
call set_one{real}(i, 1.0)
end subroutine
template subroutine set_one{t}(i, x)
deferred type :: t
integer, intent(inout) :: i
type(t), intent(in) :: x
i = 1
end subroutine
end module

program template_contains_order_01
use template_contains_order_01_m, only: set_mod
implicit none
integer :: i
real :: r
i = 0
call inc{real}(i, 1.0)
if (i /= 1) error stop
call set_mod(i)
if (i /= 1) error stop
i = 5
call helper(i)
if (i /= 6) error stop
r = ident{real}(2.5)
if (abs(r - 2.5) > 1e-6) error stop
print *, i, r
contains
subroutine helper(i)
integer, intent(inout) :: i
call inc{integer}(i, 7)
end subroutine
template subroutine inc{t}(i, x)
deferred type :: t
integer, intent(inout) :: i
type(t), intent(in) :: x
i = i + 1
end subroutine
template function ident{t}(x) result(y)
deferred type :: t
type(t), intent(in) :: x
type(t) :: y
y = x
end function
end program
