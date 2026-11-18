! A saved local of a procedure with an assumed-shape dummy is shared by
! every call, whether the actual argument is a whole array, a contiguous
! or strided section, or an expression.
module save_32_mod
implicit none
contains

subroutine count_explicit(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
n = c
end subroutine

subroutine count_implicit(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
integer :: c = 0
c = c + 1
n = c
end subroutine

integer function count_function(x) result(n)
integer, intent(in) :: x(:, :)
integer, save :: c
data c /0/
c = c + 1
n = c
end function

subroutine count_nested(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
call bump(n)
contains
subroutine bump(m)
integer, intent(out) :: m
integer, save :: c = 0
c = c + 1
m = c
end subroutine
end subroutine

end module

program save_32
use save_32_mod, only: count_explicit, count_implicit, count_function, count_nested
implicit none
real :: a(4), b(4, 2)
integer :: m(3, 3)
integer :: n

a = 1.0
b = 2.0
m = 3

call count_explicit(a, n)
if (n /= 1) error stop
call count_explicit(b(:, 1), n)
if (n /= 2) error stop
call count_explicit(b(1:3:2, 2), n)
if (n /= 3) error stop
call count_explicit(a + 1.0, n)
if (n /= 4) error stop
call count_explicit(a(2:3), n)
if (n /= 5) error stop

call count_implicit(a, n)
if (n /= 1) error stop
call count_implicit(b(1, :), n)
if (n /= 2) error stop
call count_implicit(a(4:1:-2), n)
if (n /= 3) error stop
call count_implicit(2.0 * a, n)
if (n /= 4) error stop

if (count_function(m) /= 1) error stop
if (count_function(m(1:2, :)) /= 2) error stop
if (count_function(m(::2, ::2)) /= 3) error stop
if (count_function(m + 1) /= 4) error stop

call count_nested(a, n)
if (n /= 1) error stop
call count_nested(b(2, :), n)
if (n /= 2) error stop
call count_nested(a(1:4:3), n)
if (n /= 3) error stop

call count_local(a, n)
if (n /= 1) error stop
call count_local(b(:, 2), n)
if (n /= 2) error stop
call count_local(b(2:4:2, 1), n)
if (n /= 3) error stop
call count_local(a - b(:, 1), n)
if (n /= 4) error stop

print *, "ok"

contains

subroutine count_local(x, n)
real, intent(in) :: x(:)
integer, intent(out) :: n
integer, save :: c = 0
c = c + 1
n = c
end subroutine

end program
