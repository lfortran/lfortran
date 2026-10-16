! The size of an automatic array result `y(size(x))` is taken from the
! actual argument; for a `spread` actual argument it is the size of the
! `spread` result, also when `ncopies` is not a constant (#15333).
module functions_68_mod
implicit none
contains

function f(x) result(y)
real :: x(:), y(size(x))
y = x
end function

function g(x) result(y)
real :: x(:,:), y(size(x))
y = reshape(x, [size(x)])
end function

subroutine test(n)
integer :: n
integer(8) :: n8
real :: a(2)
real :: r(2*n)
a = [1.0, 2.0]
n8 = n
if (size(f(spread(12.0, 1, n))) /= n) error stop
if (abs(maxval(f(spread(12.0, 1, n))) - 12.0) > 1e-6) error stop
if (abs(sum(f(spread(12.0, 1, n))) - 12.0*n) > 1e-5) error stop
if (size(g(spread(a, 1, n))) /= 2*n) error stop
if (size(g(spread(a, 2, n))) /= 2*n) error stop
r = g(spread(a, 2, n))
if (abs(sum(r) - 3.0*n) > 1e-5) error stop
if (abs(r(2*n) - 2.0) > 1e-6) error stop
r = g(spread(a, 1, n))
if (abs(r(1) - 1.0) > 1e-6) error stop
if (abs(r(2*n) - 2.0) > 1e-6) error stop
if (size(f(spread(12.0, 1, n - 10))) /= 0) error stop
if (size(f(spread(12.0, 1, n8))) /= n) error stop
end subroutine

end module

program functions_68
use functions_68_mod
implicit none
call test(3)
print *, "ok"
end program
