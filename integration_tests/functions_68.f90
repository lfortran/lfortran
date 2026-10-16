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

function h(x, k) result(y)
real :: x(:,:,:)
integer, intent(in) :: k
real :: y(size(x, k))
y = real(k)
end function

function g3(x) result(y)
real :: x(:,:,:), y(size(x))
y = reshape(x, [size(x)])
end function

! A rank-3 `spread` result queried with a runtime `k` in `size(x, k)`.
subroutine test_rank3(n, k)
integer :: n, k
integer :: e(3)
real :: b(2,3)
real :: r(n + 5)
b = 1.0
e = [2, n, 3]
if (size(h(spread(b, 2, n), k)) /= e(k)) error stop
r(:e(k)) = 2.0*h(spread(b, 2, n), k)
if (abs(sum(r(:e(k))) - 2.0*k*e(k)) > 1e-5) error stop
e = [n, 2, 3]
if (size(h(spread(b, 1, n), k)) /= e(k)) error stop
e = [2, 3, n]
if (size(h(spread(b, 3, n), k)) /= e(k)) error stop
end subroutine

! A `spread` of an array with a runtime `dim`.
subroutine test_dim(n, d)
integer :: n, d
real :: a(2), b(2,3)
real :: r(2*n), s(6*n)
integer :: e(3)
a = [1.0, 2.0]
b = 1.0
if (size(g(spread(a, d, n))) /= 2*n) error stop
r = 2.0*g(spread(a, d, n))
if (abs(sum(r) - 6.0*n) > 1e-5) error stop
if (abs(r(1) - 2.0) > 1e-6) error stop
if (abs(r(2*n) - 4.0) > 1e-6) error stop
if (d == 1 .and. abs(r(2) - 2.0) > 1e-6) error stop
if (d == 2 .and. abs(r(2) - 4.0) > 1e-6) error stop
s = 2.0*g3(spread(b, d, n))
if (abs(sum(s) - 12.0*n) > 1e-5) error stop
e = [2, n, 3]
if (d == 1) e = [n, 2, 3]
if (size(h(spread(b, d, n), 1)) /= e(1)) error stop
if (size(h(spread(b, d, n), 2)) /= e(2)) error stop
if (size(h(spread(b, d, n), 3)) /= e(3)) error stop
end subroutine

end module

program functions_68
use functions_68_mod
implicit none
call test(3)
call test_rank3(5, 1)
call test_rank3(5, 2)
call test_rank3(5, 3)
call test_dim(5, 1)
call test_dim(5, 2)
print *, "ok"
end program
