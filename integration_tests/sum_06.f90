! Reductions over elemental intrinsic functions of operations on array
! sections, which are accumulated element by element without an array
! temporary for the operations
module sum_06_mod
implicit none

integer :: ncalls = 0

contains

    real function shift()
        ncalls = ncalls + 1
        shift = 0.25
    end function

    ! The largest difference of neighbouring elements, as in a convergence
    ! check
    real function max_jump(x, n) result(r)
        real, intent(in) :: x(:)
        integer, intent(in) :: n
        r = maxval(abs(x(2:n) - x(1:n-1)))
    end function

    ! Sections of an assumed-shape dummy with a stride and lower bounds
    real function strided_l1(x, y, lo, hi, st) result(r)
        real, intent(in) :: x(0:), y(:)
        integer, intent(in) :: lo, hi, st
        r = sum(abs(x(lo:hi:st) - y(1:(hi - lo)/st + 1)))
    end function

end module

program sum_06
use sum_06_mod
implicit none
integer, parameter :: n = 9
real :: a(n), b(n), c(2:n+1), s, r, e(0:n-1)
real(8) :: d(n), sd
integer :: i, k, ia(n), ib(n), m
logical :: l
complex :: z(n)

do i = 1, n
    a(i) = real(i * i) / 7 - 3
    b(i) = real(mod(5 * i, n)) / 4
    c(i + 1) = real(n - i) / 3
    e(i - 1) = real(i) / 2
    d(i) = real(i, 8) / 3 - 1
    ia(i) = mod(7 * i, 11) - 5
    ib(i) = i - 4
    z(i) = cmplx(a(i), b(i))
end do

! sum, maxval, minval, product, count, any and all of elemental intrinsics
s = sum(abs(a(1:n) - b(1:n)))
r = 0
do i = 1, n
    r = r + abs(a(i) - b(i))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 1

s = maxval(abs(a(2:n) - a(1:n-1)))
r = -huge(r)
do i = 2, n
    r = max(r, abs(a(i) - a(i-1)))
end do
if (s /= r) error stop 2
if (max_jump(a, n) /= r) error stop 3

s = minval(sqrt(abs(a(1:n:2) * b(n:1:-2))))
r = huge(r)
do i = 1, n, 2
    r = min(r, sqrt(abs(a(i) * b(n + 1 - i))))
end do
if (s /= r) error stop 4

s = sum(sqrt(abs(a(1:n) * b(1:n))))
r = 0
do i = 1, n
    r = r + sqrt(abs(a(i) * b(i)))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 5

m = count(abs(a(1:n) - b(1:n)) > 0.5)
k = 0
do i = 1, n
    if (abs(a(i) - b(i)) > 0.5) k = k + 1
end do
if (m /= k) error stop 6

l = any(abs(a(2:n) - c(2:n)) < 0.1)
if (l .neqv. any([(abs(a(i) - c(i)) < 0.1, i = 2, n)])) error stop 7
l = all(exp(-abs(a - b)) > 0)
if (.not. l) error stop 8

s = product(max(a(1:4) - b(1:4), 0.5))
r = 1
do i = 1, 4
    r = r * max(a(i) - b(i), 0.5)
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 9

! Nested elemental intrinsics, whole arrays and lower bounds
s = sum(abs(sin(a(1:n) - c(2:n+1)) * e(0:n-1)))
r = 0
do i = 1, n
    r = r + abs(sin(a(i) - c(i + 1)) * e(i - 1))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 10

s = sum(abs(a - b))
r = 0
do i = 1, n
    r = r + abs(a(i) - b(i))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 11

s = maxval(exp(a) * log(b + 1))
r = -huge(r)
do i = 1, n
    r = max(r, exp(a(i)) * log(b(i) + 1))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 12

! In a mask, with integer, real(8) and complex operands
s = sum(a, mask = abs(a(1:n) - b(1:n)) > 1)
r = 0
do i = 1, n
    if (abs(a(i) - b(i)) > 1) r = r + a(i)
end do
if (abs(s - r) > 1e-5 * max(1.0, abs(r))) error stop 13

m = sum(iabs(ia(1:n) - ib(1:n)))
k = 0
do i = 1, n
    k = k + iabs(ia(i) - ib(i))
end do
if (m /= k) error stop 14

m = maxval(mod(ia(2:n) * ib(1:n-1), 5))
k = -huge(k)
do i = 2, n
    k = max(k, mod(ia(i) * ib(i - 1), 5))
end do
if (m /= k) error stop 15

sd = sum(abs(d(1:n) - 0.5d0))
if (abs(sd - sum([(abs(d(i) - 0.5d0), i = 1, n)])) > 1d-12) error stop 16

s = sum(abs(z(1:n) - (1.0, 1.0)))
r = 0
do i = 1, n
    r = r + abs(z(i) - (1.0, 1.0))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 17

! In an expression and in an IF condition
s = 1 + sum(abs(a(1:n) - b(1:n))) / 2
r = 0
do i = 1, n
    r = r + abs(a(i) - b(i))
end do
if (abs(s - (1 + r / 2)) > 1e-5 * abs(r)) error stop 18
if (maxval(abs(a(2:n) - a(1:n-1))) < 0) error stop 19

! An assumed-shape dummy with a stride and lower bounds
s = strided_l1(e, b, 1, 7, 3)
r = abs(e(1) - b(1)) + abs(e(4) - b(2)) + abs(e(7) - b(3))
if (abs(s - r) > 1e-5 * abs(r)) error stop 20

! Zero-size sections
k = 0
if (sum(abs(a(1:k) - b(1:k))) /= 0) error stop 21
if (maxval(abs(a(2:k) - a(1:k-1))) /= -huge(s)) error stop 22
if (count(abs(a(1:k) - b(1:k)) > 0) /= 0) error stop 23

! A scalar function operand of the operation is evaluated once
ncalls = 0
s = sum(abs(a(1:n) - shift()))
if (ncalls /= 1) error stop 24
r = 0
do i = 1, n
    r = r + abs(a(i) - 0.25)
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 25
ncalls = 0
s = maxval(abs(a(1:n) * shift()))
if (ncalls /= 1) error stop 26
ncalls = 0
s = minval(sqrt(abs(a(1:n) + shift())))
if (ncalls /= 1) error stop 27

! Aliasing of the target with the operand
a(1) = sum(abs(a(1:n) - b(1:n)))
r = abs(real(1) / 7 - 3 - b(1))
do i = 2, n
    r = r + abs(a(i) - b(i))
end do
if (abs(a(1) - r) > 1e-5 * abs(r)) error stop 28

print *, "ok"
end program
