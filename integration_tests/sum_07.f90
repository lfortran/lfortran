! MAXVAL, MINVAL and SUM accumulated in a loop evaluate each element of
! their operand once, and call each function in the operand as often as
! Fortran requires
module sum_07_mod
implicit none

integer :: nscalar = 0, nelemental = 0, narray = 0

contains

    real function scale()
        nscalar = nscalar + 1
        scale = 2
    end function

    impure elemental real function twice(x)
        real, intent(in) :: x
        nelemental = nelemental + 1
        twice = 2 * x
    end function

    function doubled(x) result(r)
        real, intent(in) :: x(:)
        real :: r(size(x))
        narray = narray + 1
        r = 2 * x
    end function

    subroutine reset()
        nscalar = 0
        nelemental = 0
        narray = 0
    end subroutine

end module

program sum_07
use sum_07_mod
implicit none
integer, parameter :: n = 7
real :: a(n), b(n), s, r
integer :: i, ia(n), m, k

do i = 1, n
    a(i) = real(mod(3 * i, n)) / 2 - 1
    b(i) = real(i) / 4
    ia(i) = mod(5 * i, n) - 3
end do

! Expensive elements, evaluated once each
s = maxval(exp(a) * log(b + 1))
r = -huge(r)
do i = 1, n
    r = max(r, exp(a(i)) * log(b(i) + 1))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 1
s = minval(exp(a(1:n:2)) * log(b(n:1:-2) + 1))
r = huge(r)
do i = 1, n, 2
    r = min(r, exp(a(i)) * log(b(n + 1 - i) + 1))
end do
if (abs(s - r) > 1e-5 * abs(r)) error stop 2
m = maxval(ia(2:n) * ia(1:n-1), mask = ia(2:n) > 0)
k = -huge(k) - 1
do i = 2, n
    if (ia(i) > 0) k = max(k, ia(i) * ia(i - 1))
end do
if (m /= k) error stop 3

! A scalar function in the operand is called once
call reset()
s = maxval(a(1:n) * scale())
if (nscalar /= 1) error stop 4
if (s /= 2 * maxval(a)) error stop 5
call reset()
s = minval(a + scale())
if (nscalar /= 1) error stop 6
if (s /= minval(a) + 2) error stop 7
call reset()
s = sum(a(1:n) * scale())
if (nscalar /= 1) error stop 8
call reset()
s = maxval(abs(a(1:n) - scale()), mask = b > 0.5)
if (nscalar /= 1) error stop 9

! An impure elemental function is called once per element (MAXVAL and
! MINVAL are checked in sum_09)
call reset()
s = sum(twice(a) * b)
if (nelemental /= n) error stop 13

! An array function is called once
call reset()
s = maxval(doubled(a) * b)
if (narray /= 1) error stop 14
call reset()
s = minval(doubled(a(1:n)) + 1)
if (narray /= 1) error stop 15
if (s /= 2 * minval(a) + 1) error stop 16
call reset()
s = sum(doubled(a) - b)
if (narray /= 1) error stop 17

print *, "ok"
end program
