! A `reduce` accumulator is live after the loop even when nothing after the
! loop in the same body reads it: an intent(out) dummy at the end of a
! subroutine, or a loop nested in another construct.
module gpu_metal_380_mod
implicit none
contains

subroutine dot(n, x, y, s)
integer, intent(in) :: n
real, intent(in) :: x(n), y(n)
real, intent(out) :: s
integer :: i
s = 0.0
do concurrent (i = 1:n) reduce(+:s)
    s = s + x(i)*y(i)
end do
end subroutine

subroutine amax(n, x, m)
integer, intent(in) :: n
real, intent(in) :: x(n)
real, intent(inout) :: m
integer :: i
do concurrent (i = 1:n) reduce(max:m)
    m = max(m, abs(x(i)))
end do
end subroutine

integer function count_pos(n, x) result(c)
integer, intent(in) :: n
real, intent(in) :: x(n)
integer :: i
c = 0
do concurrent (i = 1:n) reduce(+:c)
    if (x(i) > 0.0) c = c + 1
end do
end function

end module

program gpu_metal_380
use gpu_metal_380_mod, only: dot, amax, count_pos
implicit none
integer, parameter :: n = 1000
real :: x(n), y(n), s, m, totals(3)
integer :: i, k

x = 1.0
y = 2.0
call dot(n, x, y, s)
print *, s
if (abs(s - 2000.0) > 1e-3) error stop 1

x(7) = -5.0
m = 0.0
call amax(n, x, m)
print *, m
if (abs(m - 5.0) > 1e-6) error stop 2

if (count_pos(n, x) /= n - 1) error stop 3

do k = 1, 3
    s = real(k)
    do concurrent (i = 1:n) reduce(+:s)
        s = s + y(i)
    end do
    totals(k) = s
end do
print *, totals
if (any(abs(totals - [2001.0, 2002.0, 2003.0]) > 1e-3)) error stop 4
end program
