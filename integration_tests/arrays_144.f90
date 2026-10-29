! Scalar operations on runtime implied-do constructors for the C++
! backend: the constructor is materialized into a sized temp view once,
! then used as an elementwise leaf.
module arrays_144_mod
implicit none
contains
function f(a, n) result(x)
real, intent(in) :: a
integer, intent(in) :: n
real :: x(n)
integer :: i
x = a + [(i, i=0, n-1)]
end function
end module
program arrays_144
use arrays_144_mod
implicit none
real :: p(4), y(3)
integer :: i, q(5)
p = f(2.0, 4)
if (abs(p(2) - 3.0) > 1e-6) error stop 1
if (abs(p(4) - 5.0) > 1e-6) error stop 2
y = [(i, i=0, 4, 2)]
if (abs(y(1) - 0.0) > 1e-6) error stop 3
if (abs(y(2) - 2.0) > 1e-6) error stop 4
if (abs(y(3) - 4.0) > 1e-6) error stop 5
q(1:3) = [0, (i, i=10, 11)]
if (q(1) /= 0) error stop 6
if (q(2) /= 10) error stop 7
if (q(3) /= 11) error stop 8
print *, p(2), y(3), q(2)
end program arrays_144
