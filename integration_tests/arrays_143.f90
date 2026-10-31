! Scalar operations on array constructors for the C++ backend:
! whole-array and section assignments materialize the constant once.
module arrays_143_mod
implicit none
contains
function f(a) result(x)
real, intent(in) :: a
real :: x(4)
x = a + [1.0, 2.0, 3.0, 4.0]
end function
end module
program arrays_143
use arrays_143_mod
implicit none
real :: p(4)
p = f(2.0)
if (abs(p(1) - 3.0) > 1e-6) error stop 1
if (abs(p(2) - 4.0) > 1e-6) error stop 2
if (abs(p(3) - 5.0) > 1e-6) error stop 3
if (abs(p(4) - 6.0) > 1e-6) error stop 4
p = 0.0
p(2:3) = 10.0 + [20.0, 30.0]
if (abs(p(1) - 0.0) > 1e-6) error stop 5
if (abs(p(2) - 30.0) > 1e-6) error stop 6
if (abs(p(3) - 40.0) > 1e-6) error stop 7
if (abs(p(4) - 0.0) > 1e-6) error stop 8
print *, p(2), p(3)
end program arrays_143
