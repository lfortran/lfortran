! Array-valued function calls with array expression arguments for the
! C++ backend: actual args materialize into temps, results copy back.
module arrays_142_mod
contains
function outprod(x, y) result(z)
real, intent(in) :: x(2), y(2)
real :: z(2, 2)
integer :: i, j
do j = 1, 2
do i = 1, 2
z(i, j) = x(i)*y(j)
end do
end do
end function
end module
program arrays_142
use arrays_142_mod
implicit none
real :: a(2, 2), x(2), y(2)
real :: alpha
a = 1.0
x = 2.0
y = 3.0
a = outprod(0.5*x, y)
if (abs(a(1, 1) - 3.0) > 1e-6) error stop 1
if (abs(a(2, 2) - 3.0) > 1e-6) error stop 2
a = 1.0
alpha = 0.5
a = a + outprod(alpha*x, y)
if (abs(a(1, 1) - 4.0) > 1e-6) error stop 3
if (abs(a(2, 2) - 4.0) > 1e-6) error stop 4
print *, a(1, 1), a(2, 2)
end program arrays_142
