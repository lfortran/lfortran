! Test sequence association: passing an array element from a multi-dimensional
! array to a function expecting a multi-dimensional dummy argument.
! The contiguous elements starting from the given position should be used.

real function f(x)
real x(2,2)
f = x(1,1) + x(2,1) + x(1,2) + x(2,2)
end function

program array_section_25
implicit none
real :: a(3,2), r
real :: f

a(1,1) = 0.0
a(2,1) = 0.0
a(3,1) = 1.0
a(1,2) = 2.0
a(2,2) = 3.0
a(3,2) = 4.0

! Sequence association: a(3,1) starts at the 3rd element in column-major order,
! and the next 4 contiguous elements are: a(3,1)=1, a(1,2)=2, a(2,2)=3, a(3,2)=4
r = f(a(3,1))
if (abs(r - 10.0) > 1.0e-6) error stop

! Also test with a different starting element
a(2,1) = 5.0
! From a(2,1): a(2,1)=5, a(3,1)=1, a(1,2)=2, a(2,2)=3 => sum = 11
r = f(a(2,1))
if (abs(r - 11.0) > 1.0e-6) error stop

print *, "PASS"
end program
