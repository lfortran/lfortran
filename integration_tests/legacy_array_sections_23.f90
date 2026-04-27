! Test sequence association: passing a 2D array element to a function
! expecting an explicit-shape 2D array. The callee's array should map
! contiguous storage starting from the element passed.
program legacy_array_sections_23
implicit none
real :: a(3,4)
real :: result

a = 0.0
a(2,3) = 1.0
a(3,3) = 10.0
a(1,4) = 100.0
a(2,4) = 1000.0
a(3,4) = 10000.0

! Pass a(3,3) — contiguous elements from there are:
! a(3,3)=10, a(1,4)=100, a(2,4)=1000, a(3,4)=10000
! These map to x(1,1), x(2,1), x(1,2), x(2,2) in column-major
result = ff(a(3,3))
if (abs(result - 11110.0) > 1.0e-6) error stop

! Test with a different starting point
a = 0.0
a(1,1) = 2.0
a(2,1) = 3.0
a(3,1) = 5.0
a(1,2) = 7.0
result = ff(a(1,1))
if (abs(result - 17.0) > 1.0e-6) error stop

print *, "PASSED"

contains
real function ff(x)
  real :: x(2,2)
  ff = x(1,1) + x(2,1) + x(1,2) + x(2,2)
end function
end program
