! Array constructors for the C/C++ backends: constructor assignment
! stores each element into the array descriptor.
program arrays_137
implicit none
real :: a(3)
integer :: b(2)
integer :: i
a = [1.0, 2.0, 3.0]
if (abs(a(1) - 1.0) > 1e-6) error stop 1
if (abs(a(2) - 2.0) > 1e-6) error stop 2
if (abs(a(3) - 3.0) > 1e-6) error stop 3
b = [4, 5]
if (b(1) /= 4) error stop 4
if (b(2) /= 5) error stop 5
print *, a(2)
end program arrays_137
