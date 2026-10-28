! Array constructors for the C/C++ backends: constructor assignment
! stores each element into the array descriptor, including non-constant
! elements and implied-do loops.
program arrays_137
implicit none
real :: a(3)
integer :: b(2)
real :: e(3)
real :: x, y, z
integer :: i, j
a = [1.0, 2.0, 3.0]
if (abs(a(1) - 1.0) > 1e-6) error stop 1
if (abs(a(2) - 2.0) > 1e-6) error stop 2
if (abs(a(3) - 3.0) > 1e-6) error stop 3
b = [4, 5]
if (b(1) /= 4) error stop 4
if (b(2) /= 5) error stop 5
x = 7.0
y = 8.0
z = 9.0
e = [x, y, z]
if (abs(e(1) - 7.0) > 1e-6) error stop 6
if (abs(e(2) - 8.0) > 1e-6) error stop 7
if (abs(e(3) - 9.0) > 1e-6) error stop 8
e = [(j*1.0, j=1,3)]
do i = 1, 3
    if (abs(e(i) - i) > 1e-6) error stop 9
end do
print *, a(2)
end program arrays_137
