! Bounds inquiries do not require defined array elements.
program arrays_136
implicit none
real :: a(3)
real :: b(2, 2)
if (lbound(a, 1) /= 1) error stop 1
if (ubound(a, 1) /= 3) error stop 2
if (lbound(b, 1) /= 1) error stop 3
if (ubound(b, 1) /= 2) error stop 4
if (lbound(b, 2) /= 1) error stop 5
if (ubound(b, 2) /= 2) error stop 6
print *, lbound(a, 1), ubound(a, 1)
end program arrays_136
