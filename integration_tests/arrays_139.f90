! Partial array sections for the C/C++ backends: section-to-section
! copies and sections on the right-hand side lower to offset loops.
program arrays_139
implicit none
real :: a(4), b(4)
real :: c(3)
b = 2.0
a = 0.0
a(1:3) = b(2:4)
if (abs(a(1) - 2.0) > 1e-6) error stop 1
if (abs(a(2) - 2.0) > 1e-6) error stop 2
if (abs(a(3) - 2.0) > 1e-6) error stop 3
if (abs(a(4) - 0.0) > 1e-6) error stop 4
c = 0.0
c = b(1:3)
if (abs(c(2) - 2.0) > 1e-6) error stop 5
a = 0.0
a(2:3) = b(1:2) + 1.0
if (abs(a(1) - 0.0) > 1e-6) error stop 6
if (abs(a(2) - 3.0) > 1e-6) error stop 7
if (abs(a(3) - 3.0) > 1e-6) error stop 8
if (abs(a(4) - 0.0) > 1e-6) error stop 9
print *, a(2), c(2)
end program arrays_139
