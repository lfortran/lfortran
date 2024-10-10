program test_tan
implicit none
real :: x
x = tan(1.5)
print *, x
if (abs(x - 14.1014204) > 1e-5) error stop
end program
