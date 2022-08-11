program data_01
integer :: x
real :: y, z
data x /1/
data y, z /2.0, 3.0/
print *, x, y, z
if (x /= 1) error stop
if (abs(y-2.0) > 1e-5) error stop
if (abs(z-3.0) > 1e-5) error stop
end program
