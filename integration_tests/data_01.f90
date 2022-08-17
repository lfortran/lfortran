program data_01
integer :: x
real :: y, z, c1(4)
data x /1/
data y, z /2.0, 3.0/
data c1 /0.0, 0.22, -0.14, -0.21/
print *, x, y, z
if (x /= 1) error stop
if (abs(y-2.0) > 1e-5) error stop
if (abs(z-3.0) > 1e-5) error stop
if (abs(c1(1)-0.0) > 1e-5) error stop
if (abs(c1(2)-0.22) > 1e-5) error stop
if (abs(c1(4)+0.21) > 1e-5) error stop
end program
