program data_01
integer :: x
real :: y, z, c1(4), bf1, xx90, xx95
data x /1/
data y, z /2.0, 3.0/
data c1 /0.0, 0.22, -0.14, -0.21/
data bf1 /0.8/, xx90, xx95 /0.55, 0.62/
print *, x, y, z
if (x /= 1) error stop
if (abs(y-2.0) > 1e-5) error stop
if (abs(z-3.0) > 1e-5) error stop
if (abs(bf1-0.8) > 1e-5) error stop
if (abs(xx90-0.55) > 1e-5) error stop
if (abs(xx95-0.62) > 1e-5) error stop
end program
