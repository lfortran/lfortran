program data_03
integer :: x, td
real :: y, z, c1(4), bf1, xx90, xx95, b, p
print *, x, y, z
if (x /= 1) error stop
if (abs(y-2.0) > 1e-5) error stop
if (abs(z-3.0) > 1e-5) error stop
if (abs(c1(1)-0.0) > 1e-5) error stop
if (abs(c1(2)-0.22) > 1e-5) error stop
data x, td, b /1, 4, 3/
if (abs(c1(4)+0.21) > 1e-5) error stop
if (abs(bf1-0.8) > 1e-5) error stop
data y, z /2.0, 3.0/
if (abs(xx90-0.55) > 1e-5) error stop
if (abs(xx95-0.62) > 1e-5) error stop
data c1 /0.0, 0.22, -0.14, -0.21/
p = b ** (td-1)
if (abs(p-27.0) > 1e-5) error stop
data bf1 /0.8/, xx90, xx95 /0.55, 0.62/
end program
