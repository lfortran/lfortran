program complex_16
implicit none
complex :: x
real :: y

x % re = 1.2
print *, x
x % im = -3.4
print *, x
if (abs(x - (1.2, -3.4)) > 1e-6) error stop
y = sign(12.38, x % im)
print *, y
if (abs(y - (-12.38)) > 1e-6) error stop
end program
