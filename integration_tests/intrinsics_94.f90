program intrinsics_94
real :: x,y
real(kind=8) :: a, b
x = 3.0
y = 4.0
print *, hypot(x,y)
if (abs(hypot(x,y) - 5.0) > 1e-6) error stop
print *, hypot(3.0D0, 4.0D0)
if (abs(hypot(3.0D0, 4.0D0) - 5.0D0) > 1e-9) error stop
a = 12.0D0
b = 5.0D0
print *, hypot(a,b)
if (abs(hypot(a,b) - 13.0D0) > 1e-9) error stop
print *, hypot(12.0D0, 5.0D0)
if (abs(hypot(12.0D0, 5.0D0) - 13.0D0) > 1e-9) error stop
end program
