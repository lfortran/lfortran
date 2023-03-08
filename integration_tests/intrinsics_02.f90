program intrinsics_02
real :: x = sin(1.5)
real(8) :: y

y = sin(1.5)
print *, x, y

if (x /= 0.997494996) error stop
if (sin(x) /= 0.840114892) error stop
if (abs(y - 0.997494995) > 1e-7) error stop
if (abs(sin(sin(1.5) + sin(0.5+sin(0.5))) - 0.967188418) > 1e-7) error stop
if (abs(sin(sin(y) + sin(x+sin(x))) - 0.97276188) > 1e-7) error stop

end
