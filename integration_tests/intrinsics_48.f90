program intrinsics_48
implicit none

real :: x(3)

x = [1., 2., 3.]
x = log_gamma(x) + log_gamma(x + 1)
print *, x
if (x(1) /= 0.0) error stop
if (abs(x(2) - 0.693147182) > 1e-12) error stop
if (abs(x(3) - 2.48490667) > 1e-12) error stop

end program intrinsics_48
