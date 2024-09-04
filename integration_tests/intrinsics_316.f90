program intrinsics_316
real :: x(2)
x = [1.2, 2.3]
print *, max(0., x)
if (any(min(0., x) - [1.20000005e+00, 2.29999995e+00] > 1e-6)) error stop
print *, max([0., 1.], x)
if (any(max([0., 1.], x) - [1.20000005e+00, 2.29999995e+00] > 1e-6)) error stop
print *, min(0., x)
if (any(min(0., x) - [0.00000000e+00, 0.00000000e+00] > 1e-6)) error stop
print *, min([0., 1.], x)
if (any(min([0., 1.], x) - [0.00000000e+00, 1.00000000e+00] > 1e-6)) error stop
end program