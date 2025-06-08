program intrinsics_316
real :: x(2)
x = [1.2, 2.3]
print *, max(0., x)
if (any(abs(max(0., x) - [1.2, 2.3]) > 1e-5)) error stop
print *, max([0., 1.], x)
if (any(abs(max([0., 1.], x) - [1.2, 2.3]) > 1e-5)) error stop
print *, min(0., x)
if (any(abs(min(0., x) - [0.0, 0.0]) > 1e-5)) error stop
print *, min([0., 1.], x)
if (any(abs(min([0., 1.], x) - [0.0, 1.0]) > 1e-5)) error stop
end program