program main
real :: y(2) = real([2, 3])
if (abs(y(1) - 2.0) > 1e-7) error stop
if (abs(y(2) - 3.0) > 1e-7) error stop
print *, y
end program
