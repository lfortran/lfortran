program intrinsics_74
implicit none
real :: x(1,2)
x = 0
print *, x
! 0.0 floating point number is exact, we can compare it directly in here,
! this makes the test robust, since we do not know what random number we get (can be very small)
if (x(1,1) /= 0.0) error stop
if (x(1,2) /= 0.0) error stop
call random_number(x(1,2))
print *, x
if (x(1,1) /= 0.0) error stop
if (x(1,2) == 0.0) error stop
end program
