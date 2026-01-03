program test_abs
implicit none
real :: x
x = abs(1.5)
if (x < 0) error stop
x = abs(-1.5)
if (x < 0) error stop
end program
