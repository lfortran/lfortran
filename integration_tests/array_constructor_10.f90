program array_constructor_10
use array_constructor_10_m
implicit none
real :: c
call s([1, -1, 2], c)
print *, c
if (abs(c - 2.0) > 1e-6) error stop
end program
