program minmax_03
! A NaN in the first argument of MIN or MAX does not hide a later
! numeric argument. L-BFGS-B relies on that to discard a failed step.
implicit none
double precision :: nan, one, two
double precision :: zero
zero = 0.0d0
nan = zero / zero
one = 1.0d0
two = 2.0d0
if (max(nan, one) /= one) error stop 1
if (min(nan, one) /= one) error stop 2
if (max(nan, one, two) /= two) error stop 3
if (min(nan, two, one) /= one) error stop 4
print *, max(nan, one)
end program
