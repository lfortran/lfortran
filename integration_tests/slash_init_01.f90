program slash_init_01
implicit none

! Scalar slash-initialization without ::
integer j/-20/, k/20/
integer m/42/
real x/1.0/

! Scalar slash-initialization (no :: needed for this legacy syntax)
integer n/100/

! Array slash-initialization
integer arr(3)/1,2,3/

! Verify scalar values
if (j /= -20) error stop
if (k /= 20) error stop
if (m /= 42) error stop
if (x /= 1.0) error stop
if (n /= 100) error stop

! Verify array values
if (arr(1) /= 1) error stop
if (arr(2) /= 2) error stop
if (arr(3) /= 3) error stop

print *, "All tests passed"
end program
