program precision_warning_01
implicit none
integer, parameter :: dp = kind(1.0d0)
real(dp) :: x, y
! `1.3` is single precision, so `x` holds the single precision value of 1.3
x = 1.3
y = 1.3_dp
if (x == y) error stop
if (abs(x - y) > 1e-7_dp) error stop
if (x /= real(1.3, dp)) error stop
print *, "ok"
end program
