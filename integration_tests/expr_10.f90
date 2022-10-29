program expr_10
integer, parameter :: dp = kind(0.d0)
real(dp) :: x

x = 2.2e-5_dp
print *, x
if (abs(x-2.2e-5_dp) > 1e-10_dp) error stop

x = 2.2d-5
print *, x
if (abs(x-2.2e-5_dp) > 1e-10_dp) error stop
end program
