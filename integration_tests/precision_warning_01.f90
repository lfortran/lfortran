program precision_warning_01
integer, parameter :: dp = 8
real(dp) :: x, y
x = 1.3_dp
y = 1.3_dp
if (abs(x - y) > 1e-15_dp) error stop
print *, "ok"
end program
