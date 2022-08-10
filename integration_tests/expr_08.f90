program expr_08
implicit none
integer, parameter :: dp = kind(0.d0)
real(dp) :: x, y
y = 3
x = y**2
print *, y, x
if (abs(x - 9) > 1e-12_dp) error stop
end program
