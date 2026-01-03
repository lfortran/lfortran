program expr_08
implicit none
integer, parameter :: dp = kind(0.d0)
real :: x4, y4
real(dp) :: x, y
y4 = 3
x4 = y4 ** 2
print *, y4, x4
if (abs(x4 - 9) > 1e-6) error stop

y = 3
x = y**2
print *, y, x
if (abs(x - 9) > 1e-12_dp) error stop
end program
