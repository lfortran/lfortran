program intrinsics_03
use iso_fortran_env, only: dp => real64
real :: x
real(dp) :: a, r1, r2
a = 4.2_dp

x = cos(9.5)
if (abs(x + 0.997172177) > 1e-7) error stop
if (abs(cos(a) + 0.49026082) > 1e-7) error stop
if (abs(cos(cos(1.5) + cos(a+cos(a))) - 0.71640354) > 1e-7) error stop

r1 = dcos(a)
r2 = -0.4902608213406995_dp
if (dabs(r1-r2) > 1e-15_dp) error stop

end program intrinsics_03
