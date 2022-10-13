program intrinsics_03
use iso_fortran_env, only: dp => real64
real :: x
real(dp) :: a, r1, r2
x = cos(9.5)
print *, x
a = 4.2_dp
r1 = dcos(a)
r2 = -0.4902608213406995_dp
if (dabs(r1-r2) > 1e-15_dp) error stop
end
