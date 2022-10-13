program sin_01
use iso_fortran_env, only: dp => real64
implicit none
real(dp) :: x, y, r1, r2
x = sin(1.5_dp)
if (abs(x - 0.997494996_dp) > 1e-5_dp) error stop
y = 10.5_dp
r1 = dsin(x)
r2 = -0.87969575997167_dp
if (dabs(r1-r2) > 1e15_dp) error stop
end program
