program cos_01
use iso_fortran_env, only: dp => real64
implicit none
real(dp) :: x
x = cos(1.5_dp)
print *, x
if (abs(x - 7.07372016677029064e-02) > 1e-6) error stop
end program

