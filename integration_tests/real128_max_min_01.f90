program real128_max_min_01
use iso_fortran_env, only: real128
implicit none
real(real128) :: x, y
x = 1.0_real128
y = max(2.5, x)
print *, y
if (y /= 2.5_real128) error stop 1
y = min(2.5, x)
if (y /= 1.0_real128) error stop 2
y = max(x, 3.5d0)
if (y /= 3.5_real128) error stop 3
y = min(0.25d0, x, 4.0)
if (y /= 0.25_real128) error stop 4
end program
