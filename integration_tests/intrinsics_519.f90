program intrinsics_519
use iso_c_binding, only: c_long_double
implicit none
! x87 real(10): digits is 64, maxexponent is 16384, minexponent is -16381.
! That kind exists only where c_long_double is 10. A kind parameter, not a
! literal 10, so this still compiles where real(10) is absent.
integer, parameter :: kp = c_long_double
real(kp) :: x
if (kp /= 10) stop 0
x = 1.0_kp
if (digits(x) /= 64) error stop
if (digits(1.0_kp) /= 64) error stop
if (maxexponent(x) /= 16384) error stop
if (maxexponent(1.0_kp) /= 16384) error stop
if (minexponent(x) /= -16381) error stop
if (minexponent(1.0_kp) /= -16381) error stop
end program
