program real16_atan2_01
use iso_c_binding, only: c_long_double
implicit none
! Compile-time atan2 and atan2d of real(16) and x87 real(10) constants in
! every quadrant (#17816). Kind 10 exists only where the C long double kind
! is 10, so it is a kind parameter, not a literal 10.
integer, parameter :: kp = c_long_double
real(16), parameter :: a1 = atan2(1.0_16, 1.0_16)
real(16), parameter :: a2 = atan2(-1.0_16, -2.0_16)
real(16), parameter :: a3 = atan2(0.5_16, -3.0_16)
real(16), parameter :: a4 = atan2(-3.0_16, 0.0_16)
real(16), parameter :: a5 = atan2(0.0_16, -1.0_16)
real(16), parameter :: a6 = atan2(-0.0_16, 1.0_16)
real(16), parameter :: d1 = atan2d(1.0_16, -1.0_16)
real(16), parameter :: d2 = atan2d(-1.0_16, -2.0_16)
real(16), parameter :: d3 = atan2d(2.0_16, 7.0_16)
real(16), parameter :: d4 = atan2d(-2.0_16, 0.0_16)
real(kp), parameter :: b1 = atan2(-1.0_kp, -2.0_kp)
real(kp), parameter :: b2 = atan2(0.5_kp, -3.0_kp)
real(kp), parameter :: e1 = atan2d(1.0_kp, -1.0_kp)
real(kp), parameter :: e2 = atan2d(-1.0_kp, -2.0_kp)

if (abs(a1 - 0.785398163397448309615660845819875721_16) > 1.0e-30_16) error stop
if (abs(a2 - (-2.67794504458898712224838715181828864_16)) > 1.0e-30_16) error stop
if (abs(a3 - 2.97644397617516640018351509363555947_16) > 1.0e-30_16) error stop
if (abs(a4 - (-1.57079632679489661923132169163975144_16)) > 1.0e-30_16) error stop
if (abs(a5 - 3.14159265358979323846264338327950288_16) > 1.0e-30_16) error stop
if (a6 /= 0.0_16 .or. sign(1.0_16, a6) > 0.0_16) error stop
if (abs(d1 - 135.0_16) > 1.0e-30_16) error stop
if (abs(d2 - (-153.434948822922010648427806279546717_16)) > 1.0e-30_16) error stop
if (abs(d3 - 15.9453959009228547976576895232610959_16) > 1.0e-30_16) error stop
if (abs(d4 - (-90.0_16)) > 1.0e-30_16) error stop

if (kp /= 10) stop 0
if (abs(b1 - (-2.67794504458898712223_kp)) > 1.0e-18_kp) error stop
if (abs(b2 - 2.97644397617516640021_kp) > 1.0e-18_kp) error stop
if (abs(e1 - 135.0_kp) > 1.0e-16_kp) error stop
if (abs(e2 - (-153.434948822922010647_kp)) > 1.0e-16_kp) error stop
end program
