program intrinsics_517
use iso_c_binding, only: c_long_double
implicit none
! Compile-time sin, acos, and atan of real(10). That kind exists only where
! the C long double kind is 10. A kind parameter, not a literal 10, so this
! still compiles where real(10) is absent.
integer, parameter :: kp = c_long_double
real(kp), parameter :: a = acos(0.5_kp)
real(kp), parameter :: s = sin(1.0_kp)
real(kp), parameter :: t = atan(1.0_kp)
real(16), parameter :: aq = acos(0.5_16)
real(16), parameter :: sq = sin(1.0_16)
real(16), parameter :: tq = atan(1.0_16)
if (kp /= 10) stop 0
if (abs(a - 1.04719755119659774613_kp) > 1.0e-18_kp) error stop
if (abs(s - 0.841470984807896506665_kp) > 1.0e-18_kp) error stop
if (abs(t - 0.785398163397448309628_kp) > 1.0e-18_kp) error stop
! Kind 16 already folds and must keep doing so.
if (abs(aq - 1.04719755119659774615421446109316763_16) > 1.0e-30_16) error stop
if (abs(sq - 0.841470984807896506652502321630298999_16) > 1.0e-30_16) error stop
if (abs(tq - 0.785398163397448309615660845819875721_16) > 1.0e-30_16) error stop
end program
