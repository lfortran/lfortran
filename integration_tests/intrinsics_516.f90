program intrinsics_516
use iso_c_binding, only: c_long_double
implicit none
! abs, max, min, sign, and mod of x87 real(10) constants. That kind exists
! only where the C long double kind is 10 (Linux x86-64). A kind parameter,
! not a literal 10, so this still compiles where real(10) is absent
! (Apple arm64 and MSVC, where c_long_double is 8).
integer, parameter :: kp = c_long_double
real(kp) :: a, b, c, s, m
integer :: n, i
if (kp /= 10) stop 0
a = abs(-1.5_kp)
b = max(2.5_kp, 1.5_kp)
c = min(2.5_kp, 1.5_kp)
s = sign(1.5_kp, -2.0_kp)
m = mod(7.5_kp, 2.0_kp)
if (a /= 1.5_kp) error stop
if (b /= 2.5_kp) error stop
if (c /= 1.5_kp) error stop
if (s /= -1.5_kp) error stop
if (m /= 1.5_kp) error stop
! sqrt, nint, and int of kind 10 already folded correctly and must stay so.
if (sqrt(4.0_kp) /= 2.0_kp) error stop
n = nint(2.75_kp)
i = int(2.75_kp)
if (n /= 3) error stop
if (i /= 2) error stop
! Same pointer-as-double fold for the other kind-10 constant evaluators.
if (aint(2.75_kp) /= 2.0_kp) error stop
if (anint(2.75_kp) /= 3.0_kp) error stop
if (modulo(7.5_kp, 2.0_kp) /= 1.5_kp) error stop
if (dim(5.0_kp, 2.0_kp) /= 3.0_kp) error stop
if (hypot(3.0_kp, 4.0_kp) /= 5.0_kp) error stop
if (spacing(1.0_kp) /= 2.0_kp**(-63)) error stop
if (scale(1.5_kp, 2) /= 6.0_kp) error stop
if (nearest(1.0_kp, 1.0_kp) /= 1.0_kp + 2.0_kp**(-63)) error stop
! erfl and gfortran's real(10) erf differ by one ulp. spacing(1) is two
! ulps there, so this accepts that and still rejects the denormal fold.
if (abs(erf(1.0_kp) - 0.842700792949714869368_kp) > spacing(1.0_kp)) error stop
end program
