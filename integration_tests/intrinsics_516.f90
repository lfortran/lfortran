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
! Same pointer-as-double fold. The unfixed results are 1, or an address
! for bessel_yn. spacing(1) still rejects those.
if (abs(erfc(1.0_kp) - 0.157299207050285130659_kp) > spacing(1.0_kp)) error stop
if (abs(erfc_scaled(1.0_kp) - 0.427583576155807004406_kp) > spacing(1.0_kp)) error stop
if (abs(bessel_j0(1.0_kp) - 0.765197686557966551453_kp) > spacing(1.0_kp)) error stop
if (abs(bessel_jn(0, 1.0_kp) - 0.765197686557966551453_kp) > spacing(1.0_kp)) error stop
if (abs(bessel_yn(0, 1.0_kp) - 8.82569642156769579796e-2_kp) > spacing(1.0_kp)) error stop
! Kind-10 exponent is not the binary32 bit formula. The low 32 bits of
! 1.0 are 0, so that formula returns -126. fraction and set_exponent
! call the same exponent.
if (exponent(1.0_kp) /= 1) error stop
if (abs(fraction(1.0_kp) - 0.5_kp) > spacing(1.0_kp)) error stop
if (set_exponent(1.5_kp, 3) /= 6.0_kp) error stop
! Kind 10 has 64 binary digits. rrspacing(1) is 2**63, not the zero
! left when the compile-time fraction is never filled in.
if (rrspacing(1.0_kp) /= 2.0_kp**63) error stop
! The math_func macro still folded these kind-10 constants from the
! pointer bits (a denormal, 90, 1, or a huge Bessel value). A few ulps
! is the long-double libm difference; those wrong folds are not.
if (abs(bessel_j1(1.0_kp) - 0.440050585744933515957_kp) &
        > 16*spacing(0.440050585744933515957_kp)) error stop
if (abs(bessel_y0(1.0_kp) - 8.82569642156769579796e-2_kp) &
        > 16*spacing(8.82569642156769579796e-2_kp)) error stop
if (abs(bessel_y1(1.0_kp) - (-0.781212821300288716550_kp)) &
        > 16*spacing(0.781212821300288716550_kp)) error stop
if (abs(asind(0.5_kp) - 29.9999999999999999983_kp) &
        > 16*spacing(29.9999999999999999983_kp)) error stop
if (abs(acosd(0.5_kp) - 59.9999999999999999965_kp) &
        > 16*spacing(59.9999999999999999965_kp)) error stop
if (abs(atand(1.0_kp) - 45.0_kp) &
        > 16*spacing(45.0_kp)) error stop
if (abs(sind(30.0_kp) - 0.5_kp) &
        > 16*spacing(0.5_kp)) error stop
if (abs(cosd(60.0_kp) - 0.5_kp) &
        > 16*spacing(0.5_kp)) error stop
if (abs(tand(45.0_kp) - 1.0_kp) &
        > 16*spacing(1.0_kp)) error stop
! The unary evaluators still folded kind 10 from the pointer bits:
! gamma(5) was Infinity, log_gamma(1) was about 712, and log10(100)
! was about -309. These three values are exact in every real kind.
if (gamma(5.0_kp) /= 24.0_kp) error stop
if (log_gamma(1.0_kp) /= 0.0_kp) error stop
if (log10(100.0_kp) /= 2.0_kp) error stop
end program
