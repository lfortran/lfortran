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
end program
