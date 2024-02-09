program intrinsics_19
! Test intrinsics both declarations and executable statements.
! Single and double precision, real only.
integer, parameter :: dp = kind(0.d0)

real, parameter :: &
    s1 = abs(-0.5), &
    s2 = exp(0.5), &
    s3 = log(0.5), &
    s4 = erf(0.5), &
    s5 = erfc(0.5), &
    s6 = sqrt(0.5), &
    s7 = gamma(0.5), &
    s8 = atan2(0.5, 0.5), &
    s9 = log_gamma(0.5), &
    s9_ = log_gamma(log_gamma(0.5) + 3*log_gamma(0.5+log_gamma(0.5))), &
    s10 = log10(0.5)

integer, parameter :: &
    s11 = nint(3.6), &
    s12 = floor(3.6), &
    s13 = nint(-3.6), &
    s14 = floor(-3.6)

real(dp), parameter :: &
    d1 = abs(-0.5_dp), &
    d2 = exp(0.5_dp), &
    d3 = log(0.5_dp), &
    d4 = erf(0.5_dp), &
    d5 = erfc(0.5_dp), &
    d6 = sqrt(0.5_dp), &
    d7 = gamma(0.5_dp), &
    d8 = atan2(0.5_dp, 0.5_dp), &
    d9 = log_gamma(0.5_dp), &
    d9_ = log_gamma(log_gamma(0.5_dp) + 3*log_gamma(0.5_dp+log_gamma(0.5_dp))), &
    d10 = log10(0.5_dp)

integer, parameter :: &
    d11 = nint(3.6_dp), &
    d12 = floor(3.6_dp), &
    d13 = nint(-3.6_dp), &
    d14 = floor(-3.6_dp)

real :: x, x2
real(dp) :: y, y2

x = 0.5
y = 0.5_dp
x2 = 3.6
y2 = 3.6_dp

print *, abs(-0.5), abs(-0.5_dp), s1, d1, abs(-x), abs(-y)

print *, "Exp: ", exp(0.5), exp(0.5_dp), s2, d2, exp(x), exp(y)
if (abs(exp(0.5) - 1.64872122) > 1e-7) error stop
if (abs(exp(0.5_dp) - 1.64872127070012819) > 1e-7_dp) error stop
if (abs(s2 - 1.64872122) > 1e-7) error stop
if (abs(d2 - 1.64872127070012819) > 1e-7_dp) error stop
if (abs(exp(x) - 1.64872122) > 1e-7) error stop
if (abs(exp(y) - 1.64872127070012819) > 1e-7_dp) error stop

print *, "Log: ", log(0.5), log(0.5_dp), s3, d3, log(x), log(y)
if (abs(log(0.5) + 0.693147182) > 1e-7) error stop
if (abs(log(0.5_dp) + 0.693147180559945286) > 1e-7_dp) error stop
if (abs(s3 + 0.693147182) > 1e-7) error stop
if (abs(d3 + 0.693147180559945286) > 1e-7_dp) error stop
if (abs(log(x) + 0.693147182) > 1e-7) error stop
if (abs(log(y) + 0.693147180559945286) > 1e-7_dp) error stop

print *, "dlog10: ", dlog10(0.5_dp), dlog10(y2)
if (abs(dlog10(0.5_dp) + 0.301029995663981198) > 1e-7_dp) error stop
if (abs(dlog10(y2) - 0.556302500767287267) > 1e-7_dp) error stop

print *, "Erf: ", erf(0.5), erf(0.5_dp), s4, d4, erf(x), erf(y)
if (abs(erf(0.5) - 0.520499885) > 1e-7) error stop
if (abs(erf(0.5_dp) - 0.520499877813046519) > 1e-7_dp) error stop
if (abs(s4 - 0.520499885) > 1e-7) error stop
if (abs(d4 - 0.520499877813046519) > 1e-7_dp) error stop
if (abs(erf(x) - 0.520499885) > 1e-7) error stop
if (abs(erf(y) - 0.520499877813046519) > 1e-7_dp) error stop

print *, "Erfc: ", erfc(0.5), erfc(0.5_dp), s5, d5, erfc(x), erfc(y)
if (abs(erfc(0.5) - 0.479500115) > 1e-7) error stop
if (abs(erfc(0.5_dp) - 0.479500122186953481) > 1e-7_dp) error stop
if (abs(s5 - 0.479500115) > 1e-7) error stop
if (abs(d5 - 0.479500122186953481) > 1e-7_dp) error stop
if (abs(erfc(x) - 0.479500115) > 1e-7) error stop
if (abs(erfc(y) - 0.479500122186953481) > 1e-7_dp) error stop

print *, sqrt(0.5), sqrt(0.5_dp), s6, d6, sqrt(x), sqrt(y)
print *, gamma(0.5), gamma(0.5_dp), s7, d7, gamma(x), gamma(y)
print *, atan2(0.5, 0.5), atan2(0.5_dp, 0.5_dp), s8, d8, atan2(x,x), atan2(y,y)
print *, log_gamma(0.5), log_gamma(0.5_dp), s9, d9, log_gamma(x), log_gamma(y)
print *, s9_, log_gamma(log_gamma(x) + 3*log_gamma(x+log_gamma(x)))
print *, d9_, log_gamma(log_gamma(y) + 3*log_gamma(y+log_gamma(y)))

print *, "Log10: ", log10(0.5), log10(0.5_dp), s10, d10, log10(x), log10(y)
if (abs(log10(0.5) + 0.301030010) > 1e-7) error stop
if (abs(log10(0.5_dp) + 0.301029995663981198) > 1e-7_dp) error stop
if (abs(s10 + 0.301030010) > 1e-7) error stop
if (abs(d10 + 0.301029995663981198) > 1e-7_dp) error stop
if (abs(log10(x) + 0.301030010) > 1e-7) error stop
if (abs(log10(y) + 0.301029995663981198) > 1e-7_dp) error stop

print *, nint(3.6), nint(3.6_dp), s11, d11, nint(x2), nint(y2)
print *, floor(3.6), floor(3.6_dp), s12, d12, floor(x2), floor(y2)
print *, nint(-3.6), nint(-3.6_dp), s13, d13, nint(-x2), nint(-y2)
print *, floor(-3.6), floor(-3.6_dp), s14, d14, floor(-x2), floor(-y2)
if (abs(dsqrt(4.0_dp) - 2.0_dp) > 1e-15_dp) error stop
if (abs(dsqrt(y) - 0.70710678118654757_dp) > 1e-15_dp) error stop

if (abs(s9_ - log_gamma(log_gamma(x) + 3*log_gamma(x+log_gamma(x)))) > 1e-7) error stop
if (abs(d9_ - log_gamma(log_gamma(y) + 3*log_gamma(y+log_gamma(y)))) > 1e-15_dp) error stop
if (abs(s9 - 0.572364926) > 1e-7) error stop
if (abs(d9 - 0.57236494292470008) > 1e-7_dp) error stop
if (abs(s9_ - 0.656042993) > 1e-7) error stop
if (abs(d9_ - 0.65604298092238577) > 1e-7_dp) error stop

end program intrinsics_19
