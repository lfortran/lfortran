program intrinsics_19c
! Test math intrinsics both declarations and executable statements.
! Single and double precision, complex only.
integer, parameter :: dp = kind(0.d0)

real, parameter :: &
    s1 = abs((0.5,0.5)), &
    s5 = aimag((0.4,0.5))
complex, parameter :: &
    s2 = exp((0.5,0.5)), &
    s3 = log((0.5,0.5)), &
    s4 = sqrt((0.5,0.5))

real(dp), parameter :: &
    d1 = abs((0.5_dp,0.5_dp)), &
    d5 = aimag((0.4,0.5))
complex(dp), parameter :: &
    d2 = exp((0.5_dp,0.5_dp)), &
    d3 = log((0.5_dp,0.5_dp)), &
    d4 = sqrt((0.5_dp,0.5_dp))

complex :: x
real :: x1 = -1
complex(dp) :: y

x = (0.5,0.5)
y = (0.5_dp,0.5_dp)

if (abs(-1) /= 1) error stop
if (abs(int(x1)) /= 1) error stop
if (abs(-1) /= 1) error stop
if (abs(x1) /= 1.0) error stop
if (abs(-1.0) /= 1) error stop
if (abs(abs((0.5, 0.5)) - 0.707106769) > 1e-7) error stop
if (abs(abs(x) - 0.707106769) > 1e-7) error stop
if (abs(abs(-x) - 0.707106769) > 1e-7) error stop
if (abs(abs((0.5_dp, 0.5_dp)) - 0.70710678118654757) > 1e-7) error stop
if (abs(abs(y) - 0.70710678118654757) > 1e-7) error stop
if (abs(abs(-y) - 0.70710678118654757) > 1e-7) error stop
print *, exp((0.5,0.5)), exp((0.5_dp,0.5_dp)), s2, d2, exp(x), exp(y)
print *, log((0.5,0.5)), log((0.5_dp,0.5_dp)), s3, d3, log(x), log(y)
print *, sqrt((0.5,0.5)), sqrt((0.5_dp,0.5_dp)), s4, d4, sqrt(x), sqrt(y)
x = (0.4,0.5)
y = (0.4_dp,0.5_dp)
print *, aimag((0.5,0.5)), aimag((0.5_dp,0.5_dp)), s5, d5, aimag(x), aimag(y)

end
