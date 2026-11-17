program real16_gamma_poles_01
! gamma and log_gamma of real(16) arguments at their poles return
! infinity, like real(8) (#17857): gamma(+-0) = +-Inf and
! log_gamma(0), log_gamma(-n), log_gamma(-Inf) = +Inf, both folded and at
! run time. gamma(-n) and gamma(-Inf) stay NaN. The folded values overflow,
! so GFortran needs -fno-range-check to accept the parameters.
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, &
    ieee_value, ieee_positive_inf
implicit none
integer, parameter :: qp = 16
real(qp), parameter :: pg0 = gamma(0.0_qp), pgm0 = gamma(-0.0_qp)
real(qp), parameter :: pl0 = log_gamma(0.0_qp), plm1 = log_gamma(-1.0_qp), &
    plm10 = log_gamma(-10.0_qp)
real(qp), volatile :: z, m1, m10, inf

z = 0
m1 = -1
m10 = -10
inf = ieee_value(inf, ieee_positive_inf)

call check_pinf(gamma(z), 1)
call check_minf(gamma(-z), 2)
call check_pinf(log_gamma(z), 3)
call check_pinf(log_gamma(-z), 4)
call check_pinf(log_gamma(m1), 5)
call check_pinf(log_gamma(m10), 6)
call check_pinf(log_gamma(-inf), 7)
call check_pinf(log_gamma(inf), 8)
call check_pinf(gamma(inf), 9)
if (.not. ieee_is_nan(gamma(m1))) error stop "gamma(-1) is not NaN"
if (.not. ieee_is_nan(gamma(m10))) error stop "gamma(-10) is not NaN"
if (.not. ieee_is_nan(gamma(-inf))) error stop "gamma(-Inf) is not NaN"

call check_pinf(pg0, 10)
call check_minf(pgm0, 11)
call check_pinf(pl0, 12)
call check_pinf(plm1, 13)
call check_pinf(plm10, 14)
print *, "ok"

contains

subroutine check_pinf(v, k)
real(qp), intent(in) :: v
integer, intent(in) :: k
if (ieee_is_nan(v) .or. ieee_is_finite(v) .or. v < 0) then
    print *, "case", k, ": expected +Infinity"
    error stop
end if
end subroutine

subroutine check_minf(v, k)
real(qp), intent(in) :: v
integer, intent(in) :: k
if (ieee_is_nan(v) .or. ieee_is_finite(v) .or. v > 0) then
    print *, "case", k, ": expected -Infinity"
    error stop
end if
end subroutine

end program
