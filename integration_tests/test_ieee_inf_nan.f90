! Test for https://github.com/lfortran/lfortran/issues/7735
! Inf and NaN detection with ieee_value
program test_ieee_inf_nan
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: xdp(4)

    xdp = [ieee_value(1.0_dp, ieee_positive_inf), &
           ieee_value(1.0_dp, ieee_negative_inf), &
           ieee_value(1.0_dp, ieee_quiet_nan), &
           ieee_value(1.0_dp, ieee_signaling_nan)]

    if (.not. (abs(xdp(1)) > huge(xdp(1)))) error stop "+Inf check failed"
    if (.not. (abs(xdp(2)) > huge(xdp(2)))) error stop "-Inf check failed"
    if (.not. (xdp(3) /= xdp(3))) error stop "qNaN check failed"
    if (.not. (xdp(4) /= xdp(4))) error stop "sNaN check failed"

    print *, "PASSED: Inf/NaN detection"
end program test_ieee_inf_nan
