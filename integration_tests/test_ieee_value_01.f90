program test_ieee_value_01
    use ieee_arithmetic
    implicit none
    double precision :: xdp, snan, qnan, pinf, ninf

    snan = ieee_value(xdp, ieee_signaling_nan)
    qnan = ieee_value(xdp, ieee_quiet_nan)
    pinf = ieee_value(xdp, ieee_positive_inf)
    ninf = ieee_value(xdp, ieee_negative_inf)

    if (.not. ieee_is_nan(snan)) error stop "signaling NaN should be NaN"
    if (.not. ieee_is_nan(qnan)) error stop "quiet NaN should be NaN"
    if (pinf <= 0.0d0) error stop "positive inf should be positive"
    if (ninf >= 0.0d0) error stop "negative inf should be negative"
    if (pinf + ninf == pinf + ninf) error stop "inf + (-inf) should be NaN"

end program test_ieee_value_01
