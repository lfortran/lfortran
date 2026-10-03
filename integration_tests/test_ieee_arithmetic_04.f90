program test_ieee_arithmetic_04
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(4) :: x_sp
    real(8) :: x_dp

    ! --- single precision ---
    if (ieee_support_datatype(x_sp)) then

        x_sp = ieee_value(x_sp, ieee_negative_inf)
        if (.not. ieee_is_negative(x_sp)) error stop
        if (ieee_is_finite(x_sp)) error stop
        if (.not. ieee_is_nan(x_sp) .and. x_sp >= 0.0) error stop

        x_sp = ieee_value(x_sp, ieee_positive_inf)
        if (ieee_is_negative(x_sp)) error stop
        if (ieee_is_finite(x_sp)) error stop
        if (.not. ieee_is_nan(x_sp) .and. x_sp <= 0.0) error stop

        x_sp = ieee_value(x_sp, ieee_quiet_nan)
        if (.not. ieee_is_nan(x_sp)) error stop
        if (x_sp == x_sp) error stop

        x_sp = ieee_value(x_sp, ieee_positive_zero)
        if (.not. ieee_is_finite(x_sp)) error stop
        if (x_sp /= 0.0) error stop

        x_sp = ieee_value(x_sp, ieee_negative_zero)
        if (.not. ieee_is_finite(x_sp)) error stop
        if (x_sp /= 0.0) error stop

    end if

    ! --- double precision ---
    if (ieee_support_datatype(x_dp)) then

        x_dp = ieee_value(x_dp, ieee_negative_inf)
        if (.not. ieee_is_negative(x_dp)) error stop
        if (ieee_is_finite(x_dp)) error stop
        if (.not. ieee_is_nan(x_dp) .and. x_dp >= 0.0d0) error stop

        x_dp = ieee_value(x_dp, ieee_positive_inf)
        if (ieee_is_negative(x_dp)) error stop
        if (ieee_is_finite(x_dp)) error stop
        if (.not. ieee_is_nan(x_dp) .and. x_dp <= 0.0d0) error stop

        x_dp = ieee_value(x_dp, ieee_quiet_nan)
        if (.not. ieee_is_nan(x_dp)) error stop
        if (x_dp == x_dp) error stop

        x_dp = ieee_value(x_dp, ieee_positive_zero)
        if (.not. ieee_is_finite(x_dp)) error stop
        if (x_dp /= 0.0d0) error stop

        x_dp = ieee_value(x_dp, ieee_negative_zero)
        if (.not. ieee_is_finite(x_dp)) error stop
        if (x_dp /= 0.0d0) error stop

    end if

end program test_ieee_arithmetic_04
