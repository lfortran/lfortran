program test_ieee_arithmetic_05
    ! Test ieee_class classification and comparison of ieee_class_type
    ! and ieee_round_type values with operator(==) and operator(/=)
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(4) :: x_sp
    real(8) :: x_dp

    ! --- single precision ---

    ! comparing the results of two ieee_class calls (original bug)
    x_sp = ieee_value(x_sp, ieee_quiet_nan)
    if (.not. (ieee_class(x_sp) == ieee_class((x_sp)))) error stop 1
    if (ieee_class(x_sp) /= ieee_class(x_sp)) error stop 2

    if (ieee_class(x_sp) /= ieee_quiet_nan) error stop 3

    x_sp = ieee_value(x_sp, ieee_positive_inf)
    if (ieee_class(x_sp) /= ieee_positive_inf) error stop 4

    x_sp = ieee_value(x_sp, ieee_negative_inf)
    if (ieee_class(x_sp) /= ieee_negative_inf) error stop 5

    x_sp = 1.5_4
    if (ieee_class(x_sp) /= ieee_positive_normal) error stop 6

    x_sp = -1.5_4
    if (ieee_class(x_sp) /= ieee_negative_normal) error stop 7

    x_sp = ieee_value(x_sp, ieee_positive_zero)
    if (ieee_class(x_sp) /= ieee_positive_zero) error stop 8

    x_sp = ieee_value(x_sp, ieee_negative_zero)
    if (ieee_class(x_sp) /= ieee_negative_zero) error stop 9

    x_sp = tiny(x_sp)
    x_sp = x_sp / 2.0_4
    if (ieee_class(x_sp) /= ieee_positive_denormal) error stop 10

    x_sp = -tiny(x_sp)
    x_sp = x_sp / 2.0_4
    if (ieee_class(x_sp) /= ieee_negative_denormal) error stop 11

    ! classes of distinct values must compare unequal
    if (ieee_class(1.0_4) == ieee_class(-1.0_4)) error stop 12

    ! --- double precision ---

    x_dp = ieee_value(x_dp, ieee_quiet_nan)
    if (.not. (ieee_class(x_dp) == ieee_class((x_dp)))) error stop 13
    if (ieee_class(x_dp) /= ieee_quiet_nan) error stop 14

    x_dp = ieee_value(x_dp, ieee_positive_inf)
    if (ieee_class(x_dp) /= ieee_positive_inf) error stop 15

    x_dp = ieee_value(x_dp, ieee_negative_inf)
    if (ieee_class(x_dp) /= ieee_negative_inf) error stop 16

    x_dp = 1.5_8
    if (ieee_class(x_dp) /= ieee_positive_normal) error stop 17

    x_dp = -1.5_8
    if (ieee_class(x_dp) /= ieee_negative_normal) error stop 18

    x_dp = ieee_value(x_dp, ieee_positive_zero)
    if (ieee_class(x_dp) /= ieee_positive_zero) error stop 19

    x_dp = ieee_value(x_dp, ieee_negative_zero)
    if (ieee_class(x_dp) /= ieee_negative_zero) error stop 20

    x_dp = tiny(x_dp)
    x_dp = x_dp / 2.0_8
    if (ieee_class(x_dp) /= ieee_positive_denormal) error stop 21

    x_dp = -tiny(x_dp)
    x_dp = x_dp / 2.0_8
    if (ieee_class(x_dp) /= ieee_negative_denormal) error stop 22

    if (ieee_class(1.0_8) == ieee_class(-1.0_8)) error stop 23

    ! --- ieee_round_type comparisons ---

    if (.not. (ieee_nearest == ieee_nearest)) error stop 24
    if (ieee_nearest /= ieee_nearest) error stop 25
    if (ieee_nearest == ieee_up) error stop 26
    if (.not. (ieee_to_zero /= ieee_down)) error stop 27

    print *, "ok"
end program test_ieee_arithmetic_05
