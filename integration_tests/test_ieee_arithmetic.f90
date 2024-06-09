program test_ieee_arithmetic
    use iso_fortran_env, only : sp => real32, dp => real64
    use ieee_arithmetic
    implicit none
    real(4) :: a = -1.0_sp, b = 1.0_sp
    real(8) :: x = 7.9_dp, y = -3.0_dp
    
    print*, ieee_is_nan(-1.0)
    if (ieee_is_nan(-1.0) .neqv. .false.) error stop
    print*, ieee_is_finite(-1.0)
    if (ieee_is_finite(-1.0) .neqv. .true.) error stop
    print*, ieee_is_negative(-1.0)
    if (ieee_is_negative(-1.0) .neqv. .true.) error stop
    print*, ieee_copy_sign(-1.0, 1.0)
    if (ieee_copy_sign(-1.0, 1.0) /= 1.0) error stop
    print*, ieee_support_datatype(-1.0)
    if (ieee_support_datatype(-1.0) .neqv. .true.) error stop
    print*, ieee_is_normal(-1.0)
    if (ieee_is_normal(-1.0) .neqv. .true.) error stop
    print*, ieee_unordered(-1.0, 1.0)
    if (ieee_unordered(-1.0, 1.0) .neqv. .false.) error stop
    print*, ieee_logb(-1.0)
    if (ieee_logb(-1.0) /= 0) error stop
    print*, ieee_rem(-1.0, 1.0)
    if (ieee_rem(-1.0, 1.0) /= 0) error stop

    print*, ieee_is_nan(a)
    if (ieee_is_nan(a) .neqv. .false.) error stop
    print*, ieee_is_finite(a)
    if (ieee_is_finite(a) .neqv. .true.) error stop
    print*, ieee_is_negative(a)
    if (ieee_is_negative(a) .neqv. .true.) error stop
    print*, ieee_copy_sign(a, b)
    if (abs(ieee_copy_sign(a, b) - 1.0_sp) > 1e-6) error stop
    print*, ieee_support_datatype(a)
    if (ieee_support_datatype(a) .neqv. .true.) error stop
    print*, ieee_is_normal(a)
    if (ieee_is_normal(a) .neqv. .true.) error stop
    print*, ieee_unordered(a, b)
    if (ieee_unordered(a, b) .neqv. .false.) error stop
    print*, ieee_logb(a)
    if (ieee_logb(a) /= 0) error stop
    print*, ieee_rem(a, b)
    if (abs(ieee_rem(a, b) - 0) > 1e-6) error stop


    print*, ieee_is_nan(x)
    if (ieee_is_nan(x) .neqv. .false.) error stop
    print*, ieee_is_finite(x)
    if (ieee_is_finite(x) .neqv. .true.) error stop
    print*, ieee_is_negative(x)
    if (ieee_is_negative(x) .neqv. .false.) error stop
    print*, ieee_copy_sign(x, y)
    if (abs(ieee_copy_sign(x, y) - (-7.9)) > 1e-6) error stop
    print*, ieee_support_datatype(x)
    if (ieee_support_datatype(x) .neqv. .true.) error stop
    print*, ieee_is_normal(x)
    if (ieee_is_normal(x) .neqv. .true.) error stop
    print*, ieee_unordered(x, y)
    if (ieee_unordered(x, y) .neqv. .false.) error stop
    print*, ieee_logb(x)
    if (ieee_logb(x) /= 2.0) error stop
    print*, ieee_rem(x, y)
    if (abs(ieee_rem(x, y) - -1.0999999046325684) > 1e-6_dp) error stop
    
end program
