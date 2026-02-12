program test_ieee_arithmetic_fast_01
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf, ieee_is_finite
    implicit none
    real :: x, nan_val, inf_val
    
    ! Test ieee_value with ieee_quiet_nan
    nan_val = ieee_value(x, ieee_quiet_nan)
    if (.not. ieee_is_nan(nan_val)) error stop "ieee_value with ieee_quiet_nan failed"
    
    ! Test ieee_value with ieee_positive_inf
    inf_val = ieee_value(x, ieee_positive_inf)
    if (ieee_is_finite(inf_val)) error stop "ieee_value with ieee_positive_inf failed"
    if (ieee_is_nan(inf_val)) error stop "inf should not be NaN"
    
    print *, "PASS"
end program test_ieee_arithmetic_fast_01
