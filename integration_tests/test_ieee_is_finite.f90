! Test ieee_is_finite function
program test_ieee_is_finite
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp, inf_sp, nan_sp, tmp_sp
    real(real64) :: x_dp
    logical :: result
    
    print *, "Testing ieee_is_finite..."
    
    tmp_sp = 1.0_real32
    
    ! Test 1: Normal finite value
    x_sp = 1.0_real32
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(1.0) = ", result
    if (.not. result) error stop "1.0 should be finite"
    
    ! Test 2: Negative finite value
    x_sp = -100.0_real32
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(-100.0) = ", result
    if (.not. result) error stop "-100.0 should be finite"
    
    ! Test 3: Zero (finite)
    x_sp = 0.0_real32
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(0.0) = ", result
    if (.not. result) error stop "0.0 should be finite"
    
    ! Test 4: Large but finite value
    x_sp = huge(tmp_sp) * 0.5_real32
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(large) = ", result
    if (.not. result) error stop "Large value should be finite"
    
    ! Test 5: Tiny but finite value
    x_sp = tiny(tmp_sp)
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(tiny) = ", result
    if (.not. result) error stop "Tiny value should be finite"
    
    ! Test 6: Positive infinity (not finite)
    inf_sp = ieee_value(tmp_sp, ieee_positive_inf)
    result = ieee_is_finite(inf_sp)
    print *, "ieee_is_finite(+inf) = ", result
    if (result) error stop "+inf should not be finite"
    
    ! Test 7: Negative infinity (not finite)
    inf_sp = ieee_value(tmp_sp, ieee_negative_inf)
    result = ieee_is_finite(inf_sp)
    print *, "ieee_is_finite(-inf) = ", result
    if (result) error stop "-inf should not be finite"
    
    ! Test 8: NaN (not finite)
    nan_sp = ieee_value(tmp_sp, ieee_quiet_nan)
    result = ieee_is_finite(nan_sp)
    print *, "ieee_is_finite(NaN) = ", result
    if (result) error stop "NaN should not be finite"
    
    ! Test 9: Double precision finite
    x_dp = 3.14159_real64
    result = ieee_is_finite(x_dp)
    print *, "ieee_is_finite(3.14159d0) = ", result
    if (.not. result) error stop "3.14159d0 should be finite"
    
    ! Test 10: Denormal (finite)
    x_sp = ieee_value(tmp_sp, ieee_positive_denormal)
    result = ieee_is_finite(x_sp)
    print *, "ieee_is_finite(denormal) = ", result
    if (.not. result) error stop "Denormal should be finite"
    
    print *, "All ieee_is_finite tests passed!"
    
end program test_ieee_is_finite
