! Test ieee_is_normal function
program test_ieee_is_normal
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp, inf_sp, nan_sp, denorm_sp, tmp_sp
    real(real64) :: x_dp
    logical :: result
    
    print *, "Testing ieee_is_normal..."
    
    ! Test 1: Normal positive value
    x_sp = 1.0_real32
    result = ieee_is_normal(x_sp)
    print *, "ieee_is_normal(1.0) = ", result
    if (.not. result) error stop "1.0 should be normal"
    
    ! Test 2: Normal negative value
    x_sp = -5.0_real32
    result = ieee_is_normal(x_sp)
    print *, "ieee_is_normal(-5.0) = ", result
    if (.not. result) error stop "-5.0 should be normal"
    
    ! Test 3: Large normal value
    x_sp = 1.0e30_real32
    result = ieee_is_normal(x_sp)
    print *, "ieee_is_normal(1.0e30) = ", result
    if (.not. result) error stop "1.0e30 should be normal"
    
    ! Test 4: Small normal value (just above tiny)
    tmp_sp = 0.0_real32
    x_sp = tiny(tmp_sp)
    result = ieee_is_normal(x_sp)
    print *, "ieee_is_normal(tiny) = ", result
    if (.not. result) error stop "tiny should be normal"
    
    ! Test 5: Zero (not normal)
    x_sp = 0.0_real32
    result = ieee_is_normal(x_sp)
    print *, "ieee_is_normal(0.0) = ", result
    if (result) error stop "0.0 should not be normal"
    
    ! Test 6: Infinity (not normal)
    tmp_sp = 1.0_real32
    inf_sp = ieee_value(tmp_sp, ieee_positive_inf)
    result = ieee_is_normal(inf_sp)
    print *, "ieee_is_normal(+inf) = ", result
    if (result) error stop "+inf should not be normal"
    
    ! Test 7: Negative infinity (not normal)
    inf_sp = ieee_value(tmp_sp, ieee_negative_inf)
    result = ieee_is_normal(inf_sp)
    print *, "ieee_is_normal(-inf) = ", result
    if (result) error stop "-inf should not be normal"
    
    ! Test 8: NaN (not normal)
    nan_sp = ieee_value(tmp_sp, ieee_quiet_nan)
    result = ieee_is_normal(nan_sp)
    print *, "ieee_is_normal(NaN) = ", result
    if (result) error stop "NaN should not be normal"
    
    ! Test 9: Denormal number (not normal)
    denorm_sp = ieee_value(tmp_sp, ieee_positive_denormal)
    result = ieee_is_normal(denorm_sp)
    print *, "ieee_is_normal(denormal) = ", result
    if (result) error stop "Denormal should not be normal"
    
    ! Test 10: Double precision normal
    x_dp = 2.5_real64
    result = ieee_is_normal(x_dp)
    print *, "ieee_is_normal(2.5d0) = ", result
    if (.not. result) error stop "2.5d0 should be normal"
    
    print *, "All ieee_is_normal tests passed!"
    
end program test_ieee_is_normal
