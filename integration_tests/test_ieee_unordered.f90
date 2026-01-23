! Test ieee_unordered function
program test_ieee_unordered
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp, y_sp, nan_sp, tmp_sp
    real(real64) :: x_dp, y_dp
    logical :: result
    
    print *, "Testing ieee_unordered..."
    
    tmp_sp = 1.0_real32
    
    ! Test 1: Two normal values (ordered)
    x_sp = 1.0_real32
    y_sp = 2.0_real32
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(1.0, 2.0) = ", result
    if (result) error stop "1.0 and 2.0 should be ordered"
    
    ! Test 2: Same values (ordered)
    x_sp = 5.0_real32
    y_sp = 5.0_real32
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(5.0, 5.0) = ", result
    if (result) error stop "5.0 and 5.0 should be ordered"
    
    ! Test 3: Zero and non-zero (ordered)
    x_sp = 0.0_real32
    y_sp = 1.0_real32
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(0.0, 1.0) = ", result
    if (result) error stop "0.0 and 1.0 should be ordered"
    
    ! Test 4: Negative values (ordered)
    x_sp = -3.0_real32
    y_sp = -7.0_real32
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(-3.0, -7.0) = ", result
    if (result) error stop "-3.0 and -7.0 should be ordered"
    
    ! Test 5: First is NaN (unordered)
    nan_sp = ieee_value(tmp_sp, ieee_quiet_nan)
    y_sp = 1.0_real32
    result = ieee_unordered(nan_sp, y_sp)
    print *, "ieee_unordered(NaN, 1.0) = ", result
    if (.not. result) error stop "NaN and 1.0 should be unordered"
    
    ! Test 6: Second is NaN (unordered)
    x_sp = 2.0_real32
    result = ieee_unordered(x_sp, nan_sp)
    print *, "ieee_unordered(2.0, NaN) = ", result
    if (.not. result) error stop "2.0 and NaN should be unordered"
    
    ! Test 7: Both are NaN (unordered)
    result = ieee_unordered(nan_sp, nan_sp)
    print *, "ieee_unordered(NaN, NaN) = ", result
    if (.not. result) error stop "NaN and NaN should be unordered"
    
    ! Test 8: Infinity and normal (ordered)
    x_sp = ieee_value(tmp_sp, ieee_positive_inf)
    y_sp = 100.0_real32
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(+inf, 100.0) = ", result
    if (result) error stop "+inf and 100.0 should be ordered"
    
    ! Test 9: Double precision normal values (ordered)
    x_dp = 1.5_real64
    y_dp = 2.5_real64
    result = ieee_unordered(x_dp, y_dp)
    print *, "ieee_unordered(1.5d0, 2.5d0) = ", result
    if (result) error stop "1.5d0 and 2.5d0 should be ordered"
    
    ! Test 10: Infinities (ordered)
    x_sp = ieee_value(tmp_sp, ieee_positive_inf)
    y_sp = ieee_value(tmp_sp, ieee_negative_inf)
    result = ieee_unordered(x_sp, y_sp)
    print *, "ieee_unordered(+inf, -inf) = ", result
    if (result) error stop "+inf and -inf should be ordered"
    
    print *, "All ieee_unordered tests passed!"
    
end program test_ieee_unordered
