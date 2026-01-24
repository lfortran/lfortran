! Test ieee_copy_sign function
program test_ieee_copy_sign
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp, y_sp, r_sp
    real(real64) :: x_dp, y_dp, r_dp
    
    print *, "Testing ieee_copy_sign..."
    
    ! Test 1: Positive magnitude, positive sign
    x_sp = 5.0_real32
    y_sp = 3.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(5.0, 3.0) = ", r_sp
    if (r_sp /= 5.0_real32) error stop "Should be positive 5.0"
    
    ! Test 2: Positive magnitude, negative sign
    x_sp = 5.0_real32
    y_sp = -3.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(5.0, -3.0) = ", r_sp
    if (r_sp /= -5.0_real32) error stop "Should be negative 5.0"
    
    ! Test 3: Negative magnitude, positive sign
    x_sp = -7.0_real32
    y_sp = 2.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(-7.0, 2.0) = ", r_sp
    if (r_sp /= 7.0_real32) error stop "Should be positive 7.0"
    
    ! Test 4: Negative magnitude, negative sign
    x_sp = -7.0_real32
    y_sp = -2.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(-7.0, -2.0) = ", r_sp
    if (r_sp /= -7.0_real32) error stop "Should be negative 7.0"
    
    ! Test 5: Zero magnitude, positive sign
    x_sp = 0.0_real32
    y_sp = 1.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(0.0, 1.0) = ", r_sp
    if (.not. (r_sp == 0.0_real32 .and. .not. ieee_is_negative(r_sp))) then
        error stop "Should be positive zero"
    end if
    
    ! Test 6: Zero magnitude, negative sign
    x_sp = 0.0_real32
    y_sp = -1.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(0.0, -1.0) = ", r_sp
    if (.not. (r_sp == 0.0_real32 .and. ieee_is_negative(r_sp))) then
        error stop "Should be negative zero"
    end if
    
    ! Test 7: Double precision
    x_dp = 12.5_real64
    y_dp = -8.0_real64
    r_dp = ieee_copy_sign(x_dp, y_dp)
    print *, "ieee_copy_sign(12.5d0, -8.0d0) = ", r_dp
    if (r_dp /= -12.5_real64) error stop "Should be -12.5d0"
    
    ! Test 8: Copy sign from negative zero
    x_sp = 4.0_real32
    y_sp = -0.0_real32
    r_sp = ieee_copy_sign(x_sp, y_sp)
    print *, "ieee_copy_sign(4.0, -0.0) = ", r_sp
    if (r_sp /= -4.0_real32) error stop "Should be -4.0"
    
    print *, "All ieee_copy_sign tests passed!"
    
end program test_ieee_copy_sign
