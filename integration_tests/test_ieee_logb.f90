! Test ieee_logb function (IEEE unbiased exponent)
program ieee_logb_01
    use, intrinsic :: ieee_arithmetic, only: ieee_logb, ieee_is_finite
    implicit none
    
    real(4) :: x_sp, logb_sp
    real(8) :: x_dp, logb_dp
    
    print *, "Testing ieee_logb..."
    
    ! Test 1: logb(1.0) should be 0
    x_sp = 1.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(1.0) =", logb_sp
    if (abs(logb_sp) > 1e-6) error stop "logb(1.0) should be 0"
    
    ! Test 2: logb(2.0) should be 1
    x_sp = 2.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(2.0) =", logb_sp
    if (abs(logb_sp - 1.0) > 1e-6) error stop "logb(2.0) should be 1"
    
    ! Test 3: logb(4.0) should be 2
    x_sp = 4.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(4.0) =", logb_sp
    if (abs(logb_sp - 2.0) > 1e-6) error stop "logb(4.0) should be 2"
    
    ! Test 4: logb(0.5) should be -1
    x_sp = 0.5
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(0.5) =", logb_sp
    if (abs(logb_sp + 1.0) > 1e-6) error stop "logb(0.5) should be -1"
    
    ! Test 5: logb should be independent of sign
    x_sp = -8.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(-8.0) =", logb_sp
    if (abs(logb_sp - 3.0) > 1e-6) error stop "logb(-8.0) should be 3"
    
    ! Test 6: For any normal number, logb relates to exponent
    x_sp = 3.0  ! 3.0 = 1.5 * 2^1
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(3.0) =", logb_sp
    if (abs(logb_sp - 1.0) > 1e-6) error stop "logb(3.0) should be 1"
    
    ! Test 7: Double precision
    x_dp = 16.0d0
    logb_dp = ieee_logb(x_dp)
    print *, "ieee_logb(16.0d0) =", logb_dp
    if (abs(logb_dp - 4.0d0) > 1e-10) error stop "logb(16.0) should be 4"
    
    ! Test 8: Large number
    x_sp = 1024.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(1024.0) =", logb_sp
    if (abs(logb_sp - 10.0) > 1e-6) error stop "logb(1024.0) should be 10"
    
    ! Test 9: Relationship with exponent intrinsic
    x_sp = 100.0
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(100.0) =", logb_sp
    print *, "exponent(100.0) =", exponent(x_sp)
    ! logb(x) should equal exponent(x) - 1 for normal numbers
    if (abs(logb_sp - real(exponent(x_sp) - 1)) > 1e-6) then
        error stop "logb should match exponent - 1"
    end if
    
    ! Test 10: For values between 1 and 2, logb should be 0
    x_sp = 1.5
    logb_sp = ieee_logb(x_sp)
    print *, "ieee_logb(1.5) =", logb_sp
    if (abs(logb_sp) > 1e-6) error stop "logb(1.5) should be 0"
    
    print *, "All ieee_logb tests passed!"
    
end program ieee_logb_01
