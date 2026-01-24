! Test ieee_rem function (IEEE remainder operation)
program ieee_rem_01
    use, intrinsic :: ieee_arithmetic, only: ieee_rem
    implicit none
    
    real(4) :: x_sp, y_sp, rem_sp
    real(8) :: x_dp, y_dp, rem_dp
    
    print *, "Testing ieee_rem..."
    
    ! Test 1: Basic remainder
    x_sp = 5.0
    y_sp = 3.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(5.0, 3.0) =", rem_sp
    ! 5.0 = 2 * 3.0 - 1.0, so remainder is -1.0 (rounds to nearest)
    if (abs(rem_sp + 1.0) > 1e-6) error stop "ieee_rem(5.0, 3.0) should be -1.0"
    
    ! Test 2: Different from modulo
    ! modulo(5.0, 3.0) = 2.0, but ieee_rem(5.0, 3.0) = -1.0
    print *, "modulo(5.0, 3.0) =", modulo(x_sp, y_sp)
    print *, "ieee_rem(5.0, 3.0) =", rem_sp
    if (abs(modulo(x_sp, y_sp) - rem_sp) < 1e-6) then
        error stop "ieee_rem should differ from modulo for this case"
    end if
    
    ! Test 3: When x = n*y exactly, remainder is 0
    x_sp = 6.0
    y_sp = 3.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(6.0, 3.0) =", rem_sp
    if (abs(rem_sp) > 1e-6) error stop "ieee_rem(6.0, 3.0) should be 0"
    
    ! Test 4: Negative dividend
    x_sp = -5.0
    y_sp = 3.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(-5.0, 3.0) =", rem_sp
    ! -5.0 = -2 * 3.0 + 1.0, so remainder is 1.0
    if (abs(rem_sp - 1.0) > 1e-6) error stop "ieee_rem(-5.0, 3.0) should be 1.0"
    
    ! Test 5: Small values
    x_sp = 0.5
    y_sp = 0.3
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(0.5, 0.3) =", rem_sp
    ! 0.5 = 2 * 0.3 - 0.1, so remainder is -0.1
    if (abs(rem_sp + 0.1) > 1e-5) error stop "ieee_rem(0.5, 0.3) should be approximately -0.1"
    
    ! Test 6: Double precision
    x_dp = 7.0d0
    y_dp = 4.0d0
    rem_dp = ieee_rem(x_dp, y_dp)
    print *, "ieee_rem(7.0d0, 4.0d0) =", rem_dp
    ! 7.0 = 2 * 4.0 - 1.0, so remainder is -1.0
    if (abs(rem_dp + 1.0d0) > 1e-10) error stop "ieee_rem(7.0, 4.0) should be -1.0"
    
    ! Test 7: Remainder is always less than or equal to |y|/2
    x_sp = 10.0
    y_sp = 3.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(10.0, 3.0) =", rem_sp
    if (abs(rem_sp) > abs(y_sp) / 2.0 + 1e-6) then
        error stop "remainder should be <= |y|/2"
    end if
    
    ! Test 8: Symmetry test
    x_sp = 8.0
    y_sp = 3.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(8.0, 3.0) =", rem_sp
    ! 8.0 = 3 * 3.0 - 1.0, so remainder is -1.0
    if (abs(rem_sp + 1.0) > 1e-6) error stop "ieee_rem(8.0, 3.0) should be -1.0"
    
    ! Test 9: Compare with mod for exact multiples
    x_sp = 12.0
    y_sp = 4.0
    rem_sp = ieee_rem(x_sp, y_sp)
    print *, "ieee_rem(12.0, 4.0) =", rem_sp
    if (abs(rem_sp) > 1e-6) error stop "Should be zero for exact multiples"
    if (abs(rem_sp - mod(x_sp, y_sp)) > 1e-6) then
        error stop "Should match mod for exact multiples"
    end if
    
    print *, "All ieee_rem tests passed!"
    
end program ieee_rem_01
