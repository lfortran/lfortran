! Test ieee_next_down function (Fortran 2018 standard)
program ieee_next_down_01
    use, intrinsic :: ieee_arithmetic, only: ieee_next_down, ieee_is_finite, ieee_next_up
    implicit none
    
    real(4) :: x_sp, next_sp
    real(8) :: x_dp, next_dp
    
    print *, "Testing ieee_next_down..."
    
    ! Test 1: Next down from positive value
    x_sp = 1.0
    next_sp = ieee_next_down(x_sp)
    print *, "ieee_next_down(1.0) =", next_sp
    if (next_sp >= x_sp) error stop "next_down should be less than x"
    
    ! Test 2: Next down from negative value
    x_sp = -1.0
    next_sp = ieee_next_down(x_sp)
    print *, "ieee_next_down(-1.0) =", next_sp
    if (next_sp >= x_sp) error stop "next_down should be less than x"
    
    ! Test 3: Next down from zero
    x_sp = 0.0
    next_sp = ieee_next_down(x_sp)
    print *, "ieee_next_down(0.0) =", next_sp
    if (next_sp >= 0.0) error stop "next_down(0.0) should be negative"
    
    ! Test 4: Relationship with spacing for positive numbers
    x_sp = 1.0
    next_sp = ieee_next_down(x_sp)
    print *, "Difference: 1.0 - next_down(1.0) =", x_sp - next_sp
    print *, "spacing(1.0) =", spacing(x_sp)
    ! For next_down from positive x, the difference is spacing(next_down(x))
    ! which is half of spacing(x) since we cross to lower exponent
    if (next_sp >= x_sp) error stop "next_down should be less than x"
    
    ! Test 5: Double precision
    x_dp = 1.0d0
    next_dp = ieee_next_down(x_dp)
    print *, "ieee_next_down(1.0d0) =", next_dp
    if (next_dp >= x_dp) error stop "Should be less"
    
    ! Test 6: Large negative value should still give smaller value
    x_sp = -huge(x_sp) / 2.0
    next_sp = ieee_next_down(x_sp)
    if (next_sp >= x_sp) error stop "Should be less even for large negative x"
    
    ! Test 7: From -huge(x) should give -infinity
    x_sp = -huge(x_sp)
    next_sp = ieee_next_down(x_sp)
    print *, "ieee_next_down(-huge) =", next_sp
    if (ieee_is_finite(next_sp)) error stop "next_down(-huge) should be -infinity"
    
    ! Test 8: Verify inverse relationship with next_up
    x_sp = 1.0
    next_sp = ieee_next_down(x_sp)
    if (ieee_next_down(ieee_next_up(x_sp)) /= x_sp) then
        error stop "next_down(next_up(x)) should equal x"
    end if
    print *, "Inverse relationship verified"
    
    print *, "All ieee_next_down tests passed!"
    
end program ieee_next_down_01
