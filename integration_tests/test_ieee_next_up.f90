! Test ieee_next_up function (Fortran 2018 standard)
program ieee_next_up_01
    use, intrinsic :: ieee_arithmetic, only: ieee_next_up, ieee_is_finite
    implicit none
    
    real(4) :: x_sp, next_sp
    real(8) :: x_dp, next_dp
    
    print *, "Testing ieee_next_up..."
    
    ! Test 1: Next up from positive value
    x_sp = 1.0
    next_sp = ieee_next_up(x_sp)
    print *, "ieee_next_up(1.0) =", next_sp
    if (next_sp <= x_sp) error stop "next_up should be greater than x"
    
    ! Test 2: Next up from negative value
    x_sp = -1.0
    next_sp = ieee_next_up(x_sp)
    print *, "ieee_next_up(-1.0) =", next_sp
    if (next_sp <= x_sp) error stop "next_up should be greater than x"
    if (next_sp >= 0.0) error stop "next_up(-1.0) should still be negative"
    
    ! Test 3: Next up from zero
    x_sp = 0.0
    next_sp = ieee_next_up(x_sp)
    print *, "ieee_next_up(0.0) =", next_sp
    if (next_sp <= 0.0) error stop "next_up(0.0) should be positive"
    
    ! Test 4: Relationship with spacing for positive numbers
    x_sp = 1.0
    next_sp = ieee_next_up(x_sp)
    print *, "Difference: next_up(1.0) - 1.0 =", next_sp - x_sp
    print *, "spacing(1.0) =", spacing(x_sp)
    if (abs((next_sp - x_sp) - spacing(x_sp)) > 1e-10) then
        error stop "next_up - x should equal spacing(x) for positive x"
    end if
    
    ! Test 5: Double precision
    x_dp = 1.0d0
    next_dp = ieee_next_up(x_dp)
    print *, "ieee_next_up(1.0d0) =", next_dp
    if (next_dp <= x_dp) error stop "Should be greater"
    
    ! Test 6: Large positive value should still give larger value
    x_sp = huge(x_sp) / 2.0
    next_sp = ieee_next_up(x_sp)
    if (next_sp <= x_sp) error stop "Should be greater even for large x"
    
    ! Test 7: From huge(x) should give infinity
    x_sp = huge(x_sp)
    next_sp = ieee_next_up(x_sp)
    print *, "ieee_next_up(huge) =", next_sp
    if (ieee_is_finite(next_sp)) error stop "next_up(huge) should be infinity"
    
    print *, "All ieee_next_up tests passed!"
    
end program ieee_next_up_01
