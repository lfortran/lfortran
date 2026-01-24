! Test ieee_is_negative function
program ieee_is_negative_01
    use, intrinsic :: ieee_arithmetic, only: ieee_is_negative
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(4) :: x_sp, neg_zero_sp
    real(8) :: x_dp, neg_zero_dp
    
    print *, "Testing ieee_is_negative..."
    
    ! Test 1: Positive value
    x_sp = 1.0
    print *, "ieee_is_negative(1.0) =", ieee_is_negative(x_sp)
    if (ieee_is_negative(x_sp)) error stop "1.0 should not be negative"
    
    ! Test 2: Negative value
    x_sp = -1.0
    print *, "ieee_is_negative(-1.0) =", ieee_is_negative(x_sp)
    if (.not. ieee_is_negative(x_sp)) error stop "-1.0 should be negative"
    
    ! Test 3: Positive zero
    x_sp = 0.0
    print *, "ieee_is_negative(+0.0) =", ieee_is_negative(x_sp)
    if (ieee_is_negative(x_sp)) error stop "+0.0 should not be negative"
    
    ! Test 4: Negative zero (critical test!)
    neg_zero_sp = -0.0
    print *, "ieee_is_negative(-0.0) =", ieee_is_negative(neg_zero_sp)
    if (.not. ieee_is_negative(neg_zero_sp)) error stop "-0.0 should be negative"
    
    ! Test 5: Negative zero created by calculation
    neg_zero_sp = -1.0 * 0.0
    print *, "ieee_is_negative(-1.0 * 0.0) =", ieee_is_negative(neg_zero_sp)
    if (.not. ieee_is_negative(neg_zero_sp)) error stop "Calculated -0.0 should be negative"
    
    ! Test 6: Small negative value
    x_sp = -1.0e-30
    print *, "ieee_is_negative(-1.0e-30) =", ieee_is_negative(x_sp)
    if (.not. ieee_is_negative(x_sp)) error stop "Small negative should be negative"
    
    ! Test 7: Small positive value
    x_sp = 1.0e-30
    print *, "ieee_is_negative(1.0e-30) =", ieee_is_negative(x_sp)
    if (ieee_is_negative(x_sp)) error stop "Small positive should not be negative"
    
    ! Test 8: Double precision negative zero
    neg_zero_dp = -0.0d0
    print *, "ieee_is_negative(-0.0d0) =", ieee_is_negative(neg_zero_dp)
    if (.not. ieee_is_negative(neg_zero_dp)) error stop "Double precision -0.0 should be negative"
    
    ! Test 9: Large negative value
    x_sp = -huge(x_sp)
    print *, "ieee_is_negative(-huge) =", ieee_is_negative(x_sp)
    if (.not. ieee_is_negative(x_sp)) error stop "-huge should be negative"
    
    ! Test 10: Large positive value
    x_sp = huge(x_sp)
    print *, "ieee_is_negative(+huge) =", ieee_is_negative(x_sp)
    if (ieee_is_negative(x_sp)) error stop "+huge should not be negative"
    
    print *, "All ieee_is_negative tests passed!"
    
end program ieee_is_negative_01
