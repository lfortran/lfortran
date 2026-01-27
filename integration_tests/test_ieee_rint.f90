! Test ieee_rint function
program test_ieee_rint
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp, r_sp
    real(real64) :: x_dp, r_dp
    
    print *, "Testing ieee_rint..."
    
    ! Test 1: Round 2.3 (should round to 2.0 with round-to-nearest)
    x_sp = 2.3_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(2.3) = ", r_sp
    if (abs(r_sp - 2.0_real32) > 1.0e-6) error stop "2.3 should round to 2.0"
    
    ! Test 2: Round 2.7 (should round to 3.0)
    x_sp = 2.7_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(2.7) = ", r_sp
    if (abs(r_sp - 3.0_real32) > 1.0e-6) error stop "2.7 should round to 3.0"
    
    ! Test 3: Round 2.5 (should round to 2.0 - round to even)
    x_sp = 2.5_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(2.5) = ", r_sp
    if (abs(r_sp - 2.0_real32) > 1.0e-6) error stop "2.5 should round to 2.0 (nearest even)"
    
    ! Test 4: Round 3.5 (should round to 4.0 - round to even)
    x_sp = 3.5_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(3.5) = ", r_sp
    if (abs(r_sp - 4.0_real32) > 1.0e-6) error stop "3.5 should round to 4.0 (nearest even)"
    
    ! Test 5: Negative numbers
    x_sp = -2.7_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(-2.7) = ", r_sp
    if (abs(r_sp - (-3.0_real32)) > 1.0e-6) error stop "-2.7 should round to -3.0"
    
    ! Test 6: Already integer
    x_sp = 5.0_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(5.0) = ", r_sp
    if (abs(r_sp - 5.0_real32) > 1.0e-6) error stop "5.0 should remain 5.0"
    
    ! Test 7: Double precision
    x_dp = 7.6_real64
    r_dp = ieee_rint(x_dp)
    print *, "ieee_rint(7.6d0) = ", r_dp
    if (abs(r_dp - 8.0_real64) > 1.0e-10) error stop "7.6d0 should round to 8.0d0"
    
    ! Test 8: Zero
    x_sp = 0.0_real32
    r_sp = ieee_rint(x_sp)
    print *, "ieee_rint(0.0) = ", r_sp
    if (r_sp /= 0.0_real32) error stop "0.0 should remain 0.0"
    
    print *, "All ieee_rint tests passed!"
    
end program test_ieee_rint
