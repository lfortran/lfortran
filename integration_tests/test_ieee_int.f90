! Test ieee_int function
program test_ieee_int
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp
    real(real64) :: x_dp
    integer :: i
    
    print *, "Testing ieee_int..."
    
    ! Test 1: Convert 3.2 to integer (should round to 3)
    x_sp = 3.2_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(3.2) = ", i
    if (i /= 3) error stop "3.2 should convert to 3"
    
    ! Test 2: Convert 3.7 to integer (should round to 4)
    x_sp = 3.7_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(3.7) = ", i
    if (i /= 4) error stop "3.7 should convert to 4"
    
    ! Test 3: Negative number
    x_sp = -2.3_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(-2.3) = ", i
    if (i /= -2) error stop "-2.3 should convert to -2"
    
    ! Test 4: Negative number rounding
    x_sp = -2.7_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(-2.7) = ", i
    if (i /= -3) error stop "-2.7 should convert to -3"
    
    ! Test 5: Zero
    x_sp = 0.0_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(0.0) = ", i
    if (i /= 0) error stop "0.0 should convert to 0"
    
    ! Test 6: Double precision
    x_dp = 9.4_real64
    i = ieee_int(x_dp)
    print *, "ieee_int(9.4d0) = ", i
    if (i /= 9) error stop "9.4d0 should convert to 9"
    
    ! Test 7: Large value (uses default rounding mode - round to nearest)
    x_sp = 100.4_real32
    i = ieee_int(x_sp)
    print *, "ieee_int(100.4) = ", i
    if (i /= 100) error stop "100.4 should convert to 100"
    
    print *, "All ieee_int tests passed!"
    
end program test_ieee_int
