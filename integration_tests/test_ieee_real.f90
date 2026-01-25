! Test ieee_real function
program test_ieee_real
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64, int64
    implicit none
    
    integer :: i32
    integer(int64) :: i64
    real(real32) :: r_sp, x_sp
    real(real64) :: r_dp, x_dp
    
    print *, "Testing ieee_real..."
    
    ! Test 1: Convert integer to single precision
    i32 = 42
    x_sp = 0.0_real32  ! Used for kind
    r_sp = ieee_real(i32, x_sp)
    print *, "ieee_real(42, real32) = ", r_sp
    if (abs(r_sp - 42.0_real32) > 1.0e-6) error stop "42 should convert to 42.0"
    
    ! Test 2: Negative integer
    i32 = -17
    r_sp = ieee_real(i32, x_sp)
    print *, "ieee_real(-17, real32) = ", r_sp
    if (abs(r_sp - (-17.0_real32)) > 1.0e-6) error stop "-17 should convert to -17.0"
    
    ! Test 3: Zero
    i32 = 0
    r_sp = ieee_real(i32, x_sp)
    print *, "ieee_real(0, real32) = ", r_sp
    if (r_sp /= 0.0_real32) error stop "0 should convert to 0.0"
    
    ! Test 4: Convert integer to double precision
    i32 = 100
    x_dp = 0.0_real64  ! Used for kind
    r_dp = ieee_real(i32, x_dp)
    print *, "ieee_real(100, real64) = ", r_dp
    if (abs(r_dp - 100.0_real64) > 1.0e-10) error stop "100 should convert to 100.0d0"
    
    ! Test 5: Large integer (int64) to single precision
    i64 = 999_int64
    r_sp = ieee_real(i64, x_sp)
    print *, "ieee_real(999_int64, real32) = ", r_sp
    if (abs(r_sp - 999.0_real32) > 1.0e-6) error stop "999 should convert to 999.0"
    
    ! Test 6: Large integer (int64) to double precision
    i64 = 123456_int64
    r_dp = ieee_real(i64, x_dp)
    print *, "ieee_real(123456_int64, real64) = ", r_dp
    if (abs(r_dp - 123456.0_real64) > 1.0e-10) error stop "123456 should convert to 123456.0d0"
    
    print *, "All ieee_real tests passed!"
    
end program test_ieee_real
