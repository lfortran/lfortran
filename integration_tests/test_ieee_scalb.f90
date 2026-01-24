! Test ieee_scalb function - Scale by power of 2
program test_ieee_scalb
    use, intrinsic :: ieee_arithmetic, only: ieee_scalb
    implicit none
    
    real :: x_sp, result_sp
    real(8) :: x_dp, result_dp
    
    print *, "Testing ieee_scalb..."
    
    ! Test 1: Scale 1.5 by 2^3 = 12.0 (single precision)
    x_sp = 1.5
    result_sp = ieee_scalb(x_sp, 3)
    print *, "ieee_scalb(1.5, 3) =", result_sp
    if (abs(result_sp - 12.0) > 1e-6) error stop "Expected 12.0"
    
    ! Test 2: Scale 1.0 by 2^4 = 16.0 (single precision)
    x_sp = 1.0
    result_sp = ieee_scalb(x_sp, 4)
    print *, "ieee_scalb(1.0, 4) =", result_sp
    if (abs(result_sp - 16.0) > 1e-6) error stop "Expected 16.0"
    
    ! Test 3: Scale 2.0 by 2^-1 = 1.0 (single precision)
    x_sp = 2.0
    result_sp = ieee_scalb(x_sp, -1)
    print *, "ieee_scalb(2.0, -1) =", result_sp
    if (abs(result_sp - 1.0) > 1e-6) error stop "Expected 1.0"
    
    ! Test 4: Scale by 0 should return same value
    x_sp = 5.0
    result_sp = ieee_scalb(x_sp, 0)
    print *, "ieee_scalb(5.0, 0) =", result_sp
    if (abs(result_sp - 5.0) > 1e-6) error stop "Expected 5.0"
    
    ! Test 5: Double precision - scale 1.5 by 2^10 = 1536.0
    x_dp = 1.5d0
    result_dp = ieee_scalb(x_dp, 10)
    print *, "ieee_scalb(1.5d0, 10) =", result_dp
    if (abs(result_dp - 1536.0d0) > 1e-12) error stop "Expected 1536.0"
    
    ! Test 6: Large negative scale
    x_dp = 1024.0d0
    result_dp = ieee_scalb(x_dp, -10)
    print *, "ieee_scalb(1024.0d0, -10) =", result_dp
    if (abs(result_dp - 1.0d0) > 1e-12) error stop "Expected 1.0"
    
    ! Test 7: Zero should stay zero
    x_sp = 0.0
    result_sp = ieee_scalb(x_sp, 5)
    print *, "ieee_scalb(0.0, 5) =", result_sp
    if (result_sp /= 0.0) error stop "Expected 0.0"
    
    print *, "All ieee_scalb tests passed!"
    
end program test_ieee_scalb
