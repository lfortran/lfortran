! Test ieee_next_after function (Fortran 2003 standard)
program ieee_next_01
    use, intrinsic :: ieee_arithmetic, only: ieee_next_after
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(4) :: x_sp, next_sp, inf_sp
    real(8) :: x_dp, next_dp, inf_dp
    
    print *, "Testing ieee_next_after..."
    
    ! Test 1: ieee_next_after toward larger value
    x_sp = 1.0_real32
    next_sp = ieee_next_after(x_sp, 2.0_real32)
    print *, "ieee_next_after(1.0, 2.0) =", next_sp
    if (next_sp <= x_sp) error stop "next_after toward larger should be greater than x"
    
    ! Test 2: ieee_next_after toward smaller value
    x_sp = 1.0_real32
    next_sp = ieee_next_after(x_sp, 0.0_real32)
    print *, "ieee_next_after(1.0, 0.0) =", next_sp
    if (next_sp >= x_sp) error stop "next_after toward smaller should be less than x"
    
    ! Test 3: ieee_next_after toward same value should return same value
    x_sp = 1.0_real32
    next_sp = ieee_next_after(x_sp, x_sp)
    print *, "ieee_next_after(1.0, 1.0) =", next_sp
    if (next_sp /= x_sp) error stop "next_after(x, x) should equal x"
    
    ! Test 4: Verify inverse property
    x_sp = 1.0_real32
    next_sp = ieee_next_after(x_sp, 2.0_real32)
    next_sp = ieee_next_after(next_sp, 0.0_real32)
    if (next_sp /= x_sp) error stop "Should be able to go forward and back"
    print *, "Inverse property verified for single precision"
    
    ! Test 5: From 0.0 toward positive should give smallest positive
    x_sp = 0.0_real32
    next_sp = ieee_next_after(x_sp, 1.0_real32)
    print *, "ieee_next_after(0.0, 1.0) =", next_sp
    if (next_sp <= 0.0) error stop "Should be positive"
    
    ! Test 6: From 0.0 toward negative should give smallest negative
    x_sp = 0.0_real32
    next_sp = ieee_next_after(x_sp, -1.0_real32)
    print *, "ieee_next_after(0.0, -1.0) =", next_sp
    if (next_sp >= 0.0) error stop "Should be negative"
    
    ! Test 7: Double precision tests
    x_dp = 1.0_real64
    next_dp = ieee_next_after(x_dp, 2.0_real64)
    print *, "ieee_next_after(1.0d0, 2.0d0) =", next_dp
    if (next_dp <= x_dp) error stop "Should be greater"
    
    x_dp = 1.0_real64
    next_dp = ieee_next_after(x_dp, 0.0_real64)
    print *, "ieee_next_after(1.0d0, 0.0d0) =", next_dp
    if (next_dp >= x_dp) error stop "Should be less"
    
    ! Test 8: Verify spacing relationship for positive x toward larger
    x_sp = 1.0_real32
    next_sp = ieee_next_after(x_sp, 2.0_real32)
    print *, "Difference: next_after(1.0, 2.0) - 1.0 =", next_sp - x_sp
    print *, "spacing(1.0) =", spacing(x_sp)
    ! The difference should equal spacing(x) for positive x
    if (abs((next_sp - x_sp) - spacing(x_sp)) > 1e-10) then
        error stop "next_after - x should equal spacing(x)"
    end if
    
    print *, "All ieee_next_after tests passed!"
    
end program ieee_next_01
