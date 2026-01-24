! Test ieee_support_* inquiry functions
program test_ieee_support
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none
    
    real(real32) :: x_sp
    real(real64) :: x_dp
    logical :: result
    
    print *, "Testing ieee_support_* functions..."
    
    ! Test ieee_support_denormal
    result = ieee_support_denormal(x_sp)
    print *, "ieee_support_denormal(real32) = ", result
    if (.not. result) error stop "Should support denormals"
    
    result = ieee_support_denormal(x_dp)
    print *, "ieee_support_denormal(real64) = ", result
    if (.not. result) error stop "Should support denormals"
    
    ! Test ieee_support_divide
    result = ieee_support_divide(x_sp)
    print *, "ieee_support_divide(real32) = ", result
    if (.not. result) error stop "Should support divide"
    
    result = ieee_support_divide(x_dp)
    print *, "ieee_support_divide(real64) = ", result
    if (.not. result) error stop "Should support divide"
    
    ! Test ieee_support_sqrt
    result = ieee_support_sqrt(x_sp)
    print *, "ieee_support_sqrt(real32) = ", result
    if (.not. result) error stop "Should support sqrt"
    
    result = ieee_support_sqrt(x_dp)
    print *, "ieee_support_sqrt(real64) = ", result
    if (.not. result) error stop "Should support sqrt"
    
    ! Test ieee_support_standard
    result = ieee_support_standard(x_sp)
    print *, "ieee_support_standard(real32) = ", result
    if (.not. result) error stop "Should support standard"
    
    result = ieee_support_standard(x_dp)
    print *, "ieee_support_standard(real64) = ", result
    if (.not. result) error stop "Should support standard"
    
    ! Test ieee_support_io
    result = ieee_support_io(x_sp)
    print *, "ieee_support_io(real32) = ", result
    if (.not. result) error stop "Should support I/O"
    
    result = ieee_support_io(x_dp)
    print *, "ieee_support_io(real64) = ", result
    if (.not. result) error stop "Should support I/O"
    
    ! Test ieee_support_rounding
    result = ieee_support_rounding(ieee_nearest, x_sp)
    print *, "ieee_support_rounding(ieee_nearest, real32) = ", result
    if (.not. result) error stop "Should support rounding"
    
    result = ieee_support_rounding(ieee_to_zero, x_dp)
    print *, "ieee_support_rounding(ieee_to_zero, real64) = ", result
    if (.not. result) error stop "Should support rounding"
    
    ! Test ieee_support_datatype
    result = ieee_support_datatype(x_sp)
    print *, "ieee_support_datatype(real32) = ", result
    if (.not. result) error stop "Should support datatype"
    
    result = ieee_support_datatype(x_dp)
    print *, "ieee_support_datatype(real64) = ", result
    if (.not. result) error stop "Should support datatype"
    
    print *, "All ieee_support_* tests passed!"
    
end program test_ieee_support
