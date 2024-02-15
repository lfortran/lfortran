program types_real_to_complex_cast
    integer, parameter :: dp = kind(0.d0)
    complex(dp) :: c
    real(dp) :: i
    c = 1.0_dp
    i = 1.2_dp
    c = i
    if (abs(c - 1.2_dp) > 1e-8) error stop
end program
