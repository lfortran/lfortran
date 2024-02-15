program types_real_array_to_complex_array_cast
    integer, parameter :: dp = kind(0.d0)
    complex(dp) :: D(3) = [1.0_dp, 2.0_dp, 3.0_dp]
    if (any(D /= [(1.0_dp, 0.0_dp), (2.0_dp, 0.0_dp), (3.0_dp, 0.0_dp)])) \
        error stop
end program
