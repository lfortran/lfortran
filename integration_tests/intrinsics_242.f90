program intrinsics_242
    integer, parameter :: dp = kind(0.d0)

    print*, nint(1e12_dp, dp)
    if (nint(1e12_dp, dp) /= 1000000000000_dp) error stop

    print*, nint(1000000000000.0000000000000000_dp, dp)
    if (nint(1000000000000.0000000000000000_dp, dp) /= 1000000000000_dp) error stop

end program 
