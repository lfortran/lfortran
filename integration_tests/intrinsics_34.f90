program intrinsics_34
    integer, parameter :: dp=kind(0d0)
    real :: x = 3.143
    real(dp) :: y = 2.33
    print *, epsilon(x)
    print *, epsilon(y)
    print *, sqrt(epsilon(1._dp)) !Part of Minpack

    ! Below numbers are corresponding output of gfortran and not magic numbers.
    if (abs(epsilon(x) - 1.19209290E-07) > 1e-7) error stop
    if (abs(epsilon(y) - 2.2204460492503131E-016) > 1e-15) error stop
    if (abs(sqrt(epsilon(1._dp)) - 1.4901161193847656E-008) > 1e-15) error stop
end program
