program precision_02
    integer, parameter :: dp = kind(0.d0)
    real(dp) :: x, x1
    x1 = 1.1_dp
    x = 1.1 - x1
    print *, x
    if (abs(x - 2.3841857821338408E-008) > 1D-10) error stop
end program
    