program intrinsics_176
    use iso_fortran_env, only: dp => real64
    real :: x
    real(dp) :: y
    integer :: i 
    integer(8) :: j

    i = 4
    j = 5
    x = 4.1
    y = 4.1_dp
    print *, setexponent(x, i)
    if ((setexponent(x, i) - 8.19999981) > 1e-7) error stop

    print *, setexponent(4.1_dp, 5)
    if ((setexponent(4.1_dp, 5) - 16.3999999999999986_8) > 1e-12) error stop

    print *, setexponent(y, j)
    if ((setexponent(y, j) - 16.399999999999999_8) > 1e-12) error stop

    print *, setexponent(4.1, 4)
    if ((setexponent(4.1, 4) - 8.19999981) > 1e-7) error stop

    i = 11
    j = 7
    x = 0.5
    y = 0.5_dp

    print *, setexponent(x, i)
    if ((setexponent(x, i) - 1024.00000) > 1e-7) error stop

    print *, setexponent(0.5_dp, 7)
    if ((setexponent(0.5_dp, 7) - 64.000000000000000_8) > 1e-12) error stop

    print *, setexponent(y, j)
    if ((setexponent(y, j) - 64.000000000000000_8) > 1e-12) error stop

    print *, setexponent(0.5, 11)
    if ((setexponent(0.5, 11) - 1024.00000) > 1e-7) error stop

    i = 15
    j = 12
    x = -11.56
    y = -15.678_dp

    print *, setexponent(x, i)
    if ((setexponent(x, i) - (-23674.8809)) > 1e-7) error stop

    print *, setexponent(-15.678_dp, 12)
    if ((setexponent(-15.678_dp, 12) - (-4013.5680000000002_8)) > 1e-12) error stop

    print *, setexponent(y, j)
    if ((setexponent(y, j) - (-4013.5680000000002_8)) > 1e-12) error stop

    print *, setexponent(-11.56, 15)
    if ((setexponent(-11.56, 15) - (-23674.880)) > 1e-7) error stop

    print *, kind(setexponent(x, i))
    if (kind(setexponent(x, i)) /= 4) error stop

    print *, kind(setexponent(-15.678_dp, 12))
    if (kind(setexponent(-15.678_dp, 12)) /= 8) error stop

    print *, kind(setexponent(y, j))
    if (kind(setexponent(y, j)) /= 8) error stop

    print *, kind(setexponent(-11.56, 15))
    if (kind(setexponent(-11.56, 15)) /= 4) error stop

end program
