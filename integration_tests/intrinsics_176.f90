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
    print *, set_exponent(x, i)
    if ((set_exponent(x, i) - 8.19999981) > 1e-7) error stop

    print *, set_exponent(4.1_dp, 5)
    if ((set_exponent(4.1_dp, 5) - 16.3999999999999986_dp) > 1e-12) error stop

    print *, set_exponent(y, j)
    if ((set_exponent(y, j) - 16.399999999999999_dp) > 1e-12) error stop

    print *, set_exponent(4.1, 4)
    if ((set_exponent(4.1, 4) - 8.19999981) > 1e-7) error stop

    i = 11
    j = 7
    x = 0.5
    y = 0.5_dp

    print *, set_exponent(x, i)
    if ((set_exponent(x, i) - 1024.00000) > 1e-7) error stop

    print *, set_exponent(0.5_dp, 7)
    if ((set_exponent(0.5_dp, 7) - 64.000000000000000_dp) > 1e-12) error stop

    print *, set_exponent(y, j)
    if ((set_exponent(y, j) - 64.000000000000000_dp) > 1e-12) error stop

    print *, set_exponent(0.5, 11)
    if ((set_exponent(0.5, 11) - 1024.00000) > 1e-7) error stop

    i = 15
    j = 12
    x = -11.56
    y = -15.678_dp

    print *, set_exponent(x, i)
    if ((set_exponent(x, i) - (-23674.8809)) > 1e-7) error stop

    print *, set_exponent(-15.678_dp, 12)
    if ((set_exponent(-15.678_dp, 12) - (-4013.5680000000002_dp)) > 1e-12) error stop

    print *, set_exponent(y, j)
    if ((set_exponent(y, j) - (-4013.5680000000002_dp)) > 1e-12) error stop

    print *, set_exponent(-11.56, 15)
    if ((set_exponent(-11.56, 15) - (-23674.880)) > 1e-7) error stop

    print *, kind(set_exponent(x, i))
    if (kind(set_exponent(x, i)) /= 4) error stop

    print *, kind(set_exponent(-15.678_dp, 12))
    if (kind(set_exponent(-15.678_dp, 12)) /= 8) error stop

    print *, kind(set_exponent(y, j))
    if (kind(set_exponent(y, j)) /= 8) error stop

    print *, kind(set_exponent(-11.56, 15))
    if (kind(set_exponent(-11.56, 15)) /= 4) error stop

end program
