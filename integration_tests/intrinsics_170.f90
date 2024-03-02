program intrinsics_170
    use iso_fortran_env, only: sp => real32, dp => real64
    real(sp) :: x
    real(dp) :: y
    x = 4.1
    y = 4.1_dp
    print *, exponent(x)
    if (exponent(x) /= 3) error stop
    print *, exponent(4.1_dp)
    if (exponent(4.1_dp) /= 3) error stop
    print *, exponent(y)
    if (exponent(y) /= 3) error stop

    x = 0.5
    y = 0.5_dp

    print *, exponent(x)
    if (exponent(x) /= 0) error stop

    print *, exponent(0.5_dp)
    if (exponent(0.5_dp) /= 0) error stop

    print *, exponent(y)
    if (exponent(y) /= 0) error stop

    print *, exponent(0.5)
    if (exponent(0.5) /= 0) error stop

    x = -12.94
    y = -12.94_dp

    print *, exponent(x)
    if (exponent(x) /= 4) error stop

    print *, exponent(-12.94_dp)
    if (exponent(-12.94_dp) /= 4) error stop

    print *, exponent(y)
    if (exponent(y) /= 4) error stop

    print *, exponent(-12.94)
    if (exponent(-12.94) /= 4) error stop

    x = 1e+6_sp
    y = 1e+10_dp

    print *, exponent(x)
    if (exponent(x) /= 20) error stop

    print *, exponent(1e+10_dp)
    if (exponent(1e+10_dp) /= 34) error stop

    print *, exponent(y)
    if (exponent(y) /= 34) error stop

    print *, exponent(1e+6)
    if (exponent(1e+6) /= 20) error stop

    x = -1e+6_sp
    y = -1e+10_dp

    print *, exponent(x)
    if (exponent(x) /= 20) error stop

    print *, exponent(-1e+10_dp)
    if (exponent(-1e+10_dp) /= 34) error stop

    print *, exponent(y)
    if (exponent(y) /= 34) error stop

    print *, exponent(-1e+6)
    if (exponent(-1e+6) /= 20) error stop

    x = 1e-6_sp
    y = 1e-10_dp

    print *, exponent(x)
    if (exponent(x) /= -19) error stop

    print *, exponent(1e-10_dp)
    if (exponent(1e-10_dp) /= -33) error stop

    print *, exponent(y)
    if (exponent(y) /= -33) error stop

    print *, exponent(1e-6)
    if (exponent(1e-6) /= -19) error stop

    x = 0.0
    y = 0.0_dp

    print *, exponent(x)
    if (exponent(x) /= 0) error stop

    print *, exponent(0.0_dp)
    if (exponent(0.0_dp) /= 0) error stop

    print *, exponent(y)
    if (exponent(y) /= 0) error stop

    print *, exponent(0.0)
    if (exponent(0.0) /= 0) error stop

end program
