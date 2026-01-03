program intrinsics_329
    use iso_fortran_env, only: dp => real64
    real(dp), parameter :: w = -5.1_dp
    real(dp), parameter :: x = 5.8_dp
    real(dp) :: y = 6.0_dp
    real(dp) :: z = -5.8_dp
    integer(dp) :: res_4
    integer(dp) :: res_8

    res_4 = idnint(w)
    print *, res_4
    if (res_4 /= -5) error stop

    res_8 = idnint(w)
    print *, res_8
    if (res_8 /= -5) error stop

    res_4 = idnint(x)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = idnint(x)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = idnint(y)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = idnint(y)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = idnint(z)
    print *, res_4
    if (res_4 /= -6) error stop

    res_8 = idnint(z)
    print *, res_8
    if (res_8 /= -6) error stop

    res_4 = idnint(5.8_dp)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = idnint(5.8_dp)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = idnint(6.00_dp)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = idnint(6.00_dp)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = idnint(0.0_dp)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = idnint(0.0_dp)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = idnint(-412.124_dp)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = idnint(-412.124_dp)
    print *, res_8
    if (res_8 /= -412) error stop

    res_4 = idnint(-412.00_dp)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = idnint(-412.00_dp)
    print *, res_8
    if (res_8 /= -412) error stop

end program
