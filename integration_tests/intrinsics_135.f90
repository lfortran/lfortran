program intrinsics_133
    real :: w = -5.1
    real :: x = 5.8
    real :: y = 6.0
    real :: z = -5.8
    double precision :: v = 1e12_8
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8

    res_4 = nint(w)
    print *, res_4
    if (res_4 /= -5) error stop

    res_8 = nint(w, 8)
    print *, res_8
    if (res_8 /= -5) error stop

    res_4 = nint(w, 4)
    print *, res_4
    if (res_4 /= -5) error stop
    
    res_4 = nint(x)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = nint(x, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = nint(x, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = nint(y)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = nint(y, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = nint(y, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = nint(z)
    print *, res_4
    if (res_4 /= -6) error stop

    res_8 = nint(z, 8)
    print *, res_8
    if (res_8 /= -6) error stop

    res_4 = nint(z, 4)
    print *, res_4
    if (res_4 /= -6) error stop

    res_4 = nint(5.8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = nint(5.8, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = nint(6.00, 8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = nint(6.00, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = nint(0.0)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = nint(0.0, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = nint(-412.124)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = nint(-412.124, 8)
    print *, res_8
    if (res_8 /= -412) error stop

    res_4 = nint(-412.00)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = nint(-412.00, 8)
    print *, res_8
    if (res_8 /= -412) error stop

    res_8 = nint(1e12_8, 8)
    print *, res_8
    if (res_8 /= 1000000000000_8) error stop

    res_8 = nint(v, 8)
    print *, res_8
    if (res_8 /= 1000000000000_8) error stop

end program
