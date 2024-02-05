program intrinsics_133
    real :: x = 5.8
    real :: y = 6.0
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    
    res_4 = Ceiling(x)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(x, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(x, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = Ceiling(y)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(y, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(y, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = Ceiling(5.8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(5.8, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(6.00, 8)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = Ceiling(6.00, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = Ceiling(0.0)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = Ceiling(0.0, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = Ceiling(-412.124)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = Ceiling(-412.124, 8)
    print *, res_8
    if (res_8 /= -412) error stop

    res_4 = Ceiling(-412.00)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = Ceiling(-412.00, 8)
    print *, res_8
    if (res_8 /= -412) error stop

end program
