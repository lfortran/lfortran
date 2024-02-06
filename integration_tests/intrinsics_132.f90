program intrinsics_132
    real :: x = 5.8
    real :: y = 6.0
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    
    res_4 = floor(x)
    print *, res_4
    if (res_4 /= 5) error stop

    res_8 = floor(x, 8)
    print *, res_8
    if (res_8 /= 5) error stop

    res_4 = floor(x, 4)
    print *, res_4
    if (res_4 /= 5) error stop

    res_4 = floor(y)
    print *, res_4
    if (res_4 /= 6) error stop

    res_8 = floor(y, 8)
    print *, res_8
    if (res_8 /= 6) error stop

    res_4 = floor(y, 4)
    print *, res_4
    if (res_4 /= 6) error stop

    res_4 = floor(5.8)
    print *, res_4
    if (res_4 /= 5) error stop

    res_8 = floor(5.8, 8)
    print *, res_8
    if (res_8 /= 5) error stop

    res_4 = floor(0.0)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = floor(0.0, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = floor(0.01)
    print *, res_4
    if (res_4 /= 0) error stop

    res_8 = floor(0.01, 8)
    print *, res_8
    if (res_8 /= 0) error stop

    res_4 = floor(-0.01)
    print *, res_4
    if (res_4 /= -1) error stop

    res_8 = floor(-0.01, 8)
    print *, res_8
    if (res_8 /= -1) error stop

    res_4 = floor(-412.124)
    print *, res_4
    if (res_4 /= -413) error stop

    res_8 = floor(-412.124, 8)
    print *, res_8
    if (res_8 /= -413) error stop

    res_4 = floor(-412.00)
    print *, res_4
    if (res_4 /= -412) error stop

    res_8 = floor(-412.00, 8)
    print *, res_8
    if (res_8 /= -412) error stop

end program
