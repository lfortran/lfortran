program intrinsics_58
    integer :: x, y, signval
    real :: x1, y1, signval1
    integer :: int_res(4)

    ! Compile time tests
    if (sign( 5,  10) /=  5) error stop
    if (sign(-5,  10) /=  5) error stop
    if (sign( 5, -10) /= -5) error stop
    if (sign(-5, -10) /= -5) error stop

    if (sign( 5.,  10.) /=  5.) error stop
    if (sign(-5.,  10.) /=  5.) error stop
    if (sign( 5., -10.) /= -5.) error stop
    if (sign(-5., -10.) /= -5.) error stop

    ! Ensure that compile time broadcasting works
    !> positive second argument
    int_res = sign([1, 2, 5, -1], 2)
    print *, int_res
    if (int_res(1) /= 1) error stop
    if (int_res(2) /= 2) error stop
    if (int_res(3) /= 5) error stop
    if (int_res(4) /= 1) error stop

    !> negative second argument
    int_res = sign([1, 2, -5, 1], -3)
    print *, int_res
    if (int_res(1) /= -1) error stop
    if (int_res(2) /= -2) error stop
    if (int_res(3) /= -5) error stop
    if (int_res(4) /= -1) error stop

    ! Runtime tests
    x = 5
    y = 10
    signval = sign(x, y)
    print *, signval
    if( signval /= 5 ) error stop

    x = 5
    y = -10
    signval = sign(x, y)
    print *, signval
    if( signval /= -5 ) error stop

    x = -5
    y = 10
    signval = sign(x, y)
    print *, signval
    if( signval /= 5 ) error stop

    x = -5
    y = -10
    signval = sign(x, y)
    print *, signval
    if( signval /= -5 ) error stop

    x1 = 5.0
    y1 = 10.0
    signval1 = sign(x1, y1)
    print *, signval1
    if( signval1 /= 5.0 ) error stop

    x1 = 5.0
    y1 = -10.0
    signval1 = sign(x1, y1)
    print *, signval1
    if( signval1 /= -5.0 ) error stop

    x1 = -5.0
    y1 = 10.0
    signval1 = sign(x1, y1)
    print *, signval1
    if( signval1 /= 5.0 ) error stop

    x1 = -5.0
    y1 = -10.0
    signval1 = sign(x1, y1)
    print *, signval1
    if( signval1 /= -5.0 ) error stop
end program intrinsics_58
