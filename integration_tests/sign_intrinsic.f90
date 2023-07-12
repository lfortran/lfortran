program sign_intrinsic
    integer :: x, y, signval
    real :: x1, y1, signval1

    ! Compile time tests
    if (sign( 5,  10) /=  5) error stop
    if (sign(-5,  10) /=  5) error stop
    if (sign( 5, -10) /= -5) error stop
    if (sign(-5, -10) /= -5) error stop

    if (sign( 5.,  10.) /=  5.) error stop
    if (sign(-5.,  10.) /=  5.) error stop
    if (sign( 5., -10.) /= -5.) error stop
    if (sign(-5., -10.) /= -5.) error stop

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

end program
