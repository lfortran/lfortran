program intrinsic_142
    real :: a, b, c
    double precision :: result
    a = 5.2
    b = 3.2
    c = -4.1

    result = dprod(a,b)
    print *, result
    if (abs(result - (16.64)) > 1e-5) error stop

    result = dprod(a,c)
    print *, dprod(a,c)
    if (abs(result - (-21.32)) > 1e-5) error stop

    result = dprod(b,c)
    print *, result
    if (abs(result - (-13.12)) > 1e-5) error stop

    result = dprod(5.2,3.2)
    print *, result
    if (abs(result - (16.64)) > 1e-5) error stop

    result = dprod(5.2,-4.1)
    print *, result
    if (abs(result - (-21.32)) > 1e-5) error stop

    result = dprod(3.2,-4.1)
    print *, result
    if (abs(result - (-13.12)) > 1e-5) error stop

    print *, kind(dprod(a,b))
    if (kind(dprod(a,b)) /= 8) error stop

    print *, kind(dprod(5.2, 3.2))
    if (kind(dprod(5.2, 3.2)) /= 8) error stop
    
end program
