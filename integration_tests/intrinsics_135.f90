program intrinsics_133
    real :: w = -5.1
    real :: x = 5.8
    real :: y = 6.0
    real :: z = -5.8
    double precision :: v = 1e12_8
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    integer, parameter :: x1 = nint(1.0)
    integer, parameter :: x2 = nint(1.21_8)
    integer, parameter :: ar1(3) = nint([91.0, 21.20, 33.10])
    integer, parameter :: ar2(3) = nint([91.0, 21.20, 33.10], 8)
    real(4) :: arr1(3) = [91.0, 21.20, 33.10]
    real(8) :: arr2(3) = [91.0, 21.20, 33.10]

    print *, x1
    if (x1 /= 1) error stop
    print *, x2
    if (x2 /= 1) error stop
    print *, ar1
    if (any(ar1 /= [91, 21, 33])) error stop
    print *, ar2
    if (any(ar2 /= [91, 21, 33])) error stop
    print *, nint(arr1)
    if (any(nint(arr1) /= [91, 21, 33])) error stop
    print *, nint(arr2, 8)
    if (any(nint(arr2, 8) /= [91, 21, 33])) error stop
    
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
