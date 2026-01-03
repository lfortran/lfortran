program intrinsics_133
    real :: x = 5.8
    real :: y = 6.0
    integer, parameter :: array_size = 3
    integer(kind = 4) :: res_4
    integer(kind = 8) :: res_8
    integer(kind=4) :: res_4_arr(array_size)
    integer(kind=8) :: res_8_arr(array_size)
    
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

     ! Compile time broadcasting
    res_4_arr = Ceiling([real :: 1.2, 3.3, 5])
    print *, res_4_arr(1)
    if (res_4_arr(1) /= 2) error stop
    print *, res_4_arr(2)
    if (res_4_arr(2) /= 4) error stop
    print *, res_4_arr(3)
    if (res_4_arr(3) /= 5) error stop

     ! Compile time broadcasting
    res_4_arr = Ceiling([real :: 1.2, 3.3, 5])
    print *, res_4_arr(1)
    if (res_4_arr(1) /= 2) error stop
    print *, res_4_arr(2)
    if (res_4_arr(2) /= 4) error stop
    print *, res_4_arr(3)
    if (res_4_arr(3) /= 5) error stop

    res_8_arr = Ceiling([real(8) :: 1.2, 3.3, 5], kind=8)
    print *, res_8_arr(1)
    if (res_8_arr(1) /= 2) error stop
    if (kind(res_8_arr(1)) /= 8) error stop
    print *, res_8_arr(2)
    if (res_8_arr(2) /= 4) error stop
    if (kind(res_8_arr(2)) /= 8) error stop
    print *, res_8_arr(3)
    if (res_8_arr(3) /= 5) error stop
    if (kind(res_8_arr(3)) /= 8) error stop

end program
