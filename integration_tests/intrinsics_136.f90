program intrinsics_136
    integer(4) :: x, y
    integer(8) :: i, j

    integer(4) :: arg_x(5) = [12, -13, 18, 20, 67]
    integer(4) :: arg_y(5) = [1, 2, 3, 4, 5]
    integer(8) :: arg_x2(5) = [103, 87, 88, -95, -134]
    integer(8) :: arg_y2(5) = [11, 12, 4, 0, 10]
    integer(4) :: res_x(5)
    integer(8) :: res_x2(5)

    integer(4), parameter :: res(5) = rshift([12, -13, 18, 20, 67], [1, 2, 3, 4, 5])
    integer(8), parameter :: res2(5) = rshift([103, 87, 88, -95, -134], [11, 12, 4, 0, 10])

    integer(4) :: expected_res(5) = [6, -4, 2, 1, 2]
    integer(8) :: expected_res2(5) = [0, 0, 5, -95, -1]

    res_x = rshift(arg_x, arg_y)
    res_x2 = rshift(arg_x2, arg_y2)

    do i = 1, size(res)
        print *, res(i)
        if (res(i) /= expected_res(i)) error stop
    end do

    do i = 1, size(res2)
        print *, res2(i)
        if (res2(i) /= expected_res2(i)) error stop
    end do

    do i = 1, size(res_x)
        print *, res_x(i)
        if (res_x(i) /= expected_res(i)) error stop
    end do

    do i = 1, size(res_x2)
        print *, res_x2(i)
        if (res_x2(i) /= expected_res2(i)) error stop
    end do

    x = 16
    y = 2
    i = 8
    j = 4

    print*, rshift(10_4, 0)
    if ( rshift(10_4, 0) /= 10) error stop

    print*, rshift(-10_8, 1)
    if ( rshift(-10_8, 1) /= -5 ) error stop

    print*, rshift(not(10_4), 2)
    if ( rshift(not(10_4), 2) /= -3 ) error stop

    print*, rshift(not(10_8), 3)
    if ( rshift(not(10_8), 3) /= -2 ) error stop

    print*, rshift(-x, y)
    if ( rshift(-x, y) /= -4 ) error stop

    print*, rshift(i, j)
    if ( rshift(i, j) /= 0 ) error stop

    print*, rshift(10, 1)
    if ( rshift(10, 1) /= 5 ) error stop

    print*, rshift(8, 2)
    if ( rshift(8, 2) /= 2 ) error stop
end