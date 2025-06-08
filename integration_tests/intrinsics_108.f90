program intrinsics_108
    integer(4) :: x, y, i_4
    integer(8) :: i, j
    integer :: arr3(3)

    integer(4) :: arg_x(5) = [12, 13, 18, 20, 67]
    integer(4) :: arg_y(5) = [1, 2, 3, 4, 5]
    integer(8) :: arg_x2(5) = [103, 87, 88, 95, 134]
    integer(8) :: arg_y2(5) = [11, 12, 4, 0, 10]
    integer(4) :: res_x(5)
    integer(8) :: res_x2(5)

    integer(4), parameter :: res(5) = shiftr([12, 13, 18, 20, 67], [1, 2, 3, 4, 5])
    integer(8), parameter :: res2(5) = shiftr([103, 87, 88, 95, 134], [11, 12, 4, 0, 10])

    integer(4) :: expected_res(5) = [6, 3, 2, 1, 2]
    integer(8) :: expected_res2(5) = [0, 0, 5, 95, 0]

    integer(4), parameter :: comp1 = shiftr(12, 1)
    integer(4), parameter :: neg1 = shiftr(-122, 23)
    integer(8), parameter :: comp2 = shiftr(103, 11)
    integer(8), parameter :: neg2 = shiftr(-19381002102129_8, 63)

    print *, comp1
    if (comp1 /= 6) error stop

    print *, comp2
    if (comp2 /= 0) error stop

    res_x = shiftr(arg_x, arg_y)
    res_x2 = shiftr(arg_x2, arg_y2)

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

    print*, shiftr(10_4, 0)
    if (.not. shiftr(10_4, 0) == 10) error stop

    print*, shiftr(-10_8, 1)
    ! if ( shiftr(i, j) /= 9223372036854775803 ) error stop ! Does not work yet

    print*, shiftr(not(10_4), 2)
    ! if ( shiftr(i, j) /= 1073741821 ) error stop ! Does not work yet

    print*, shiftr(not(10_8), 3)
    ! if ( shiftr(i, j) /= 2305843009213693950 ) error stop ! Does not work yet

    print*, shiftr(-x, y)
    ! if ( shiftr(-x, y) /= 1073741820 ) error stop

    print*, shiftr(i, j)
    if ( shiftr(i, j) /= 0 ) error stop

    print*, shiftr(10, 1)
    if ( shiftr(10, 1) /= 5 ) error stop

    print*, shiftr(8, 2)
    if ( shiftr(8, 2) /= 2 ) error stop

    ! test broadcasting of `shiftr`
    arr3 = shiftr([20, 11, 8], 1)
    if (arr3(1) /= 10) error stop
    if (arr3(2) /= 5) error stop
    if (arr3(3) /= 4) error stop

    arr3 = shiftr([20, 11, 8], [1, 2, 3])
    if (arr3(1) /= 10) error stop
    if (arr3(2) /= 2) error stop
    if (arr3(3) /= 1) error stop

    i_4 = -122
    j = -19381002102129_8
    x = 23
    y = 63
    print *, shiftr(i_4, x), shiftr(j, y)
    if (shiftr(i_4, x) /= 511 .or. shiftr(j, y) /= 1) error stop
    print *, neg1, neg2
    if (neg1 /= 511 .or. neg2 /= 1) error stop
end
