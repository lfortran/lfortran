program intrinsics_124
    integer(4) :: x, y
    integer(8) :: i, j

    integer(4) :: arg_x(5) = [12, -13, 18, 20, 67]
    integer(4) :: arg_y(5) = [1, 2, 3, 4, 5]
    integer(8) :: arg_x2(5) = [103, 87, 88, -95, -134]
    integer(8) :: arg_y2(5) = [11, 12, 4, 0, 10]
    integer(4) :: res_x(5)
    integer(8) :: res_x2(5)

    integer(4), parameter :: res(5) = lshift([12, -13, 18, 20, 67], [1, 2, 3, 4, 5])
    integer(8), parameter :: res2(5) = lshift([103, 87, 88, -95, -134], [11, 12, 4, 0, 10])

    integer(4) :: expected_res(5) = [24, -52, 144, 320, 2144]
    integer(8) :: expected_res2(5) = [210944, 356352, 1408, -95, -137216]

    integer(4), parameter :: comp1 = lshift(12, 1)
    integer(8), parameter :: comp2 = lshift(103, 11)

    print *, comp1
    if (comp1 /= 24) error stop

    print *, comp2
    if (comp2 /= 210944) error stop

    res_x = lshift(arg_x, arg_y)
    res_x2 = lshift(arg_x2, arg_y2)

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

    print*, lshift(10_4, 0)
    if (.not. lshift(10_4, 0) == 10) error stop

    print*, lshift(-10_8, 1)
    if ( lshift(-10_8, 1) /= -20 ) error stop

    print*, lshift(not(10_4), 2)
    if ( lshift(not(10_4), 2) /= -44 ) error stop 

    print*, lshift(not(10_8), 3)
    if ( lshift(not(10_8), 3) /= -88 ) error stop 
   

    print*, lshift(-x, y)
    if ( lshift(-x, y) /= -64 ) error stop

    print*, lshift(i, j)
    if ( lshift(i, j) /= 128 ) error stop

    print*, lshift(10, 1)
    if ( lshift(10, 1) /= 20 ) error stop

    print*, lshift(8, 2)
    if ( lshift(8, 2) /= 32 ) error stop
end
