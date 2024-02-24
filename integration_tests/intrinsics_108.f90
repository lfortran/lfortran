program intrinsics_108
    integer(4) :: x, y
    integer(8) :: i, j
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
end
