program intrinsics_136
    integer(4) :: x, y
    integer(8) :: i, j
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