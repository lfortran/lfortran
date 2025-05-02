program intrinsics_130

    integer(4) :: x, y1, y2, y3
    integer(1) :: i1 = 7_1
    integer(2) :: i2 = 7_2
    integer(4) :: i4 = 7_4
    integer(8) :: i8 = 7_8
    x = 10
    y1 = 2
    y2 = 0
    y3 = -2

    print*, ishft(10_4, 2)
    print*, ishft(10_4, 0)
    print*, ishft(10_4, -2)

    print*, ishft(x, y1)
    if (ishft(x, y1) /= 40) error stop
    print*, ishft(x, y2)
    if (ishft(x, y2) /= 10) error stop
    print*, ishft(x, y3)
    if (ishft(x, y3) /= 2) error stop

    i8 = ishft(i1, 2_1)
    if (i8 /= 28) error stop
    i8 = ishft(i2, 2)
    if (i8 /= 28) error stop
    i8 = ishft(i4, 2_1)
    if (i8 /= 28) error stop
    i8 = ishft(i8, 2)
    if (i8 /= 112) error stop
    i4 = ishft(i1, 2)
    if (i4 /= 28) error stop
    i4 = ishft(i2, 2_1)
    if (i4 /= 28) error stop
    i4 = ishft(i4, 2)
    if (i4 /= 112) error stop
    i4 = ishft(i8, 2_2)
    if (i4 /= 448) error stop
    i2 = ishft(i1, 2)
    if (i2 /= 28) error stop
    i2 = ishft(i2, 2)
    if (i2 /= 112) error stop
    i2 = ishft(i4, 2_4)
    if (i2 /= 1792) error stop
    i2 = ishft(i8, 2)
    if (i2 /= 448) error stop
    i1 = ishft(i1, 2)
    if (i1 /= 28) error stop
    i1 = ishft(i2, 2_2)
    if (i1 /= 0) error stop
    i1 = ishft(i4, 2)
    if (i1 /= 0) error stop
    i1 = ishft(i8, 2_2)
    if (i1 /= -64) error stop
end