program intrinsics_130

    integer(4) :: x1, x2, x3, y1, y2, y3
    x1 = 10
    x2 = 10
    x3 = 10
    y1 = 2
    y2 = 0
    y3 = -2

    print*, ishft(10_4, 2)
    print*, ishft(10_4, 0)
    print*, ishft(10_4, -2)

    print*, ishft(x1, y1)
    ! if (ishft(x1, y1) /= 40) error stop
    print*, ishft(x2, y2)
    ! if (ishft(x2, y2) /= 10) error stop
    print*, ishft(x3, y3)
    ! if (ishft(x3, y3) /= 2) error stop
   
end