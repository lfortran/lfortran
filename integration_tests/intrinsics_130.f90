program intrinsics_130

    integer(4) :: x, y1, y2, y3
    x = 10
    y1 = 2
    y2 = 0
    y3 = -2

    print*, ishft(10_4, 2)
    print*, ishft(10_4, 0)
    print*, ishft(10_4, -2)

    ! print*, ishft(x, y1) ! Floating point exception (core dumped)
    ! if (ishft(x, y1) /= 40) error stop
    print*, ishft(x, y2)
    if (ishft(x, y2) /= 10) error stop
    print*, ishft(x, y3)
    if (ishft(x, y3) /= 2) error stop
   
end