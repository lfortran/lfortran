program intrinsics_123
    integer(4) :: x, y
    integer(8) :: i, j
    x = 16
    y = 2
    i = 8
    j = 4

    print*, shiftl(10_4, 0)
    if (.not. shiftl(10_4, 0) == 10) error stop

    print*, shiftl(-10_8, 1)
    if ( shiftl(-10_8, 1) /= -20 ) error stop

    print*, shiftl(not(10_4), 2)
    if ( shiftl(not(10_4), 2) /= -44 ) error stop ! Does not work yet

    print*, shiftl(not(10_8), 3)
    if ( shiftl(not(10_8), 3) /= -88 ) error stop ! Does not work yet
   

    print*, shiftl(-x, y)
    if ( shiftl(-x, y) /= -64 ) error stop

    print*, shiftl(i, j)
    if ( shiftl(i, j) /= 128 ) error stop

    print*, shiftl(10, 1)
    if ( shiftl(10, 1) /= 20 ) error stop

    print*, shiftl(8, 2)
    if ( shiftl(8, 2) /= 32 ) error stop
end
