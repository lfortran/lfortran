program intrinsics_124
    integer(4) :: x, y
    integer(8) :: i, j
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
