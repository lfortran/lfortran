program intrinsics_107
    integer(4) :: x = 5
    integer(8) :: y = 5
    print *, trailz(0_4)
    if ( trailz(0_4) /= 32 ) error stop
    print *, trailz(0_8)
    if ( trailz(0_8) /= 32 ) error stop
    print *, trailz(x)
    if ( trailz(x) /= 0 ) error stop
    print *, trailz(y)
    if ( trailz(y) /= 0 ) error stop
end program

