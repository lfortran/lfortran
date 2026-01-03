PROGRAM intrinsics_107
    integer(4) :: x = 5
    integer(8) :: y = 5
    WRITE (*,*) trailz(0_4)
    if ( trailz(0_4) /= 32 ) error stop
    WRITE (*,*) trailz(0_8)
    if ( trailz(0_8) /= 64 ) error stop
    WRITE (*,*) trailz(x)
    if ( trailz(x) /= 0 ) error stop
    WRITE (*,*) trailz(y)
    if ( trailz(y) /= 0 ) error stop
END PROGRAM