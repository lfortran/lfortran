program intrinsics_385
    implicit none

    real(4) :: x, up, down

    x = 1.0

    up = nearest(x, 1.0)
    down = nearest(x, -1.0)
    print *, up
    print *, down
    if (up /= 1.00000012) error stop
    if (down /= 0.999999940) error stop
end program intrinsics_385
