program intrinsics_67
    double precision :: x, y

    x = 1.0D0
    y = -2.0D0

    print *, dsign(x, y)
    print *, dsign(x, -y)
    print *, dsign(-x, y)
    print *, dsign(-x, -y)
    if (abs(dsign(x, y) - (-1.0D0)) > 1d-14) error stop
    if (abs(dsign(x, -y) - (1.0D0)) > 1d-14) error stop
    if (abs(dsign(-x, y) - (-1.0D0)) > 1d-14) error stop
    if (abs(dsign(-x, -y) - (1.0D0)) > 1d-14) error stop

    print *, dsign(0.0D0, y)
    print *, dsign(0.0D0, -y)
    print *, dsign(x, 0.0D0)
    print *, dsign(-x, 0.0D0)

    if (abs(dsign(0.0D0, y) - (-0.0D0)) > 1d-14) error stop
    if (abs(dsign(0.0D0, -y) - (0.0D0)) > 1d-14) error stop
    if (abs(dsign(x, 0.0D0) - (1.0D0)) > 1d-14) error stop
    if (abs(dsign(-x, 0.0D0) - (1.0D0)) > 1d-14) error stop

    print *, dsign(0.0D0, 0.0D0)
    print *, dsign(-0.0D0, 0.0D0)
    print *, dsign(0.0D0, -0.0D0)
    print *, dsign(-0.0D0, -0.0D0)
    if (abs(dsign(0.0D0, 0.0D0) - (0.0D0)) > 1d-14) error stop
    if (abs(dsign(-0.0D0, 0.0D0) - (0.0D0)) > 1d-14) error stop
    if (abs(dsign(0.0D0, -0.0D0) - (-0.0D0)) > 1d-14) error stop
    if (abs(dsign(-0.0D0, -0.0D0) - (-0.0D0)) > 1d-14) error stop
end program
