program intrinsics_481
    ! sign() on real(4) and real(8) at run time, including a negative zero
    ! second argument (#13918)
    implicit none
    real :: x, y, z, r
    real(8) :: xd, yd, zd, rd

    x = -1.0
    y = 2.0
    z = -0.0

    r = sign(y, x)
    print *, r
    if (r /= -2.0) error stop

    r = sign(x, y)
    print *, r
    if (r /= 1.0) error stop

    r = sign(y, z)
    print *, r
    if (r /= -2.0) error stop

    r = sign(x, -z)
    print *, r
    if (r /= 1.0) error stop

    xd = -1.5d0
    yd = 3.25d0
    zd = -0.0d0

    rd = sign(yd, xd)
    print *, rd
    if (rd /= -3.25d0) error stop

    rd = sign(xd, yd)
    print *, rd
    if (rd /= 1.5d0) error stop

    rd = sign(yd, zd)
    print *, rd
    if (rd /= -3.25d0) error stop

    rd = sign(xd, -zd)
    print *, rd
    if (rd /= 1.5d0) error stop
end program
