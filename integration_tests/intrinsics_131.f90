program intrinsics_131
    integer :: a, b
    integer(kind=8) :: c, d
    a = 10
    b = 2

    c = 10_8
    d = 2_8
    print *, ishftc(a, b)
    if (ishftc(a, b) /= 40) error stop

    print *, ishftc(c, d)
    if (ishftc(c, d) /= 40_8) error stop

    print *, ishftc(10, 2)
    if (ishftc(10, 2) /= 40) error stop

    print *, ishftc(10_8, 2_8)
    if (ishftc(10_8, 2_8) /= 40_8) error stop

    print *, ishftc(10, -1)
    if (ishftc(10, -1) /= 5) error stop

end program
