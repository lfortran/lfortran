program intrinsics_131
    integer :: a, b
    integer(kind=8) :: c, d

    a = 10
    b = 2
    c = 10_8
    d = 2_8


    print *, ishftc(a, b)
    if (ishftc(a, b) /= 40) error stop

    print *, ishftc(a, -b)
    ! if (ishftc(a, -b) /= -2147483646) error stop TODO: Fix this

    print *, ishftc(-a, b)
    ! if (ishftc(-a, b) /= -37) error stop TODO: Fix this

    print *, ishftc(-a, -b)
    ! if (ishftc(-a, -b) /= -1073741827) error stop TODO: Fix this


    print *, ishftc(10, 2)
    if (ishftc(10, 2) /= 40) error stop

    print *, ishftc(10, -2)
    ! ! if (ishftc(10, -2) /= (-2147483646)) error stop TODO: Fix this

    print *, ishftc(-10, 2)
    ! ! if (ishftc(-10, 2) /= -37) error stop TODO: Fix this

    print *, ishftc(-10, -2)
    ! ! if (ishftc(-10, -2) /= -1073741827) error stop TODO: Fix this


    print *, ishftc(c, d)
    if (ishftc(c, d) /= 40_8) error stop

    print *, ishftc(c, -d)
    ! ! if (ishftc(c, -d) /= -9223372036854775806_8) error stop TODO: Fix this

    print *, ishftc(-c, d)
    ! ! if (ishftc(-c, d) /= -37_8) error stop TODO: Fix this

    print *, ishftc(-c, -d)
    ! ! if (ishftc(-c, -d) /= -4611686018427387907_8) error stop TODO: Fix this


    print *, ishftc(10_8, 2_8)
    if (ishftc(10_8, 2_8) /= 40_8) error stop

    print *, ishftc(10_8, -2_8)
    ! ! if (ishftc(10_8, -2_8) /= -9223372036854775806_8) error stop TODO: Fix this

    print *, ishftc(-10_8, 2_8)
    ! ! if (ishftc(-10_8, 2_8) /= -37_8) error stop TODO: Fix this

    print *, ishftc(-10_8, -2_8)
    ! if (ishftc(-10_8, -2_8) /= -4611686018427387907_8) error stop TODO: Fix this
   
end program
