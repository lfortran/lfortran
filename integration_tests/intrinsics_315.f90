program intrinsics_315
    integer (kind = 4):: a1 = 2147483647
    integer (kind = 8):: a2 = 2147483648_8
    integer (kind = 4):: b1 = 0_4
    integer (kind = 8) :: b2 = 2147483648_8
    logical :: c = .TRUE.
    real(kind = 4) :: r1 = 1.0_4
    real(kind = 8) :: r2 = 3.4028234663852886e+40_8
    real(kind = 4) :: p1 = 1.0_4
    real(kind = 8) :: p2 = 3.4028234663852886e+40_8

    print *, OUT_OF_RANGE(2147483648_8, 0_4, .TRUE.)
    if (OUT_OF_RANGE(2147483648_8, 0_4, .TRUE.) .neqv. .true.) error stop
    
    print *, OUT_OF_RANGE(6372_4, 2147483648_8, .TRUE.)
    if (OUT_OF_RANGE(6372_4, 2147483648_8, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(6372_4, 2147_4, .TRUE.)
    if (OUT_OF_RANGE(6372_4, 2147_4, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(2147483648_8, 9223372036854775807_8, .TRUE.)
    if (OUT_OF_RANGE(2147483648_8, 9223372036854775807_8, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(a2, b1, c)
    if (OUT_OF_RANGE(a2, b1, c) .neqv. .true.) error stop

    print *, OUT_OF_RANGE(a1, b2, c)
    if (OUT_OF_RANGE(a1, b2, c) .neqv. .false.) error stop

    a1 = 6372
    a2 = 2147483648_8
    b1 = 2147_4
    b2 = 9223372036854775807_8

    print *, OUT_OF_RANGE(a1, b1, c)
    if (OUT_OF_RANGE(a1, b1, c) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(a1, b2, c)
    if (OUT_OF_RANGE(a1, b2, c) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(3.4028234663852886e+40_8, 1.0_4, .TRUE.)
    if (OUT_OF_RANGE(3.4028234663852886e+40_8, 1.0_4, .TRUE.) .neqv. .true.) error stop

    print *, OUT_OF_RANGE(1.0_4, 3.4028234663852886e+40_8, .TRUE.)
    if (OUT_OF_RANGE(1.0_4, 3.4028234663852886e+40_8, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(3.4028235e+35_4, 1.0_4, .TRUE.)
    if (OUT_OF_RANGE(3.4028235e+35_4, 1.0_4, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(1.7976931348623158e+308_8, 2.0_8, .TRUE.)
    if (OUT_OF_RANGE(1.7976931348623158e+308_8, 2.0_8, .TRUE.) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(r2, p1, c)
    if (OUT_OF_RANGE(r2, p1, c) .neqv. .true.) error stop

    print *, OUT_OF_RANGE(r2, p2, c)
    if (OUT_OF_RANGE(r2, p2, c) .neqv. .false.) error stop

    r1 = 1.0_4
    r2 = 1.7976931348623158e+308_8
    p1 = 2.0_4
    p2 = 3.4028235e+38_4

    print *, OUT_OF_RANGE(r1, p1, c)
    if (OUT_OF_RANGE(r1, p1, c) .neqv. .false.) error stop

    print *, OUT_OF_RANGE(r2, p2, c)
    if (OUT_OF_RANGE(r2, p2, c) .neqv. .false.) error stop

end program
