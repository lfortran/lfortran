program intrinsics_123

    print*, lshift(10_4, 0)
    if (.not. lshift(10_4, 0) == 10) error stop

    print*, lshift(-10_8, 1)
    if (.not. lshift(-10_8, 1) == -20 ) error stop

    print*, lshift(not(10_4), 0)
    if (.not. lshift(not(10_4), 0) == -11 ) error stop

    print*, lshift(not(10_4), 2)
    if (.not. lshift(not(10_4), 2) == -44 ) error stop

    print*, lshift(not(10_8), 3)
    if (.not. lshift(not(10_8), 3) == -88 ) error stop

end