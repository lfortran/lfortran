program intrinsics_144
    logical :: a(3), b(3)
    logical :: res

    a = .true.
    b = .false.

    res = dot_product(a, b)
    print *, res
    if (res) error stop

    res = dot_product([.true., .true., .true.], [.false., .false., .false.])
    print *, res
    if (res) error stop


    res = dot_product([.true., .true., .true.], [.true., .true., .true.])
    print *, res
    if (.not. res) error stop

    b = .true.
    res = dot_product(a, b)
    print *, res
    if (.not. res) error stop

    res = dot_product([.false., .false., .false.], [.false., .false., .false.])
    print *, res
    if (res) error stop

    a = .false.
    b = .false.

    res = dot_product(a, b)
    print *, res
    if (res) error stop

end program
