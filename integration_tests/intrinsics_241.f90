program intrinsics_241
    logical :: a, b
    logical(8) :: c, d
    a = .true.
    b = .false.
    c = .true.
    d = .false.

    print *, kind(logical(a, 8))
    if(kind(logical(a, 8)) /= 8) error stop

    print *, kind(logical(b, 4))
    if(kind(logical(b, 4)) /= 4) error stop

    print *, kind(logical(c, 8))
    if(kind(logical(c, 8)) /= 8) error stop

    print *, kind(logical(d, 4))
    if(kind(logical(d, 4)) /= 4) error stop

    print *, kind(logical(.true., 8))
    if(kind(logical(.true., 8)) /= 8) error stop

    print *, kind(logical(.false., 4))
    if(kind(logical(.false., 4)) /= 4) error stop

    print *, kind(logical(.true._8, 8))
    if(kind(logical(.true._8, 8)) /= 8) error stop

    print *, kind(logical(.false._8, 4))
    if(kind(logical(.false._8, 4)) /= 4) error stop

end program 