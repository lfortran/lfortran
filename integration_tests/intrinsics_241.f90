program intrinsics_241
    logical :: a, b
    logical(8) :: c, d
    logical(8), parameter :: e = logical(.true., 8)
    logical(4), parameter :: f = logical(.false., 4)
    logical(4), parameter :: ar1(3) = logical([.true., .false., .true.], 4)
    a = .true.
    b = .false.
    c = .true.
    d = .false.

    print *, e
    if (e .neqv. .true.) error stop
    print *, kind(f)
    if (f .neqv. .false.) error stop

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

    print *, ar1
    if(any(ar1 .neqv. [.true., .false., .true.])) error stop
end program 