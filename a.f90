program main
    logical(4)::x = .true.
    logical(kind = 8)::y = .true.

    print*, logical(.true., 8)
    if (logical(.true., 8) .neqv. y) error stop

    print*, kind(y)
    print*, kind(logical(.true., 8))
    print*, logical(x, 4)
    if (logical(x, 4) .neqv. .true.) error stop

    print*, logical(y)
    if (logical(y) .neqv. .false.) error stop

    print*, kind(logical(x, 8))
    print*, kind(logical(y))
end
