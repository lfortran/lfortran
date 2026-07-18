program logical_short_circuit_01
    ! Fortran permits short-circuit evaluation of .and. and .or.. This test
    ! uses the common idiom where the first operand guards evaluation of the
    ! second so that an out-of-bounds array element (a(0)) is not accessed.
    ! With array bounds checking enabled this must not abort at run time.
    implicit none
    integer :: a(3)
    integer :: i
    logical :: r

    a = [10, 20, 30]

    ! .and.: when the guard (i >= 1) is false, the second operand a(i)
    ! must not be evaluated (it would index the out-of-bounds a(0)).
    i = 0
    if (i >= 1 .and. a(i) == 10) then
        error stop "and: guarded operand must not be evaluated"
    end if

    ! .and. with the guard true: the second operand is evaluated and used.
    i = 2
    r = (i >= 1 .and. a(i) == 20)
    if (.not. r) error stop "and: expected true"

    i = 1
    r = (i >= 1 .and. a(i) == 20)
    if (r) error stop "and: expected false"

    ! .or.: when the guard (i < 1) is true, the second operand must not
    ! be evaluated (it would index the out-of-bounds a(0)).
    i = 0
    if (.not. (i < 1 .or. a(i) == 10)) then
        error stop "or: guarded operand must not be evaluated"
    end if

    ! .or. with the guard false: the second operand is evaluated and used.
    i = 3
    r = (i < 1 .or. a(i) == 30)
    if (.not. r) error stop "or: expected true"

    i = 3
    r = (i < 1 .or. a(i) == 99)
    if (r) error stop "or: expected false"

    ! chained .and.: left-to-right association, so the leftmost false guard
    ! short-circuits the rest and no out-of-bounds access happens.
    i = 0
    if (i >= 1 .and. a(i) == 10 .and. a(i + 1) == 20) then
        error stop "chained and: guarded operand must not be evaluated"
    end if

    ! non-short-circuit operators still evaluate both operands correctly.
    if ((.true. .neqv. .false.) .neqv. .true.) error stop "neqv"
    if ((.true. .eqv. .true.) .neqv. .true.) error stop "eqv"

    print *, "All tests passed."
end program logical_short_circuit_01
