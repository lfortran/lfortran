program any_03
    implicit none
    integer :: a(2, 3), b(2, 3)
    logical :: res1(3), res2(2)
    logical :: all_res1(3), all_res2(2)

    a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    b = reshape([1, 0, 3, 4, 0, 6], [2, 3])

    ! a == b is:
    ! [[T, T, F],
    !  [F, T, T]]

    res1 = any(a == b, 1)
    if (res1(1) .neqv. .true.) error stop
    if (res1(2) .neqv. .true.) error stop
    if (res1(3) .neqv. .true.) error stop

    res2 = any(a == b, 2)
    if (res2(1) .neqv. .true.) error stop
    if (res2(2) .neqv. .true.) error stop

    all_res1 = all(a == b, 1)
    if (all_res1(1) .neqv. .false.) error stop
    if (all_res1(2) .neqv. .true.) error stop
    if (all_res1(3) .neqv. .false.) error stop

    all_res2 = all(a == b, 2)
    if (all_res2(1) .neqv. .false.) error stop
    if (all_res2(2) .neqv. .false.) error stop
end program
