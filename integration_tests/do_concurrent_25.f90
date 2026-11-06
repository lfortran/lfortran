program do_concurrent_25
    ! Logical operators in the DO CONCURRENT REDUCE locality specifier
    implicit none
    integer, parameter :: n = 10
    logical :: mask(n)
    logical :: all_set, any_set, eqv_r, neqv_r
    logical :: all_pos, any_big
    integer :: i, j, a(n, 3)

    do i = 1, n
        mask(i) = mod(i, 3) == 0
        do j = 1, 3
            a(i, j) = i * j
        end do
    end do

    ! mask has three .true. values (i = 3, 6, 9)
    all_set = .true.
    any_set = .false.
    eqv_r = .true.
    neqv_r = .false.
    do concurrent (i = 1:n) reduce(.and.: all_set) reduce(.or.: any_set) &
            reduce(.eqv.: eqv_r) reduce(.neqv.: neqv_r)
        all_set = all_set .and. mask(i)
        any_set = any_set .or. mask(i)
        eqv_r = eqv_r .eqv. mask(i)
        neqv_r = neqv_r .neqv. mask(i)
    end do
    print *, all_set, any_set, eqv_r, neqv_r
    if (all_set) error stop
    if (.not. any_set) error stop
    ! .eqv. over n values starting from .true.: .true. iff the number of
    ! .false. values (7) is even
    if (eqv_r) error stop
    ! .neqv. starting from .false.: .true. iff the number of .true. values
    ! (3) is odd
    if (.not. neqv_r) error stop

    ! Two variables in one clause, multiple loop indices, upper-case operators
    all_pos = .true.
    any_big = .false.
    do concurrent (i = 1:n, j = 1:3) REDUCE(.AND.: all_pos) REDUCE(.OR.: any_big)
        all_pos = all_pos .and. a(i, j) > 0
        any_big = any_big .or. a(i, j) > 25
    end do
    if (.not. all_pos) error stop
    if (.not. any_big) error stop

    all_pos = .true.
    any_big = .false.
    do concurrent (i = 1:n) reduce(.and.: all_pos, any_big)
        all_pos = all_pos .and. a(i, 1) > 0
        any_big = any_big .and. a(i, 1) > 0
    end do
    if (.not. all_pos) error stop
    if (any_big) error stop

    print *, "ok"
end program
