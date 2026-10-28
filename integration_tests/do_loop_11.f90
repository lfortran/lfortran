program do_loop_11
    implicit none
    integer :: i, j, total, first, last, step, calls
    integer :: alias
    integer(8) :: wide
    equivalence (i, alias)

    do i = 1, 5
    end do
    if (alias /= 6) error stop "normal completion"

    do i = 1, 0
        error stop "positive zero trip body"
    end do
    if (i /= 1) error stop "positive zero trip index"
    do i = 0, 1, -1
        error stop "negative zero trip body"
    end do
    if (i /= 0) error stop "negative zero trip index"

    total = 0
    do i = 5, 1, -2
        total = total + i
    end do
    if (i /= -1 .or. total /= 9) error stop "negative completion"
    do wide = 2_8, 6_8, 3_8
    end do
    if (wide /= 8_8) error stop "integer kind"

    do i = 1, 5
        if (i == 3) exit
    end do
    if (i /= 3) error stop "conditional exit"
    do i = 2, 5
        exit
    end do
    if (i /= 2) error stop "direct exit"

    total = 0
    do i = 1, 5
        if (i >= 3) cycle
        total = total + i
    end do
    if (i /= 6 .or. total /= 3) error stop "cycle completion"
    do i = 5, 1, -2
        cycle
    end do
    if (i /= -1) error stop "negative cycle completion"

    outer: do i = 1, 5
        do j = 1, 5
            if (j == 2) exit outer
        end do
    end do outer
    if (i /= 1 .or. j /= 2) error stop "named outer exit"

    total = 0
    outer_cycle: do i = 1, 3
        do j = 1, 4
            if (j == 2) cycle outer_cycle
            total = total + 1
        end do
    end do outer_cycle
    if (i /= 4 .or. j /= 2 .or. total /= 3) error stop "named outer cycle"

    do i = 1, 2
        do j = 1, 3
            exit
        end do
        if (j /= 1) error stop "inner exit"
    end do
    if (i /= 3) error stop "outer completion"

    total = 0
    unbounded: do
        total = total + 1
        do j = 1, 2
            if (total == 3) exit unbounded
        end do
        cycle unbounded
    end do unbounded
    if (total /= 3 .or. j /= 1) error stop "unbounded exit"

    do i = 1, 5
        if (i == 3) go to 10
    end do
    error stop "goto not taken"
10  if (alias /= 3) error stop "goto index"

    first = 2
    last = 6
    step = 2
    calls = 0
    total = 0
    do i = bound(first), bound(last), bound(step)
        total = total + i
        first = 100
        last = 100
        step = 1
    end do
    if (calls /= 3 .or. i /= 8 .or. total /= 12) error stop "bounds evaluated once"

    first = 6
    last = 2
    step = -2
    do i = first, last, step
        last = 100
        step = 1
    end do
    if (i /= 0) error stop "variable negative step"

    total = 0
    do i = 1, 35
        total = total + i
    end do
    if (i /= 36 .or. total /= 630) error stop "unrolled completion"
contains
    integer function bound(value)
        integer, intent(in) :: value
        calls = calls + 1
        bound = value
    end function
end program
