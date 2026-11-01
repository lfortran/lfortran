program do_variable_after_loop_01
    ! Using the variable of a DO loop after the loop is conforming, and
    ! gives a style suggestion only when the variable is read before it is
    ! assigned again.
    implicit none
    integer :: i, j, k, m, n, s, a(3)
    n = 4
    s = 0

    ! Suggested: the value after a normal end, known at compile time.
    do i = 1, 10
        s = s + i
    end do
    if (i /= 11) error stop

    ! Suggested: an EXIT may leave the loop.
    do j = 1, n
        if (j == 3) exit
    end do
    if (j /= 3) error stop

    ! Suggested: run-time bounds, read in a nested construct.
    do k = n, 1, -1
        s = s + k
    end do
    if (s > 0) then
        if (k /= 0) error stop
    end if

    ! Suggested: no iterations, the variable is the start value.
    do i = 5, 1
    end do
    if (i /= 5) error stop

    ! Not suggested: assigned again before it is read.
    do i = 1, 3
    end do
    i = 7
    if (i /= 7) error stop

    ! Not suggested: the next loop sets the variable again.
    do j = 1, 2
    end do
    do j = 1, 3
        s = s + j
    end do

    ! Not suggested: a DO CONCURRENT, an implied DO loop or a FORALL with
    ! the same variable does not read its value.
    do m = 1, 3
    end do
    do concurrent (m = 1:3)
        a(m) = m
    end do
    a = [(m * 2, m = 1, 3)]
    if (any(a /= [2, 4, 6])) error stop
    forall (m = 1:3) a(m) = m
    if (any(a /= [1, 2, 3])) error stop
    m = 0

    ! Not suggested: assigned again only in a branch, which may not run.
    do i = 1, 2
    end do
    if (s > 0) then
        i = 3
        if (i /= 3) error stop
    end if
    print *, s
end program
