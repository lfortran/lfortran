program do_loop_22
    ! EXIT and CYCLE of an enclosing DO, DO WHILE, BLOCK or SELECT CASE construct.
    implicit none
    integer :: i, j, k, n, cnt

    cnt = 0
    outer: do i = 1, 4
        do j = 1, 4
            if (j == 2) cycle outer
            if (i == 3) exit outer
            cnt = cnt + 1
        end do
    end do outer
    print *, i, j, cnt
    if (i /= 3 .or. j /= 1 .or. cnt /= 2) error stop

    ! Three loops deep, and a second inner loop after the one that leaves.
    cnt = 0
    n = 3
    a: do i = 1, 3
        b: do j = 1, 3
            do k = 1, 3
                if (k == 2) cycle b
                if (j == 3) cycle a
                cnt = cnt + 1
            end do
        end do b
        do k = 1, n
            n = n + 0
            cnt = cnt + 100
        end do
    end do a
    print *, i, j, k, cnt
    if (i /= 4 .or. j /= 3 .or. k /= 1 .or. cnt /= 6) error stop

    ! A DO WHILE loop.
    cnt = 0
    i = 0
    w: do while (i < 5)
        i = i + 1
        do j = 1, 3
            if (i == 2) cycle w
            if (i == 4) exit w
            cnt = cnt + 1
        end do
    end do w
    print *, i, cnt
    if (i /= 4 .or. cnt /= 6) error stop

    ! An EXIT of a BLOCK, and an EXIT in a BLOCK of the loop around it.
    cnt = 0
    do i = 1, 5
        blk: block
            if (i == 2) exit blk
            if (i == 4) exit
            cnt = cnt + 1
        end block blk
        cnt = cnt + 10
    end do
    print *, i, cnt
    if (i /= 4 .or. cnt /= 32) error stop

    ! An EXIT of a named SELECT CASE.
    cnt = 0
    do i = 1, 3
        sel: select case (i)
        case (2)
            if (cnt > 0) exit sel
            cnt = cnt + 100
        case default
            cnt = cnt + 1
        end select sel
        cnt = cnt + 10
    end do
    print *, i, cnt
    if (i /= 4 .or. cnt /= 32) error stop
end program
