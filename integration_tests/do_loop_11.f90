program do_loop_11
    ! The DO variable keeps a well defined value once the loop terminates: it
    ! is the value that first failed the loop test, i.e. start + n*step where n
    ! is the iteration count (F2018 11.1.7.4.3, 11.1.7.4.5). An EXIT instead
    ! leaves it at the value the interrupted iteration was using.
    implicit none

    integer :: i, j, n, k
    integer :: table(3)

    ! Unit step
    do i = 1, 5
    end do
    if (i /= 6) error stop

    ! Step that does not divide the trip count evenly
    do i = 1, 10, 3
    end do
    if (i /= 13) error stop

    do i = 1, 10, 4
    end do
    if (i /= 13) error stop

    ! Negative step
    do i = 10, 1, -2
    end do
    if (i /= 0) error stop

    do i = 10, 1, -3
    end do
    if (i /= -2) error stop

    ! Single iteration
    do i = 3, 3
    end do
    if (i /= 4) error stop

    ! Zero-trip loops keep the start value
    do i = 1, 0
    end do
    if (i /= 1) error stop

    do i = 5, 1
    end do
    if (i /= 5) error stop

    do i = 1, 10, -1
    end do
    if (i /= 1) error stop

    ! Bounds and step that are only known at run time
    n = 3
    k = 2
    do i = 1, n, k
    end do
    if (i /= 5) error stop

    k = -2
    do i = 9, 1, k
    end do
    if (i /= -1) error stop

    n = 0
    do i = 1, n
    end do
    if (i /= 1) error stop

    ! EXIT leaves the DO variable at the value of the interrupted iteration
    do i = 1, 5
        if (i == 3) exit
    end do
    if (i /= 3) error stop

    do i = 1, 5
        if (i == 2) then
            exit
        else
            j = i
        end if
    end do
    if (i /= 2) error stop
    if (j /= 1) error stop

    do i = 10, 1, -3
        if (i == 4) exit
    end do
    if (i /= 4) error stop

    ! A named EXIT leaves the loop it names, from any depth
    outer: do i = 1, 10
        do j = 1, 3
            if (i == 4) exit outer
        end do
    end do outer
    if (i /= 4) error stop
    if (j /= 1) error stop

    ! An unnamed EXIT only leaves the innermost loop
    do i = 1, 3
        do j = 1, 5
            if (j == 2) exit
        end do
        if (j /= 2) error stop
    end do
    if (i /= 4) error stop

    ! The zero-trip guard the standard makes possible
    table = [10, 20, 30]
    n = 0
    do i = 1, n
        if (table(i) == 20) exit
    end do
    if (i <= n) error stop

    print *, "ok"
end program
