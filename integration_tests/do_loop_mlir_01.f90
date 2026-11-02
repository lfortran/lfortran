program do_loop_mlir_01
    implicit none
    integer :: i, j, total

    total = 0
    do i = 1, 3
        total = total + i
    end do
    if (total /= 6) error stop
    if (i /= 4) error stop

    do i = 2, 1
        error stop
    end do
    if (i /= 2) error stop

    total = 0
    do i = 5, 1, -2
        total = total + i
    end do
    if (total /= 9) error stop
    if (i /= -1) error stop

    do i = 1, 5
        if (i == 3) exit
    end do
    if (i /= 3) error stop

    do i = 2, 5
        exit
        error stop
    end do
    if (i /= 2) error stop

    total = 0
    outer: do i = 1, 3
        do j = 1, 3
            if (j == 2) cycle outer
            total = total + 1
        end do
    end do outer
    if (i /= 4) error stop
    if (j /= 2) error stop
    if (total /= 3) error stop

    outer_exit: do i = 1, 3
        do j = 1, 3
            if (j == 2) exit outer_exit
        end do
    end do outer_exit
    if (i /= 1) error stop
    if (j /= 2) error stop
end program
