program while_10
    implicit none
    logical(1) :: finished(4), busy(3)
    logical(8) :: flags(2)
    integer :: count, i

    finished = .true.
    count = 0
    do while (all(finished))
        finished = .false.
        count = count + 1
        if (count > 1) error stop
    end do
    if (count /= 1) error stop

    busy = .true.
    count = 0
    do while (any(busy))
        count = count + 1
        busy(count) = .false.
        if (count > 3) error stop
    end do
    if (count /= 3) error stop
    if (any(busy)) error stop

    finished = .true.
    busy = .false.
    busy(2) = .true.
    count = 0
    do while (all(finished) .and. any(busy))
        count = count + 1
        if (count == 2) busy = .false.
        if (count > 2) error stop
    end do
    if (count /= 2) error stop

    flags = .false.
    i = 0
    do while (.not. all(flags))
        i = i + 1
        flags(i) = .true.
        if (i > 2) error stop
    end do
    if (i /= 2) error stop

    print *, count, i
end program
