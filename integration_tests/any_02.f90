program any_02
    logical :: l, c(2, 3, 4)
    logical :: c1(2), c2(3), c3(4)
    logical :: c4(2, 3), c5(2, 4), c6(3, 4)
    integer :: a(2, 3, 4), b(2, 3, 4), i, j, k

    a = 1
    b(1, :, :) = 1
    b(2, :, :) = 2
    c = a == b

    c6 = any(a == b, 1)
    print *, "c6", c6
    do i = lbound(c6, 1), ubound(c6, 1)
        do j = lbound(c6, 2), ubound(c6, 2)
            if (.not. c6(i, j)) error stop
        end do
    end do


    c5 = any(c, 2)
    print *, "c5", c5
    do j = lbound(c5, 2), ubound(c5, 2)
        if (.not. c5(1, j)) error stop
    end do
    do j = lbound(c5, 1), ubound(c5, 1)
        if (c5(2, j)) error stop
    end do

    c4 = any(c, 3)
    print *, "c4", c4
    do j = lbound(c4, 2), ubound(c4, 2)
        if (.not. c4(1, j)) error stop
    end do
    do j = lbound(c4, 1), ubound(c4, 1)
        if (c4(2, j)) error stop
    end do

    c3 = any(any(c, dim=1), 1)
    print *, "c3", c3
    do i = lbound(c3, 1), ubound(c3, 1)
        if (.not. c3(i)) error stop
    end do

    c2 = any(any(c, 1), 2)
    print *, "c2", c2
    do i = lbound(c2, 1), ubound(c2, 1)
        if (.not. c2(i)) error stop
    end do

    c1 = any(any(c, 2), dim=2)
    print *, "c1", c1
    if (.not. c1(1)) error stop
    if (c1(2)) error stop

    l = any(c)
    print *, l
    if (.not. l) error stop

    l = any(any(any(c, 2), dim=2), 1)
    print *, l
    if (.not. l) error stop

end program any_02
