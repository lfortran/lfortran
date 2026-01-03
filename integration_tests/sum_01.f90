program test_sum

    integer :: x(5, 10), i, j, xsum, xsum1(10), correct1(10), correct2(10)
    logical :: modx(5, 10)

    correct1 = (/20, 25, 30, 35, 40, 45, 50, 55, 60, 65/)
    correct2 = (/8, 15, 12, 21, 16, 27, 20, 33, 24, 39/)

    do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
            x(i, j) = i + j
            modx(i, j) = mod(i + j, 2) == 1
        end do
    end do

    xsum = sum(x)
    print *, xsum
    if( xsum /= 425 ) error stop

    xsum = sum(x, mask=modx)
    print *, xsum
    if( xsum /= 215 ) error stop

    xsum1 = sum(x, 1)
    print *, xsum1
    do i = lbound(xsum1, 1), ubound(xsum1, 1)
        if( xsum1(i) /= correct1(i) ) error stop
    end do

    xsum1 = sum(x, 1, mask=modx)
    print *, xsum1
    do i = lbound(xsum1, 1), ubound(xsum1, 1)
        if( xsum1(i) /= correct2(i) ) error stop
    end do

    xsum1 = sum(x, dim=1)
    print *, xsum1
    do i = lbound(xsum1, 1), ubound(xsum1, 1)
        if( xsum1(i) /= correct1(i) ) error stop
    end do

    xsum1 = sum(x, dim=1, mask=modx)
    print *, xsum1
    do i = lbound(xsum1, 1), ubound(xsum1, 1)
        if( xsum1(i) /= correct2(i) ) error stop
    end do

end program
