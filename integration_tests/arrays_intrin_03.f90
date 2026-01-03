program arrays_intrin_03

    integer :: i, j
    integer(8) :: x(3, 5), xmaxval, xmaxval1(5), correct1(5), correct2(5)
    logical :: modx(3, 5)

    correct1 = (/4_8, 5_8, 6_8, 7_8, 8_8/)
    correct2 = (/3_8, 5_8, 5_8, 7_8, 7_8/)

    do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
            x(i, j) = i + j
            modx(i, j) = mod(i + j, 2) == 1
        end do
    end do

    xmaxval = maxval(x)
    print *, xmaxval
    if( xmaxval /= 8_8 ) error stop

    xmaxval = maxval(x, mask=modx)
    print *, xmaxval
    if( xmaxval /= 7_8 ) error stop

    xmaxval1 = maxval(x, 1)
    print *, xmaxval1
    do i = lbound(xmaxval1, 1), ubound(xmaxval1, 1)
        if( xmaxval1(i) /= correct1(i) ) error stop
    end do

    xmaxval1 = maxval(x, 1, mask=modx)
    print *, xmaxval1
    do i = lbound(xmaxval1, 1), ubound(xmaxval1, 1)
        if( xmaxval1(i) /= correct2(i) ) error stop
    end do

    xmaxval1 = maxval(x, dim=1)
    print *, xmaxval1
    do i = lbound(xmaxval1, 1), ubound(xmaxval1, 1)
        if( xmaxval1(i) /= correct1(i) ) error stop
    end do

    xmaxval1 = maxval(x, dim=1, mask=modx)
    print *, xmaxval1
    do i = lbound(xmaxval1, 1), ubound(xmaxval1, 1)
        if( xmaxval1(i) /= correct2(i) ) error stop
    end do

end program
