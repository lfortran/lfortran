program arrays_intrin_05

    integer :: i, j
    integer(8) :: x(3, 5), xminval, xminval1(5), correct1(5), correct2(5)
    logical :: modx(3, 5)

    correct1 = [2_8, 3_8, 4_8, 5_8, 6_8]
    correct2 = [3_8, 3_8, 5_8, 5_8, 7_8]

    do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
            x(i, j) = i + j
            modx(i, j) = mod(i + j, 2) == 1
        end do
    end do

    print *, x
    xminval = minval(x)
    print *, xminval
    if( xminval /= 2_8 ) error stop

    xminval = minval(x, mask=modx)
    print *, xminval
    if( xminval /= 3_8 ) error stop

    xminval1 = minval(x, 1)
    print *, xminval1
    do i = lbound(xminval1, 1), ubound(xminval1, 1)
        if( xminval1(i) /= correct1(i) ) error stop
    end do

    xminval1 = minval(x, 1, mask=modx)
    print *, xminval1
    do i = lbound(xminval1, 1), ubound(xminval1, 1)
        if( xminval1(i) /= correct2(i) ) error stop
    end do

    xminval1 = minval(x, dim=1)
    print *, xminval1
    do i = lbound(xminval1, 1), ubound(xminval1, 1)
        if( xminval1(i) /= correct1(i) ) error stop
    end do

    xminval1 = minval(x, dim=1, mask=modx)
    print *, xminval1
    do i = lbound(xminval1, 1), ubound(xminval1, 1)
        if( xminval1(i) /= correct2(i) ) error stop
    end do

end program