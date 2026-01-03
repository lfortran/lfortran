program test_product

    integer :: i, j
    integer(8) :: x(3, 5), xproduct, xproduct1(5), correct1(5), correct2(5)
    logical :: modx(3, 5)

    correct1 = (/24_8, 60_8, 120_8, 210_8, 336_8/)
    correct2 = (/3_8, 15_8, 5_8, 35_8, 7_8/)

    do i = lbound(x, 1), ubound(x, 1)
        do j = lbound(x, 2), ubound(x, 2)
            x(i, j) = i + j
            modx(i, j) = mod(i + j, 2) == 1
        end do
    end do

    xproduct = product(x)
    print *, xproduct
    if( xproduct /= 12192768000_8 ) error stop

    xproduct = product(x, mask=modx)
    print *, xproduct
    if( xproduct /= 55125 ) error stop

    xproduct1 = product(x, 1)
    print *, xproduct1
    do i = lbound(xproduct1, 1), ubound(xproduct1, 1)
        if( xproduct1(i) /= correct1(i) ) error stop
    end do

    xproduct1 = product(x, 1, mask=modx)
    print *, xproduct1
    do i = lbound(xproduct1, 1), ubound(xproduct1, 1)
        if( xproduct1(i) /= correct2(i) ) error stop
    end do

    xproduct1 = product(x, dim=1)
    print *, xproduct1
    do i = lbound(xproduct1, 1), ubound(xproduct1, 1)
        if( xproduct1(i) /= correct1(i) ) error stop
    end do

    xproduct1 = product(x, dim=1, mask=modx)
    print *, xproduct1
    do i = lbound(xproduct1, 1), ubound(xproduct1, 1)
        if( xproduct1(i) /= correct2(i) ) error stop
    end do

end program
