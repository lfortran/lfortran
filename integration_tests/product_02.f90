program product_02
    integer :: i, j
    integer(8) :: x(3, 5), xdiff


    do i = 1, 3
        do j = 1, 5
            x(i, j) = i + j
        end do
    end do

    xdiff = abs(product(x) - 12192768000_8)
    print *, xdiff
    if( xdiff /= 0_8 ) error stop

end program
