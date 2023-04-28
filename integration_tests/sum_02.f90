program sum_02

    integer :: x(5, 10), xdiff, i, j

    do i = 1, 5
        do j = 1, 10
            x(i, j) = i + j
        end do
    end do

    xdiff = abs(sum(x) - 425)
    print *, xdiff
    if( xdiff /= 0 ) error stop

end program
