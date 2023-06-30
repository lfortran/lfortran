program arrays_intrin_04
    integer :: i, j
    real(8) :: x(3, 5), xdiff


    do i = 1, 3
        do j = 1, 5
            x(i, j) = (i + j) / 7
        end do
    end do

    xdiff = abs(maxval(x) - (8 / 7))
    print *, xdiff
    if( xdiff > 1e-6 ) error stop

end program
