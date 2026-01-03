program arrays_intrin_04
    integer :: i, j
    integer, parameter :: dp = kind(0.d0)
    real(dp) :: x(3, 5), xdiff


    do i = 1, 3
        do j = 1, 5
            x(i, j) = (i + j) / 7._dp
        end do
    end do

    xdiff = abs(maxval(x) - (8._dp / 7))
    print *, xdiff, maxval(x)
    if( xdiff > 1e-6_dp ) error stop

end program
