program arrays_intrin_06
    integer :: i, j
    integer, parameter :: dp = kind(0.d0)
    real(dp) :: x(3, 5), xdiff
    do i = 1, 3
        do j = 1, 5
            x(i, j) = (i + j) / 7._dp
        end do
    end do
    print *, minval(x)
    if (abs(minval(x) - 0.28571428571428570) > 1e-6) error stop
end program
