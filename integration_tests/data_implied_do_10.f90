program data_implied_do_10
    implicit none
    integer :: i, j, k
    real :: skew(5, 5)

    data ((skew(k, j), j = 1, k), k = 1, 5) / 15 * 0.0 /
    data ((skew(k, j), j = k + 1, 5), k = 1, 4) / 10 * 1.0 /

    do i = 1, 5
        do j = 1, 5
            if (j <= i) then
                if (skew(i, j) /= 0.0) error stop
            else
                if (skew(i, j) /= 1.0) error stop
            end if
        end do
    end do
end program data_implied_do_10
