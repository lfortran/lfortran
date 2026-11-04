program gpu_metal_713
    ! Index-set splitting of nests whose frame rows, placed in front of the
    ! interior, lie below -huge(0) - 1, or whose bounds lie at huge(0): the
    ! frame arithmetic is done in integer(8), and the split is not taken
    ! when the rows do not fit the loop index's kind.
    implicit none
    integer :: d(4), e(8), f(3, 1000), h(4, 4), k(4), i, j

    ! A strip of the inner dimension: one row of frame in front of -huge(0).
    d = 0
    do concurrent (j = -2147483647:-2147483644, i = 1:4)
        if (i == 1) d(j + 2147483648_8) = 1
    end do
    print *, d
    if (any(d /= 1)) error stop 1

    ! The same strip one row higher: the first row is -huge(0) - 1.
    k = 0
    do concurrent (j = -2147483646:-2147483643, i = 1:4)
        if (i == 1) k(j + 2147483647_8) = 1
    end do
    print *, k
    if (any(k /= 1)) error stop 5

    ! One dimension, two iterations of frame at each end.
    e = 0
    do concurrent (j = -2147483647:-2147483640)
        if (j < -2147483645) then
            e(j + 2147483648_8) = 1
        else if (j > -2147483642) then
            e(j + 2147483648_8) = 2
        else
            e(j + 2147483648_8) = 3
        end if
    end do
    print *, e
    if (any(e /= [1, 1, 3, 3, 3, 3, 2, 2])) error stop 2

    ! An interior one column wide: the strips take two rows of frame for
    ! every row of the interior, far below its first row.
    f = 0
    do concurrent (j = -2147483000:-2147482001, i = 1:3)
        if (i == 1) then
            f(i, j + 2147483001) = 1
        else if (i == 3) then
            f(i, j + 2147483001) = 3
        else
            f(i, j + 2147483001) = 2
        end if
    end do
    print *, sum(f(1, :)), sum(f(2, :)), sum(f(3, :))
    if (any(f(1, :) /= 1) .or. any(f(2, :) /= 2) .or. any(f(3, :) /= 3)) &
        error stop 3

    ! The upper end of the domain at huge(0).
    h = 0
    do concurrent (j = 2147483644:2147483647, i = 1:4)
        if (j == 2147483647 .or. i == 4) then
            h(i, j - 2147483643) = 1
        else
            h(i, j - 2147483643) = 2
        end if
    end do
    print *, h
    if (any(h(4, :) /= 1) .or. any(h(:, 4) /= 1) .or. any(h(1:3, 1:3) /= 2)) &
        error stop 4
end program
