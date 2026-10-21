program gpu_tile_high_bounds_i4
    implicit none
    integer :: a(64, 62), b(62, 64), c(64, 32), d(34, 32)
    integer(4) :: i, j, lo, hi

    ! These partial tiles must be rejected before adding a high lower bound.
    ! test_gpu.cpp pins both 61 x 62 nests to 4096 tiled threads.
    lo = huge(0) - 61
    hi = huge(0) - 1
    a = 0
    b = 0
    do concurrent (i = lo:hi, j = 1:62)
        if (i >= lo) then
            a(i - lo + 1, j) = 1
        else
            a(64, j) = 2
        end if
    end do
    do concurrent (i = 1:62, j = lo:hi)
        if (j >= lo) then
            b(i, j - lo + 1) = 1
        else
            b(i, 64) = 2
        end if
    end do
    print *, "high-bound padding:", sum(a(62:64, :)), sum(b(:, 62:64))
    if (any(a(1:61, :) /= 1)) error stop 1
    if (any(b(:, 1:61) /= 1)) error stop 2
    if (any(a(62:64, :) /= 0)) error stop 3
    if (any(b(:, 62:64) /= 0)) error stop 4

    ! Negative bounds, including one near the lowest representable index.
    lo = -huge(0)
    hi = lo + 60
    c = 0
    do concurrent (j = -31:-1, i = lo:hi)
        c(i - lo + 1, j + 32) = 1
    end do
    if (any(c(1:61, 1:31) /= 1)) error stop 5
    if (any(c(62:64, :) /= 0)) error stop 6
    if (any(c(:, 32) /= 0)) error stop 7

    ! The same high-bound reconstruction in a nest that must stay flat.
    lo = huge(0) - 33
    hi = huge(0) - 1
    d = 0
    do concurrent (j = -15:15, i = lo:hi)
        d(i - lo + 1, j + 16) = 1
    end do
    if (any(d(1:33, 1:31) /= 1)) error stop 8
    if (any(d(34, :) /= 0)) error stop 9
    if (any(d(:, 32) /= 0)) error stop 10
    print *, "ok"
end program
