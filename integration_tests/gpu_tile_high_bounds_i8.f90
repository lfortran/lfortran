program gpu_tile_high_bounds_i8
    implicit none
    integer :: a(64, 62), b(62, 64), c(64, 32), d(34, 32)
    integer(8) :: i, j, lo, hi

    ! Widening integer(4) arithmetic alone cannot protect integer(8) indices.
    ! test_gpu.cpp pins both 61 x 62 nests to 4096 tiled threads.
    lo = huge(0_8) - 61_8
    hi = huge(0_8) - 1_8
    a = 0
    b = 0
    do concurrent (i = lo:hi, j = 1_8:62_8)
        if (i >= lo) then
            a(i - lo + 1_8, j) = 1
        else
            a(64, j) = 2
        end if
    end do
    do concurrent (i = 1_8:62_8, j = lo:hi)
        if (j >= lo) then
            b(i, j - lo + 1_8) = 1
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
    lo = -huge(0_8)
    hi = lo + 60_8
    c = 0
    do concurrent (j = -31_8:-1_8, i = lo:hi)
        c(i - lo + 1_8, j + 32_8) = 1
    end do
    if (any(c(1:61, 1:31) /= 1)) error stop 5
    if (any(c(62:64, :) /= 0)) error stop 6
    if (any(c(:, 32) /= 0)) error stop 7

    ! The same high-bound reconstruction in a nest that must stay flat.
    lo = huge(0_8) - 33_8
    hi = huge(0_8) - 1_8
    d = 0
    do concurrent (j = -15_8:15_8, i = lo:hi)
        d(i - lo + 1_8, j + 16_8) = 1
    end do
    if (any(d(1:33, 1:31) /= 1)) error stop 8
    if (any(d(34, :) /= 0)) error stop 9
    if (any(d(:, 32) /= 0)) error stop 10
    print *, "ok"
end program
