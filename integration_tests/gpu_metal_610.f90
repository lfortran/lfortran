program gpu_metal_610
    ! Offloaded nests of two loops whose iterations run in several launches
    ! of the kernel (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=7, so a chunk covers the whole of the fastest
    ! dimension when it has at most 7 values and part of the next one, and
    ! splits the fastest dimension itself when it has more.
    implicit none
    integer :: a(10, 9), b(3, 20), c(-4:6, 2:12), e(5, 6), i, j, n, m
    integer :: lo1, hi1, lo2, hi2

    ! Constant extents; the fastest dimension, of 10, is split.
    a = 0
    do concurrent (i = 1:10, j = 1:9)
        a(i, j) = 100 * i + j
    end do
    do j = 1, 9
        do i = 1, 10
            if (a(i, j) /= 100 * i + j) error stop 1
        end do
    end do

    ! Constant extents; the fastest dimension, of 3, fits, and the second
    ! is split into ranges of 2 values.
    b = 0
    do concurrent (i = 1:3, j = 1:20)
        b(i, j) = 100 * i + j
    end do
    do j = 1, 20
        do i = 1, 3
            if (b(i, j) /= 100 * i + j) error stop 2
        end do
    end do

    ! Run-time bounds, negative ones among them.
    lo1 = -4
    hi1 = 6
    lo2 = 2
    hi2 = 12
    c = 0
    do concurrent (i = lo1:hi1, j = lo2:hi2)
        c(i, j) = 100 * i + j
    end do
    do j = 2, 12
        do i = -4, 6
            if (c(i, j) /= 100 * i + j) error stop 3
        end do
    end do

    ! Run-time bounds over part of the array, with a mask.
    n = 4
    m = 5
    e = -1
    do concurrent (i = 2:n, j = 1:m, mod(i + j, 2) == 0)
        e(i, j) = i * j
    end do
    do j = 1, 6
        do i = 1, 5
            if (i >= 2 .and. i <= n .and. j <= m &
                    .and. mod(i + j, 2) == 0) then
                if (e(i, j) /= i * j) error stop 4
            else
                if (e(i, j) /= -1) error stop 5
            end if
        end do
    end do

    ! An empty second dimension.
    m = 0
    e = 9
    do concurrent (i = 1:5, j = 1:m)
        e(i, j) = 0
    end do
    if (any(e /= 9)) error stop 6
    print *, "ok"
end program
