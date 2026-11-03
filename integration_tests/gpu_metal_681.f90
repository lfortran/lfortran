program gpu_metal_681
    ! Offloaded nests a device may run in tiles, with scalars the host reads
    ! after the loop (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=1000: a nest of more iterations runs in chunks
    ! numbered flat, the last of them partial, and one of fewer runs as one
    ! launch, in tiles where the device has them. Each nest assigns a scalar
    ! the host reads in one iteration only, which a chunk other than the
    ! first runs where the nest is split; some also have a scalar every
    ! iteration assigns and then reads. Every element and result is checked.
    implicit none
    real :: x(0:65, 0:65), y(64, 64), z(1500, 3)
    integer :: i, j, n, m, last, t, cnt, ecnt
    real :: s, mx, es, emx

    do j = 0, 65
        do i = 0, 65
            x(i, j) = real(i + 100 * j)
        end do
    end do

    ! Run-time extents, 64 by 40 = 2560 iterations: chunks of 15 values of
    ! `j` (960 iterations), 15, 15 and 10.
    n = 64
    m = 40
    y = 0
    last = -1
    do concurrent (i = 1:n, j = 1:m)
        t = i + j
        y(i, j) = x(i, j - 1) + x(i, j + 1) + real(t - i - j)
        if (i == n .and. j == m) last = 1000 * i + j
    end do
    do j = 1, 64
        do i = 1, 64
            if (j <= m) then
                if (y(i, j) /= 2 * x(i, j)) error stop 1
            else
                if (y(i, j) /= 0) error stop 2
            end if
        end do
    end do
    if (last /= 64040) error stop 3

    ! A fastest dimension that is not a multiple of a tile's width, 33 by
    ! 50 = 1650 iterations: chunks of 30 values of `j` and 20.
    n = 33
    m = 50
    y = 0
    last = -1
    do concurrent (i = 1:n, j = 1:m)
        y(i, j) = x(i + 1, j) - x(i - 1, j)
        if (i == n .and. j == m) last = 1000 * i + j
    end do
    do j = 1, 64
        do i = 1, 64
            if (i <= n .and. j <= m) then
                if (y(i, j) /= 2) error stop 4
            else
                if (y(i, j) /= 0) error stop 5
            end if
        end do
    end do
    if (last /= 33050) error stop 6

    ! A fastest dimension of more than the limit, 1500 by 3: each value of
    ! `j` runs in chunks of 1000 and 500 values of `i`.
    n = 1500
    m = 3
    z = 0
    last = -1
    do concurrent (i = 1:n, j = 1:m)
        z(i, j) = real(i - 7 * j)
        if (i == n .and. j == m) last = 10 * i + j
    end do
    do j = 1, 3
        do i = 1, 1500
            if (z(i, j) /= real(i - 7 * j)) error stop 7
        end do
    end do
    if (last /= 15003) error stop 8

    ! Run-time extents, 32 by 24 = 768 iterations: one launch.
    n = 32
    m = 24
    y = 0
    last = -1
    do concurrent (i = 1:n, j = 1:m)
        t = 2 * i
        y(i, j) = x(i - 1, j) + x(i + 1, j) + real(t - 2 * i)
        if (i == n .and. j == m) last = 1000 * i + j
    end do
    do j = 1, 64
        do i = 1, 64
            if (i <= n .and. j <= m) then
                if (y(i, j) /= 2 * x(i, j)) error stop 9
            else
                if (y(i, j) /= 0) error stop 10
            end if
        end do
    end do
    if (last /= 32024) error stop 11

    ! Reductions with a mask and a scalar the host reads, over 64 by 40
    ! iterations in chunks.
    n = 64
    m = 40
    s = 0
    mx = -1
    cnt = 0
    last = -1
    do concurrent (i = 1:n, j = 1:m, mod(i + j, 3) /= 0) &
            reduce(+:s, cnt) reduce(max:mx)
        s = s + x(i, j)
        mx = max(mx, x(i, j) - real(50 * j))
        cnt = cnt + 1
        if (i == n - 1 .and. j == m) last = 1000 * i + j
    end do
    es = 0
    emx = -1
    ecnt = 0
    do j = 1, m
        do i = 1, n
            if (mod(i + j, 3) == 0) cycle
            es = es + x(i, j)
            emx = max(emx, x(i, j) - real(50 * j))
            ecnt = ecnt + 1
        end do
    end do
    if (s /= es) error stop 12
    if (mx /= emx) error stop 13
    if (cnt /= ecnt) error stop 14
    if (last /= 63040) error stop 15
    print *, "ok"
end program
