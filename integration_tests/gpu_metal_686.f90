program gpu_metal_686
    ! Offloaded single loops and nests whose iterations run in several
    ! launches of the kernel (lfortran/lfortran#16569), on every device
    ! that offloads them, Metal included (lfortran/lfortran#16541). The test
    ! is compiled with --gpu-chunk-limit=5: most loops run in many chunks,
    ! the last of them partial.
    implicit none
    integer, parameter :: m = 23
    integer :: a(m), b(-7:15), c(4, 9), d(7, 3, 2), e(3, 8), f(m)
    integer :: i, j, k, n, lo, hi, p, q

    ! A single loop over a constant extent: chunks of 5, the last of 3.
    a = 0
    do concurrent (i = 1:m)
        a(i) = 3 * i + 1
    end do
    do i = 1, m
        if (a(i) /= 3 * i + 1) error stop 1
    end do

    ! A single loop over run-time bounds, negative ones among them.
    lo = -7
    hi = 15
    b = 0
    do concurrent (i = lo:hi)
        b(i) = i * i - 2
    end do
    do i = -7, 15
        if (b(i) /= i * i - 2) error stop 2
    end do

    ! A nest over run-time extents: the fastest dimension, of 4, fits, so
    ! each chunk covers one value of the next one.
    n = 4
    p = 9
    c = 0
    do concurrent (i = 1:n, j = 1:p)
        c(i, j) = 100 * i + j
    end do
    do j = 1, 9
        do i = 1, 4
            if (c(i, j) /= 100 * i + j) error stop 3
        end do
    end do

    ! A nest of three over constant extents: the fastest dimension, of 7,
    ! is split, one value of each of the others per chunk.
    d = 0
    do concurrent (i = 1:7, j = 1:3, k = 1:2)
        d(i, j, k) = 100 * i + 10 * j + k
    end do
    do k = 1, 2
        do j = 1, 3
            do i = 1, 7
                if (d(i, j, k) /= 100 * i + 10 * j + k) error stop 4
            end do
        end do
    end do

    ! A mask, over a nest whose fastest dimension, of 3, fits once in a
    ! chunk: each chunk covers one value of `j`.
    q = 8
    e = -1
    do concurrent (i = 1:3, j = 1:q, mod(i + 2 * j, 3) /= 0)
        e(i, j) = i * j
    end do
    do j = 1, 8
        do i = 1, 3
            if (mod(i + 2 * j, 3) /= 0) then
                if (e(i, j) /= i * j) error stop 5
            else
                if (e(i, j) /= -1) error stop 6
            end if
        end do
    end do

    ! Empty loops: nothing runs, nothing is written.
    f = 7
    n = 0
    do concurrent (i = 1:n)
        f(i) = 0
    end do
    lo = 5
    hi = 4
    do concurrent (i = 1:m, j = lo:hi)
        f(i) = 0
    end do
    do concurrent (j = lo:hi, i = 1:m)
        f(i) = 0
    end do
    if (any(f /= 7)) error stop 7
    print *, "ok"
end program
