program gpu_metal_680
    ! Offloaded loops over the ends of the integer(4) range whose iterations
    ! run in several launches of the kernel (lfortran/lfortran#16569). The
    ! kernel numbers the iterations of a chunk from 0 in 32 bits and adds
    ! the start of the chunk's box, which the host computes. The test is
    ! compiled with --gpu-chunk-limit=7, so every split dimension below has
    ! three or more chunks, the last one partial, and its first or last
    ! value is -huge(0) or huge(0). Every element and result is checked.
    implicit none
    integer :: a(20), b(3, 20), c(10, 3), d(5, 10), i, j, k, lo, hi, n
    integer :: s, mn, mx, p, cnt, last, es, emn, emx, ep, ecnt, elast

    lo = -huge(0)
    hi = huge(0)
    n = 3

    ! A single loop ending at huge(0): 20 iterations in chunks of 7, 7, 6.
    a = 0
    do concurrent (i = hi - 19:hi)
        a(i - (hi - 20)) = hi - i + 1
    end do
    do k = 1, 20
        if (a(k) /= 21 - k) error stop 1
    end do

    ! A single loop starting at -huge(0).
    a = 0
    do concurrent (i = lo:lo + 19)
        a(i - lo + 1) = i - lo + 7
    end do
    do k = 1, 20
        if (a(k) /= k + 6) error stop 2
    end do

    ! The fastest dimension, of 3, fits; the second, starting at -huge(0),
    ! is split into ranges of 2 values.
    b = 0
    do concurrent (i = 1:n, j = lo:lo + 19)
        b(i, j - lo + 1) = 100 * i + (j - lo)
    end do
    do j = 1, 20
        do i = 1, 3
            if (b(i, j) /= 100 * i + j - 1) error stop 3
        end do
    end do

    ! The fastest dimension, ending at huge(0), is split itself.
    c = 0
    do concurrent (i = hi - 9:hi, j = 1:n)
        c(i - (hi - 10), j) = (hi - i) + 1000 * j
    end do
    do j = 1, 3
        do i = 1, 10
            if (c(i, j) /= 10 - i + 1000 * j) error stop 4
        end do
    end do

    ! Both ends: the 5 values of `i` from -huge(0) fit, and each chunk runs
    ! one value of `j`, which ends at huge(0).
    d = 0
    do concurrent (i = lo:lo + 4, j = hi - 9:hi)
        d(i - lo + 1, j - (hi - 10)) = (i - lo) + 10 * (hi - j)
    end do
    do j = 1, 10
        do i = 1, 5
            if (d(i, j) /= i - 1 + 10 * (10 - j)) error stop 5
        end do
    end do

    ! The same nest with constant bounds, split at compile time.
    d = 0
    do concurrent (i = -huge(0):-huge(0) + 4, j = huge(0) - 9:huge(0))
        d(i - lo + 1, j - (hi - 10)) = (i - lo) + 7 * (hi - j)
    end do
    do j = 1, 10
        do i = 1, 5
            if (d(i, j) /= i - 1 + 7 * (10 - j)) error stop 6
        end do
    end do

    ! Reductions whose results are the ends of the range, a mask and a
    ! scalar the host reads after the loop, over the same nest.
    s = 0
    mn = 0
    mx = 0
    p = 1
    cnt = 0
    last = 0
    do concurrent (i = lo:lo + 4, j = hi - 9:hi, mod(j, 3) /= 2) &
            reduce(+:s, cnt) reduce(min:mn) reduce(max:mx) reduce(*:p)
        s = s + (i - lo) + (hi - j)
        mn = min(mn, i)
        mx = max(mx, j)
        p = p * (1 + mod(i - lo, 2))
        cnt = cnt + 1
        if (i == lo + 4 .and. j == hi) last = 10 * (i - lo) + (hi - j) + 1
    end do
    es = 0
    emn = 0
    emx = 0
    ep = 1
    ecnt = 0
    elast = 0
    ! A serial DO loop must not end at huge(0): its variable would step
    ! past it, so it runs over offsets.
    do k = 9, 0, -1
        j = hi - k
        do i = lo, lo + 4
            if (mod(j, 3) == 2) cycle
            es = es + (i - lo) + (hi - j)
            emn = min(emn, i)
            emx = max(emx, j)
            ep = ep * (1 + mod(i - lo, 2))
            ecnt = ecnt + 1
            if (i == lo + 4 .and. j == hi) elast = 10 * (i - lo) + (hi - j) + 1
        end do
    end do
    if (s /= es) error stop 7
    if (mn /= emn .or. mn /= lo) error stop 8
    if (mx /= emx .or. mx /= hi) error stop 9
    if (p /= ep) error stop 10
    if (cnt /= ecnt) error stop 11
    if (last /= elast .or. last /= 41) error stop 12
    print *, "ok"
end program
