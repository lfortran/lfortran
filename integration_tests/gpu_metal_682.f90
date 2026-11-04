program gpu_metal_682
    ! Offloaded loops with integer(8) indices over the ends of the
    ! integer(8) range whose iterations run in several launches of the
    ! kernel (lfortran/lfortran#16569). The kernel numbers the iterations of
    ! such a nest in 64 bits and adds the start of the chunk's box, which
    ! the host computes; the box of the last chunk must end at huge(0_8)
    ! without stepping past it. The test is compiled with
    ! --gpu-chunk-limit=7, so every split dimension below has three or more
    ! chunks, the last one partial. Every element and result is checked.
    implicit none
    integer(8) :: lo, hi, i, j, k, s, mx, cnt, es, emx, ecnt
    integer :: a(20), d(5, 10), m

    lo = -huge(0_8)
    hi = huge(0_8)

    ! A single loop ending at huge(0_8): 20 iterations in chunks of 7, 7, 6.
    a = 0
    do concurrent (i = hi - 19:hi)
        a(i - (hi - 20)) = int(hi - i) + 1
    end do
    do m = 1, 20
        if (a(m) /= 21 - m) error stop 1
    end do

    ! A single loop starting at -huge(0_8).
    a = 0
    do concurrent (i = lo:lo + 19)
        a(i - lo + 1) = int(i - lo) + 7
    end do
    do m = 1, 20
        if (a(m) /= m + 6) error stop 2
    end do

    ! Both ends: the 5 values of `i` from -huge(0_8) fit, and each chunk
    ! runs one value of `j`, which ends at huge(0_8).
    d = 0
    do concurrent (i = lo:lo + 4, j = hi - 9:hi)
        d(i - lo + 1, j - (hi - 10)) = int(i - lo) + 10 * int(hi - j)
    end do
    do m = 1, 10
        if (any(d(:, m) /= [(k + 10 * (10 - m), k = 0, 4)])) error stop 3
    end do

    ! Reductions whose results are the ends of the range, with a mask.
    s = 0
    mx = lo
    cnt = 0
    do concurrent (i = lo:lo + 4, j = hi - 9:hi, mod(j, 3_8) /= 2) &
            reduce(+:s, cnt) reduce(max:mx)
        s = s + (i - lo) + (hi - j)
        mx = max(mx, j)
        cnt = cnt + 1
    end do
    es = 0
    emx = lo
    ecnt = 0
    ! A serial DO loop must not end at huge(0_8): its variable would step
    ! past it, so it runs over offsets.
    do k = 9, 0, -1
        j = hi - k
        do i = lo, lo + 4
            if (mod(j, 3_8) == 2) cycle
            es = es + (i - lo) + (hi - j)
            emx = max(emx, j)
            ecnt = ecnt + 1
        end do
    end do
    if (s /= es) error stop 4
    if (mx /= emx .or. mx /= hi) error stop 5
    if (cnt /= ecnt) error stop 6
    print *, "ok"
end program
