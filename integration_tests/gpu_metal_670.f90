program gpu_metal_670
    ! Reductions in launches that run in chunks (--gpu-chunk-limit=4),
    ! together with a liveout scalar, a per-thread workspace (a local array
    ! of run-time size in the body), an error stop that never fires, and a
    ! packed launch (more array arguments than are handed over one by one).
    ! A max of negative values and a min of positive ones differ from what
    ! accumulators left at zero would give.
    implicit none
    integer :: i, n, m, s, mx, last, e, emx
    real :: t, f
    real :: a(40), r(40)
    real :: a0(40), a1(40), a2(40), a3(40), a4(40), a5(40), a6(40), a7(40)
    real :: a8(40), a9(40), a10(40), a11(40), a12(40), a13(40), a14(40)
    real :: a15(40), a16(40), a17(40), a18(40), a19(40), a20(40), a21(40)
    real :: a22(40), a23(40), a24(40), a25(40), a26(40), a27(40), a28(40)
    real :: a29(40), a30(40), a31(40), a32(40), a33(40)

    do i = 1, 40
        a(i) = real(mod(7 * i, 13) - 6)
    end do

    ! 29 iterations in chunks of 4, the last one partial.
    n = 29
    m = 3
    s = 2
    t = 0.5
    mx = -100
    last = -1
    do concurrent (i = 1:n) reduce(+:s, t) reduce(max:mx)
        block
            integer :: w(m)
            w = i
            if (i > 1000) error stop "never"
            s = s + sum(w)
            t = t + a(i)
            mx = max(mx, -w(m) - 2 * mod(i, 11))
        end block
        if (i == n) last = 10 * i
    end do
    e = 2 + m * n * (n + 1) / 2
    f = 0.5
    emx = -100
    do i = 1, n
        f = f + a(i)
        emx = max(emx, -i - 2 * mod(i, 11))
    end do
    if (s /= e) error stop 1
    if (t /= f) error stop 2
    if (mx /= emx) error stop 3
    if (last /= 10 * n) error stop 4

    ! A packed launch: 37 iterations in chunks of 4.
    a0 = 1; a1 = 2; a2 = 3; a3 = 4; a4 = 5; a5 = 6; a6 = 7; a7 = 8
    a8 = 9; a9 = 10; a10 = 11; a11 = 12; a12 = 13; a13 = 14; a14 = 15
    a15 = 16; a16 = 17; a17 = 18; a18 = 19; a19 = 20; a20 = 21; a21 = 22
    a22 = 23; a23 = 24; a24 = 25; a25 = 26; a26 = 27; a27 = 28; a28 = 29
    a29 = 30; a30 = 31; a31 = 32; a32 = 33; a33 = 34
    n = 37
    t = 1
    s = 50
    last = -1
    r = 0
    do concurrent (i = 1:n) reduce(+:t) reduce(min:s)
        r(i) = a0(i) + a1(i) + a2(i) + a3(i) + a4(i) + a5(i) + a6(i) &
            + a7(i) + a8(i) + a9(i) + a10(i) + a11(i) + a12(i) + a13(i) &
            + a14(i) + a15(i) + a16(i) + a17(i) + a18(i) + a19(i) &
            + a20(i) + a21(i) + a22(i) + a23(i) + a24(i) + a25(i) &
            + a26(i) + a27(i) + a28(i) + a29(i) + a30(i) + a31(i) &
            + a32(i) + a33(i) + a(i)
        t = t + r(i) - real(i)
        s = min(s, int(a(i)) + 7 + mod(i, 5))
        if (i == n) last = 11 * i
    end do
    f = 1
    emx = 50
    do i = 1, n
        if (r(i) /= 595 + a(i)) error stop 5
        f = f + (595 + a(i)) - real(i)
        emx = min(emx, int(a(i)) + 7 + mod(i, 5))
    end do
    if (r(n + 1) /= 0) error stop 6
    if (t /= f) error stop 7
    if (s /= emx) error stop 8
    if (last /= 11 * n) error stop 9
    print *, "ok"
end program
