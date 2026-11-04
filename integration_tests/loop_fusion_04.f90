program loop_fusion_04
    ! Which adjacent nests --fast fuses because it is faster: only nests
    ! that share most of their arrays and run the same way.
    implicit none
    integer, parameter :: n = 8
    real :: a(n), b(n), c(n), d(n), e(n), f(n), g(n), h(n)
    real :: r, t, u(n), v(n), x(n, n), y(n, n), w(n, n)
    integer :: idx(n), cnt(n), i, k, l, m, p

    a = [(real(i), i = 1, n)]
    b = 2
    idx = [1, 2, 1, 3, 1, 2, 8, 8]
    cnt = 0

    ! The same arrays (fused).
    do i = 1, n
        c(i) = a(i) + b(i)
    end do
    do k = 1, n
        d(k) = a(k) * b(k)
    end do

    ! No array in common (not fused).
    do i = 1, n
        e(i) = a(i) + 1
    end do
    do l = 1, n
        f(l) = b(l) * 3
    end do

    ! One of the loops does not vectorize (not fused).
    do i = 1, n
        g(i) = a(i) / b(i)
    end do
    do m = 1, n
        cnt(idx(m)) = cnt(idx(m)) + int(a(m))
    end do

    ! A recurrence and a loop without one (not fused).
    h(1) = 0
    do i = 2, n
        h(i) = h(i - 1) + a(i)
    end do
    do p = 2, n
        e(p) = a(p) * 2
    end do

    ! A copy and a reduction of the copied array, whose initialization of
    ! the accumulator runs before the fused nest (fused).
    y = 1
    w = 3
    y(2, 3) = 7
    x = 0
    x(1:n,1:n) = y(1:n,1:n)
    r = maxval(abs(x(1:n,1:n) - w(1:n,1:n)))
    if (r /= 4 .or. x(2, 3) /= 7 .or. sum(x) /= n*n + 6) error stop 5

    ! The initialization of a sum is also read by the first nest, or by
    ! a statement in between (not fused).
    t = 1
    do i = 1, n
        u(i) = a(i) + t
    end do
    t = 0
    do k = 1, n
        t = t + u(k)
        v(k) = u(k)
    end do
    if (t /= 44 .or. any(v /= a + 1)) error stop 6
    do i = 1, n
        u(i) = a(i) * 2
    end do
    t = 0
    r = t + 1
    do l = 1, n
        t = t + u(l)
        v(l) = u(l)
    end do
    if (t /= 72 .or. r /= 1 .or. any(v /= 2*a)) error stop 7

    ! One of the loops calls a function that does not vectorize (not
    ! fused).
    do i = 1, n
        u(i) = exp(a(i) / 8)
    end do
    do m = 1, n
        v(m) = u(m) + a(m)
    end do
    if (abs(v(n) - (exp(1.0) + n)) > 1e-4) error stop 8

    if (any(c /= a + 2) .or. any(d /= 2*a)) error stop 1
    if (any(f /= 6) .or. any(abs(g - a / 2) > 1e-5)) error stop 2
    if (any(cnt /= [9, 8, 4, 0, 0, 0, 0, 15])) error stop 3
    if (h(n) /= 35 .or. e(n) /= 16 .or. e(1) /= 2) error stop 4
    print *, "ok"
end program
