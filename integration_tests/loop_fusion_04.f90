program loop_fusion_04
    ! Which adjacent nests --fast fuses because it is faster: only nests
    ! that share most of their arrays and run the same way.
    implicit none
    integer, parameter :: n = 8
    real :: a(n), b(n), c(n), d(n), e(n), f(n), g(n), h(n)
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

    if (any(c /= a + 2) .or. any(d /= 2*a)) error stop 1
    if (any(f /= 6) .or. any(abs(g - a / 2) > 1e-5)) error stop 2
    if (any(cnt /= [9, 8, 4, 0, 0, 0, 0, 15])) error stop 3
    if (h(n) /= 35 .or. e(n) /= 16 .or. e(1) /= 2) error stop 4
    print *, "ok"
end program
