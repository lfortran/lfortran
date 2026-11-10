program gpu_lanes_block_02
! A launch of a few iterations, on an NVIDIA GPU, gives each the lanes of a
! whole block: the warps of the block share out a reduction, combine their
! results through shared memory behind a block barrier, and share out the
! loops of independent iterations that follow. Sums, a product, a maximum, a
! minimum and an integer sum of lengths that do not divide into the lanes;
! the values are small integers and powers of two, so every order of the
! operations gives the same bits, which are compared exactly.
implicit none
integer, parameter :: n = 1000, m = 3
real :: a(n, m), s(m), p(m), mx(m), mn(m), y(n, m), t, u, r, v
integer :: k(m), i, j, c
do j = 1, m
    do i = 1, n
        a(i, j) = mod(7*i + 5*j, 11) - 5
    end do
end do
do concurrent (j = 1:m) local(t, u, r, v, i, c)
    t = 0
    u = 1
    r = -1.0e30
    v = 1.0e30
    c = j
    do i = 1, n
        t = t + a(i, j)
        if (mod(i, 100) == 0) u = u*2
        r = max(r, a(i, j)*i)
        v = min(v, a(i, j) - i)
        c = c + mod(i, 7)
    end do
    s(j) = t
    p(j) = u
    mx(j) = r
    mn(j) = v
    k(j) = c
    do concurrent (i = 1:n)
        y(i, j) = a(i, j)*u + t - v
    end do
end do
do j = 1, m
    if (s(j) /= sum(a(:, j))) error stop "sum"
    if (p(j) /= 2.0**10) error stop "product"
    if (mx(j) /= maxval(a(:, j)*[(real(i), i = 1, n)])) error stop "max"
    if (mn(j) /= minval(a(:, j) - [(real(i), i = 1, n)])) error stop "min"
    c = j
    do i = 1, n
        c = c + mod(i, 7)
    end do
    if (k(j) /= c) error stop "integer sum"
    do i = 1, n
        if (y(i, j) /= a(i, j)*p(j) + s(j) - mn(j)) error stop "scaled column"
    end do
end do
print *, "ok"
end program
