program gpu_lanes_reduce_01
! Parallel loops of few iterations whose inner loops accumulate into private
! scalars, which an NVIDIA GPU shares out between the lanes of a warp and
! combines afterwards: sums down columns, a matrix-vector product, several
! accumulators of an n-body-like loop with temporaries, a product, a maximum,
! a minimum, a difference, an accumulator that starts from a value, and an
! integer sum. Inner loops of lengths that do not divide into the lanes.
implicit none
integer, parameter :: dp = kind(0.d0), m = 37, n = 101
real(dp) :: a(n, m), x(n), s(m), y(n), f(n), g(n), p(m), mx(m), mn(m), d(m)
real(dp) :: t, u, r, dx
integer :: i, j, k(m), c
do j = 1, m
    do i = 1, n
        a(i, j) = mod(3*i + 7*j, 13) - 6
    end do
end do
do i = 1, n
    x(i) = 1 + mod(i, 5)
end do
do concurrent (j = 1:m) local(t, i)
    t = 0
    do i = 1, n
        t = t + a(i, j)
    end do
    s(j) = t
end do
do concurrent (i = 1:n) local(t, j)
    t = 0
    do j = 1, m
        t = t + a(i, j)*x(mod(j, n) + 1)
    end do
    y(i) = t
end do
do concurrent (i = 1:n) local(t, u, j, r, dx)
    t = 0
    u = 1
    do j = 1, n
        dx = x(j) - x(i)
        r = 1/sqrt(dx*dx + 0.5_dp)
        t = t + dx*r
        u = u + r*r
    end do
    f(i) = t
    g(i) = u
end do
do concurrent (j = 1:m) local(t, u, r, dx, i, c)
    t = 1
    u = -1.0e300_dp
    r = 1.0e300_dp
    dx = 100
    c = 3
    do i = 1, n
        t = t*(1 + a(i, j)/1000)
        u = max(u, a(i, j)*i)
        r = min(a(i, j) - i, r)
        dx = dx - a(i, j)
        c = c + mod(i, 4)
    end do
    p(j) = t
    mx(j) = u
    mn(j) = r
    d(j) = dx
    k(j) = c
end do
do j = 1, m
    if (abs(s(j) - sum(a(:, j))) > 1e-9_dp) error stop "column sum"
    if (abs(p(j) - product(1 + a(:, j)/1000)) > 1e-12_dp) error stop "product"
    if (mx(j) /= maxval(a(:, j)*[(i, i = 1, n)])) error stop "max"
    if (mn(j) /= minval(a(:, j) - [(i, i = 1, n)])) error stop "min"
    if (abs(d(j) - (100 - sum(a(:, j)))) > 1e-9_dp) error stop "difference"
    if (k(j) /= 3 + sum([(mod(i, 4), i = 1, n)])) error stop "integer sum"
end do
do i = 1, n
    t = 0
    do j = 1, m
        t = t + a(i, j)*x(mod(j, n) + 1)
    end do
    if (abs(y(i) - t) > 1e-9_dp) error stop "matrix-vector"
    t = 0
    u = 1
    do j = 1, n
        dx = x(j) - x(i)
        r = 1/sqrt(dx*dx + 0.5_dp)
        t = t + dx*r
        u = u + r*r
    end do
    if (abs(f(i) - t) > 1e-9_dp .or. abs(g(i) - u) > 1e-9_dp) error stop "forces"
end do
print *, "ok"
end program
