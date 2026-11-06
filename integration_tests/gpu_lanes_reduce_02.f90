program gpu_lanes_reduce_02
! Parallel loops of few iterations whose inner loops accumulate into private
! scalars of 4 bytes, which Metal shares out between the lanes of a
! SIMD-group and combines with simd_sum, simd_product, simd_min and
! simd_max: column sums, a matrix-vector product, a product, a maximum, a
! minimum, a difference, an accumulator that starts from a value and an
! integer sum, over inner loops whose lengths do not divide into the lanes.
! The values are small integers and powers of two, so every order of the
! operations gives the same bits, which are compared exactly. The last loop
! has more iterations than a launch gives lanes to, and runs the inner loop
! as it was on one thread per iteration. Metal combines no 8-byte values, so
! an integer(8) accumulator stays on one lane per iteration. Nor is a loop
! shared out whose bounds read an accumulator (`do i = 1, nn` that counts
! `nn` down): the other lanes start it at zero.
use, intrinsic :: iso_fortran_env, only: int64
implicit none
integer, parameter :: m = 37, n = 101, big = 3000
real :: a(n, m), x(n), s(m), y(n), p(m), mx(m), mn(m), d(m), e(big)
real :: t, u, r, dx, ref
integer :: i, j, k(m), c, kb(big)
integer(int64) :: l(m), c8
integer :: nn, h(m)
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
do concurrent (j = 1:m) local(t, u, r, dx, i, c)
    t = 1
    u = -1.0e30
    r = 1.0e30
    dx = 100
    c = 3
    do i = 1, n
        t = t*2.0**(mod(i + j, 3) - 1)
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
do concurrent (j = 1:m) local(c8, i)
    c8 = 0
    do i = 1, n
        c8 = c8 + int(i, int64)*huge(0) + j
    end do
    l(j) = c8
end do
do concurrent (j = 1:m) local(nn, t, i)
    nn = n + j
    t = 0
    do i = 1, nn
        t = t + i
        nn = nn - 1
    end do
    h(j) = int(t)*1000 + nn
end do
do concurrent (j = 1:big) local(t, i, c)
    t = 0
    c = 0
    do i = 1, n
        t = t + a(i, mod(j, m) + 1)
        c = c + mod(i + j, 7)
    end do
    e(j) = t
    kb(j) = c
end do
do j = 1, m
    if (s(j) /= sum(a(:, j))) error stop "column sum"
    ref = 1
    do i = 1, n
        ref = ref*2.0**(mod(i + j, 3) - 1)
    end do
    if (p(j) /= ref) error stop "product"
    if (mx(j) /= maxval(a(:, j)*[(i, i = 1, n)])) error stop "max"
    if (mn(j) /= minval(a(:, j) - [(i, i = 1, n)])) error stop "min"
    if (d(j) /= 100 - sum(a(:, j))) error stop "difference"
    if (k(j) /= 3 + sum([(mod(i, 4), i = 1, n)])) error stop "integer sum"
    if (l(j) /= int(n, int64)*(n + 1)/2*huge(0) + n*j) error stop "integer(8) sum"
    if (h(j) /= (n + j)*(n + j + 1)/2*1000) error stop "bounds read an accumulator"
end do
do i = 1, n
    t = 0
    do j = 1, m
        t = t + a(i, j)*x(mod(j, n) + 1)
    end do
    if (y(i) /= t) error stop "matrix-vector"
end do
do j = 1, big
    if (e(j) /= sum(a(:, mod(j, m) + 1))) error stop "one lane sum"
    if (kb(j) /= sum([(mod(i + j, 7), i = 1, n)])) error stop "one lane integer"
end do
print *, "ok"
end program
