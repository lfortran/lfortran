program gpu_lanes_independent_01
! Parallel loops of few iterations, on an NVIDIA GPU, whose bodies hold a
! reduction followed by loops whose iterations are independent: a vector
! scaled by its norm and by a weight, and an iteration with automatic arrays
! that it fills in a loop of its own and sums afterwards. The warp that runs
! an iteration shares the loops out between its lanes, where the first lane
! used to run them all. The values are small integers and powers of two, so
! every order of the operations gives the same bits, which are compared
! exactly.
implicit none
integer, parameter :: n = 301, m = 5
integer :: x(n, m), w(n), y(n, m), z(m), ref(n, m), i, j
real :: xr(n, m), yr(n, m), wr(n), r
real :: s
x = 0
do j = 1, m
    do i = 1, n
        x(i, j) = mod(5*i + 3*j, 7) - 3
    end do
end do
do i = 1, n
    w(i) = 1 + mod(i, 3)
end do
xr = x
wr = w
do concurrent (j = 1:m) local(r, s, i)
    s = 0
    do i = 1, n
        s = s + xr(i, j)*xr(i, j)
    end do
    r = 0.5
    if (s > 100) r = 0.25
    do concurrent (i = 1:n)
        yr(i, j) = xr(i, j)*r*wr(i)
    end do
end do
do concurrent (j = 1:m)
    block
        integer :: tmp(n), total

        do concurrent (i = 1:n)
            tmp(i) = x(i, j)*w(i)
        end do
        total = sum(tmp*x(:, j))
        z(j) = total
        do concurrent (i = 1:n)
            y(i, j) = tmp(i) + total
        end do
    end block
end do
do j = 1, m
    s = 0
    do i = 1, n
        s = s + xr(i, j)*xr(i, j)
    end do
    r = 0.5
    if (s > 100) r = 0.25
    do i = 1, n
        if (yr(i, j) /= xr(i, j)*r*wr(i)) error stop "scaled vector"
    end do
    if (z(j) /= sum(x(:, j)*w*x(:, j))) error stop "automatic array sum"
    ref(:, j) = x(:, j)*w + z(j)
    if (any(y(:, j) /= ref(:, j))) error stop "automatic array copy"
end do
print *, "ok"
end program
