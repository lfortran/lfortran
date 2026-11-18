program gpu_array_reduction_02
! A REDUCE of an element of an array in a kernel whose iterations run on
! several lanes (gpu_vector_lanes.cpp): the iterations of a matrix-vector
! product, 4 rows each, share the loop over the columns between their
! lanes, update their rows of `x` and add the squares of what they wrote to
! `ss(2)`, as the residual update of a transformer computes the sum of
! squares the next RMSNorm needs, starting from the constant the statement
! before the loop assigns it. Every lane of an iteration holds the
! same sum, which counts once. The next loop reads `ss(2)` on the device,
! with no access of the host in between. In the second product the lanes
! share the sum of the element itself. The values are small integers, so the
! results are exact and equal those of the same loops on the host.
implicit none
integer, parameter :: nc = 4096
integer :: nr, q, i, c, n
real, allocatable :: w(:, :), v(:), x(:), y(:), rx(:), ry(:)
real :: ss(3), rss(3), dot(2), rdot(2), a1, a2, a3, a4
integer :: rows(4)

rows = [4, 128, 1280, 8192]
do q = 1, size(rows)
    nr = rows(q)
    allocate(w(nc, nr), v(nc), x(nr), y(nr), rx(nr), ry(nr))
    do n = 1, nr
        do c = 1, nc
            w(c, n) = 0
            if (mod(c + 3*n, 256) == 0) w(c, n) = real(mod(c + n, 3) - 1)
        end do
        x(n) = real(mod(n, 5) - 2)
    end do
    do c = 1, nc
        v(c) = real(mod(c, 7) - 3)
    end do
    rx = x

    ss = [1.0, 2.0, 3.0]
    ss(2) = 0.5
    do concurrent (i = 1:nr/4) reduce(+: ss)
        block
            real :: b1, b2, b3, b4
            integer :: cc, m
            m = 4*i - 3
            b1 = 0; b2 = 0; b3 = 0; b4 = 0
            do cc = 1, nc
                b1 = b1 + w(cc, m)*v(cc)
                b2 = b2 + w(cc, m+1)*v(cc)
                b3 = b3 + w(cc, m+2)*v(cc)
                b4 = b4 + w(cc, m+3)*v(cc)
            end do
            x(m) = x(m) + b1
            x(m+1) = x(m+1) + b2
            x(m+2) = x(m+2) + b3
            x(m+3) = x(m+3) + b4
            ss(2) = ss(2) + ((x(m)*x(m) + x(m+1)*x(m+1)) &
                + (x(m+2)*x(m+2) + x(m+3)*x(m+3)))
        end block
    end do
    do concurrent (i = 1:nr)
        y(i) = x(i) * ss(2)
    end do

    rss = [1.0, 0.5, 3.0]
    do i = 1, nr/4
        n = 4*i - 3
        a1 = 0; a2 = 0; a3 = 0; a4 = 0
        do c = 1, nc
            a1 = a1 + w(c, n)*v(c)
            a2 = a2 + w(c, n+1)*v(c)
            a3 = a3 + w(c, n+2)*v(c)
            a4 = a4 + w(c, n+3)*v(c)
        end do
        rx(n) = rx(n) + a1
        rx(n+1) = rx(n+1) + a2
        rx(n+2) = rx(n+2) + a3
        rx(n+3) = rx(n+3) + a4
        rss(2) = rss(2) + ((rx(n)*rx(n) + rx(n+1)*rx(n+1)) &
            + (rx(n+2)*rx(n+2) + rx(n+3)*rx(n+3)))
    end do
    ry = rx * rss(2)
    if (any(ss /= rss) .or. any(x /= rx) .or. any(y /= ry)) then
        print *, "residual", nr, ss, rss
        error stop 1
    end if

    ! The lanes share the loop that adds to the element.
    dot = [5.0, -1.0]
    do concurrent (i = 1:nr) reduce(+: dot)
        do c = 1, nc
            dot(1) = dot(1) + w(c, i)*v(c)
        end do
    end do
    rdot = [5.0, -1.0]
    do i = 1, nr
        do c = 1, nc
            rdot(1) = rdot(1) + w(c, i)*v(c)
        end do
    end do
    if (any(dot /= rdot)) then
        print *, "dot", nr, dot, rdot
        error stop 2
    end if
    deallocate(w, v, x, y, rx, ry)
end do
print *, "ok"
end program
