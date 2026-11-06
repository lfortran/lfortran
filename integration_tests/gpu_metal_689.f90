program gpu_metal_689
    ! Offloaded loops compiled with --gpu-chunk-limit=1, so that every
    ! iteration runs in a launch of its own (lfortran/lfortran#16569), on
    ! every device that offloads them, Metal included
    ! (lfortran/lfortran#16541): a nest, a loop that reads what the chunks
    ! of the one before it wrote, reductions, a scalar the host reads after
    ! the loop, and a private array of run-time size.
    implicit none
    integer :: a(6, 5), b(30), i, j, n, p, s, mx, last
    real :: t, d(12, 3)

    n = 6
    p = 5
    a = 0
    do concurrent (i = 1:n, j = 1:p)
        a(i, j) = 10 * i + j
    end do
    do concurrent (i = 1:n * p)
        b(i) = a(mod(i - 1, n) + 1, (i - 1) / n + 1) + 1
    end do
    do j = 1, 5
        do i = 1, 6
            if (a(i, j) /= 10 * i + j) error stop 1
            if (b(i + 6 * (j - 1)) /= 10 * i + j + 1) error stop 2
        end do
    end do

    s = 3
    mx = -1000
    t = 0.25
    last = -1
    do concurrent (i = 1:n * p, mod(i, 7) /= 3) reduce(+:s) reduce(max:mx) &
            reduce(+:t)
        s = s + b(i)
        mx = max(mx, -b(i))
        t = t + 0.5
        if (i == n * p) last = b(i)
    end do
    if (s /= 3 + sum(b) - b(3) - b(10) - b(17) - b(24)) error stop 3
    if (mx /= -minval(b)) error stop 4
    if (t /= 0.25 + 0.5 * 26) error stop 5
    if (last /= b(30)) error stop 6

    d = 0
    do concurrent (i = 1:12)
        block
            real :: w(p)
            do j = 1, p
                w(j) = real(i * j)
            end do
            do j = 1, 3
                d(i, j) = w(j) + w(p + 1 - j)
            end do
        end block
    end do
    do j = 1, 3
        do i = 1, 12
            if (d(i, j) /= real(i * j + i * (p + 1 - j))) error stop 7
        end do
    end do
    print *, "ok"
end program
