program gpu_kernel_source_19
    ! A smoothing iteration with one-sided ends, compiled for a device
    ! cache of 32 KiB: the time loop runs its two parallel loops on
    ! windows of rows (time skewing), and the split of the first one takes
    ! the rows of its interior in the window from the host.
    implicit none
    integer, parameter :: n = 5003
    real :: u(n), d(n), r(n), e(n)
    integer :: it, k
    do k = 1, n
        u(k) = sin(0.01 * k) + 0.1 * mod(k, 5)
    end do
    r = u
    do it = 1, 6
        do concurrent (k = 1:n)
            if (k == 1) then
                d(k) = u(2) - u(1)
            else if (k == n) then
                d(k) = u(n - 1) - u(n)
            else
                d(k) = u(k - 1) - 2.0 * u(k) + u(k + 1)
            end if
        end do
        do concurrent (k = 1:n)
            u(k) = u(k) + 0.2 * d(k)
        end do
    end do
    do it = 1, 6
        do k = 1, n
            if (k == 1) then
                e(k) = r(2) - r(1)
            else if (k == n) then
                e(k) = r(n - 1) - r(n)
            else
                e(k) = r(k - 1) - 2.0 * r(k) + r(k + 1)
            end if
        end do
        r = r + 0.2 * e
    end do
    if (maxval(abs(u - r)) > 1e-5) error stop 1
    print *, "ok"
end program
