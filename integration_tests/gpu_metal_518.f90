program gpu_metal_518
    ! The consumer's bounds depend on the variable of the DO loop around
    ! the pair, so it writes different elements of the field in each step:
    ! the field and its copy must not take turns.
    implicit none
    real, allocatable :: q(:), f(:), qh(:), fh(:)
    integer :: n, s, i
    n = 10
    allocate(q(n), f(n - 1), qh(n), fh(n - 1))
    do i = 1, n
        q(i) = real(i*i)
    end do
    qh = q
    do s = 1, 4
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - s)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    ! The same on the host.
    do s = 1, 4
        do i = 1, n - 1
            fh(i) = qh(i + 1) - qh(i)
        end do
        do i = 2, n - s
            qh(i) = qh(i) + 0.25 * (fh(i) - fh(i - 1))
        end do
    end do
    print *, q
    if (any(abs(q - qh) > 1e-3)) error stop
    if (any(abs(f - fh) > 1e-3)) error stop
    if (abs(q(6) - 38.0) > 1e-3) error stop
end program
