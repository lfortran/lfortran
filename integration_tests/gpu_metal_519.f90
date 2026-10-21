program gpu_metal_519
    ! Another statement of the time loop reads the field the consumer
    ! updates: the field must hold the current values in every step, so
    ! it cannot take turns with its copy.
    implicit none
    real, allocatable :: q(:), f(:), qh(:), fh(:)
    real :: total, total_h
    integer :: n, s, i
    n = 8
    allocate(q(n), f(n), qh(n), fh(n))
    do i = 1, n
        q(i) = real(i*i)
    end do
    qh = q
    total = 0.0
    total_h = 0.0
    do s = 1, 4
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
        total = total + q(4)
    end do
    ! The same on the host.
    do s = 1, 4
        do i = 1, n - 1
            fh(i) = qh(i + 1) - qh(i)
        end do
        do i = 2, n - 1
            qh(i) = qh(i) + 0.25 * (fh(i) - fh(i - 1))
        end do
        total_h = total_h + qh(4)
    end do
    print *, total
    print *, q
    if (abs(total - total_h) > 1e-3) error stop
    if (abs(total - 68.9921875) > 1e-3) error stop
    if (any(abs(q - qh) > 1e-3)) error stop
end program
