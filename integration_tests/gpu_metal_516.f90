program gpu_metal_516
    ! The loop around the producer and the consumer runs no iteration in
    ! the last iteration of the outer loop (its start depends on the outer
    ! loop's variable): the original loops do not run there, so the last
    ! execution of the pair is not the one the fusion keeps as written.
    implicit none
    real, allocatable :: q(:), f(:), qh(:), fh(:)
    integer :: n, s, t, i
    n = 8
    allocate(q(n), f(n), qh(n), fh(n))
    do i = 1, n
        q(i) = real(i*i)
    end do
    qh = q
    f = -1.0
    fh = -1.0
    do s = 1, 3
        do t = s, 2
            do concurrent (i = 1:n)
                f(i) = 2.0 * q(i)
            end do
            do concurrent (i = 2:n - 1)
                q(i) = q(i) + 0.125 * (f(i + 1) - f(i - 1))
            end do
        end do
    end do
    ! The same on the host.
    do s = 1, 3
        do t = s, 2
            do i = 1, n
                fh(i) = 2.0 * qh(i)
            end do
            do i = 2, n - 1
                qh(i) = qh(i) + 0.125 * (fh(i + 1) - fh(i - 1))
            end do
        end do
    end do
    print *, f
    print *, q
    if (any(abs(f - fh) > 1e-3)) error stop
    if (any(abs(q - qh) > 1e-3)) error stop
end program
