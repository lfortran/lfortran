program gpu_kernel_source_20
    ! A flux loop and the update of the field it reads, in a time loop:
    ! with `--gpu=cuda` the loops are fused (the update recomputes the
    ! fluxes it reads), and the field takes turns with its copy, so the
    ! kernel source has the fused update twice, once in each direction,
    ! and no flux kernel outside the last step's original loops.
    implicit none
    real, allocatable :: q(:), f(:), qh(:), fh(:)
    integer :: n, s, i
    n = 16
    allocate(q(n), f(n - 1), qh(n), fh(n - 1))
    do i = 1, n
        q(i) = real(i*i)
    end do
    qh = q
    do s = 1, 5
        do concurrent (i = 1:n - 1)
            f(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.25 * (f(i) - f(i - 1))
        end do
    end do
    ! The same on the host.
    do s = 1, 5
        do i = 1, n - 1
            fh(i) = qh(i + 1) - qh(i)
        end do
        do i = 2, n - 1
            qh(i) = qh(i) + 0.25 * (fh(i) - fh(i - 1))
        end do
    end do
    print *, q
    if (any(abs(q - qh) > 1e-3)) error stop
    if (any(abs(f - fh) > 1e-3)) error stop
end program
