program gpu_kernel_source_22
    ! The stages of a time step differ by a SELECT CASE in the update,
    ! whose branches each read the element of the field they write: the
    ! loops are still fused, and the field takes turns with its copy. The
    ! odd number of fused executions leaves the last values in the copy.
    implicit none
    real, allocatable :: q(:), f(:), qh(:), fh(:)
    integer :: n, s, stage, i
    n = 16
    allocate(q(n), f(n - 1), qh(n), fh(n - 1))
    do i = 1, n
        q(i) = real(i*i)
    end do
    qh = q
    do s = 1, 2
        do stage = 1, 3
            do concurrent (i = 1:n - 1)
                f(i) = q(i + 1) - q(i)
            end do
            do concurrent (i = 2:n - 1)
                select case (stage)
                case (1)
                    q(i) = q(i) + 0.125 * (f(i) - f(i - 1))
                case default
                    q(i) = 0.5 * q(i) + 0.25 * (f(i) - f(i - 1))
                end select
            end do
        end do
    end do
    ! The same on the host.
    do s = 1, 2
        do stage = 1, 3
            do i = 1, n - 1
                fh(i) = qh(i + 1) - qh(i)
            end do
            do i = 2, n - 1
                select case (stage)
                case (1)
                    qh(i) = qh(i) + 0.125 * (fh(i) - fh(i - 1))
                case default
                    qh(i) = 0.5 * qh(i) + 0.25 * (fh(i) - fh(i - 1))
                end select
            end do
        end do
    end do
    print *, q
    if (any(abs(q - qh) > 1e-3)) error stop
    if (any(abs(f - fh) > 1e-3)) error stop
end program
