program gpu_kernel_source_18
    ! A flux loop fused into the update loop after it, in a time loop: the
    ! fused loop recomputes the flux where the update reads it and writes
    ! the field into a copy it takes turns with; the original loops, kept
    ! for the last step, run as written.
    implicit none
    integer, parameter :: n = 20, steps = 5
    real, allocatable :: q(:), flux(:)
    real :: expected(n), expected_flux(n - 1)
    integer :: step, i
    allocate(q(n), flux(n))
    do i = 1, n
        q(i) = real(mod(i * 7, 11))
    end do
    expected = q
    flux = 0
    do step = 1, steps
        do concurrent (i = 1:n)
            if (i < n) flux(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 2:n - 1)
            q(i) = q(i) + 0.1 * (flux(i) - flux(i - 1))
        end do
    end do
    do step = 1, steps
        expected_flux = expected(2:n) - expected(1:n - 1)
        expected(2:n - 1) = expected(2:n - 1) + 0.1 * (expected_flux(2:n - 1) &
            - expected_flux(1:n - 2))
    end do
    if (maxval(abs(q - expected)) > 1e-4) error stop 1
    if (maxval(abs(flux(1:n - 1) - expected_flux)) > 1e-4) error stop 2
    print *, "ok"
end program
