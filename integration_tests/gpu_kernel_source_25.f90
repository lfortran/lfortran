module gpu_kernel_source_25_m
    implicit none
contains
    pure real function dflux(f, i) result(r)
        real, intent(in) :: f(:)
        integer, intent(in) :: i
        r = f(i) - f(i - 1)
    end function
end module

program gpu_kernel_source_25
    ! The flux loop writes the flux from its third index only (a test), and
    ! the flux is allocated from there. The fusion cannot take its lower
    ! bound to be the loop's first index (1), which the bounds checked at
    ! run time exclude: the fused loop reads the bound instead, and runs.
    use gpu_kernel_source_25_m
    implicit none
    integer, parameter :: n = 20, steps = 5
    real, allocatable :: q(:), flux(:), hq(:), hflux(:)
    integer :: step, i
    allocate(q(n), flux(3:n - 1), hq(n), hflux(3:n - 1))
    do i = 1, n
        q(i) = real(mod(i * 7, 11))
    end do
    flux = 0
    hq = q
    hflux = 0
    do step = 1, steps
        do concurrent (i = 1:n)
            if (i >= 3 .and. i < n) flux(i) = q(i + 1) - q(i)
        end do
        do concurrent (i = 3:n - 3)
            q(i) = q(i) + 0.1 * dflux(flux, i)
        end do
    end do
    do step = 1, steps
        do i = 3, n - 1
            hflux(i) = hq(i + 1) - hq(i)
        end do
        do i = 3, n - 3
            hq(i) = hq(i) + 0.1 * dflux(hflux, i)
        end do
    end do
    if (maxval(abs(q - hq)) > 1e-4) error stop 1
    if (maxval(abs(flux - hflux)) > 1e-4) error stop 2
    print *, "ok"
end program
