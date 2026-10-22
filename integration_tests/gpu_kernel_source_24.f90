module gpu_kernel_source_24_m
    implicit none
contains
    pure real function dflux(f, i) result(r)
        real, intent(in) :: f(:)
        integer, intent(in) :: i
        r = f(i) - f(i - 1)
    end function
end module

program gpu_kernel_source_24
    ! A flux loop fused into an update loop that reads the flux through a
    ! pure function of the whole array. Inlining the function shifts its
    ! subscripts by the lower bound of the allocatable flux; the fused loop
    ! takes that bound to be the producer's first index (1), checked at run
    ! time, so that its subscripts are constant offsets of the loop index.
    ! Allocated with another lower bound (0), every element the update
    ! reads is still one the flux loop writes, but the check fails and the
    ! original loops run, with the same results as the loops on the host.
    use gpu_kernel_source_24_m
    implicit none
    integer, parameter :: n = 20, steps = 5
    real, allocatable :: q(:), flux(:), hq(:), hflux(:)
    integer :: step, i, lb
    do lb = 1, 0, -1
        allocate(q(n), flux(lb:n), hq(n), hflux(lb:n))
        do i = 1, n
            q(i) = real(mod(i * 7, 11))
        end do
        flux = 0
        hq = q
        hflux = 0
        do step = 1, steps
            do concurrent (i = 1:n)
                if (i < n) flux(i) = q(i + 1) - q(i)
            end do
            do concurrent (i = 3:n - 1)
                q(i) = q(i) + 0.1 * dflux(flux, i)
            end do
        end do
        do step = 1, steps
            do i = 1, n - 1
                hflux(i) = hq(i + 1) - hq(i)
            end do
            do i = 3, n - 1
                hq(i) = hq(i) + 0.1 * dflux(hflux, i)
            end do
        end do
        if (maxval(abs(q - hq)) > 1e-4) error stop 1
        if (maxval(abs(flux - hflux)) > 1e-4) error stop 2
        deallocate(q, flux, hq, hflux)
    end do
    print *, "ok"
end program
