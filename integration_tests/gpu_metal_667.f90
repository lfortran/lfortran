program gpu_metal_667
    ! Reductions over offloaded nests whose largest chunk has fewer
    ! iterations than the chunk limit (lfortran/lfortran#16569). The test is
    ! compiled with --gpu-chunk-limit=7: a chunk covers the 3 values of `i`
    ! whole and 2 values of `j`, 6 iterations, so only 6 accumulators are
    ! started, and the host must fold no others. The maximum of negative
    ! values, the minimum of positive ones and a product would all change
    ! if it folded one that holds whatever its storage held.
    implicit none
    integer :: i, j, n
    integer :: v(3, 20)
    integer :: imax, imin, iprod, isum, emax, emin, eprod, esum
    real :: rmax, rmin, fmax, fmin

    do j = 1, 20
        do i = 1, 3
            v(i, j) = 1 + mod(5 * i + 3 * j, 17)
        end do
    end do

    do n = 1, 20
        imax = -1000
        imin = 1000
        iprod = 1
        isum = 0
        rmax = -1000
        rmin = 1000
        do concurrent (i = 1:3, j = 1:n) reduce(max:imax) reduce(min:imin) &
                reduce(*:iprod) reduce(+:isum) reduce(max:rmax) &
                reduce(min:rmin)
            imax = max(imax, -v(i, j))
            imin = min(imin, v(i, j))
            if (v(i, j) == 17) iprod = iprod * 3
            isum = isum + v(i, j)
            rmax = max(rmax, -0.5 * real(v(i, j)))
            rmin = min(rmin, 0.5 * real(v(i, j)))
        end do
        emax = -1000
        emin = 1000
        eprod = 1
        esum = 0
        fmax = -1000
        fmin = 1000
        do j = 1, n
            do i = 1, 3
                emax = max(emax, -v(i, j))
                emin = min(emin, v(i, j))
                if (v(i, j) == 17) eprod = eprod * 3
                esum = esum + v(i, j)
                fmax = max(fmax, -0.5 * real(v(i, j)))
                fmin = min(fmin, 0.5 * real(v(i, j)))
            end do
        end do
        if (imax /= emax) error stop 1
        if (imin /= emin) error stop 2
        if (iprod /= eprod) error stop 3
        if (isum /= esum) error stop 4
        if (rmax /= fmax) error stop 5
        if (rmin /= fmin) error stop 6
    end do
    print *, "ok"
end program
