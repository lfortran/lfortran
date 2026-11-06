program gpu_metal_666
    ! Reductions over offloaded nests whose iterations run in several
    ! launches of the kernel (lfortran/lfortran#16569). The test is compiled
    ! with a small --gpu-chunk-limit (7, and 1). Each result is compared
    ! with the same reduction run serially on the host.
    implicit none
    integer :: a(10, 9), c(-4:6, 2:12), d(3, 4, 5)
    integer :: i, j, k, n, m, p, lo1, hi1, lo2, hi2
    integer :: isum, iprod, imin, imax, esum, eprod, emin, emax
    real :: rsum, rmax, fsum, fmax

    do j = 1, 9
        do i = 1, 10
            a(i, j) = mod(13 * i + 7 * j, 31) - 15
        end do
    end do
    do j = 2, 12
        do i = -4, 6
            c(i, j) = mod(5 * i + 11 * j + 40, 29) - 14
        end do
    end do
    do k = 1, 5
        do j = 1, 4
            do i = 1, 3
                d(i, j, k) = mod(3 * i + 5 * j + 7 * k, 19) - 9
            end do
        end do
    end do

    ! Constant extents; the fastest dimension, of 10, is split.
    isum = -7
    imin = 1000
    imax = -1000
    do concurrent (i = 1:10, j = 1:9) reduce(+:isum) reduce(min:imin) &
            reduce(max:imax)
        isum = isum + a(i, j)
        imin = min(imin, a(i, j))
        imax = max(imax, a(i, j))
    end do
    esum = -7
    emin = 1000
    emax = -1000
    do j = 1, 9
        do i = 1, 10
            esum = esum + a(i, j)
            emin = min(emin, a(i, j))
            emax = max(emax, a(i, j))
        end do
    end do
    if (isum /= esum) error stop 1
    if (imin /= emin) error stop 2
    if (imax /= emax) error stop 3

    ! Run-time bounds, negative ones among them, with a mask.
    lo1 = -4
    hi1 = 6
    lo2 = 2
    hi2 = 12
    isum = 0
    iprod = 1
    rmax = -1000
    do concurrent (i = lo1:hi1, j = lo2:hi2, mod(i + j + 10, 4) /= 1) &
            reduce(+:isum) reduce(*:iprod) reduce(max:rmax)
        isum = isum + c(i, j)
        if (c(i, j) == 14) iprod = iprod * (-2)
        rmax = max(rmax, 0.5 * real(c(i, j)))
    end do
    esum = 0
    eprod = 1
    fmax = -1000
    do j = lo2, hi2
        do i = lo1, hi1
            if (mod(i + j + 10, 4) == 1) cycle
            esum = esum + c(i, j)
            if (c(i, j) == 14) eprod = eprod * (-2)
            fmax = max(fmax, 0.5 * real(c(i, j)))
        end do
    end do
    if (isum /= esum) error stop 4
    if (iprod /= eprod) error stop 5
    if (rmax /= fmax) error stop 6

    ! Three dimensions with run-time extents.
    n = 3
    m = 4
    p = 5
    rsum = 0
    imin = 1000
    do concurrent (i = 1:n, j = 1:m, k = 1:p) reduce(+:rsum) &
            reduce(min:imin)
        rsum = rsum + 0.25 * real(d(i, j, k))
        imin = min(imin, d(i, j, k) * i)
    end do
    fsum = 0
    emin = 1000
    do k = 1, p
        do j = 1, m
            do i = 1, n
                fsum = fsum + 0.25 * real(d(i, j, k))
                emin = min(emin, d(i, j, k) * i)
            end do
        end do
    end do
    if (rsum /= fsum) error stop 7
    if (imin /= emin) error stop 8

    ! An empty second dimension: the scalars keep their values.
    m = 0
    isum = 11
    rmax = 2.5
    do concurrent (i = 1:10, j = 1:m) reduce(+:isum) reduce(max:rmax)
        isum = isum + a(i, j)
        rmax = max(rmax, real(a(i, j)))
    end do
    if (isum /= 11) error stop 9
    if (rmax /= 2.5) error stop 10
    print *, "ok"
end program
