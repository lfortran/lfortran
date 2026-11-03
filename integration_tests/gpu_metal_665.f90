program gpu_metal_665
    ! Reductions over single offloaded loops whose iterations run in several
    ! launches of the kernel (lfortran/lfortran#16569). The test is compiled
    ! with a small --gpu-chunk-limit (7, and 1), so a loop of 23 iterations
    ! runs in chunks, the last of them partial: every chunk keeps combining
    ! into the accumulators the first one started, and the host folds them
    ! once into what the scalar held before the loop. Each result is
    ! compared with the same reduction run serially on the host.
    implicit none
    integer, parameter :: m = 23
    integer :: n, i, lo, hi
    integer :: isum, iprod, imin, imax, ia, io, ix
    integer :: esum, eprod, emin, emax, ea, eo, ex
    real :: rsum, rprod, rmin, rmax, a(m), b(m)
    real :: fsum, fprod, fmin, fmax

    n = m
    do i = 1, m
        a(i) = 0.25 * real(mod(7 * i, m) - 11)
        b(i) = real(i)
    end do

    ! Integer +, *, min and max over a run-time extent.
    isum = 5
    iprod = 3
    imin = 1000
    imax = -1000
    do concurrent (i = 1:n) reduce(+:isum) reduce(*:iprod) &
            reduce(min:imin) reduce(max:imax)
        isum = isum + i
        if (mod(i, 6) == 0) iprod = iprod * 2
        imin = min(imin, mod(7 * i, m) - 11)
        imax = max(imax, mod(7 * i, m) - 11)
    end do
    esum = 5
    eprod = 3
    emin = 1000
    emax = -1000
    do i = 1, n
        esum = esum + i
        if (mod(i, 6) == 0) eprod = eprod * 2
        emin = min(emin, mod(7 * i, m) - 11)
        emax = max(emax, mod(7 * i, m) - 11)
    end do
    if (isum /= esum) error stop 1
    if (iprod /= eprod) error stop 2
    if (imin /= emin) error stop 3
    if (imax /= emax) error stop 4

    ! The bitwise reductions.
    ia = -1
    io = 0
    ix = 12345
    do concurrent (i = 1:n) reduce(iand:ia) reduce(ior:io) reduce(ieor:ix)
        ia = iand(ia, ieor(-1, ishft(1, mod(i, 5))))
        io = ior(io, ishft(1, mod(3 * i, 17)))
        ix = ieor(ix, i * 37)
    end do
    ea = -1
    eo = 0
    ex = 12345
    do i = 1, n
        ea = iand(ea, ieor(-1, ishft(1, mod(i, 5))))
        eo = ior(eo, ishft(1, mod(3 * i, 17)))
        ex = ieor(ex, i * 37)
    end do
    if (ia /= ea) error stop 5
    if (io /= eo) error stop 6
    if (ix /= ex) error stop 7

    ! Real +, *, min and max over a constant extent beyond the limit, with
    ! values the sums and products hold exactly.
    rsum = 1.5
    rprod = 0.5
    rmin = 100
    rmax = -100
    do concurrent (i = 1:m) reduce(+:rsum) reduce(*:rprod) &
            reduce(min:rmin) reduce(max:rmax)
        rsum = rsum + a(i)
        if (mod(i, 4) == 0) rprod = rprod * 2.0
        rmin = min(rmin, a(i))
        rmax = max(rmax, a(i))
    end do
    fsum = 1.5
    fprod = 0.5
    fmin = 100
    fmax = -100
    do i = 1, m
        fsum = fsum + a(i)
        if (mod(i, 4) == 0) fprod = fprod * 2.0
        fmin = min(fmin, a(i))
        fmax = max(fmax, a(i))
    end do
    if (rsum /= fsum) error stop 8
    if (rprod /= fprod) error stop 9
    if (rmin /= fmin) error stop 10
    if (rmax /= fmax) error stop 11

    ! Run-time bounds, negative ones among them, and a mask.
    lo = -9
    hi = 13
    isum = 0
    imax = -1000
    do concurrent (i = lo:hi, mod(i + 20, 3) /= 0) reduce(+:isum) &
            reduce(max:imax)
        isum = isum + i * i
        imax = max(imax, -abs(i - 4))
    end do
    esum = 0
    emax = -1000
    do i = lo, hi
        if (mod(i + 20, 3) == 0) cycle
        esum = esum + i * i
        emax = max(emax, -abs(i - 4))
    end do
    if (isum /= esum) error stop 12
    if (imax /= emax) error stop 13

    ! A sum the body takes of an array section, inlined into the kernel,
    ! added into a reduction.
    rsum = 0
    do concurrent (i = 1:n) reduce(+:rsum)
        rsum = rsum + sum(b(1:i))
    end do
    fsum = 0
    do i = 1, n
        fsum = fsum + sum(b(1:i))
    end do
    if (rsum /= fsum) error stop 14

    ! No iterations: the scalars keep their values.
    lo = 5
    hi = 4
    isum = 77
    rmin = -3.5
    do concurrent (i = lo:hi) reduce(+:isum) reduce(min:rmin)
        isum = isum + i
        rmin = min(rmin, a(i))
    end do
    if (isum /= 77) error stop 15
    if (rmin /= -3.5) error stop 16

    ! A mask no iteration passes: the scalar keeps its value.
    imin = 42
    do concurrent (i = 1:n, i > n) reduce(min:imin)
        imin = min(imin, -i)
    end do
    if (imin /= 42) error stop 17
    print *, "ok"
end program
