program gpu_metal_687
    ! Reductions over offloaded loops whose iterations run in several
    ! launches of the kernel (lfortran/lfortran#16569), on every device
    ! that offloads them, Metal included (lfortran/lfortran#16541). The test
    ! is compiled with --gpu-chunk-limit=3: every chunk keeps combining into
    ! the accumulators the first one started, and the host folds them once
    ! into what each scalar held before the loop. Each result is compared
    ! with the same reduction run serially on the host.
    implicit none
    integer, parameter :: m = 17
    integer :: i, j, n, p
    integer :: isum, iprod, imin, imax, esum, eprod, emin, emax
    real :: rsum, rprod, rmin, rmax, fsum, fprod, fmin, fmax
    real :: a(m), b(5, m)
    integer :: ia, io, ea, eo

    do i = 1, m
        a(i) = 0.5 * real(mod(5 * i, m) - 8)
        do j = 1, 5
            b(j, i) = real(mod(3 * i + j, 7))
        end do
    end do

    ! Integer +, *, min and max over a single loop of run-time extent.
    n = m
    isum = 4
    iprod = -1
    imin = 500
    imax = -500
    do concurrent (i = 1:n) reduce(+:isum) reduce(*:iprod) &
            reduce(min:imin) reduce(max:imax)
        isum = isum + 2 * i - 9
        if (mod(i, 4) == 1) iprod = iprod * 3
        imin = min(imin, mod(11 * i, m) - 6)
        imax = max(imax, mod(11 * i, m) - 6)
    end do
    esum = 4
    eprod = -1
    emin = 500
    emax = -500
    do i = 1, n
        esum = esum + 2 * i - 9
        if (mod(i, 4) == 1) eprod = eprod * 3
        emin = min(emin, mod(11 * i, m) - 6)
        emax = max(emax, mod(11 * i, m) - 6)
    end do
    if (isum /= esum) error stop 1
    if (iprod /= eprod) error stop 2
    if (imin /= emin) error stop 3
    if (imax /= emax) error stop 4

    ! Real +, *, min and max over a nest of run-time extents, with values
    ! the sums and products hold exactly.
    p = 5
    rsum = -2.5
    rprod = 1.0
    rmin = 100
    rmax = -100
    do concurrent (j = 1:p, i = 1:n) reduce(+:rsum) reduce(*:rprod) &
            reduce(min:rmin) reduce(max:rmax)
        rsum = rsum + a(i) * real(j)
        if (j == 2 .and. mod(i, 5) == 0) rprod = rprod * 2.0
        rmin = min(rmin, a(i) - real(j))
        rmax = max(rmax, a(i) + real(j))
    end do
    fsum = -2.5
    fprod = 1.0
    fmin = 100
    fmax = -100
    do i = 1, n
        do j = 1, p
            fsum = fsum + a(i) * real(j)
            if (j == 2 .and. mod(i, 5) == 0) fprod = fprod * 2.0
            fmin = min(fmin, a(i) - real(j))
            fmax = max(fmax, a(i) + real(j))
        end do
    end do
    if (rsum /= fsum) error stop 5
    if (rprod /= fprod) error stop 6
    if (rmin /= fmin) error stop 7
    if (rmax /= fmax) error stop 8

    ! The bitwise reductions, with a mask. (.and. and .or. have no device
    ! lowering yet, lfortran/lfortran#16725.)
    ia = -1
    io = 0
    do concurrent (i = 1:n, mod(i, 3) /= 0) reduce(iand:ia) reduce(ior:io)
        ia = iand(ia, ieor(-1, ishft(1, mod(i, 6))))
        io = ior(io, ishft(1, mod(5 * i, 13)))
    end do
    ea = -1
    eo = 0
    do i = 1, n
        if (mod(i, 3) == 0) cycle
        ea = iand(ea, ieor(-1, ishft(1, mod(i, 6))))
        eo = ior(eo, ishft(1, mod(5 * i, 13)))
    end do
    if (ia /= ea) error stop 9
    if (io /= eo) error stop 10

    ! The sum of an array section, inlined into the kernel, added into a
    ! reduction.
    rsum = 1
    do concurrent (i = 1:n) reduce(+:rsum)
        rsum = rsum + sum(b(:, i))
    end do
    fsum = 1
    do i = 1, n
        fsum = fsum + sum(b(:, i))
    end do
    if (rsum /= fsum) error stop 11
    print *, "ok"
end program
