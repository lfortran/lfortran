program gpu_metal_677
    ! Reductions over nests with constant extents above the chunk limit
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=7: a chunk covers the 3 values of `i` whole and 2 of
    ! the 9 of `j`, 6 iterations, and the last one only 1 value of `j`. The
    ! accumulators started by the first chunk must collect every chunk.
    implicit none
    integer :: i, j
    integer :: v(3, 9)
    integer :: imax, imin, isum, emax, emin, esum
    real :: rprod, fprod

    do j = 1, 9
        do i = 1, 3
            v(i, j) = 1 + mod(5 * i + 3 * j, 17)
        end do
    end do

    imax = -1000
    imin = 1000
    isum = 0
    rprod = 1
    do concurrent (i = 1:3, j = 1:9) reduce(max:imax) reduce(min:imin) &
            reduce(+:isum) reduce(*:rprod)
        imax = max(imax, -v(i, j))
        imin = min(imin, v(i, j))
        isum = isum + v(i, j)
        if (v(i, j) > 14) rprod = rprod * 2
    end do

    emax = -1000
    emin = 1000
    esum = 0
    fprod = 1
    do j = 1, 9
        do i = 1, 3
            emax = max(emax, -v(i, j))
            emin = min(emin, v(i, j))
            esum = esum + v(i, j)
            if (v(i, j) > 14) fprod = fprod * 2
        end do
    end do
    if (imax /= emax) error stop 1
    if (imin /= emin) error stop 2
    if (isum /= esum) error stop 3
    if (rprod /= fprod) error stop 4
    print *, "ok"
end program
