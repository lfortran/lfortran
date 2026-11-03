program gpu_metal_668
    ! Reductions over offloaded loops with integer(8) indices whose
    ! iterations run in several launches of the kernel
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=7. Each result is compared with the same reduction
    ! run serially on the host.
    implicit none
    integer(8) :: k, lo, hi, l
    integer :: i
    integer(8) :: ksum, kmax, esum, emax
    real(8) :: dsum, fsum
    integer :: w(30), x(4, 9)
    do i = 1, 30
        w(i) = mod(11 * i, 30) - 12
    end do
    x = reshape([(mod(7 * i, 13) - 6, i = 1, 36)], [4, 9])

    lo = 3
    hi = 29
    ksum = 10
    kmax = -huge(0_8)
    dsum = 0.5d0
    do concurrent (k = lo:hi) reduce(+:ksum) reduce(max:kmax) reduce(+:dsum)
        ksum = ksum + k * w(k)
        kmax = max(kmax, int(w(k), 8) * 1000000000_8)
        dsum = dsum + 0.125d0 * w(k)
    end do
    esum = 10
    emax = -huge(0_8)
    fsum = 0.5d0
    do k = lo, hi
        esum = esum + k * w(k)
        emax = max(emax, int(w(k), 8) * 1000000000_8)
        fsum = fsum + 0.125d0 * w(k)
    end do
    if (ksum /= esum) error stop 1
    if (kmax /= emax) error stop 2
    if (dsum /= fsum) error stop 3

    ! A nest of two integer(8) indices.
    ksum = 0
    do concurrent (l = 1_8:4_8, k = 1_8:9_8) reduce(+:ksum)
        ksum = ksum + x(l, k) * k
    end do
    esum = 0
    do k = 1, 9
        do i = 1, 4
            esum = esum + x(i, k) * k
        end do
    end do
    if (ksum /= esum) error stop 4
    print *, "ok"
end program
