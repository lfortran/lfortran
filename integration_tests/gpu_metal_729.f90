program gpu_metal_729
    ! The logical reductions of offloaded loops whose iterations run in
    ! many launches of the kernel (lfortran/lfortran#16725). The test is
    ! compiled with a small --gpu-chunk-limit, so every chunk keeps
    ! combining into the accumulators the first one started, and the host
    ! folds them into what the variable held before the loop. The .eqv. and
    ! .neqv. results depend on the parity of the number of .true. values, so
    ! a slot lost or folded twice changes them. Each result is compared with
    ! the same reduction run serially on the host.
    implicit none
    integer, parameter :: m = 1001
    integer :: i, n, k
    logical :: a, o, e, x, ea, eo, ee, ex
    logical :: v(m)

    n = m
    do k = 0, 3
        do i = 1, m
            v(i) = mod(7 * i + k, 11) < 5 + k
        end do
        a = mod(k, 2) == 0
        o = mod(k, 2) == 1
        e = k < 2
        x = k >= 2
        ea = a; eo = o; ee = e; ex = x
        do concurrent (i = 1:n) reduce(.and.:a) reduce(.or.:o) &
                reduce(.eqv.:e) reduce(.neqv.:x)
            a = a .and. v(i)
            o = o .or. v(i)
            e = e .eqv. v(i)
            x = x .neqv. v(i)
        end do
        do i = 1, n
            ea = ea .and. v(i)
            eo = eo .or. v(i)
            ee = ee .eqv. v(i)
            ex = ex .neqv. v(i)
        end do
        if (a .neqv. ea) error stop 1
        if (o .neqv. eo) error stop 2
        if (e .neqv. ee) error stop 3
        if (x .neqv. ex) error stop 4
    end do

    ! All .true. but one, over a run-time extent with an odd number of
    ! iterations: one more or one fewer .true. value flips .eqv. and .neqv.
    n = m - 2
    v = .true.
    v(n / 2) = .false.
    a = .true.; o = .false.; e = .true.; x = .false.
    do concurrent (i = 1:n) reduce(.and.:a) reduce(.or.:o) &
            reduce(.eqv.:e) reduce(.neqv.:x)
        a = a .and. v(i)
        o = o .or. .not. v(i)
        e = e .eqv. v(i)
        x = x .neqv. v(i)
    end do
    if (a) error stop 5
    if (.not. o) error stop 6
    ! .eqv. of .true. with 998 .true. values and one .false.
    if (e) error stop 7
    ! .neqv. of .false. with 998 .true. values.
    if (x) error stop 8

    print *, "ok"
end program
