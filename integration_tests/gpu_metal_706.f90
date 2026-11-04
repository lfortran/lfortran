program gpu_metal_706
    ! Offloaded nests with a run-time extent that never run in chunks: the
    ! kind of `j` and its constant start keep them within the chunk limit
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=4000. The first nest has 30 * 127 = 3810 iterations,
    ! which fit, but whole tiles of 32 by 8 would start 32 * 128 = 4096
    ! threads, more than the limit: it runs flat, one thread per iteration.
    ! The second one, 31 by 64 iterations, runs in tiles of 32 by 64. `j`
    ! reaches huge(0), so a padded thread past its end must not compute an
    ! index.
    ! Every element is checked.
    implicit none
    integer, parameter :: base = huge(0) - 127
    integer :: a(30, 127), b(31, 64), i, j, n

    n = huge(0)
    a = 0
    do concurrent (i = 1:30, j = huge(0) - 126:n)
        a(i, j - base) = i + 100 * (j - base)
    end do
    do j = 1, 127
        do i = 1, 30
            if (a(i, j) /= i + 100 * j) error stop 1
        end do
    end do

    n = huge(0) - 63
    b = 0
    do concurrent (i = 1:31, j = huge(0) - 126:n)
        b(i, j - base) = 2 * i + 1000 * (j - base)
    end do
    do j = 1, 64
        do i = 1, 31
            if (b(i, j) /= 2 * i + 1000 * j) error stop 2
        end do
    end do
    print *, "ok"
end program
