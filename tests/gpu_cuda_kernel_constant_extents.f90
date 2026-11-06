program gpu_cuda_kernel_constant_extents
    ! Nests whose extents are all constants are decided at compile time
    ! (lfortran/lfortran#16569). Compile-only: the first and last nests run
    ! more than 2**31 threads.
    implicit none
    integer :: i, j, k, s
    real, allocatable :: y(:, :)
    real :: z(4, 5, 6), w(64, 64)
    integer :: v(10, 20)
    ! 64 * 33554431 = 2147483584 iterations, at most huge(0), but whole
    ! tiles of 32 by 8 would start 2**31 threads: the kernel numbers the
    ! iterations flat, takes no tile flag and no bounds, and the launch
    ! runs as one.
    allocate(y(64, 33554431))
    do concurrent (i = 1:64, j = 1:33554431)
        y(i, j) = 1
    end do
    ! A 3-D nest and a whole number of tiles that fit: constant bounds, no
    ! chunks; the 64 by 64 nest keeps its tiles.
    do concurrent (i = 1:4, j = 1:5, k = 1:6)
        z(i, j, k) = i + j + k
    end do
    do concurrent (i = 1:64, j = 1:64)
        w(i, j) = i - j
    end do
    ! A reduction over constant extents that fit: no first-chunk flag and
    ! no bounds.
    v = 1
    s = 0
    do concurrent (i = 1:10, j = 1:20) reduce(+:s)
        s = s + v(i, j)
    end do
    ! 2**32 iterations, more than huge(0): `j` is split and its bounds are
    ! parameters, while `i` stays a constant, and the kernel numbers the
    ! iterations flat, as tiles would start more threads than one launch
    ! runs.
    do concurrent (i = 1:65536, j = 1:65536)
        if (i == j) v(1, 1) = 2
    end do
    print *, sum(z), sum(w), s, v(1, 1)
end program
