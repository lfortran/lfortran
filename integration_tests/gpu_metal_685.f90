program gpu_metal_685
    ! Offloaded nests of integer(4) loops with more than huge(0) iterations
    ! in all, though every index value fits in integer(4)
    ! (lfortran/lfortran#16541). The host counts the iterations in
    ! integer(8) and runs the kernel, which numbers them in 32 bits, in
    ! chunks of at most huge(0) iterations (lfortran/lfortran#16569). Only
    ! a few iterations write anything, so the test checks that every chunk
    ! ran, the last one included.
    implicit none
    integer :: c(3), d(3), i, j, n, m, k

    ! Run-time extents: 715827883 * 3 = 2**31 + 1 iterations.
    n = 715827883
    m = n
    k = 3
    c = 0
    do concurrent (j = 1:n, i = 1:k)
        if (j == m) c(i) = i
    end do
    if (c(1) /= 1 .or. c(2) /= 2 .or. c(3) /= 3) error stop 1

    ! Constant extents, the same number of iterations.
    d = 0
    do concurrent (j = 1:715827883, i = 1:3)
        if (j == m) d(i) = i
    end do
    if (d(1) /= 1 .or. d(2) /= 2 .or. d(3) /= 3) error stop 2
    print *, "ok"
end program
