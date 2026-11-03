program gpu_metal_685
    ! Offloaded nests of integer(4) loops with more than huge(0) iterations
    ! in all, though every index value fits in integer(4)
    ! (lfortran/lfortran#16541). The host counts the iterations in
    ! integer(8) and runs the kernel, which numbers them in 32 bits, in
    ! chunks of at most huge(0) iterations (lfortran/lfortran#16569): here
    ! a first chunk of j = 1:715827882 (a 2**31-thread dispatch) and a
    ! last chunk of j = 715827883. Only the iterations at j = 1 and at
    ! j = m write anything, so the test checks that the first and the last
    ! chunk both ran.
    implicit none
    integer :: c(3), d(3), e(3), f(3), i, j, n, m, k

    ! Run-time extents: 715827883 * 3 = 2**31 + 1 iterations.
    n = 715827883
    m = n
    k = 3
    c = 0
    e = 0
    do concurrent (j = 1:n, i = 1:k)
        if (j == 1) e(i) = 10*i
        if (j == m) c(i) = i
    end do
    if (e(1) /= 10 .or. e(2) /= 20 .or. e(3) /= 30) error stop 1
    if (c(1) /= 1 .or. c(2) /= 2 .or. c(3) /= 3) error stop 2

    ! Constant extents, the same number of iterations.
    d = 0
    f = 0
    do concurrent (j = 1:715827883, i = 1:3)
        if (j == 1) f(i) = 10*i
        if (j == m) d(i) = i
    end do
    if (f(1) /= 10 .or. f(2) /= 20 .or. f(3) /= 30) error stop 3
    if (d(1) /= 1 .or. d(2) /= 2 .or. d(3) /= 3) error stop 4
    print *, "ok"
end program
