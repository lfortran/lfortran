program gpu_metal_704
    ! A nest with an empty dimension has no iterations, however many values
    ! its other dimensions have: the launch must check the product of the
    ! extents for overflow only when no extent is zero
    ! (lfortran/lfortran#16569). Here the other extents multiply to more
    ! than huge(0_8), so a check that ignored the empty dimension would stop
    ! the program. The empty dimension is the outermost, so the CPU runs no
    ! iteration either.
    implicit none
    integer :: a(1), i, j, k, n, m, s
    n = huge(0)
    m = 0
    a = 7
    do concurrent (k = 1:m, i = -n:n, j = -n:n)
        a(1) = 0
    end do
    if (a(1) /= 7) error stop 1
    do concurrent (k = 5:4, i = -n:n, j = -n:n)
        a(1) = 0
    end do
    if (a(1) /= 7) error stop 2
    s = 3
    do concurrent (k = 1:m, i = -n:n, j = -n:n) reduce(+:s)
        s = s + 1
    end do
    if (s /= 3) error stop 3
    ! A nest that fits is still counted and run.
    do concurrent (k = 1:2, i = 1:3, j = 1:4) reduce(+:s)
        s = s + k * i * j
    end do
    if (s /= 3 + 3 * 6 * 10) error stop 4
    print *, "ok"
end program
