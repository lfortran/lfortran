program gpu_metal_611
    ! Offloaded nests of three loops whose iterations run in several
    ! launches of the kernel (lfortran/lfortran#16569). The test is compiled
    ! with --gpu-chunk-limit=7.
    implicit none
    integer :: a(2, 3, 5), b(4, 4, 4), c(9, 2, 3), d(-1:3, 0:4, 2:5)
    integer :: i, j, k, n1, n2, n3

    ! The first two dimensions, 6 iterations, fit; the third is split.
    a = 0
    do concurrent (i = 1:2, j = 1:3, k = 1:5)
        a(i, j, k) = 100 * i + 10 * j + k
    end do
    do k = 1, 5
        do j = 1, 3
            do i = 1, 2
                if (a(i, j, k) /= 100 * i + 10 * j + k) error stop 1
            end do
        end do
    end do

    ! The first dimension fits, the second is split, one value of the
    ! third per chunk.
    b = 0
    do concurrent (i = 1:4, j = 1:4, k = 1:4)
        b(i, j, k) = 100 * i + 10 * j + k
    end do
    do k = 1, 4
        do j = 1, 4
            do i = 1, 4
                if (b(i, j, k) /= 100 * i + 10 * j + k) error stop 2
            end do
        end do
    end do

    ! The fastest dimension alone is longer than the limit.
    c = 0
    do concurrent (i = 1:9, j = 1:2, k = 1:3)
        c(i, j, k) = 100 * i + 10 * j + k
    end do
    do k = 1, 3
        do j = 1, 2
            do i = 1, 9
                if (c(i, j, k) /= 100 * i + 10 * j + k) error stop 3
            end do
        end do
    end do

    ! Run-time bounds, and a mask.
    n1 = 3
    n2 = 4
    n3 = 5
    d = -1
    do concurrent (i = -1:n1, j = 0:n2, k = 2:n3, i /= j)
        d(i, j, k) = 100 * i + 10 * j + k
    end do
    do k = 2, 5
        do j = 0, 4
            do i = -1, 3
                if (i /= j) then
                    if (d(i, j, k) /= 100 * i + 10 * j + k) error stop 4
                else
                    if (d(i, j, k) /= -1) error stop 5
                end if
            end do
        end do
    end do

    ! No iterations.
    n2 = -1
    d = 3
    do concurrent (i = -1:n1, j = 0:n2, k = 2:n3)
        d(i, j, k) = 0
    end do
    if (any(d /= 3)) error stop 6
    print *, "ok"
end program
