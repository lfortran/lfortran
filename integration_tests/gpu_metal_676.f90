program gpu_metal_676
    ! Nests with constant extents above the chunk limit
    ! (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=50. In the first nest a chunk covers the 10 values of
    ! `i` whole and 5 of the 7 of `j`, so every value of `k` runs a chunk of
    ! 50 iterations and a partial one of 20. The second nest has the shape
    ! of a tiled launch, whose whole tiles would start more threads than the
    ! limit: it runs flat, in chunks of one value of `j`.
    implicit none
    integer :: i, j, k
    integer :: a(10, 7, 3), b(40, 40)

    a = 0
    do concurrent (i = 1:10, j = 1:7, k = 1:3)
        a(i, j, k) = 100 * i + 10 * j + k
    end do
    b = 0
    do concurrent (i = 1:40, j = 1:40)
        b(i, j) = i - 2 * j
    end do
    do k = 1, 3
        do j = 1, 7
            do i = 1, 10
                if (a(i, j, k) /= 100 * i + 10 * j + k) error stop 1
            end do
        end do
    end do
    do j = 1, 40
        do i = 1, 40
            if (b(i, j) /= i - 2 * j) error stop 2
        end do
    end do
    print *, "ok"
end program
