program gpu_metal_679
    ! A nest with constant extents whose iterations fit the chunk limit but
    ! whose whole tiles would not (lfortran/lfortran#16569). The test is
    ! compiled with --gpu-chunk-limit=480. The 64 by 7 nest has 448
    ! iterations, which tiles of 32 by 8 pad to 512 threads, more than the
    ! limit: it runs as one flat launch. The 64 by 8 nest has 512, more than
    ! the limit, and runs flat in chunks. The 32 by 8 nest is one whole tile
    ! of 256 threads and runs tiled, as one launch.
    implicit none
    integer :: i, j
    integer :: a(64, 7), b(64, 8), c(32, 8)

    a = 0
    do concurrent (i = 1:64, j = 1:7)
        a(i, j) = 1000 * i + j
    end do
    b = 0
    do concurrent (i = 1:64, j = 1:8)
        b(i, j) = 1000 * j + i
    end do
    c = 0
    do concurrent (i = 1:32, j = 1:8)
        c(i, j) = i + 100 * j
    end do
    do j = 1, 8
        do i = 1, 64
            if (j <= 7) then
                if (a(i, j) /= 1000 * i + j) error stop 1
            end if
            if (b(i, j) /= 1000 * j + i) error stop 2
            if (i <= 32) then
                if (c(i, j) /= i + 100 * j) error stop 3
            end if
        end do
    end do
    print *, "ok"
end program
