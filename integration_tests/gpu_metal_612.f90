program gpu_metal_612
    ! Offloaded nests a device may run in tiles, whose iterations run in
    ! several launches of the kernel (lfortran/lfortran#16569). The test is
    ! compiled with --gpu-chunk-limit=1000: a nest of more iterations runs
    ! in chunks numbered flat, and one of fewer as one launch, in tiles
    ! where the device has them.
    implicit none
    real :: x(0:65, 0:65), y(64, 64), z(32, 16), w(32, 8, 10)
    integer :: i, j, k, n, m

    do j = 0, 65
        do i = 0, 65
            x(i, j) = real(i + 100 * j)
        end do
    end do

    ! Constant extents, 4096 iterations: in chunks.
    y = 0
    do concurrent (i = 1:64, j = 1:64)
        y(i, j) = x(i - 1, j) + x(i + 1, j) + x(i, j - 1) + x(i, j + 1)
    end do
    do j = 1, 64
        do i = 1, 64
            if (y(i, j) /= 4 * x(i, j)) error stop 1
        end do
    end do

    ! Constant extents, 512 iterations: one launch.
    z = 0
    do concurrent (i = 1:32, j = 1:16)
        z(i, j) = x(i, j) + 1
    end do
    do j = 1, 16
        do i = 1, 32
            if (z(i, j) /= x(i, j) + 1) error stop 2
        end do
    end do

    ! Run-time extents, 768 iterations: one launch.
    n = 32
    m = 24
    y = 0
    do concurrent (i = 1:n, j = 1:m)
        y(i, j) = x(i - 1, j) + x(i + 1, j)
    end do
    do j = 1, 64
        do i = 1, 64
            if (i <= n .and. j <= m) then
                if (y(i, j) /= 2 * x(i, j)) error stop 3
            else
                if (y(i, j) /= 0) error stop 4
            end if
        end do
    end do

    ! Run-time extents, 2560 iterations: in chunks.
    n = 64
    m = 40
    y = 0
    do concurrent (i = 1:n, j = 1:m)
        y(i, j) = x(i, j - 1) + x(i, j + 1)
    end do
    do j = 1, 64
        do i = 1, 64
            if (j <= m) then
                if (y(i, j) /= 2 * x(i, j)) error stop 5
            else
                if (y(i, j) /= 0) error stop 6
            end if
        end do
    end do

    ! Three dimensions, 2560 iterations: in chunks.
    w = 0
    do concurrent (i = 1:32, j = 1:8, k = 1:10)
        w(i, j, k) = x(i, j) + k
    end do
    do k = 1, 10
        do j = 1, 8
            do i = 1, 32
                if (w(i, j, k) /= x(i, j) + k) error stop 7
            end do
        end do
    end do
    print *, "ok"
end program
