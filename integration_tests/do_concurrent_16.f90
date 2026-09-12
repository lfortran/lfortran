program do_concurrent_16
    integer, parameter :: nx = 5, ny = 4, nz = 3
    integer :: i, j, k, total
    integer :: a(nx, ny), b(nx, ny, nz)

    a = 0
    do concurrent (i = 1:nx, j = 1:ny)
        a(i, j) = (10 * i + j) ** 2
    end do

    total = 0
    do j = 1, ny
        do i = 1, nx
            if (a(i, j) /= (10 * i + j) ** 2) error stop
            total = total + a(i, j)
        end do
    end do
    if (total /= 25150) error stop

    b = 0
    do concurrent (i = 1:nx, j = 1:ny, k = 1:nz)
        b(i, j, k) = i + 100 * j + 10000 * k
    end do

    total = 0
    do k = 1, nz
        do j = 1, ny
            do i = 1, nx
                if (b(i, j, k) /= i + 100 * j + 10000 * k) error stop
                total = total + b(i, j, k)
            end do
        end do
    end do
    if (total /= 1215180) error stop

    print *, "ok"
end program
