program do_concurrent_17
    integer, parameter :: nx = 6, ny = 5
    integer :: i, j, row
    integer :: a(nx, ny)
    integer :: total, largest

    do j = 1, ny
        do i = 1, nx
            a(i, j) = i * j
        end do
    end do

    total = 0
    largest = 0
    do concurrent (i = 1:nx, j = 1:ny) shared(a) local(row) &
            reduce(+:total) reduce(max:largest)
        row = a(i, j)
        total = total + row
        largest = max(largest, row)
    end do

    if (total /= 315) error stop
    if (largest /= 30) error stop

    print *, "ok"
end program
