program file_06
    implicit none

    integer :: u = 11, i, j
    real :: arr(2, 3)
    real, allocatable :: alloc_arr(:, :)

    open(u, file="file_06_data.dat", form="unformatted", access="stream", status="old")
    read(u) arr
    close(u)

    do i = 1, 2
        do j = 1, 3
            print *, arr(i, j)
        end do
    end do

    print *, sum(arr)
    if (abs(sum(arr) - 17.4599991) > 1e-6) error stop

    open(u, file="file_06_data.dat", form="unformatted", access="stream", status="old")
    allocate(alloc_arr(2, 3))
    read(u) alloc_arr
    close(u)


    do i = 1, 2
        do j = 1, 3
            print *, alloc_arr(i, j)
        end do
    end do

    if (abs(sum(alloc_arr) - 17.4599991) > 1e-6) error stop

end program
