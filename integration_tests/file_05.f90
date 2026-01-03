program file_05
    implicit none

    integer :: u = 11, i, j
    integer :: arr(2, 3)
    integer, allocatable :: alloc_arr(:, :)

    open(u, file="file_05_data.dat", form="unformatted", access="stream", status="old")
    read(u) arr
    close(u)

    do i = 1, 2
        do j = 1, 3
            print *, arr(i, j)
        end do
    end do

    print *, sum(arr)
    if (sum(arr) /= 8) error stop

    open(u, file="file_05_data.dat", form="unformatted", access="stream", status="old")
    allocate(alloc_arr(2, 3))
    read(u) alloc_arr
    close(u)


    do i = 1, 2
        do j = 1, 3
            print *, alloc_arr(i, j)
        end do
    end do

    if (sum(alloc_arr) /= 8) error stop

end program
