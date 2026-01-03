program file_09
    implicit none

    integer :: u = 11, i, j
    character :: arr(2, 2)
    character, allocatable :: alloc_arr(:, :)

    print *, "1"
    open(u, file="file_01_data.txt", form="unformatted", access="stream", status="old")
    read(u) arr
    close(u)

    if (arr(1, 1) /= '1' .or. arr(1, 2) /= '1') error stop
    print *, arr

    open(u, file="file_01_data.txt", form="unformatted", access="stream", status="old")
    allocate(alloc_arr(2, 2))
    read(u) alloc_arr
    close(u)

    if (alloc_arr(2, 1) /= '0' .or. alloc_arr(2, 2) /= '3') error stop
    print *, alloc_arr
end program file_09
