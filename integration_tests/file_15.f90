program file_15
    implicit none
    integer :: items(10), u = 10, i, n

    open(u, file='file_03_data.txt', action='read')

    read(u, *)
    do i = 1, 10
        read(u, *) n
        items(i) = n
    end do

    close(u)

    if (items(2) /= 102) error stop
    if (sum(items) /= 1055) error stop
end program file_15
