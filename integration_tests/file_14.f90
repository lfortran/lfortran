program file_14
    implicit none
    real, pointer :: line(:)
    integer :: u = 10
    open(u, file='file_08.txt', action='read')
    allocate(line(4))
    read(u, *) line
    close(u)

    if (line(1) /= 1.) error stop
    if (line(2) /= 2.) error stop
    if (line(3) /= 3.) error stop
    if (line(4) /= 4.) error stop
end program file_14
