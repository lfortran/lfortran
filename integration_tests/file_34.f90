program file_34
    implicit none
    integer :: io, ios
    character(len=:), allocatable :: temp2
    character(len=:), allocatable :: temp

    temp2 = 'HELLO'

    open(newunit=io, file="config.toml", access="stream", form="formatted", &
         status="replace", iostat=ios)
    if (ios /= 0) stop "open-for-write failed"
    write(io, '(a)', iostat=ios) trim(temp2)
    if (ios /= 0) stop "write failed"
    close(io)

    allocate(character(len=5) :: temp)

    open(newunit=io, file="config.toml", access="stream", form="formatted", &
         status="old", iostat=ios)
    if (ios /= 0) stop "open-for-read failed"

    read(io, '(a)') temp

    print *, "First character read =", temp

    close(io, status="delete")
    if (temp /= "HELLO") error stop
end program file_34
