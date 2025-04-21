program file_29
    integer :: io, stat
    character(len=31) :: buffer
    character(len=3) :: temp = "no"
    open(newunit=io, status="scratch")
    write(io, "(a)") repeat("abc", 10), repeat("def", 100), repeat("ghi", 1000)
    rewind(io)
    read(io, '(a)', advance="no", iostat=stat) buffer
    if (stat /= -2) error stop
    read(io, '(a)', advance="no", iostat=stat) buffer
    if (stat /= 0) error stop
    rewind(io)
    read(io, '(a)', advance="yes", iostat=stat) buffer
    if (stat /= 0) error stop
    rewind(io)
    read(io, '(a)', advance=temp, iostat=stat) buffer
    if (stat /= -2) error stop
    temp = "yes"
    read(io, '(a)', advance=temp, iostat=stat) buffer
    if (stat /= 0) error stop
    close(io)
end program