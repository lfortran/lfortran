program read_30
    implicit none

    integer :: io_w, io_r, stat
    integer :: filesize
    character(len=:), allocatable :: buffer

    character(len=*), parameter :: toml_text = &
        'name = "core_lib"' // new_line('a')

    open(newunit=io_w, file="config.toml", access="stream", &
         form="unformatted", status="replace", iostat=stat)
    if (stat /= 0) error stop "open(write) failed"

    write(io_w, iostat=stat) toml_text
    if (stat /= 0) error stop "write failed"

    close(io_w)

    open(newunit=io_r, file="config.toml", access="stream", &
         form="unformatted", position="append", status="old", iostat=stat)
    if (stat /= 0) error stop "open(read) failed"

    inquire(unit=io_r, size=filesize)
    if (filesize <= 1) error stop "config.toml too small"

    allocate(character(len=filesize) :: buffer)
    buffer(filesize:filesize) = "?"

    read(io_r, pos=1, iostat=stat) buffer(:filesize-1)
    if (stat /= 0) error stop "stream read failed"

    close(io_r, status='delete')

    if (index(buffer, 'name') == 0) then
        error stop "expected key 'name' not found"
    end if

    if (buffer(filesize:filesize) /= "?") then
        error stop "buffer overflow detected"
    end if

    print *, "CONFIG CONTENT:"
    print *, trim(buffer)
    if (trim(buffer) /= 'name = "core_lib"?') error stop "config content mismatch"

end program read_30
