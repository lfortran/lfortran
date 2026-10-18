program file_open_12
    implicit none
    integer :: ios
    logical :: opened
    character(len=8) :: buf

    ! FILE= omitted, STATUS='scratch': the runtime supplies the name.
    open(unit=19, status='scratch', iostat=ios)
    if (ios /= 0) error stop 1
    inquire(unit=19, opened=opened, iostat=ios)
    if (ios /= 0) error stop 2
    if (.not. opened) error stop 3
    write(19, '(a)', iostat=ios) 'scratch'
    if (ios /= 0) error stop 4
    rewind(19, iostat=ios)
    if (ios /= 0) error stop 5
    buf = ''
    read(19, '(a)', iostat=ios) buf
    if (ios /= 0) error stop 6
    if (buf /= 'scratch') error stop 7
    close(19, iostat=ios)
    if (ios /= 0) error stop 8
    inquire(unit=19, opened=opened, iostat=ios)
    if (ios /= 0) error stop 9
    if (opened) error stop 10

    ! FILE= omitted and STATUS omitted (default UNKNOWN). Same generated-name path.
    open(unit=21, iostat=ios)
    if (ios /= 0) error stop 11
    inquire(unit=21, opened=opened, iostat=ios)
    if (ios /= 0) error stop 12
    if (.not. opened) error stop 13
    write(21, '(a)', iostat=ios) 'unknown'
    if (ios /= 0) error stop 14
    rewind(21, iostat=ios)
    if (ios /= 0) error stop 15
    buf = ''
    read(21, '(a)', iostat=ios) buf
    if (ios /= 0) error stop 16
    if (buf /= 'unknown') error stop 17
    close(21, status='delete', iostat=ios)
    if (ios /= 0) error stop 18
    inquire(unit=21, opened=opened, iostat=ios)
    if (ios /= 0) error stop 19
    if (opened) error stop 20
end program file_open_12
