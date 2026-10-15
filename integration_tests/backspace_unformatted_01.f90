program backspace_unformatted_01
    ! BACKSPACE steps back one record in an unformatted sequential file
    implicit none
    character :: c
    integer :: ios

    open(10, file='backspace_unformatted_01.dat', form='unformatted', status='replace')
    write(10) 'a'
    write(10) 'b'
    write(10) 'c'
    backspace 10
    backspace 10
    read(10) c
    if (c /= 'b') error stop
    read(10) c
    if (c /= 'c') error stop
    read(10, iostat=ios) c
    if (ios >= 0) error stop
    backspace 10
    read(10, iostat=ios) c
    if (ios >= 0) error stop
    backspace 10
    endfile 10
    backspace 10
    backspace 10
    read(10) c
    if (c /= 'c') error stop
    close(10, status='delete')
    print *, "PASS"
end program backspace_unformatted_01
