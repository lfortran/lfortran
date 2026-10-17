program rewind_03
    ! An unformatted sequential WRITE goes at the current position and ends the file there
    implicit none
    integer :: k, ios

    open(10, file='rewind_03.dat', form='unformatted', status='replace')
    write(10) 1
    write(10) 2
    rewind 10
    write(10) 99
    rewind 10
    read(10) k
    if (k /= 99) error stop
    read(10, iostat=ios) k
    if (ios >= 0) error stop

    ! After end of file, BACKSPACE then WRITE adds a record at the end
    backspace 10
    write(10) 5
    rewind 10
    read(10) k
    if (k /= 99) error stop
    read(10) k
    if (k /= 5) error stop
    read(10, iostat=ios) k
    if (ios >= 0) error stop
    close(10, status='delete')
    print *, "PASS"
end program rewind_03
