program file_77
    ! A WRITE to a unit that was never opened goes to the file fort.<unit>,
    ! like other compilers do (#3924)
    implicit none
    integer, parameter :: u = 3924
    character(len=*), parameter :: fname = 'fort.3924'
    character(len=32) :: line
    logical :: exist
    integer :: ios, v

    inquire(file=fname, exist=exist)
    if (exist) then
        open(unit=u, file=fname, status='old')
        close(u, status='delete')
    end if

    write(u, "(A)") 'foobar foobar'
    close(u)

    inquire(file=fname, exist=exist)
    if (.not. exist) error stop 1
    open(newunit=v, file=fname, status='old', action='read')
    line = ''
    read(v, "(A)", iostat=ios) line
    if (ios /= 0) error stop 2
    if (trim(line) /= 'foobar foobar') error stop 3
    close(v, status='delete')
    print *, trim(line)
end program file_77
