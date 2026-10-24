program file_74
    ! CLOSE of a named file opened on a pre-connected unit number closes the
    ! file: its data is written, and STATUS='DELETE' can remove it.
    implicit none
    integer :: ios, u
    character(len=16) :: line
    do u = 5, 6
        open(unit=u, file='file_74.dat', status='replace', action='write')
        write(u, '(a)') 'unit payload'
        close(u, iostat=ios)
        if (ios /= 0) error stop 'close failed'
        open(unit=10, file='file_74.dat', status='old', action='read')
        read(10, '(a)', iostat=ios) line
        if (ios /= 0) error stop 'closed file data not written'
        if (trim(line) /= 'unit payload') error stop 'wrong file contents'
        close(10)
    end do
    open(unit=5, file='file_74.dat', status='old')
    close(5, status='delete', iostat=ios)
    if (ios /= 0) error stop 'delete failed'
    open(unit=10, file='file_74.dat', status='old', iostat=ios)
    if (ios == 0) error stop 'file not deleted'
end program
