program format_82
    implicit none

    integer :: unit, ios
    character(64) :: line1, line2

    open (newunit=unit, status='scratch', action='readwrite', iostat=ios)
    if (ios /= 0) error stop

    write (unit, 100) 1.0, 2.0, 3.0, 4.0
100 format ('values are: ', 4(f4.1)/'next line')

    rewind(unit)

    read (unit, '(A)', iostat=ios) line1
    if (ios /= 0) error stop

    read (unit, '(A)', iostat=ios) line2
    if (ios /= 0) error stop

    if (index(line1, 'next line') /= 0) error stop
    if (trim(line2) /= 'next line') error stop

    close(unit)
end program format_82