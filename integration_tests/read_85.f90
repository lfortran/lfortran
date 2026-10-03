program read_85
    ! Test '/' (slash) record separator in format when reading from
    ! an internal file (character array). Each array element is one record.
    implicit none
    integer :: a, b, c
    character(len=5) :: arr(3)

    arr(1) = '10   '
    arr(2) = '20   '
    arr(3) = '30   '

    ! Single slash: advance to next record
    read(unit=arr, fmt='(I2,/,I2)') a, b
    if (a /= 10) error stop
    if (b /= 20) error stop

    ! Two slashes: skip a record
    read(unit=arr, fmt='(I2,//,I2)') a, c
    if (a /= 10) error stop
    if (c /= 30) error stop

    ! Format reversion with character array (implicit record advance)
    read(unit=arr, fmt='(I2)') a, b, c
    if (a /= 10) error stop
    if (b /= 20) error stop
    if (c /= 30) error stop

    print *, "PASS"
end program
