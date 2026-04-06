program format_83
    implicit none
    character(len=5) :: a, b, pad
    integer :: ios

    ! Create file with only ONE word
    open(10, file='format_83_data.txt', status='replace')
    write(10, '(A)') 'HELLO'
    close(10)

    a = "-----"
    b = "-----"

    open(10, file='format_83_data.txt', pad='yes')
    read(10, '(A5, A5)', iostat=ios) a, b
    inquire(10, pad=pad)

    if (pad /= "YES") error stop
    if (ios /= 0) error stop
    if (b /= "     ") error stop
    close(10)

    a = "-----"
    b = "-----"

    open(10, file='format_83_data.txt', pad='no')
    read(10, '(A5, A5)', iostat=ios) a, b
    inquire(10, pad=pad)
    if (pad /= "NO") error stop
    if (ios == 0) error stop
    if (b /= "-----") error stop
    close(10)

end program format_83

