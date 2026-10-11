program write_49
    ! write with an iostat= variable of a kind other than the default
    implicit none
    character(5) :: s
    integer(8) :: ios8
    integer(2) :: ios2
    real :: x

    ios8 = -99
    write(s, '(i3)', iostat=ios8) 8
    if (ios8 /= 0) error stop 1
    if (s /= "  8  ") error stop 2
    ios2 = -99
    write(s, '(i3)', iostat=ios2) 9
    if (ios2 /= 0) error stop 3
    if (s /= "  9  ") error stop 4

    ! The item does not match its edit descriptor
    x = 1.5
    ios8 = 0
    write(s, '(i5)', iostat=ios8) x
    if (ios8 == 0) error stop 5

    ios8 = -99
    write(*, '(a)', iostat=ios8) "unit"
    if (ios8 /= 0) error stop 6
end program
