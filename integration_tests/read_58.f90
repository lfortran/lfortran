program read_58
    implicit none
    integer(1) :: i8
    integer(2) :: i16
    character(len=10) :: s

    s = "42"
    read(s, *) i8
    if (i8 /= 42) error stop
    print *, i8

    s = "-7"
    read(s, *) i8
    if (i8 /= -7) error stop
    print *, i8

    s = "12345"
    read(s, *) i16
    if (i16 /= 12345) error stop
    print *, i16

    s = "-99"
    read(s, *) i16
    if (i16 /= -99) error stop
    print *, i16
end program
