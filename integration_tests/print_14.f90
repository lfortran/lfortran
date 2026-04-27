program print_14
    implicit none
    integer :: i
    integer(1) :: i1
    integer(2) :: i2
    integer(4) :: i4
    integer(8) :: i8
    character(len=30) :: buf

    i = 2
    write(buf, *) i
    if (len_trim(buf) /= 12) error stop
    if (trim(adjustl(buf)) /= "2") error stop

    i4 = -12345
    write(buf, *) i4
    if (len_trim(buf) /= 12) error stop
    if (trim(adjustl(buf)) /= "-12345") error stop

    i1 = 2
    write(buf, *) i1
    if (len_trim(buf) /= 5) error stop

    i2 = 2
    write(buf, *) i2
    if (len_trim(buf) /= 7) error stop

    i8 = 2
    write(buf, *) i8
    if (len_trim(buf) /= 21) error stop

    print *, "PASS"
end program
