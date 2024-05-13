program write_07
    implicit none

    integer :: stat = 1
    real :: value = 3.14
    character(len=20) :: buffer

    write(buffer, '(F6.2)', iostat=stat) value
    print *, buffer, stat

    if (stat /= 0) error stop

end program
