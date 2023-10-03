subroutine read_array()
    integer :: j
    double precision :: a(1, 2, 3)
    j = 10
    open(unit=j, file="file_08.txt")
    read(j, *) a
    close(j)
    print *, a
    if (abs(a(1, 1, 1) - 1.0D0) > 1e-10) error stop
    if (abs(a(1, 2, 1) - 2.0D0) > 1e-10) error stop
    if (abs(a(1, 1, 2) - 3.0D0) > 1e-10) error stop
    if (abs(a(1, 2, 2) - 4.0D0) > 1e-10) error stop
    if (abs(a(1, 1, 3) - 5.0D0) > 1e-10) error stop
    if (abs(a(1, 2, 3) - 6.0D0) > 1e-10) error stop
end subroutine

program file_08
    call read_array()
end program
