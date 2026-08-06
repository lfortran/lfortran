program read_double_iostat_01
    implicit none
    integer :: unit, status
    double precision :: values(2)

    values = -1.0d0
    open(newunit=unit, status="scratch", form="formatted")
    write(unit, "(A)") "2.0"
    rewind(unit)
    read(unit, *, iostat=status) values
    close(unit)
    if (status >= 0 .or. values(1) /= 2.0d0 .or. values(2) /= -1.0d0) error stop
end program
