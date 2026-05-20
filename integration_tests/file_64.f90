program file_64
    ! Test: unformatted WRITE/READ on a preconnected (implicitly opened) unit.
    ! Per the Fortran standard, writing to a unit without an explicit OPEN
    ! should implicitly connect the unit with the format matching the I/O stmt.
    implicit none
    integer :: ival

    ival = 42

    ! Unformatted write to unit 8 (no explicit OPEN)
    write(8) ival
    rewind 8
    ival = 0
    read(8) ival

    if (ival /= 42) error stop

    close(8, status="delete")
    print *, "PASSED"
end program
