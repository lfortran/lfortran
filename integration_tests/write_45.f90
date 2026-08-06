program write_45
    implicit none
    integer :: unit
    complex :: source(2, 3), actual(3)

    source(1, :) = [(1.0, -1.0), (2.0, -2.0), (3.0, -3.0)]
    source(2, :) = [(4.0, -4.0), (5.0, -5.0), (6.0, -6.0)]
    open(newunit=unit, status="scratch", access="direct", &
        form="unformatted", recl=24)
    write(unit, rec=1) source(2, :)
    read(unit, rec=1) actual
    close(unit)
    if (any(actual /= source(2, :))) error stop
end program
