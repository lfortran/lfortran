program read_99
    implicit none
    integer :: unit, i, n
    real :: values(4), a(2), b(2)

    n = 2
    values = [1.0, 10.0, 2.0, 20.0]
    open(newunit=unit, status="scratch", access="direct", &
        form="unformatted", recl=16)
    write(unit, rec=1) values
    read(unit, rec=1) (a(i), b(i), i=1,n)
    close(unit)
    if (any(a /= [1.0, 2.0]) .or. any(b /= [10.0, 20.0])) error stop
end program
