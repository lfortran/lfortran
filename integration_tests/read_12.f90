program read_12
    implicit none
    complex :: z4
    complex(8) :: z8

    open(10, file='_read_12_test.dat', status='replace')
    write(10, *) (1.0, 2.0)
    write(10, *) (3.0d0, 4.0d0)
    close(10)

    open(10, file='_read_12_test.dat', status='old')
    read(10, *) z4
    read(10, *) z8
    close(10, status='delete')

    if (abs(real(z4) - 1.0) > 1e-5) error stop
    if (abs(aimag(z4) - 2.0) > 1e-5) error stop

    if (abs(real(z8) - 3.0d0) > 1d-10) error stop
    if (abs(aimag(z8) - 4.0d0) > 1d-10) error stop

    print *, "OK"
end program
