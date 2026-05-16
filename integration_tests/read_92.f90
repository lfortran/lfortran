program read_92
    implicit none

    complex :: a(4)
    double complex :: b(3)

    open(10, status="scratch")
    write(10, *) "4*(1.0,2.0)"
    rewind(10)
    read(10, *) a(:)
    close(10)

    if (any(a /= cmplx(1.0, 2.0))) error stop

    open(11, status="scratch")
    write(11, *) "3*(3.0d0,4.0d0)"
    rewind(11)
    read(11, *) b(:)
    close(11)

    if (any(b /= dcmplx(3.0d0, 4.0d0))) error stop
end program read_92