! Test: complex*16 parameter with complex(4) literal - kind cast
program complex_29
    implicit none

    complex*16 zero, one, z
    parameter (zero = (0.0E+0, 0.0E+0), one = (1.0E+0, 0.0E+0))

    z = one + zero

    if (dabs(dble(z) - 1.0d0) > 1.0d-12) error stop
    if (dabs(dimag(z)) > 1.0d-12) error stop

    print *, 'PASS'
end program
