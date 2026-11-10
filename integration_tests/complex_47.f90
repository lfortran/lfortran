program complex_47
    implicit none
    complex :: z, za(3)
    complex(8) :: zd
    z = (1.0, 1.0)
    z%im = 2.0
    z%re = 3.0
    zd = (1.0d0, 1.0d0)
    zd%im = 5.0d0
    za = (0.0, 0.0)
    za(2)%im = 4.0
    za(3)%re = 6.0
    print *, real(z), aimag(z), real(zd), aimag(zd)
    if (abs(real(z) - 3.0) > 1e-6 .or. abs(aimag(z) - 2.0) > 1e-6) error stop
    if (abs(real(zd) - 1.0d0) > 1d-12 .or. abs(aimag(zd) - 5.0d0) > 1d-12) error stop
    if (abs(aimag(za(2)) - 4.0) > 1e-6 .or. abs(real(za(2))) > 1e-6) error stop
    if (abs(real(za(3)) - 6.0) > 1e-6 .or. abs(aimag(za(3))) > 1e-6) error stop
end program
