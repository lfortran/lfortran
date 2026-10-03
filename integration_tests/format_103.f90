program format_103
    implicit none
    real :: r1
    double precision :: d1
    character(14) :: c14

    r1 = 77.123
    write(c14, '(g14.4e4)') r1
    if (c14 /= '   77.12      ') error stop

    d1 = 77.123d0
    write(c14, '(g14.4e4)') d1
    if (c14 /= '   77.12      ') error stop

    r1 = 1214635.1
    write(c14, '(g14.4e4)') r1
    if (c14 /= '  0.1215E+0007') error stop

    d1 = 1214635.1d0
    write(c14, '(g14.4e4)') d1
    if (c14 /= '  0.1215E+0007') error stop
end program format_103