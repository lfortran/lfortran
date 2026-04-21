program format_86
    implicit none

    character(14) :: c1
    character(20) :: c2
    double precision :: d1, d2

    d1 = 123.456d0
    d2 = 789.012d0

    write (c1, '(G14.8)') d1
    if (c1 /= ' 123.45600') error stop

    write (c2, '(G20.2)') d2
    if (c2 /= '            0.79E+03') error stop

    write (c2, '(G20.2E4)') d2
    if (c2 /= '          0.79E+0003') error stop

end program format_86
