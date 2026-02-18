program format_64
    implicit none
    character(40) :: line

    write(line, '(2I5, 1X, E10.5, BN, 2I5, F6.1)') 1, 2, 3.0, 4, 5, 6.0
    if (line(1:5)   /= '    1') error stop
    if (line(6:10)  /= '    2') error stop
    if (line(11:11) /= ' ')     error stop
    if (line(12:21) /= '.30000E+01') error stop
    if (line(22:26) /= '    4') error stop
    if (line(27:31) /= '    5') error stop
    if (line(32:37) /= '   6.0') error stop

    write(line, '(I5, BZ, I5, F6.1)') 10, 20, 3.5
    if (line(1:5)   /= '   10') error stop
    if (line(6:10)  /= '   20') error stop
    if (line(11:16) /= '   3.5') error stop

    print *, "All BN/BZ format tests passed."
end program format_64
