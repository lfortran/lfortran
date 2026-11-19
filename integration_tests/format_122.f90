program format_122
    ! A FORMAT statement whose first item is a parenthesized group
    implicit none
    character(len=80) :: line
    integer :: k, it, j
    real :: r, a(4)

    k = 1
    it = 42
    r = 0.5
    a = [1.0, 2.0, 3.0, 4.0]

    write (line, 100) k, r
100 format((I1), 1x, E13.6)
    print '(a)', trim(line)
    if (trim(line) /= '1  0.500000E+00') error stop

    write (line, 200) r, k
200 format((E13.6), I3)
    print '(a)', trim(line)
    if (trim(line) /= ' 0.500000E+00  1') error stop

    write (line, 300) k, it, r, a
300 format((1x, I1), (1x, I7), (1x, E13.6), 4(1x, E10.3))
    print '(a)', trim(line)
    if (trim(line) /= ' 1      42  0.500000E+00  0.100E+01  0.200E+01' // &
        '  0.300E+01  0.400E+01') error stop

    write (line, 400) k, r
400 format(I1, (E13.6))
    print '(a)', trim(line)
    if (trim(line) /= '1 0.500000E+00') error stop

    write (line, 500) k
500 format((I1))
    print '(a)', trim(line)
    if (trim(line) /= '1') error stop

    write (line, 600) k, it
600 format((I2), 'a)', I3)
    print '(a)', trim(line)
    if (trim(line) /= ' 1a) 42') error stop

    write (line, 700) k, it
700 format(I2, 'a)', I3)
    print '(a)', trim(line)
    if (trim(line) /= ' 1a) 42') error stop

    line = '7  0.250000E+00'
    read (line, 100) j, r
    if (j /= 7) error stop
    if (abs(r - 0.25) > 1e-6) error stop

    print 100, k, 0.5
end program format_122
