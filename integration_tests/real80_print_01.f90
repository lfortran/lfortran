program real80_print_01
    ! Formatted output of real(10) values: literals, values computed at run
    ! time, array elements (element stride) and items that follow a real(10)
    ! item in the same output list (#17031, #13935). All values are exactly
    ! representable in every real kind.
    implicit none
    real(10) :: x, y, z, a(3)
    character(len=60) :: s
    integer :: i

    x = 1.5_10
    i = 3
    y = x * i
    z = -y / 8
    a(1) = 1.0_10
    a(2) = i - 1
    a(3) = x * 2

    write(s, '(f8.3)') x
    if (s /= '   1.500') error stop 1
    write(s, '(f8.3)') y
    if (s /= '   4.500') error stop 2
    write(s, '(f10.5)') z
    if (s /= '  -0.56250') error stop 3
    write(s, '(f8.3)') 2.25_10
    if (s /= '   2.250') error stop 4
    write(s, '(es12.4)') y
    if (s /= '  4.5000E+00') error stop 5
    write(s, '(e12.4)') x
    if (s /= '  0.1500E+01') error stop 6
    write(s, '(3f6.2)') a
    if (s /= '  1.00  2.00  3.00') error stop 7
    write(s, '(f6.2)') a(3)
    if (s /= '  3.00') error stop 8
    write(s, '(f6.2,i3,f6.2,a2)') x, i, y, 'ok'
    if (s /= '  1.50  3  4.50ok') error stop 9
    write(s, '(2f6.2)') a(2:3)
    if (s /= '  2.00  3.00') error stop 10
    print '(a)', 'ok'
end program
