program format_46
    implicit none
    integer :: i, j
    real :: x
    character(8) :: string1, string2

    string1 = '42'
    read (string1, 100) i
    print 100, i
    if (i /= 42) error stop
100 format (I3)

    string1 = '  123   '
    read (string1, '(I5)') j
    if (j /= 123) error stop
    print *, 'j =', j

    string2 = '3.14159 '
    read (string2, '(F7.5)') x
    if (abs(x - 3.14159) > 0.0001) error stop
    print *, 'x =', x

    string1 = '-99     '
    read (string1, '(I4)') i
    if (i /= -99) error stop
    print *, 'i =', i

end program format_46
