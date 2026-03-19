program format_71
    implicit none
    real :: f1, f2, f3, f4, f5
    integer :: i1, i2

    open(10, file='format_69.dat', status='replace', form='formatted')
    write(10, '(a)') '7.77-0.747E-02  +0.549E022'
    write(10, '(a)') '+0.662E-00  0.468-1011'
    rewind(10)

    i2 = -42
    read(10, '(F4.2, (2(E10.3)), I2)') f1, f2, f3, i1, f4, f5, i2
    close(10)

    if (abs(f1 - 7.77) > 1.0e-4) error stop "f1 mismatch"
    if (abs(f2 + 0.747e-2) > 1.0e-4) error stop "f2 mismatch"
    if (abs(f3 - 0.549) > 1.0e-4) error stop "f3 mismatch"
    if (abs(f4 - 0.662) > 1.0e-4) error stop "f4 mismatch"
    if (abs(f5 - 0.468e-10) > 1.0e-14) error stop "f5 mismatch"
    if (i1 /= 22) error stop "i1 mismatch"
    if (i2 /= 11) error stop "i2 mismatch"
end program format_71