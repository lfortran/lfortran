! test "T" format specifier
program format_14
    implicit none

    character(len=30) wr

    write(wr, '(I5, T6, F5.2)') 1, sqrt(real(1))
    if (wr /= "    1 1.00") error stop
    write(wr, '(I5, T6, F5.2)') 2, sqrt(real(2))
    if (wr /= "    2 1.41") error stop

    write(wr, '(I5, T10, F5.2)') 1, sqrt(real(1))
    if (wr /= "    1     1.00") error stop
    write(wr, '(I5, T10, F5.2)') 2, sqrt(real(2))
    if (wr /= "    2     1.41") error stop

    write(wr, '(I4, T5, I4)') 2, 1
    if (wr /= "   2   1") error stop

    write(wr, '(I4, T5, I4, T10, I5)') 2, 1, 1
    print *, wr
    if (wr /= "   2   1     1") error stop

end program format_14
