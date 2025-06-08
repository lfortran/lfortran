! test "T"/"TL"/"TR" format specifier
program format_14
    implicit none

    character(len=30) wr
    character(len=20) :: hello_god
    hello_god = "Hello God!"

    ! Test "T" format specifier (tab)
    write(wr, '(I5, T6, F5.2)') 1, sqrt(real(1))
    print *, wr
    if (wr /= "    1 1.00") error stop
    write(wr, '(I5, T6, F5.2)') 2, sqrt(real(2))
    print *, wr
    if (wr /= "    2 1.41") error stop

    write(wr, '(I5, T10, F5.2)') 1, sqrt(real(1))
    print *, wr
    if (wr /= "    1     1.00") error stop
    write(wr, '(I5, T10, F5.2)') 2, sqrt(real(2))
    print *, wr
    if (wr /= "    2     1.41") error stop

    write(wr, '(I4, T5, I4)') 2, 1
    print *, wr
    if (wr /= "   2   1") error stop

    write(wr, '(I4, T5, I4, T10, I5)') 2, 1, 1
    print *, wr
    if (wr /= "   2   1     1") error stop

    ! Test "TL" format specifier (tab left)
    write(wr, '(I4, T10, TL2, F5.2)') 5, sqrt(real(5))
    print *, wr
    if (wr /= "   5    2.24") error stop
    write(wr, '(I4, T10, TL3, F5.2)') 6, sqrt(real(6))
    print *, wr
    if (wr /= "   6   2.45") error stop
    write(wr, "(TR7, TL3, A)") hello_god
    print *, wr
    if (wr /= "    Hello God!") error stop

    ! Test "TR" format specifier (tab right)
    write(wr, '(I4, TR2, F5.2)') 3, sqrt(real(3))
    print *, wr
    if (wr /= "   3   1.73") error stop
    write(wr, '(I4, TR4, F5.2)') 4, sqrt(real(4))
    print *, wr
    if (wr /= "   4     2.00") error stop
    write(wr, "(TR5, A)") hello_god
    print *, wr
    if (wr /= "     Hello God!") error stop
end program format_14
