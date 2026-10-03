program test_sign_modes
    implicit none
    character(len=20) :: s1, s2, s3

    ! Unit 10 → Explicit PLUS
    open(unit=10, file="plus.txt", status="replace", sign="plus", form="formatted")

    ! Unit 20 → Explicit SUPPRESS
    open(unit=20, file="suppress.txt", status="replace", sign="suppress", form="formatted")

    ! Unit 30 → No SIGN specified (default)
    open(unit=30, file="default.txt", status="replace", form="formatted")

    ! Inquire SIGN modes
    inquire(unit=10, sign=s1)
    inquire(unit=20, sign=s2)
    inquire(unit=30, sign=s3)

    if (trim(s1) /= "PLUS") error stop
    if (trim(s2) /= "SUPPRESS") error stop
    if (trim(s3) /= "PROCESSOR_DEFINED") error stop

    ! Write same numbers to all files
    write(10, '(F6.2)') 3.14, -3.14
    write(20, '(F6.2)') 3.14, -3.14
    write(30, '(F6.2)') 3.14, -3.14

    close(10)
    close(20)
    close(30)

    open(unit=11, file="plus.txt", status="old", form="formatted")
    open(unit=21, file="suppress.txt", status="old", form="formatted")
    open(unit=31, file="default.txt", status="old", form="formatted")

    read(11, '(A)') s1
    read(11, '(A)') s2
    if (trim(s1) /= " +3.14") error stop
    if (trim(s2) /= " -3.14") error stop

    read(21, '(A)') s1
    read(21, '(A)') s2
    if (trim(s1) /= "  3.14") error stop
    if (trim(s2) /= " -3.14") error stop

    read(31, '(A)') s1
    read(31, '(A)') s2
    if (trim(s1) /= "  3.14") error stop
    if (trim(s2) /= " -3.14") error stop

end program
