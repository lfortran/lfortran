program format_111
    ! Decimal edit mode: the DECIMAL= specifier on OPEN and on a data
    ! transfer statement, and the DC/DP control edit descriptors.
    implicit none
    character(len=40) :: s
    character(len=20) :: dmode
    real :: x, y
    complex :: z

    ! DC and DP switch the mode from where they appear in the format
    write(s, '(F8.3,1X,DC,F8.3,1X,DP,F8.3)') 1.5, 2.5, 3.5
    if (s /= "   1.500    2,500    3.500") error stop "dc/dp in format"

    ! DECIMAL= on the data transfer statement (internal file)
    write(s, '(F8.3)', decimal="comma") 1.5
    if (s /= "   1,500") error stop "statement decimal=comma"
    write(s, '(F8.3)', decimal="POINT") 1.5
    if (s /= "   1.500") error stop "statement decimal=point"

    ! every real edit descriptor honours the mode
    write(s, '(E12.4)', decimal="comma") 1.5
    if (s /= "  0,1500E+01") error stop "E descriptor"
    write(s, '(ES12.4)', decimal="comma") 1.5
    if (s /= "  1,5000E+00") error stop "ES descriptor"
    write(s, '(EN12.4)', decimal="comma") 1.5
    if (s /= "  1,5000E+00") error stop "EN descriptor"
    write(s, '(D12.4)', decimal="comma") 1.5d0
    if (s /= "  0,1500D+01") error stop "D descriptor"
    write(s, '(G12.4)', decimal="comma") 1.5
    if (s /= "   1,500    ") error stop "G descriptor"

    ! literal text inside a format is not affected
    write(s, '("a.b",F6.2)', decimal="comma") 2.25
    if (s /= "a.b  2,25") error stop "literal in format"

    ! list directed output uses a comma decimal, and a semicolon between
    ! the two parts of a complex value
    write(s, *, decimal="comma") 1.5
    if (index(s, ",") == 0 .or. index(s, ".") /= 0) error stop "list directed real"
    z = (1.5, 2.5)
    write(s, *, decimal="comma") z
    if (index(s, ";") == 0 .or. index(s, ".") /= 0) error stop "list directed complex"

    ! the connection's mode comes from OPEN, a statement level DECIMAL=
    ! overrides it for that statement only
    open(10, file="format_111_data.txt", status="replace", decimal="comma")
    inquire(10, decimal=dmode)
    if (dmode /= "COMMA") error stop "inquire decimal"
    write(10, '(F8.3)') 1.5
    write(10, '(F8.3)', decimal="point") 1.5
    write(10, '(F8.3)') 1.5
    close(10)

    open(10, file="format_111_data.txt", status="old")
    read(10, '(A)') s
    if (s /= "   1,500") error stop "open decimal=comma"
    read(10, '(A)') s
    if (s /= "   1.500") error stop "statement overrides connection"
    read(10, '(A)') s
    if (s /= "   1,500") error stop "statement mode leaked"
    close(10)

    ! reading a comma decimal back
    open(10, file="format_111_data.txt", status="old", decimal="comma")
    read(10, '(F8.3)') x
    if (abs(x - 1.5) > 1.0e-6) error stop "formatted read"
    close(10)

    open(10, file="format_111_data.txt", status="old", decimal="comma")
    read(10, *) x
    if (abs(x - 1.5) > 1.0e-6) error stop "list directed read"
    close(10)

    open(10, file="format_111_data.txt", status="old")
    read(10, *, decimal="comma") x
    if (abs(x - 1.5) > 1.0e-6) error stop "list directed read, statement decimal"
    close(10, status="delete")

    ! internal file list directed input: a comma decimal and a semicolon
    ! value separator
    s = "3,5 4,5"
    read(s, *, decimal="comma") x, y
    if (abs(x - 3.5) > 1.0e-6 .or. abs(y - 4.5) > 1.0e-6) error stop "internal list read"
    s = "1,5;2,5"
    read(s, *, decimal="comma") x, y
    if (abs(x - 1.5) > 1.0e-6 .or. abs(y - 2.5) > 1.0e-6) error stop "semicolon separator"
    s = "(1,5;2,5)"
    read(s, *, decimal="comma") z
    if (abs(real(z) - 1.5) > 1.0e-6 .or. abs(aimag(z) - 2.5) > 1.0e-6) then
        error stop "internal complex read"
    end if
end program format_111
