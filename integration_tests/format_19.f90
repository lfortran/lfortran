program format_19
    implicit none

    real(4) :: x4
    character(len=32) :: buffer

    x4 = 9.33264777E-43

    write(buffer, "(ES0.8)") x4
    print *, buffer
    if (buffer /= "9.33264777E-43") error stop "Incorrect formatting of x4"

    write(buffer, "(ES0.8)") tiny(0.0)
    print *, buffer
    if (buffer /= "1.17549435E-38") error stop "Incorrect formatting of tiny(0.0)"

    write(buffer, "(ES0.8)") huge(0.0)
    print *, buffer
    if (buffer /= "3.40282347E+38") error stop "Incorrect formatting of huge(0.0)"
end program
