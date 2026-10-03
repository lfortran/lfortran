program format_107
    implicit none
    integer :: unit
    character(3) :: line
    character(2) :: quoted_dollar

    open(newunit=unit, status="scratch", action="readwrite")
    write(unit, 10) "a"
    write(unit, "(a,$)") "b"
    write(unit, "(a)") "c"
    write(unit, '("$",a)') "d"
    rewind(unit)
    read(unit, "(a)") line
    read(unit, "(a)") quoted_dollar
    close(unit)

    print *, line, quoted_dollar
    if (line /= "abc") error stop
    if (quoted_dollar /= "$d") error stop

    print 10, "ok"
10  format(a,$)
end program format_107
