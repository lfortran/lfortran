program format_123
    ! Parentheses inside a character string edit descriptor do not close or
    ! open a format group
    implicit none
    character(len=40) :: line

    write (line, "(I2, 'a)', I3)") 1, 42
    print '(a)', trim(line)
    if (trim(line) /= ' 1a) 42') error stop

    write (line, '(I2, "a)", I3)') 1, 42
    print '(a)', trim(line)
    if (trim(line) /= ' 1a) 42') error stop

    write (line, "(I2, '(a', I3)") 1, 42
    print '(a)', trim(line)
    if (trim(line) /= ' 1(a 42') error stop

    write (line, "(I2, 'it''s)', I3)") 1, 42
    print '(a)', trim(line)
    if (trim(line) /= " 1it's) 42") error stop

    write (line, "((I2), ')', I3)") 1, 42
    print '(a)', trim(line)
    if (trim(line) /= ' 1) 42') error stop

    write (line, "(I2, 'a)', 2(I3, ')'))") 1, 42, 43
    print '(a)', trim(line)
    if (trim(line) /= ' 1a) 42) 43)') error stop

    print "(I2, 'a)', I3)", 1, 42
end program format_123
