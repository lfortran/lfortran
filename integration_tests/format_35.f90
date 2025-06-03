program format_35
    implicit none
    character fmt*10, line*80, letters*26
    integer i
    letters = 'abcdefghijklmnopqrstuvwxyz'
    fmt = '(99(1X,A))'
    write(line,fmt) (letters(i:i), i=1,26)
    print "(A)", trim(line)
end program format_35
