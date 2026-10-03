program format_75
    implicit none
    character(len=32) :: s

    write(s, '(O0.3)') 34
    if (trim(s) /= '042') error stop

    write(s, '(O3)') 34
    if (s(1:3) /= ' 42') error stop
end program format_75
