program format_13
    implicit none

    character(len=10) :: flc

    write(flc, '(i0)') -2_1
    print *, flc

    if (flc /= "-2") error stop

end program
