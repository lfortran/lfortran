program if_08
    ! A statement label on `end if` (end-if-stmt, R1138) is a branch target
    implicit none
    integer :: i

    i = 1
    if (i > 0) then
        i = 2
        go to 86
        i = 3
86  end if
    if (i /= 2) error stop

    if (i == 2) then
        i = 4
    else
        go to 91
        i = 5
91  end if
    if (i /= 4) error stop

    if (i == 0) then
        i = 10
    else if (i == 4) then
        go to 92
        i = 20
    else
        i = 30
92  end if
    if (i /= 4) error stop

    print *, "ok"
end program
