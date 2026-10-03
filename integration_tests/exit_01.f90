program exit_01
    implicit none
    integer :: x

    x = 5
    myif: if (x > 0) then
        if (x == 5) exit myif
        x = 10
    end if myif
    if (x /= 5) error stop

    x = 3
    outer: if (x > 0) then
        inner: if (x > 2) then
            exit outer
            x = 99
        end if inner
        x = 77
    end if outer
    if (x /= 3) error stop
end program
