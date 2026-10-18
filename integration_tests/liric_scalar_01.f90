program liric_scalar_01
    implicit none
    integer :: x = 3, y = 7
    logical :: take_then = .true.

    if (x /= 3) error stop
    if (take_then) then
        x = x + y
    else
        error stop
    end if
    if (x /= 10) error stop

    take_then = .false.
    if (take_then) then
        error stop
    else
        y = x / y
    end if
    if (y /= 1) error stop
    if (x > y) then
        if (x == 10) then
            x = x - y
        else
            error stop
        end if
    else
        error stop
    end if
    if (x /= 9) error stop
end program
