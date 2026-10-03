program liric_error_stop
    implicit none
    integer :: x
    x = 1
    if (x == 1) then
        error stop
        x = 2
    else
        x = 3
    end if
    x = 4
end program
