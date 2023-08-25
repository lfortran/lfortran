subroutine add(x, y)
    integer x, y
    x = 4
    y = 5
end subroutine Add

program main
    integer x, y
    call add(x, y)
    if(x /= 4) error stop
    if(y /= 5) error stop
    if(x + y /= 9) error stop
end program Main
