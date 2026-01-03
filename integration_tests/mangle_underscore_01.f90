subroutine add(x, y)
    real :: x, y
    print *, x + y
    if (abs(x + y - 0.7) > 1e-7) error stop
end subroutine

subroutine sub(x,y)
    real :: x, y
    print *, x - y
    if (abs(x - y - (-4.5)) > 1e-7) error stop

end subroutine

program mangle_underscore_01
    call add(-1.9, 2.6)
    call sub(-1.9, 2.6)
end program
