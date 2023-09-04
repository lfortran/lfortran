program aint_01
    real :: x
    x = 4.23
    print *, aint(x)
    if (abs(aint(x) - 4.0) > 1e-10) error stop

    x = -4.23
    print *, aint(x)
    if (abs(aint(x)  - (-4.0)) > 1e-10) error stop
end program
    
    