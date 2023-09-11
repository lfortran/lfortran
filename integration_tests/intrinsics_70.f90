program intrinsics_69
    real :: x
    x = 4.23
    print *, aint(x)
    if (abs(aint(x) - 4.0) > 1e-10) error stop

    x = -4.23
    print *, aint(x)
    if (abs(aint(x)  - (-4.0)) > 1e-10) error stop

    print *, aint(0.0)
    if (abs(aint(0.0) - 0.0) > 1e-10) error stop

    print *, aint(4.23)
    if (abs(aint(4.23) - 4.0) > 1e-10) error stop

    print *, aint(-4.23, 4)
    if (abs(aint(-4.23, 4) - (-4.0)) > 1e-10) error stop
end program
