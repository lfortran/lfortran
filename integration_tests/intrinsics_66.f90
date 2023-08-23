program intrinsics_66
    real :: x,y(2)
    x = 1.0
    y = 2.0

    print *, sign(y(1),x)
    print *, sign(y(2),x)
    print *, sign(y(2), -x)
    if (abs(sign(y(1),x) - 2.0) > 1e-9) error stop
    if (abs(sign(y(2),x) - 2.0) > 1e-9) error stop
    if (abs(sign(y(2),-x) - (-2.0)) > 1e-9) error stop
end program
