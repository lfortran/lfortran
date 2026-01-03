program precision_01
    double precision :: x,x1, x2
    x1 = 1.234D0
    x2 = 4.567D0
    x = 1.0 - x1 / x2
    print *, x
    if (abs(x - 0.72980074447120646D0) > 1d-10) error stop
end program
