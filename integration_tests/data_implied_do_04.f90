real function func()
    integer i
    real coef(5,4)
    data (coef(i,1),i=1,5)/1.0,1.0,3*0.0/
    if (abs(coef(1,1) - 1.0) > 1e-8) error stop
    if (abs(coef(2,1) - 1.0) > 1e-8) error stop
    if (abs(coef(3,1) - 0.0) > 1e-8) error stop
    if (abs(coef(4,1) - 0.0) > 1e-8) error stop
    if (abs(coef(5,1) - 0.0) > 1e-8) error stop
    return 
end function

program data_implied_do_04
    real y
    y = func()
end program
