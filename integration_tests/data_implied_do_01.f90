program data_implied_do_01
    real :: a(5)
    integer :: i
    DATA (a(i),i=1,5) /1.0, 2.0, 3*0.0/
    print *, a
    if (abs(a(1) - 1.0) > 1e-8) error stop
    if (abs(a(2) - 2.0) > 1e-8) error stop
    if (abs(a(3) - 0.0) > 1e-8) error stop
    if (abs(a(4) - 0.0) > 1e-8) error stop
    if (abs(a(5) - 0.0) > 1e-8) error stop
end program
