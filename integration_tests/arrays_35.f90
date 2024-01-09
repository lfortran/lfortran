program arrays_35
    real :: x(2)
    x = [real :: 9, 2.1]
    print *, x
    if (abs(x(1) - 9.0) > 1e-7) error stop
    if (abs(x(2) - 2.1) > 1e-7) error stop
end program
