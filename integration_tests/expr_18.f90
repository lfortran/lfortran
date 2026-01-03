program expr_18
    complex, parameter :: x = 123

    print *, x
    if (abs(x - (123, 0)) > 1e-5) error stop
end program
