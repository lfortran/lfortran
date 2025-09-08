program test
    complex, parameter :: x(2) = (1.0, 0.3)
    real :: y_re(2), y_im(2)
    
    ! Test extracting real parts
    y_re = x%re
    print *, "Real parts:", y_re
    
    ! Test extracting imaginary parts
    y_im = x%im
    print *, "Imaginary parts:", y_im
    
    ! Assert checks
    if (abs(y_re(1) - 1.0) > 1e-5) error stop "Real part 1 failed"
    if (abs(y_re(2) - 1.0) > 1e-5) error stop "Real part 2 failed"
    if (abs(y_im(1) - 0.3) > 1e-5) error stop "Imaginary part 1 failed"
    if (abs(y_im(2) - 0.3) > 1e-5) error stop "Imaginary part 2 failed"
    
    print *, x%re, x%im
    print *, "All tests passed!"
end