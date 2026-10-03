program complex_drift
    complex :: c
    integer :: i
    c = (1.0, 2.0)
    i = 42

    if (abs(aimag(c) - 2.0) > 1e-6) error stop
    if (abs(real(c) - 1.0) > 1e-6) error stop
    if (i /= 42) error stop
end program
