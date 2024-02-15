program types_real_to_complex_cast
    complex(8) :: c
    real :: i
    c = 1.0
    i = 1.2
    c = i
    if (abs(c - 1.2) > 1e-6) error stop
end program
