program complex_08
    implicit none
    complex :: x = conjg((0, 0))
    complex :: y = conjg((2.0, -5.5))
    complex :: z = conjg((42, 3.14))

    if (abs(x - (0, 0)) > 1e-5) error stop
    if (abs(y - (2.0, 5.5)) > 1e-5) error stop
    if (abs(z - (42, -3.14)) > 1e-5) error stop

    print *, x, y, z
end program
