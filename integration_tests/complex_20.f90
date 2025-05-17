program complex_20
    complex(8), parameter :: x = (1.0D0, 2.0D0)

    real(8), parameter :: re = x%re
    real(8), parameter :: ri = x%im

    if (abs(re - 1) > 1e-5) error stop
    if (abs(ri - 2) > 1e-5) error stop
end program
