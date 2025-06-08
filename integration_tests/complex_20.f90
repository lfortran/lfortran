program complex_20
    complex(8), parameter :: x = (1.129D0, -2.12D0)

    real(8), parameter :: re = x%re
    real(8), parameter :: ri = x%im

    if (abs(re - 1.129D0) > 1e-15) error stop
    if (abs(ri - (-2.12D0)) > 1e-15) error stop
end program
