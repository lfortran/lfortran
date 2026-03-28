program intrinsics_445
    implicit none
    real :: x, zero
    zero = 0.0
    x = log_gamma(1.0)
    if (abs(x) > 1.0e-6) error stop
    x = log_gamma(2.0)
    if (abs(x) > 1.0e-6) error stop
    x = log_gamma(0.5)
    if (abs(x - 0.5723649) > 1.0e-5) error stop
    x = log_gamma(zero)
    if (x <= huge(x)) error stop
    print *, x
end program intrinsics_445
