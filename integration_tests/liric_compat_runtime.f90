program liric_compat_runtime
    implicit none
    real(8) :: x, y
    integer :: n

    x = 1.0_8
    y = sinh(x)
    if (.not. (abs(y - 1.1752011936438014_8) < 1.0e-12_8)) error stop
    x = 0.0_8
    y = sinh(x)
    if (.not. (abs(y) < 1.0e-12_8)) error stop

    n = 37
    if (n /= 37) error stop
end program
