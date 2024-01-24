program intrinsics_74
    implicit none

    real :: x(1, 2)
    x = 0
    call srand(0)
    call random_number(x(1, 2))
    print *, x

    if (abs(x(1, 1) - 0.0) > 1e-5) error stop
    if (abs(x(1, 2) - 0.0) < 1e-5) error stop
end program
