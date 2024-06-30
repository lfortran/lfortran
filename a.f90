program intrinsics_74
    implicit none

    real :: x(1, 3)
    x = 0
    call random_init(.true., .true.)
    call random_number(x(1, 2))
    call random_init(.true., .true.)
    call random_number(x(1, 3))
    print *, x

    if (abs(x(1, 1) - 0.0) > 1e-5) error stop
    if (abs(x(1, 2) - 0.0) < 1e-8) error stop
    ! The two values must be the same, due to `random_init()` above
    if (x(1, 2) /= x(1, 3)) error stop
end program