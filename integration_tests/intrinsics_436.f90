program intrinsics_436
    implicit none
    integer, parameter :: seed = 86456
    real :: x, y

    call srand(seed)
    x = rand()
    if (x < 0.0 .or. x > 1.0) error stop
    y = rand()
    if (y < 0.0 .or. y > 1.0) error stop
    if (abs(x - y) < 1e-10) error stop

    call srand(seed)
    if (abs(rand() - x) > 1e-6) error stop
    if (abs(rand() - y) > 1e-6) error stop

    x = rand(seed)
    y = rand()
    call srand(seed)
    if (abs(rand() - x) > 1e-6) error stop
    if (abs(rand() - y) > 1e-6) error stop
end program intrinsics_436
