program volatile_04
    ! VOLATILE attribute statement (F2018 C868), as opposed to the
    ! inline `type, volatile ::` attribute covered by volatile_01..03
    implicit none
    real :: x
    integer :: n, m
    volatile :: x
    volatile :: n, m

    x = 1.0
    n = 42
    m = 7

    x = x + 1.0
    n = n + 1
    m = m + 1

    if (abs(x - 2.0) > 1e-6) error stop
    if (n /= 43) error stop
    if (m /= 8) error stop

    print *, x, n, m
end program volatile_04
