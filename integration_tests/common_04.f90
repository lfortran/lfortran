program main
    integer :: a, b, c(5), d
    common / block_1 / a, b, c
    a = 2
    b = 1
    d = b
    c = 4
    c(1) = 2
    b = c(3)
    if (a /= 2) error stop
    if (b /= 4) error stop
    if (c(1) /= 2) error stop
    if (c(2) /= 4) error stop
    if (c(3) /= 4) error stop
    if (c(4) /= 4) error stop
    if (c(5) /= 4) error stop
    if (d /= 1) error stop
    print *, a, b, c, c(5), d
end program