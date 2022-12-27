program debug
    implicit none
    integer, parameter :: a = 10, b = -20, c = 30
    integer, parameter :: d = -40, e = 0, f = 50
    if (min(a, b, c, d, e, f) /= -40) error stop
    if (max(a, b, c, d, e, f) /= 50) error stop
    if (max(a, b) /= 10) error stop
    if (min(a, b) /= -20) error stop
    if (max(a, b, c) /= 30) error stop
end program
