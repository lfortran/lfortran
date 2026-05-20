program equivalence_33
    ! Test chained equivalence statements across separate EQUIVALENCE declarations
    implicit none
    integer :: a, b, c, d
    equivalence(a, b)
    equivalence(b, c)
    equivalence(c, d)

    d = 10
    if (a /= 10) error stop
    if (b /= 10) error stop
    if (c /= 10) error stop

    a = 42
    if (b /= 42) error stop
    if (c /= 42) error stop
    if (d /= 42) error stop

    print *, a, b, c, d
end program
