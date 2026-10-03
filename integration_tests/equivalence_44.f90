program equivalence_44
    implicit none
    ! Array-array EQUIVALENCE must preserve non-default integer kinds.
    ! Case 1: larger anchor array is used as backing storage (backing swap).
    integer(8) :: a(1), b(2)
    equivalence (a(1), b(1))
    ! Case 2: equal-sized arrays, no backing swap.
    integer(8) :: c(2), d(2)
    equivalence (c(1), d(1))

    b(1) = 4294967297_8
    b(2) = -4294967297_8
    if (a(1) /= 4294967297_8) error stop
    a(1) = 8589934593_8
    if (b(1) /= 8589934593_8) error stop
    if (b(2) /= -4294967297_8) error stop

    d(1) = 4294967297_8
    d(2) = -4294967297_8
    if (c(1) /= 4294967297_8) error stop
    if (c(2) /= -4294967297_8) error stop
    c(1) = 8589934593_8
    if (d(1) /= 8589934593_8) error stop
end program
