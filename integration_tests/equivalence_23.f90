program equivalence_23
    implicit none
    integer :: i1(2,2), i2(2,2), i3(2,2)
    equivalence (i1(2,2), i2(2,2), i3(2,2))

    i1 = 1
    i2 = 2
    i3 = 3

    if (any(i1 /= 3)) error stop
    if (any(i2 /= 3)) error stop
    if (any(i3 /= 3)) error stop

    i1 = 1
    if (any(i3 /= 1)) error stop
end program
