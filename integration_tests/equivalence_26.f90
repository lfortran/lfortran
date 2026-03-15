program equivalence_26
    implicit none

    integer :: i1
    common // i1
    integer :: i2, i3, i4

    equivalence (i1, i2, i3, i4)

    i1 = 3
    i2 = 2
    i3 = 3

    if (i1 /= 3) error stop
    if (i2 /= 3) error stop

    i1 = 1

    if (i3 /= 1) error stop

    i4 = i1 + i2 + i3

    if (i4 /= 3) error stop

end program
