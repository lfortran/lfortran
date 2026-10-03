program equivalence_25
    implicit none

    integer :: i1, i2, i3
    equivalence (i1, i2), (i2, i3)

    i1 = 1
    i2 = 2
    i3 = 3

    if (i1 /= 3) error stop
    if (i2 /= 3) error stop
    if (i3 /= 3) error stop

    i1 = 42
    if (i2 /= 42) error stop
    if (i3 /= 42) error stop
end program
