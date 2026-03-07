program equivalence_24
    implicit none

    equivalence (is1, ia1)
    integer :: ia1(3), is1
    common /BLK9/ is1

    ia1 = [3, 2, 1]
    if (is1 /= 3) error stop

    is1 = 42
    if (ia1(1) /= 42) error stop
end program
