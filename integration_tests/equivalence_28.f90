program equivalence_28
    implicit none
    integer :: ia1(4), ia2(4)
    equivalence (ia1, ia2(3))

    ia1 = [11, 12, 13, 14]
    ia2 = [ 1,  2,  3,  4]
    if (.not. all(ia1 == [3, 4, 13, 14])) error stop
    if (.not. all(ia2 == [1, 2,  3,  4])) error stop
end program equivalence_28