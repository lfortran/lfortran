program equivalence_43
    implicit none
    integer :: node(2, 2), node0(2, 2)
    equivalence (node(1, 1), node0(1, 1))
    data node0 / 1, 2, 3, 4 /

    if (node0(1, 1) /= 1) error stop
    if (node0(2, 1) /= 2) error stop
    if (node0(1, 2) /= 3) error stop
    if (node0(2, 2) /= 4) error stop
    if (any(node /= node0)) error stop
end program
