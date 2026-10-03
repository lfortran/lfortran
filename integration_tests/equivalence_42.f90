program equivalence_42
    implicit none
    integer :: storage(1)
    integer :: values(1, 2)
    equivalence (storage(1), values(1, 1))
    data values / 1, 0 /

    if (storage(1) /= 1) error stop
    if (values(1, 1) /= 1) error stop
    if (values(1, 2) /= 0) error stop
end program
