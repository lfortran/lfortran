program equivalence_41
    implicit none
    real :: storage(1, 2), first(1), second(1)
    common /shared/ storage
    equivalence (storage(1, 1), first(1))
    equivalence (storage(1, 2), second(1))

    first = 1.25
    second = 2.5
    if (storage(1, 1) /= 1.25) error stop
    if (storage(1, 2) /= 2.5) error stop

    storage(1, 1) = 3.75
    storage(1, 2) = 4.5
    if (first(1) /= 3.75) error stop
    if (second(1) /= 4.5) error stop
end program
