program random_number_01
    ! Verify that random_number() produces a deterministic, repeatable
    ! sequence when seeded with the same value.
    implicit none
    integer :: n
    integer, allocatable :: seed(:)
    real :: x1, x2, y1, y2

    call random_seed(size=n)
    allocate(seed(n))
    seed = 42

    ! Seed and generate two numbers
    call random_seed(put=seed)
    call random_number(x1)
    call random_number(x2)

    ! Re-seed with the same value and generate again
    call random_seed(put=seed)
    call random_number(y1)
    call random_number(y2)

    ! Sequences from the same seed must be identical
    if (abs(x1 - y1) > 1e-8) error stop
    if (abs(x2 - y2) > 1e-8) error stop

    ! Values must be in [0, 1)
    if (x1 < 0.0 .or. x1 >= 1.0) error stop
    if (x2 < 0.0 .or. x2 >= 1.0) error stop

    ! Consecutive values should differ
    if (abs(x1 - x2) < 1e-8) error stop
end program
