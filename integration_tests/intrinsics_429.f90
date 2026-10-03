program intrinsics_429
    ! Test: random_seed(put=...) must reset the RNG to the same state
    implicit none
    real :: a, b, c
    integer :: seed_size
    integer, allocatable :: seed(:)

    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed = 12345

    call random_seed(put=seed)
    call random_number(a)

    call random_seed(put=seed)
    call random_number(b)

    call random_seed(put=seed)
    call random_number(c)

    if (a /= b) error stop
    if (b /= c) error stop
end program
