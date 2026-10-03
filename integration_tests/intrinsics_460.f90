program intrinsics_460
implicit none
    integer :: i, num_seed
    integer, allocatable :: seed(:), seed_check(:)

    call random_seed(size=num_seed)
    allocate(seed(num_seed))
    allocate(seed_check(num_seed))
    seed = [(i, i=1, num_seed)]

    call random_seed(put=seed)
    call random_seed(get=seed_check)

    do i = 1, num_seed
        if (seed(i) /= seed_check(i)) error stop
    end do
end program intrinsics_460
