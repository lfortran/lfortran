program intrinsics_352
    implicit none
    integer :: n
    integer :: seed_to_put(10)
    integer :: seed_to_get(10)
    integer, allocatable :: seed(:)

    call random_seed(size=n)
    allocate(seed(n))
    call random_seed(get=seed)
    seed_to_put = [1,2,3,4,5,6, 7, 8, 9, 10]
    call random_seed(size=n)
    print *, n
    call random_seed(put=seed_to_put)
    print *, seed_to_put
    call random_seed(get=seed_to_get)
    print *, seed_to_get
    call init_random_seed()
    contains
    subroutine init_random_seed()
        INTEGER :: i, n, clock
        INTEGER, DIMENSION(:), ALLOCATABLE :: seed

        CALL RANDOM_SEED(size = n)
        ALLOCATE(seed(n))

        CALL SYSTEM_CLOCK(COUNT=clock)

        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        CALL RANDOM_SEED(put=seed)

        DEALLOCATE(seed)
    end subroutine init_random_seed
end program
