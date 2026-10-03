program random_init_02
    implicit none
    integer, parameter :: num_trials = 10
    integer :: i, n, num_differ
    integer, allocatable :: seeds(:), seeds2(:)
    integer :: first_seeds(num_trials)

    call random_seed(size=n)
    allocate(seeds(n), seeds2(n))

    do i = 1, num_trials
        call random_init(.false., .false.)
        call random_seed(get=seeds)
        first_seeds(i) = seeds(1)
    end do

    num_differ = 0
    do i = 2, num_trials
        if (first_seeds(i) /= first_seeds(1)) num_differ = num_differ + 1
    end do
    if (num_differ == 0) error stop "non-repeatable random_init produced identical seeds across all trials"

    call random_init(.true., .false.)
    call random_seed(get=seeds)
    call random_init(.true., .false.)
    call random_seed(get=seeds2)

    do i = 1, n
        if (seeds(i) /= seeds2(i)) error stop "repeatable random_init produced different seeds"
    end do
end program
