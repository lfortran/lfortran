program random_seed_01
    implicit none
    integer :: sz, i
    call random_seed(size=sz)
    if (sz <= 0) error stop "random_seed size must be positive"
    block
        integer, allocatable :: got0(:)
        allocate(got0(sz))
        ! random_seed(get=...) called before any prior put/bare random_seed()
        ! must not observe the seed buffer's initial all-zero state
        ! (regression test for #12428)
        call random_seed(get=got0)
        block
            logical :: all_zero
            all_zero = .true.
            do i = 1, sz
                if (got0(i) /= 0) all_zero = .false.
            end do
            if (all_zero) error stop "random_seed(get=...) returned all zeros before any prior seed"
        end block
    end block
    block
        integer, allocatable :: vals(:), got(:)
        allocate(vals(sz), got(sz))
        vals = 42
        call random_seed(put=vals)
        call random_seed(get=got)
        do i = 1, sz
            if (got(i) /= vals(i)) error stop "random_seed get did not match put"
        end do

        ! bare `random_seed()` must reseed and refresh the internal state,
        ! not leave it unchanged (regression test for #12428)
        call random_seed()
        call random_seed(get=got)
        block
            logical :: unchanged
            unchanged = .true.
            do i = 1, sz
                if (got(i) /= vals(i)) unchanged = .false.
            end do
            if (unchanged) error stop "random_seed() did not reseed"
        end block
    end block
end program random_seed_01
