program random_seed_01
    implicit none
    integer :: sz
    call random_seed(size=sz)
    if (sz <= 0) error stop "random_seed size must be positive"
    block
        integer :: vals(sz)
        vals = 42
        call random_seed(put=vals)
        call random_seed(get=vals)
    end block
end program random_seed_01
