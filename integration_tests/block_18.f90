program block_18
    implicit none
    integer :: x

    data_block: block
        x = 42
    end block data_block

    if (x /= 42) error stop
    print *, "ok"
end program block_18
