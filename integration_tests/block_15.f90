program block_15
    implicit none
    block
        use block_15_mod, only: x
        if (x /= 42) error stop
    end block
    print *, "ok"
end program
