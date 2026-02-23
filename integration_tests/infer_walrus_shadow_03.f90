program infer_walrus_shadow_03
    implicit none
    x = 42
    if (x /= 42) error stop
    block
        x = 2.718d0
        if (abs(x - 2.718d0) > 1.0d-10) error stop
    end block
    if (x /= 42) error stop
    print *, "PASSED"
end program
