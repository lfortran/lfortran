program infer_walrus_shadow_02
    implicit none
    x := 10
    if (x /= 10) error stop
    block
        x := 3.14d0
        if (abs(x - 3.14d0) > 1.0d-10) error stop
        block
            x := .true.
            if (.not. x) error stop
        end block
        if (abs(x - 3.14d0) > 1.0d-10) error stop
    end block
    if (x /= 10) error stop
    print *, "PASSED"
end program
