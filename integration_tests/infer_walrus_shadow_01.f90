program infer_walrus_shadow_01
    implicit none
    x := 5
    if (x /= 5) error stop
    block
        x := "hello"
        if (x /= "hello") error stop
    end block
    if (x /= 5) error stop
    print *, "PASSED"
end program
