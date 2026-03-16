program infer_walrus_01
    implicit none
    call test()
contains
    subroutine test()
        x := 42
        y := 3.14d0
        z := (1.0d0, 2.0d0)
        flag := .true.
        s := "hello"
        if (x /= 42) error stop
        if (abs(y - 3.14d0) > 1.0d-10) error stop
        if (abs(real(z) - 1.0d0) > 1.0d-10) error stop
        if (abs(aimag(z) - 2.0d0) > 1.0d-10) error stop
        if (.not. flag) error stop
        if (s /= "hello") error stop
        x = 100
        if (x /= 100) error stop
        print *, "PASSED"
    end subroutine
end program
