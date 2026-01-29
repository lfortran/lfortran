program ilp64_stop_01
    implicit none
    integer :: x
    x = 1
    ! Test error stop with literal
    if (x /= 1) error stop 1
    ! Test stop with literal
    ! (only test success path - we just need the compilation to succeed)
    print *, "PASS"
end program
