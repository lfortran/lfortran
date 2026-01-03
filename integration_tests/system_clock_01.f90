program system_clock_01
    implicit none
    integer :: arg = 0

    call system_clock(arg)
    print *, arg
    if (arg == 0) error stop
end program system_clock_01
