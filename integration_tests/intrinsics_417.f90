program intrinsics_417
    implicit none
    integer :: t1, t2, rate
    call system_clock(t1, rate)
    call sleep(0)
    call system_clock(t2)
    if (t2 < t1) error stop
end program intrinsics_417