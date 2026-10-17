program system_clock_01
    use iso_fortran_env, only: int64
    implicit none
    integer :: arg = 0
    integer :: count, rate, maximum, i
    integer(int64) :: count8, rate8, maximum8

    call system_clock(arg)
    print *, arg
    if (arg == 0) error stop

    ! COUNT must stay in 0..COUNT_MAX, wrapping instead of overflowing (#15046)
    do i = 1, 5
        call system_clock(count, rate, maximum)
        if (rate <= 0) error stop
        if (maximum /= huge(maximum)) error stop
        if (count < 0 .or. count > maximum) error stop
        call system_clock(count8, rate8, maximum8)
        if (rate8 <= 0_int64) error stop
        if (maximum8 /= huge(maximum8)) error stop
        if (count8 < 0_int64 .or. count8 > maximum8) error stop
    end do
end program system_clock_01
