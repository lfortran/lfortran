program test_ieee_exceptions_01
    use, intrinsic :: ieee_exceptions
    implicit none
    type(ieee_flag_type), parameter :: out_of_range(2) = &
        [ieee_overflow, ieee_underflow]
    logical :: flags(2)
    logical :: one

    call ieee_set_halting_mode(out_of_range, .false.)
    call ieee_get_flag(out_of_range, flags)
    if (any(flags)) error stop 1
    call ieee_set_flag(ieee_overflow, .false.)
    call ieee_get_flag(ieee_overflow, one)
    if (one) error stop 2
end program
