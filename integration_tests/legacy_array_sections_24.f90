program main
    use reexport_mod
    implicit none
    type(ieee_flag_type) :: flags(2)
    flags = [ieee_invalid, ieee_overflow]
    call ieee_set_halting_mode(flags, .false.)
    print *, "ok"
end program
