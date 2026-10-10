program legacy_array_sections_24
    use legacy_array_sections_24_m
    implicit none
    type(ieee_flag_type) :: flags(2)
    flags = [ieee_invalid, ieee_overflow]
    call ieee_set_halting_mode(flags, .false.)
end program legacy_array_sections_24