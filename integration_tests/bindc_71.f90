module bindc_71_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
    character(kind=c_char), bind(C) :: bindc_71_default
    character(kind=c_char), bind(C, name="bindc_71_named") :: named
    character(kind=c_char), bind(C, name="bindc_71_initialized") :: initialized = 'I'
    integer(c_int), bind(C, name="bindc_71_integer") :: number
end module

program bindc_71
    use iso_c_binding, only: c_int
    use bindc_71_mod
    implicit none
    interface
        integer(c_int) function bindc_71_check() bind(C)
            import :: c_int
        end function
        subroutine bindc_71_fill() bind(C)
        end subroutine
    end interface

    if (bindc_71_default /= 'Q') error stop
    if (named /= 'R') error stop
    if (initialized /= 'I') error stop
    if (number /= 5) error stop

    bindc_71_default = 'A'
    named = 'B'
    if (bindc_71_check() /= 0) error stop

    call bindc_71_fill()
    if (bindc_71_default /= 'X') error stop
    if (named /= 'Y') error stop
    if (initialized /= 'Z') error stop
end program
