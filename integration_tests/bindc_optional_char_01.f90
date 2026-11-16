module bindc_optional_char_01_m
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function check(expected, x) bind(c, name="bindc_optional_char_01_check")
            import :: c_int
            integer(c_int), value :: expected
            type(*), optional, intent(in) :: x(*)
        end function
    end interface
contains
    subroutine receive_internal(expected, x)
        integer(c_int), intent(in) :: expected
        type(*), optional, intent(in) :: x(*)
        if (present(x) .neqv. (expected /= 0_c_int)) error stop 2
    end subroutine

    subroutine forward(expected, s)
        integer(c_int), intent(in) :: expected
        character(len=*), optional, intent(in) :: s
        if (check(expected, s) /= 0_c_int) error stop 1
        call receive_internal(expected, s)
    end subroutine
end module

program bindc_optional_char_01
    use bindc_optional_char_01_m
    implicit none
    character(len=4) :: value

    value = 'ABCD'
    call forward(0_c_int)
    call forward(1_c_int, value)
    call forward(2_c_int, value(3:4))
    call forward(0_c_int)
end program
