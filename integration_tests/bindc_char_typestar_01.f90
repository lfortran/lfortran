module bindc_char_typestar_01_m
    use iso_c_binding, only: c_int
    implicit none
    interface
        function check_chars(buf, expected, n) result(r) bind(c, name='bindc_char_typestar_01_check')
            import :: c_int
            type(*), intent(in) :: buf(*)
            type(*), intent(in) :: expected(*)
            integer(c_int), value :: n
            integer(c_int) :: r
        end function
    end interface
contains
    subroutine check_array_constructor(value)
        character(*), intent(in) :: value
        if (check_chars([value], value, len(value)) /= 0) error stop 3
    end subroutine
end module

program bindc_char_typestar_01
    use bindc_char_typestar_01_m
    implicit none
    character(len=5) :: s
    character(len=3) :: a(2)
    s = "hello"
    a = ["abc", "def"]
    ! scalar character variable
    if (check_chars(s, "hello", 5) /= 0) error stop 1
    ! character array
    if (check_chars(a, "abcdef", 6) /= 0) error stop 2
    ! array constructor of a character variable
    call check_array_constructor("Scalars")
    if (check_chars([s], "hello", 5) /= 0) error stop 4
end program
