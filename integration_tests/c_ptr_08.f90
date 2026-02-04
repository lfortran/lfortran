program c_ptr_08
    use iso_c_binding
    implicit none

    character(kind=c_char, len=*), parameter :: hex_str = "00FF" // c_null_char
    integer(c_long) :: result

    result = c_strtol(hex_str)
    print *, "Result:", result
    if (result /= 255_c_long) error stop

contains

    integer(c_long) function c_strtol(string)
        use iso_c_binding
        character(kind=c_char, len=*), intent(in), target :: string

        interface
            integer(c_long) function strtol(str, endptr, base) bind(C, name="strtol")
                use iso_c_binding
                type(c_ptr), intent(in), value :: str
                type(c_ptr), intent(inout) :: endptr
                integer(c_int), intent(in), value :: base
            end function strtol
        end interface

        type(c_ptr) :: str1, strend

        str1   = c_loc(string)
        strend = c_null_ptr

        c_strtol = strtol(str1, strend, 16_c_int)
    end function c_strtol

end program c_ptr_08
