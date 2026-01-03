module string_19_stdlib_string_type
implicit none

type :: string_type
    sequence
    character(len=:), allocatable :: raw
end type string_type

interface len
    module procedure :: len_string
end interface len

interface char
    module procedure :: char_string
end interface char

contains

elemental function len_string(string) result(length)
    type(string_type), intent(in) :: string
    integer :: length

end function len_string

pure function char_string(string) result(character_string)
    type(string_type), intent(in) :: string
    character(len=len(string)) :: character_string

end function char_string

end module

module string_19_stdlib_strings
use string_19_stdlib_string_type
implicit none

interface padl
    module procedure :: padl_char_default
    module procedure :: padl_char_pad_with
end interface padl

contains

    pure function compute_lps(string) result(lps_array)
        character(len=*), intent(in) :: string
        integer :: lps_array(len(string))
    end function compute_lps

    function compute_lps_use(string) result(l)
        character(len=*), intent(in) :: string
        integer :: l
        print *, compute_lps(string)
    end function compute_lps_use

    function compute_lps_use1(string) result(l)
        type(string_type), intent(in) :: string
        integer :: l
        print *, char(string)
    end function compute_lps_use1

    pure function padl_string_default(string, output_length) result(res)
        type(string_type), intent(in) :: string
        integer, intent(in) :: output_length
        type(string_type) :: res

        res = string_type(padl(char(string), output_length, " "))

    end function padl_string_default

    pure function padl_string_pad_with(string, output_length, pad_with) result(res)
        type(string_type), intent(in) :: string
        integer, intent(in) :: output_length
        character(len=1), intent(in) :: pad_with
        type(string_type) :: res

        res = string_type(padl(char(string), output_length, pad_with))

    end function padl_string_pad_with

    pure function padl_char_default(string, output_length) result(res)
        character(len=*), intent(in) :: string
        integer, intent(in) :: output_length
        character(len=len(string)) :: res

        res = padl(string, output_length, " ")

    end function padl_char_default

    pure function padl_char_pad_with(string, output_length, pad_with) result(res)
        character(len=*), intent(in) :: string
        integer, intent(in) :: output_length
        character(len=1), intent(in) :: pad_with
        character(len=max(len(string), output_length)) :: res
        integer :: string_length

    end function padl_char_pad_with

end module

program stdlib_strings_use
use string_19_stdlib_strings
implicit none
end program
