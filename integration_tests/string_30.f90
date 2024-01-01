module stdlib_string_type
    implicit none
    private

    public :: string_type

    public :: operator(//)

    type :: string_type
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface operator(//)
        module procedure :: concat_string_string
        module procedure :: concat_string_char
        module procedure :: concat_char_string
    end interface operator(//)

contains

    elemental function concat_string_string(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string

    end function concat_string_string

    elemental function concat_string_char(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(string_type) :: string

    end function concat_string_char

    elemental function concat_char_string(lhs, rhs) result(string)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string

    end function concat_char_string

end module stdlib_string_type

module stdlib_ansi
    use stdlib_string_type, only : string_type
    implicit none
    private

    public :: ansi_code

    public :: to_string, operator(//)

    public :: concat_left_str, concat_right_str

    type :: ansi_code
        private
        integer(1) :: style = -1_1
        integer(1) :: bg = -1_1
        integer(1) :: fg = -1_1
    end type ansi_code

    interface to_string
        pure module function to_string_ansi_code(code) result(str)
            type(ansi_code), intent(in) :: code
            character(len=:), allocatable :: str
        end function to_string_ansi_code
    end interface to_string

    interface operator(//)

        pure module function concat_left_str(lval, code) result(str)
            type(string_type), intent(in) :: lval
            type(ansi_code), intent(in) :: code
            type(string_type) :: str
        end function concat_left_str

        pure module function concat_right_str(code, rval) result(str)
            type(string_type), intent(in) :: rval
            type(ansi_code), intent(in) :: code
            type(string_type) :: str
        end function concat_right_str
    end interface operator(//)

end module stdlib_ansi

submodule (stdlib_ansi) stdlib_ansi_operator
    use stdlib_string_type, only : operator(//)
    implicit none

contains

    pure module function concat_left_str(lval, code) result(str)
        type(string_type), intent(in) :: lval
        type(ansi_code), intent(in) :: code
        type(string_type) :: str

        str = lval // to_string(code)
    end function concat_left_str

    pure module function concat_right_str(code, rval) result(str)
        type(string_type), intent(in) :: rval
        type(ansi_code), intent(in) :: code
        type(string_type) :: str

        str = to_string(code) // rval
    end function concat_right_str

end submodule stdlib_ansi_operator
