module string_type_separate_compilation_23
    implicit none
    private

    public :: string_type
    public :: operator(//)

    type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    interface operator(//)
        module procedure :: concat_string_string
    end interface operator(//)

contains

    elemental function concat_string_string(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string
    end function concat_string_string

end module string_type_separate_compilation_23


module ansi_separate_compilation_23
    implicit none
    private

    public :: ansi_code
    public :: operator(//)

    type :: ansi_code
        private
        integer(1) :: style = -1
        integer(1) :: bg = -1
        integer(1) :: fg = -1
    end type ansi_code

    interface operator(//)
        pure module function concat_left(lval, code) result(str)
            character(len=*), intent(in) :: lval
            type(ansi_code), intent(in) :: code
            character(len=:), allocatable :: str
        end function concat_left
    end interface operator(//)

end module ansi_separate_compilation_23