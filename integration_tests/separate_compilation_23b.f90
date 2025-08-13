submodule (ansi_separate_compilation_23) stdlib_ansi_operator
    use string_type_separate_compilation_23, only : operator(//)
    implicit none

contains

    pure module function concat_left(lval, code) result(str)
        character(len=*), intent(in) :: lval
        type(ansi_code), intent(in) :: code
        character(len=:), allocatable :: str
    end function concat_left

end submodule stdlib_ansi_operator