module allocate62_mod
    implicit none

    type :: token_stack
        integer :: dummy = 0
    end type token_stack

    type, public :: equation_parser
        character(:), allocatable :: equation
        character(:), allocatable :: variable_name
        character(:), allocatable :: infix_formula
        integer :: n_indep_vars
        type(indep_var), dimension(:), allocatable :: indep_vars
        type(token_stack) :: infix
        type(token_stack) :: postfix
    contains
        procedure :: touch
    end type equation_parser

    type :: indep_var
        character(:), allocatable :: value
    end type indep_var

    interface equation_parser
        procedure construct_equation_parser
    end interface equation_parser

contains

    function construct_equation_parser(indep_vars) result(parser)
        type(equation_parser) :: parser
        character(*) :: indep_vars(:)
        integer :: i
        integer :: n_indep_vars

        n_indep_vars = size(indep_vars)
        allocate(parser%indep_vars(1:n_indep_vars))
        parser%n_indep_vars = n_indep_vars

        do i = 1, n_indep_vars
            parser%indep_vars(i)%value = trim(indep_vars(i))
        end do
    end function construct_equation_parser

    subroutine touch(parser)
        class(equation_parser), intent(inout) :: parser
        if (parser%n_indep_vars > 0) then
            parser%n_indep_vars = parser%n_indep_vars
        end if
    end subroutine touch

end module allocate62_mod

program allocate_62
    use allocate62_mod
    implicit none

    type(equation_parser) :: parser
    character(10) :: names(2)

    names = [character(10) :: "x", "y"]
    parser = equation_parser(names)
    call parser%touch()

    if (parser%n_indep_vars /= 2) error stop 1
    if (parser%indep_vars(1)%value /= "x") error stop 2
    if (parser%indep_vars(2)%value /= "y") error stop 3
end program allocate_62
