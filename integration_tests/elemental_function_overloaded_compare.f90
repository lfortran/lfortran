! program extracted as MRE from https://github.com/fortran-lang/stdlib
! there is missing implementation of functions/subroutines like
! check_logical, eq_string_char, new_string_from_integer_int32 etc.
! but as that's not need to be able to compile and run the MRE,
! that's being skipped for now, actually we tried having their implementation
! as well, but the program failed to run (take that as a TODO)
module mod_elemental_function_overloaded_compare
    use iso_fortran_env, only: int32
    implicit none
    !> String type holding an arbitrary sequence of characters.

    interface operator(==)
        module procedure :: eq_string_char
    end interface operator(==)

    type :: string_type
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    contains

    subroutine check_logical(expression)
        logical, intent(in) :: expression
        if (.not. expression) then
            print *, "Condition not fulfilled"
        end if
    end subroutine check_logical

    elemental function eq_string_char(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_eq
    end function eq_string_char

    elemental module function new_string_from_integer_int32(val) result(new)
        integer(int32), intent(in) :: val
        type(string_type) :: new
    end function new_string_from_integer_int32

    subroutine test_constructor()
        character(len=128) :: flc

        write(flc, '(i0)') -1026191
        call check_logical(new_string_from_integer_int32(-1026191) == trim(flc))
    end subroutine test_constructor

end module mod_elemental_function_overloaded_compare

program test_elemental_function_overloaded_compare
    use mod_elemental_function_overloaded_compare
    implicit none
    call test_constructor()
end program
