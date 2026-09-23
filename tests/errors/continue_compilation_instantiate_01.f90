! Erroneous INSTANTIATE statements under --continue-compilation.
!
! Each of the three statements below is reported cleanly without the flag. With
! it, the symbol table visitor reports the error and keeps going, leaving the
! symbols the body visitor would instantiate uncreated; the body visitor then
! used to hand those nulls to down_cast and to instantiate_body and crash.
!
! The requirement and the template here are deliberately valid, and this file
! deliberately contains no other kind of error, so that only the instantiate
! statements are under test. See
! tests/errors/continue_compilation_templates_01.f90 for errors in the
! requirement itself.

module continue_compilation_instantiate_01_mod
    implicit none

    requirement add_r {T, op}
        deferred type :: T
        deferred interface
            function op(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template add_t(T, op)
        require add_r {T, op}
    contains
        function add_generic(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = op(x, y)
        end function
    end template

contains

    integer function add_int(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

end module continue_compilation_instantiate_01_mod

program continue_compilation_instantiate_01
    use continue_compilation_instantiate_01_mod
    implicit none

    ! Unknown template name
    instantiate add_unknown_t {integer, add_int}, only: add1 => add_generic

    ! Unknown instantiation argument
    instantiate add_t {integer, add_unknown}, only: add2 => add_generic

    ! Wrong number of instantiation arguments
    instantiate add_t {integer}, only: add3 => add_generic

end program continue_compilation_instantiate_01
