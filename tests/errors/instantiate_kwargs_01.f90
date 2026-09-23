! Keyword instantiation arguments (R1630) that break the correspondence rules
! of 16.5.5.1 para 2.
!
! The requirement and the template here are deliberately valid, and every
! instantiate statement below is wrong only in how its arguments correspond to
! the template's deferred arguments, so that nothing else is under test.

module instantiate_kwargs_01_mod
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

    template add_t {T, op}
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

end module instantiate_kwargs_01_mod

program instantiate_kwargs_01
    use instantiate_kwargs_01_mod
    implicit none

    ! C1625: a positional argument cannot follow a keyword one
    instantiate add_t {op = add_int, integer}, only: add1 => add_generic

    ! C1626: `typ` is not the name of a deferred argument
    instantiate add_t {typ = integer, op = add_int}, only: add2 => add_generic

    ! Two instantiation arguments correspond to the deferred argument `t`
    instantiate add_t {T = integer, T = integer}, only: add3 => add_generic

    ! No instantiation argument corresponds to the deferred argument `op`
    instantiate add_t {T = integer}, only: add4 => add_generic

end program instantiate_kwargs_01
