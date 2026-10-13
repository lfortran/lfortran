module template_operator_mod_02_tmpl
    implicit none

    template asg_t{T, U, conv}
        deferred type :: T
        deferred type :: U
        deferred interface
            subroutine conv(x, y)
                type(T), intent(out) :: x
                type(U), intent(in) :: y
            end subroutine
        end interface
        interface assignment(=)
            module procedure conv
        end interface
    contains
        subroutine set(x, y)
            type(T), intent(out) :: x
            type(U), intent(in) :: y
            x = y
        end subroutine
    end template
end module

module template_operator_mod_02_user
    use template_operator_mod_02_tmpl
    implicit none

    type :: a_t
        integer :: i
    end type

    type :: b_t
        real :: r
    end type

    instantiate asg_t{a_t, integer, from_int}
    instantiate asg_t{b_t, a_t, from_a}, only: set_b => set

contains

    subroutine from_int(x, y)
        type(a_t), intent(out) :: x
        integer, intent(in) :: y
        x%i = 2*y
    end subroutine

    subroutine from_a(x, y)
        type(b_t), intent(out) :: x
        type(a_t), intent(in) :: y
        x%r = y%i + 0.5
    end subroutine
end module
