module template_defined_assignment_01_tmpl
    implicit none

    requirement defined_assignment_r{lhs_t, rhs_t, assign_i}
        deferred type :: lhs_t, rhs_t
        deferred interface
            subroutine assign_i(lhs, rhs)
                type(lhs_t), intent(out) :: lhs
                type(rhs_t), intent(in) :: rhs
            end subroutine
        end interface
    end requirement

    template copy_tmpl{copy_t, original_t, assign_s}
        deferred type :: copy_t, original_t
        require :: defined_assignment_r{copy_t, original_t, assign_s}
        interface assignment(=)
            module procedure assign_s
        end interface
    contains
        function copy(original) result(c)
            type(original_t), intent(in) :: original
            type(copy_t) :: c
            c = original
        end function
    end template
end module

module template_defined_assignment_01_use
    use template_defined_assignment_01_tmpl
    implicit none
    type :: a_t
        integer :: i
    end type
    type :: b_t
        real :: x
    end type
    instantiate copy_tmpl{b_t, a_t, b_from_a}, only: copy_ab => copy
contains
    subroutine b_from_a(lhs, rhs)
        type(b_t), intent(out) :: lhs
        type(a_t), intent(in) :: rhs
        lhs%x = 2.0 * rhs%i
    end subroutine
end module

program template_defined_assignment_01
    use template_defined_assignment_01_use
    implicit none
    type(a_t) :: a
    type(b_t) :: b
    a%i = 21
    b = copy_ab(a)
    print *, b%x
    if (abs(b%x - 42.0) > 1e-6) error stop
end program
