! Tests the standard `deferred type ::` spelling of a deferred type argument
! (Fortran 2028 draft J3/26-007, R1616:
!     deferred-type-declaration-stmt is DEFERRED TYPE :: deferred-arg-name-list)
! both with a single name and with a name list, in a requirement and in a
! template.
module template_deferred_type_01_m
    implicit none
    private
    public :: test_deferred_type

    requirement add_r(t, add)
        deferred type :: t
        pure function add(lhs, rhs) result(res)
            type(t), intent(in) :: lhs
            type(t), intent(in) :: rhs
            type(t) :: res
        end function
    end requirement

    template double_tmpl(t, u, add_t, add_u)
        deferred type :: t, u
        require :: add_r(t, add_t), add_r(u, add_u)
        private
        public :: double_both
      contains
        subroutine double_both(a, b)
            type(t), intent(inout) :: a
            type(u), intent(inout) :: b
            a = add_t(a, a)
            b = add_u(b, b)
        end subroutine
    end template

contains

    subroutine test_deferred_type()
        instantiate double_tmpl(integer, real, operator(+), operator(+)), &
            only: double_both_ir => double_both
        integer :: i
        real :: r
        i = 3
        r = 1.5
        call double_both_ir(i, r)
        print *, i, r
        if (i /= 6) error stop
        if (abs(r - 3.0) > 1e-6) error stop
    end subroutine

end module

program template_deferred_type_01
    use template_deferred_type_01_m
    implicit none

    call test_deferred_type()

end program
