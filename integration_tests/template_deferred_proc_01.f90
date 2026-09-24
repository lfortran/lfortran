! Tests the DEFERRED PROCEDURE form of a deferred procedure declaration
! (Fortran 2028 draft J3/26-007r1, R1622:
!     deferred-proc-decl-stmt is DEFERRED PROCEDURE ( interface-name )
!                                [ :: ] deferred-proc-name-list
! ), which declares a deferred argument that is a procedure by naming an
! existing interface instead of spelling one out in place. The `::` is
! optional and the name list may hold several names, so a requirement uses the
! bare spelling, a template the `::` spelling with two names at once, and a
! templated subprogram the `::` spelling with one name.
module template_deferred_proc_01_m
    implicit none
    private
    public :: test_deferred_proc

    ! The interfaces the deferred procedures below are declared with.
    abstract interface
        pure function binop(lhs, rhs) result(res)
            integer, intent(in) :: lhs
            integer, intent(in) :: rhs
            integer :: res
        end function

        pure function unop(arg) result(res)
            integer, intent(in) :: arg
            integer :: res
        end function
    end interface

    ! R1622 without the optional `::`.
    requirement binop_r {f}
        deferred procedure (binop) f
    end requirement

    ! R1622 with the `::` and a two-name deferred-proc-name-list. The third
    ! deferred procedure of the template comes in through the requirement.
    template combine_tmpl {f, g, h}
        require :: binop_r {f}
        deferred procedure (binop) :: g, h
        private
        public :: combine
      contains
        pure function combine(a, b) result(res)
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer :: res
            res = h(f(a, b), g(a, b))
        end function
    end template

contains

    pure function add_ints(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        integer :: res
        res = lhs + rhs
    end function

    pure function mul_ints(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        integer :: res
        res = lhs * rhs
    end function

    pure function sub_ints(lhs, rhs) result(res)
        integer, intent(in) :: lhs
        integer, intent(in) :: rhs
        integer :: res
        res = lhs - rhs
    end function

    pure function negate_int(arg) result(res)
        integer, intent(in) :: arg
        integer :: res
        res = -arg
    end function

    ! A templated subprogram declares its deferred procedure the same way.
    template subroutine apply_twice{op}(x)
        deferred procedure (unop) :: op
        integer, intent(inout) :: x
        x = op(op(x))
    end subroutine

    subroutine test_deferred_proc()
        instantiate combine_tmpl {add_ints, mul_ints, sub_ints}, &
            only: combine_amS => combine
        instantiate combine_tmpl {mul_ints, add_ints, add_ints}, &
            only: combine_maA => combine
        instantiate :: apply_twice_negate => apply_twice {negate_int}
        integer :: i
        ! sub_ints(add_ints(3, 4), mul_ints(3, 4)) == 7 - 12
        i = combine_amS(3, 4)
        print *, i
        if (i /= -5) error stop
        ! add_ints(mul_ints(3, 4), add_ints(3, 4)) == 12 + 7
        i = combine_maA(3, 4)
        print *, i
        if (i /= 19) error stop
        i = 6
        call apply_twice_negate(i)
        print *, i
        if (i /= 6) error stop
    end subroutine

end module

program template_deferred_proc_01
    use template_deferred_proc_01_m
    implicit none

    call test_deferred_proc()

end program
