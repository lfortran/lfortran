! A templated subprogram carries TEMPLATE in its prefix in the Fortran 2028
! working draft (J3/26-007r1, 16.1.2), as corrected by J3 paper 26-158, which
! spells the deferred argument list with braces:
!
!     R1611 templated-function-stmt    is  prefix FUNCTION template-name
!               { deferred-arg-name-list } ( [ dummy-arg-name-list ] ) [ suffix ]
!     R1612 templated-subroutine-stmt  is  prefix SUBROUTINE template-name
!               { deferred-arg-name-list } ( [ dummy-arg-list ] )
!               [ proc-language-binding-spec ]
!
!     C1609 TEMPLATE shall appear in the prefix of a templated-function-stmt or
!           a templated-subroutine-stmt.
!
! LFortran used to accept the deferred argument list with no TEMPLATE in the
! prefix at all; this test pins that each of those spellings is now a syntax
! error. See integration_tests/template_prefix_01.f90 for the accepted ones.

module templated_subp_syntax_1
contains

    subroutine swap{t}(x, y)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine

    pure subroutine copy_into{t}(x, y)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine

    function pick_last{t}(x, y) result(res)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = y
    end function

    pure function pick_second{t}(x, y) result(res)  ! {Error} Token '{' is unexpected here
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = y
    end function

end module
