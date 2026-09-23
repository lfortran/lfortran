! Tests R1611 and R1612: the standard spelling of a templated subprogram, with
! a TEMPLATE prefix (C1609), the deferred argument list in braces and the dummy
! argument list in parentheses. J3/26-158 corrected both rules to braces; the
! parenthesised deferred argument list that 26-007r1 showed is not accepted.
!
! A prefix is a list, so TEMPLATE can appear anywhere in it. The grammar spells
! that as four alternatives per subprogram kind, and all eight are exercised
! below, numbered to match:
!
!     1/5  TEMPLATE KW                             (no other prefix keyword)
!     2/6  TEMPLATE fn_mod_plus KW                 (keywords after TEMPLATE)
!     3/7  fn_mod_plus TEMPLATE KW                 (keywords before TEMPLATE)
!     4/8  fn_mod_plus TEMPLATE fn_mod_plus KW     (keywords on both sides)
!
! The fourth shape of each kind is the only one that joins two prefix lists, so
! 4 and 8 are what cover concat_prefix().
module template_prefix_01_m
    implicit none

contains

    ! 1. TEMPLATE SUBROUTINE
    template subroutine swap{t} (x, y)
        deferred type :: t
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine swap

    ! 2. TEMPLATE fn_mod_plus SUBROUTINE
    template pure subroutine store{t} (x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine store

    ! 3. fn_mod_plus TEMPLATE SUBROUTINE
    pure template subroutine copy_into{t} (x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine copy_into

    ! 4. fn_mod_plus TEMPLATE fn_mod_plus SUBROUTINE (concat_prefix)
    recursive template pure subroutine assign_once{t} (x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine assign_once

    ! 5. TEMPLATE FUNCTION
    template function pick_last{t} (x, y) result(res)
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = y
    end function pick_last

    ! 6. TEMPLATE fn_mod_plus FUNCTION
    template pure function pick_first{t} (x, y) result(res)
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = x
    end function pick_first

    ! 7. fn_mod_plus TEMPLATE FUNCTION
    pure template function pick_second{t} (x, y) result(res)
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = y
    end function pick_second

    ! 8. fn_mod_plus TEMPLATE fn_mod_plus FUNCTION (concat_prefix)
    elemental template pure function same{t} (x) result(res)
        deferred type :: t
        type(t), intent(in) :: x
        type(t) :: res
        res = x
    end function same

    subroutine test_subroutine()
        integer :: a, b
        real :: x, y
        a = 1
        b = 2
        call swap{integer}(a, b)
        if (a /= 2) error stop
        if (b /= 1) error stop
        x = 1.5
        y = 2.5
        call swap{real}(x, y)
        if (abs(x - 2.5) > 1e-6) error stop
        if (abs(y - 1.5) > 1e-6) error stop
        call store{integer}(7, a)
        if (a /= 7) error stop
        call copy_into{integer}(8, a)
        if (a /= 8) error stop
        call assign_once{integer}(9, a)
        if (a /= 9) error stop
        call assign_once{real}(9.5, x)
        if (abs(x - 9.5) > 1e-6) error stop
    end subroutine

    subroutine test_function()
        if (pick_second{integer}(3, 4) /= 4) error stop
        if (pick_first{integer}(3, 4) /= 3) error stop
        if (abs(pick_second{real}(1.5, 2.5) - 2.5) > 1e-6) error stop
        if (abs(pick_first{real}(1.5, 2.5) - 1.5) > 1e-6) error stop
        if (pick_last{integer}(3, 4) /= 4) error stop
        if (abs(pick_last{real}(1.5, 2.5) - 2.5) > 1e-6) error stop
        if (same{integer}(5) /= 5) error stop
        if (abs(same{real}(1.5) - 1.5) > 1e-6) error stop
    end subroutine

end module

program template_prefix_01
    use template_prefix_01_m
    implicit none
    call test_subroutine()
    call test_function()
    print *, "ok"
end program
