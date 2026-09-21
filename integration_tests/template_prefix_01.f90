! Tests R1611 and R1612: the standard spelling of a templated subprogram, with
! a TEMPLATE prefix (C1609), the deferred argument list in braces and the dummy
! argument list in parentheses. J3/26-158 corrected both rules to braces; the
! parenthesised deferred argument list that 26-007r1 showed is not accepted.
module template_prefix_01_m
    implicit none

contains

    template subroutine swap{t} (x, y)
        deferred type :: t
        type(t), intent(inout) :: x, y
        type(t) :: tmp
        tmp = x
        x = y
        y = tmp
    end subroutine swap

    pure template function pick_second{t} (x, y) result(res)
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = y
    end function pick_second

    template pure function pick_first{t} (x, y) result(res)
        deferred type :: t
        type(t), intent(in) :: x, y
        type(t) :: res
        res = x
    end function pick_first

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
    end subroutine

    subroutine test_function()
        if (pick_second{integer}(3, 4) /= 4) error stop
        if (pick_first{integer}(3, 4) /= 3) error stop
        if (abs(pick_second{real}(1.5, 2.5) - 2.5) > 1e-6) error stop
        if (abs(pick_first{real}(1.5, 2.5) - 1.5) > 1e-6) error stop
    end subroutine

end module

program template_prefix_01
    use template_prefix_01_m
    implicit none
    call test_subroutine()
    call test_function()
    print *, "ok"
end program
