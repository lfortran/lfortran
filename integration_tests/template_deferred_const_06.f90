! Bind deferred constants in the template scope rather than shadowing them in
! the procedure, including constants used by local named-constant initializers.
module template_deferred_const_06_m
    implicit none
contains
    template integer function add{n}(x) result(res)
        deferred integer, parameter :: n
        integer, intent(in) :: x
        res = x + n
    end function

    template subroutine add_in_place{n}(x)
        deferred integer, parameter :: n
        integer, intent(inout) :: x
        integer, parameter :: offset = n
        x = x + offset
    end subroutine

    subroutine check()
        integer, parameter :: seven = 7, minus_three = -3, zero = 0
        instantiate :: add7 => add{seven}
        instantiate :: addm3 => add{minus_three}
        instantiate :: inc7 => add_in_place{seven}
        instantiate :: incm3 => add_in_place{minus_three}
        integer :: x

        x = -5
        if (add7(x) /= 2) error stop
        if (addm3(x) /= -8) error stop
        call inc7(x)
        if (x /= 2) error stop
        call incm3(x)
        if (x /= -1) error stop
        if (add7(x) /= 6) error stop
        if (add{zero}(5) /= 5) error stop
        call add_in_place{minus_three}(x)
        if (x /= -4) error stop
    end subroutine
end module

program template_deferred_const_06
    use template_deferred_const_06_m, only: check
    implicit none
    call check()
end program
