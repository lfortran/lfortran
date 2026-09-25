module template_instantiate_rename_02_m
    implicit none
    template identity_t {t}
        deferred type :: t
    contains
        function identity(x)
            type(t), intent(in) :: x
            type(t) :: identity
            identity = x
        end function
    end template
end module

program template_instantiate_rename_02
    use template_instantiate_rename_02_m, only: identity_t
    implicit none

    template values_t {}
    contains
        integer function five()
            five = 5
        end function

        integer function five_explicit() result(r)
            r = 5
        end function

        integer function accumulate(n)
            integer, intent(in) :: n
            integer :: i
            accumulate = 0
            do i = 1, n
                accumulate = accumulate + i
            end do
        end function

        integer function call_five()
            call_five = five()
        end function
    end template

    instantiate values_t {}, only: fivei => five, expliciti => five_explicit, &
        sumi => accumulate, calli => call_five
    instantiate identity_t {integer}, only: identity_i => identity
    instantiate identity_t {real}, only: identity_r => identity

    if (fivei() /= 5) error stop
    if (expliciti() /= 5) error stop
    if (sumi(3) /= 6) error stop
    if (sumi(4) /= 10) error stop
    ! A call to a renamed sibling must still use the function substitution.
    if (calli() /= 5) error stop
    if (identity_i(11) /= 11) error stop
    if (abs(identity_r(2.5) - 2.5) > 1e-6) error stop
    call check_unrenamed()
    call check_nested()

contains

    subroutine check_unrenamed()
        instantiate values_t {}, only: five
        if (five() /= 5) error stop
    end subroutine

    subroutine check_nested()
        instantiate values_t {}, only: nested_five => five
        if (nested_five() /= 5) error stop
    end subroutine
end program
