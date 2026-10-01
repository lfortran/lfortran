! Concrete standard-Fortran oracle for template_instantiate_rename_02.
! GFortran supports USE renaming, not the experimental template syntax.
module template_instantiate_rename_02_oracle_m
    implicit none
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

    integer function identity_integer(x)
        integer, intent(in) :: x
        identity_integer = x
    end function

    real function identity_real(x)
        real, intent(in) :: x
        identity_real = x
    end function
end module

program template_instantiate_rename_02_oracle
    use template_instantiate_rename_02_oracle_m, only: fivei => five, &
        expliciti => five_explicit, sumi => accumulate, calli => call_five, &
        identity_i => identity_integer, identity_r => identity_real
    use template_instantiate_rename_02_oracle_m, only: five
    implicit none

    if (fivei() /= 5) error stop
    if (expliciti() /= 5) error stop
    if (five() /= 5) error stop
    if (sumi(3) /= 6) error stop
    if (sumi(4) /= 10) error stop
    if (calli() /= 5) error stop
    if (identity_i(11) /= 11) error stop
    if (abs(identity_r(2.5) - 2.5) > 1e-6) error stop
    call check_nested()

contains

    subroutine check_nested()
        if (fivei() /= 5) error stop
    end subroutine
end program
