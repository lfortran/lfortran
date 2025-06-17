module optional_06_mod1
    implicit none
    public :: cauchy

    interface cauchy
        module procedure cauchy_sngl_opt
    end interface cauchy

contains
    subroutine cauchy_sngl_opt(i)
        integer, intent(out) :: i
        i = 6
    end subroutine cauchy_sngl_opt
end module optional_06_mod1

module optional_06_mod2
    use optional_06_mod1
    implicit none
contains
    subroutine wind0_sngl(i, f)
        implicit none
        integer, intent(out) :: i
        interface
            function f(z) result(r)
                integer :: r
                complex, intent(in) :: z
            end function f
        end interface
        optional :: f
        complex :: z

        z = (1.0, 2.0)
        i = 0

        if (present(f)) then
            i = f(z)
        else
            call cauchy(i)
        end if
    end subroutine wind0_sngl
end module optional_06_mod2
