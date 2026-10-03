module separate_compilation_50a_mod
    implicit none
contains
    subroutine get_exp_a(x, r)
        real(8), intent(in) :: x
        integer, intent(out) :: r
        r = exponent(x)
    end subroutine get_exp_a
end module separate_compilation_50a_mod
