module separate_compilation_50b_mod
    implicit none
contains
    subroutine get_exp_b(x, r)
        real(8), intent(in) :: x
        integer, intent(out) :: r
        r = exponent(x)
    end subroutine get_exp_b
end module separate_compilation_50b_mod
