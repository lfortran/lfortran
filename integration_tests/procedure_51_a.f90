module types
    implicit none
    integer, parameter :: dp = kind(1.0d0)
end module

module mixings
    use types, only: dp
    implicit none

    interface
        subroutine F_fn(x, y, energies)
            import :: dp
            real(dp), intent(in) :: x(:)
            real(dp), intent(out) :: y(:), energies(:)
        end subroutine

        real(dp) function integral_fn(x)
            import :: dp
            real(dp), intent(in) :: x(:)
        end function
    end interface

contains

    subroutine mixing_linear(F, integral, x0, nenergies, max_iter, alpha, &
            l2_eps, eig_eps, x_out)
        procedure(F_fn) :: F
        procedure(integral_fn) :: integral
        real(dp), intent(in) :: x0(:)
        integer, intent(in) :: nenergies, max_iter
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: l2_eps, eig_eps
        real(dp), intent(out) :: x_out(:)

        real(dp), dimension(size(x0)) :: x_i, y_i, r_i
        real(dp) :: energies(nenergies)
        real(dp) :: x_i_norm, r_i_norm
        integer :: i

        x_i = x0
        do i = 1, max_iter
            call F(x_i, y_i, energies)
            r_i = y_i - x_i
            x_i_norm = sqrt(integral(x_i**2))
            r_i_norm = sqrt(integral(r_i**2))
            if (x_i_norm < 0.0_dp .or. r_i_norm < 0.0_dp) error stop
            if (l2_eps < 0.0_dp .or. eig_eps < 0.0_dp) error stop
            x_i = x_i + alpha * r_i
        end do
        x_out = x_i
    end subroutine

end module
