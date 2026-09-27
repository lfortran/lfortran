! A named constant of a template initialized with a mathematical intrinsic
! (`sin`, `cos`, `exp`, `log`, ...) of a deferred constant is evaluated when
! the template is instantiated (#13476).

module template_deferred_const_12_m
    implicit none

    template t {n}
        deferred integer, parameter :: n
    contains
        real function f_sin()
            real, parameter :: x = sin(real(n))
            f_sin = x
        end function

        real(8) function f_sum()
            real(8), parameter :: x = cos(real(n, 8)) + tan(real(n, 8)) &
                + atan(real(n, 8)) + tanh(real(n, 8)) + asinh(real(n, 8))
            f_sum = x
        end function

        real function f_exp_log()
            real, parameter :: x = exp(real(n)) + log(real(n)) + log10(real(n))
            f_exp_log = x
        end function

        integer function f_size()
            integer, parameter :: k = nint(10 * sin(real(n))) + 2
            integer :: a(k)
            f_size = size(a)
        end function
    end template
end module

program template_deferred_const_12
    use template_deferred_const_12_m
    implicit none
    instantiate t {3}, only: f_sin3 => f_sin, f_sum3 => f_sum, &
        f_exp_log3 => f_exp_log, f_size3 => f_size
    real(8), parameter :: y = 3.0d0

    if (abs(f_sin3() - sin(3.0)) > 1e-6) error stop 1
    if (abs(f_sum3() - (cos(y) + tan(y) + atan(y) + tanh(y) + asinh(y))) &
        > 1d-12) error stop 2
    if (abs(f_exp_log3() - (exp(3.0) + log(3.0) + log10(3.0))) > 1e-5) &
        error stop 3
    if (f_size3() /= 3) error stop 4
    print *, f_sin3(), f_sum3(), f_exp_log3(), f_size3()
end program
