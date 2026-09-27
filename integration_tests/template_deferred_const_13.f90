! A named constant of a template initialized with a reduction (`sum`,
! `product`, `maxval`, `minval`, `dot_product`) of an array constructor of a
! deferred constant is evaluated when the template is instantiated (#13476).

module template_deferred_const_13_m
    implicit none

    template t {n}
        deferred integer, parameter :: n
    contains
        integer function f_sum()
            integer, parameter :: x = sum([n, n])
            f_sum = x
        end function

        integer function f_product()
            integer, parameter :: x = product([n, n + 1, 2])
            f_product = x
        end function

        integer function f_maxval_minval()
            integer, parameter :: x = maxval([n, 1, -n]) - minval([n, 1, -n])
            f_maxval_minval = x
        end function

        integer function f_dot_product()
            integer, parameter :: x = dot_product([n, 2], [n, 3])
            f_dot_product = x
        end function

        real function f_real_sum()
            real, parameter :: x = sum([0.5, real(n)])
            f_real_sum = x
        end function

        integer function f_size()
            integer, parameter :: k = sum([n, 2*n])
            integer :: a(k)
            f_size = size(a)
        end function
    end template
end module

program template_deferred_const_13
    use template_deferred_const_13_m
    implicit none
    instantiate t {3}, only: f_sum3 => f_sum, f_product3 => f_product, &
        f_maxval_minval3 => f_maxval_minval, f_dot_product3 => f_dot_product, &
        f_real_sum3 => f_real_sum, f_size3 => f_size

    if (f_sum3() /= 6) error stop 1
    if (f_product3() /= 24) error stop 2
    if (f_maxval_minval3() /= 6) error stop 3
    if (f_dot_product3() /= 15) error stop 4
    if (abs(f_real_sum3() - 3.5) > 1e-6) error stop 5
    if (f_size3() /= 9) error stop 6
    print *, f_sum3(), f_product3(), f_maxval_minval3(), f_dot_product3(), &
        f_real_sum3(), f_size3()
end program
