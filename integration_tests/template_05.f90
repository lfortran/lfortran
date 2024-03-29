module template_05_m

    requirement req(T, op)
        type, deferred :: t
        elemental function op(x, y) result(r)
            type(T), intent(in) :: x, y
            type(T) :: r
        end function
    end requirement

    template tmp(T, plus)
        require :: req(T, plus)

        template tmp_minus(minus)
            require :: req(T, minus)
            require :: req(T, plus)

            template tmp_mult(mult)
                require :: req(T, mult)

                template tmp_div(div)
                    require :: req(T, div)
                end template

            end template

        contains

            function g_minus(x, y) result(r)
                type(T), intent(in) :: x, y
                type(T) :: r
                r = minus(x, y)
            end function

        end template

    contains
        
        function g_plus(x, y) result(r)
            type(T), intent(in) :: x, y
            type(T) :: r
            r = plus(x, y)
        end function
    end template

end module

program template_05
use template_05_m

instantiate tmp(integer, operator(+)), &
    only: integer_plus => g_plus, integer_tmp_minus => tmp_minus
instantiate integer_tmp_minus(operator(-)), &
    only: integer_minus => g_minus, integer_tmp_mult => tmp_mult

instantiate tmp(real, operator(+)), &
    only: real_plus => g_plus, real_tmp_minus => tmp_minus
instantiate real_tmp_minus(operator(-)), &
    only: real_minus => g_minus, real_tmp_mult => tmp_mult

end program