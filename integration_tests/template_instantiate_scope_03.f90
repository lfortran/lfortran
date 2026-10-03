program template_instantiate_scope_03
    implicit none

    template unary{op}
        deferred interface
            integer function op(x)
                integer, intent(in) :: x
            end function
        end interface
    contains
        integer function apply(x) result(value)
            integer, intent(in) :: x
            value = op(x)
        end function
    end template

    ! A contained procedure takes precedence over the intrinsic of that name.
    instantiate unary{abs}, only: apply_abs => apply
    instantiate unary{abs}, only: apply_abs_again => apply
    if (apply_abs(-3) /= 4) error stop
    if (apply_abs_again(2) /= 9) error stop

contains

    integer function abs(x)
        integer, intent(in) :: x
        abs = x + 7
    end function

end program
