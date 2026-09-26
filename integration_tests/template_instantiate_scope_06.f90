program template_instantiate_scope_06
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

    instantiate unary{abs}, only: apply_abs => apply
    if (apply_abs(-3) /= 13) error stop
    if (abs(-4) /= 14) error stop
contains
    integer function abs(x) result(value)
        integer, intent(in) :: x
        instantiate unary{host_abs}, only: apply_host => apply
        value = apply_host(x)
    end function

    ! The nested instantiation must also find a later host procedure.
    integer function host_abs(x) result(value)
        integer, intent(in) :: x
        value = 10 - x
    end function
end program
