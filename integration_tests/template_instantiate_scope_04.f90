module template_instantiate_scope_04_m
    implicit none
    integer, parameter :: offset = 10
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
contains
    integer function op(x) result(value)
        integer, intent(in) :: x
        value = x + 20
    end function

    subroutine check()
        instantiate unary{op}, only: apply_op => apply
        integer, parameter :: offset = 20
        procedure(op), pointer :: callback
        callback => op
        ! Even an ordinary call must see the completed nearest host scope.
        if (op(2) /= 22) error stop
        if (apply_op(3) /= 23) error stop
        if (callback(4) /= 24) error stop
    contains
        integer function op(x) result(value)
            integer, intent(in) :: x
            integer, parameter :: captured = offset
            value = x + captured
        end function
    end subroutine
end module

program template_instantiate_scope_04
    use template_instantiate_scope_04_m, only: check
    implicit none
    call check()
end program
