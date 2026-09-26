module template_instantiate_scope_08_m
    implicit none
    template unary{op}
        deferred interface
            integer function op(x)
                integer, intent(in) :: x
            end function
        end interface
    end template
    template action{op}
        deferred interface
            subroutine op(x)
                integer, intent(inout) :: x
            end subroutine
        end interface
    end template
    interface abs
        module procedure abs, abs_real
    end interface
    interface adjust
        module procedure adjust, adjust_real
    end interface
    instantiate unary{abs}
    instantiate unary{op=abs}
    instantiate action{adjust}
    instantiate action{op=adjust}
contains
    integer function abs(x) result(value)
        integer, intent(in) :: x
        value = x + 7
    end function
    real function abs_real(x) result(value)
        real, intent(in) :: x
        value = x + 9
    end function
    subroutine adjust(x)
        integer, intent(inout) :: x
        x = x + 5
    end subroutine
    subroutine adjust_real(x)
        real, intent(inout) :: x
        x = x + 6
    end subroutine
end module

program template_instantiate_scope_08
    use template_instantiate_scope_08_m, only: module_abs => abs, module_adjust => adjust
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
    template action{op}
        deferred interface
            subroutine op(x)
                integer, intent(inout) :: x
            end subroutine
        end interface
    contains
        subroutine invoke(x)
            integer, intent(inout) :: x
            call op(x)
        end subroutine
    end template
    interface local_op
        procedure local_op
    end interface
    interface increment
        procedure increment
    end interface
    instantiate unary{local_op}, only: apply_op => apply
    instantiate unary{op=local_op}, only: apply_again => apply
    instantiate action{increment}, only: invoke_increment => invoke
    integer :: n
    real :: r

    if (module_abs(-3) /= 4) error stop
    if (module_abs(-3.0) /= 6.0) error stop
    n = 1
    r = 2.0
    call module_adjust(n)
    call module_adjust(r)
    if (n /= 6 .or. r /= 8.0) error stop
    if (local_op(2) /= 22) error stop
    if (apply_op(3) /= 23) error stop
    if (apply_again(4) /= 24) error stop
    call increment(n)
    call invoke_increment(n)
    if (n /= 12) error stop
contains
    integer function local_op(x) result(value)
        integer, intent(in) :: x
        value = x + 20
    end function
    subroutine increment(x)
        integer, intent(inout) :: x
        x = x + 3
    end subroutine
end program
