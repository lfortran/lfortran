module template_instantiate_scope_02_m
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

    instantiate unary{op}, only: module_apply => apply
    ! A later default access statement also applies to the contained actual.
    private
    public :: check_sub, check_fun, check_op

contains

    subroutine check_sub(base)
        integer, intent(in) :: base
        instantiate unary{op}, only: local_apply => apply
        integer :: value
        value = local_apply(3)
        if (value /= base + 3) error stop
    contains
        ! This local procedure, not the module's op, is the actual argument.
        integer function op(x)
            integer, intent(in) :: x
            op = base + x
        end function
    end subroutine

    function check_fun(base) result(value)
        integer, intent(in) :: base
        instantiate unary{op}, only: local_apply => apply
        integer :: value
        value = local_apply(4)
    contains
        integer function op(x)
            integer, intent(in) :: x
            op = base + 2 * x
        end function
    end function

    integer function op(x)
        integer, intent(in) :: x
        op = x + 10
    end function

    integer function check_op()
        check_op = op(2)
    end function

end module

program template_instantiate_scope_02
    use template_instantiate_scope_02_m
    implicit none
    if (check_op() /= 12) error stop
    call check_sub(20)
    call check_sub(30)
    if (check_fun(30) /= 38) error stop
    if (check_fun(40) /= 48) error stop
    if (op() /= 99) error stop
contains
    integer function op()
        op = 99
    end function
end program
