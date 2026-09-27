module template_defined_op_arg_03_ops
    implicit none
    interface operator(.minus.)
        procedure sub_int
    end interface
contains
    pure function sub_int(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x - y
    end function
end module

module template_defined_op_arg_03_m
    implicit none
    requirement r {T, op}
        deferred type :: T
        deferred interface
            pure function op(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    end requirement

    template t {T, op}
        require :: r {T, op}
    contains
        pure function f(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
            z = op(x, y)
        end function
    end template
end module

program template_defined_op_arg_03
    use template_defined_op_arg_03_ops
    use template_defined_op_arg_03_m
    implicit none
    instantiate t {operator(.minus.), integer}, only: g => f
    print *, g(1, 2)
end program
