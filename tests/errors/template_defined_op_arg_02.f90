module template_defined_op_arg_02_ops
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

module template_defined_op_arg_02_m
    implicit none
contains
    template function applyr{op}(x, y) result(r)
        deferred interface
            pure real function op(x, y)
                real, intent(in) :: x, y
            end function
        end interface
        real, intent(in) :: x, y
        real :: r
        r = op(x, y)
    end function
end module

program template_defined_op_arg_02
    use template_defined_op_arg_02_ops
    use template_defined_op_arg_02_m
    implicit none
    print *, applyr{operator(.minus.)}(9.0, 2.0)
end program
