module template_defined_op_arg_01_m
    implicit none
contains
    template function apply{op}(x, y) result(r)
        deferred interface
            pure integer function op(x, y)
                integer, intent(in) :: x, y
            end function
        end interface
        integer, intent(in) :: x, y
        integer :: r
        r = op(x, y)
    end function
end module

program template_defined_op_arg_01
    use template_defined_op_arg_01_m
    implicit none
    print *, apply{operator(.plus.)}(9, 2)
end program
