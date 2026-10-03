module template_defined_op_01_ops
    implicit none

    interface operator(.minus.)
        procedure sub_int, sub_real
    end interface

contains

    pure function sub_int(x, y) result(r)
        integer, intent(in) :: x, y
        integer :: r
        r = x - y
    end function

    pure function sub_real(x, y) result(r)
        real, intent(in) :: x, y
        real :: r
        r = x - y
    end function
end module

module template_defined_op_01_m
    implicit none

    requirement binop_R {T, op}
        deferred type :: T
        deferred interface
            pure function op(x, y) result(r)
                type(T), intent(in) :: x, y
                type(T) :: r
            end function
        end interface
    end requirement

    template binop_t {T, op}
        require :: binop_R {T, op}
    contains
        pure function twice(x, y) result(r)
            type(T), intent(in) :: x, y
            type(T) :: r
            r = op(op(x, y), y)
        end function
    end template

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

program template_defined_op_01
    use template_defined_op_01_ops
    use template_defined_op_01_m
    implicit none

    instantiate binop_t {integer, operator(.minus.)}, only: twice_int => twice
    instantiate binop_t {real, op=operator(.minus.)}, only: twice_real => twice

    if (apply{operator(.minus.)}(9, 2) /= 7) error stop
    if (twice_int(10, 3) /= 4) error stop
    if (abs(twice_real(10.0, 2.5) - 5.0) > 1.0e-6) error stop

    print *, "OK"
end program
