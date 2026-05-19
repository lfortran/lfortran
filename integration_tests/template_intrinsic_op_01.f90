module template_intrinsic_op_01_m
    implicit none

    requirement op_R(T, V, op_func)
        type, deferred :: T
        type, deferred :: V
        pure elemental function op_func(lhs, rhs) result(res)
            type(T), intent(in) :: lhs
            type(T), intent(in) :: rhs
            type(V) :: res
        end function
    end requirement

    template op_t(T, V, op_func)
        require :: op_R(T, V, op_func)
    contains
        pure elemental function call_op_func(x) result(res)
            type(V) :: res
            type(T) :: x
            res = op_func(x, x)
        end function
    end template
end module

program template_intrinsic_op_01
    use template_intrinsic_op_01_m
    implicit none

    instantiate op_t(integer, integer, operator(+)), only: int_add => call_op_func
    instantiate op_t(integer, real,    operator(+)), only: int_add_r => call_op_func
    instantiate op_t(integer, real,    operator(-)), only: int_sub_r => call_op_func
    instantiate op_t(integer, real,    operator(*)), only: int_mul_r => call_op_func
    instantiate op_t(integer, real,    operator(/)), only: int_div_r => call_op_func

    if (int_add(5) /= 10) error stop
    if (abs(int_add_r(5) - 10.0) > 1.0e-6) error stop
    if (abs(int_sub_r(5) -  0.0) > 1.0e-6) error stop
    if (abs(int_mul_r(5) - 25.0) > 1.0e-6) error stop
    if (abs(int_div_r(5) -  1.0) > 1.0e-6) error stop

    print *, "OK"
end program
