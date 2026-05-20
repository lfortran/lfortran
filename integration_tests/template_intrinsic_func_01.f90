module template_intrinsic_func_01_m
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
        pure elemental function call_op_func(x, y) result(res)
            type(V) :: res
            type(T) :: x, y
            res = op_func(x, y)
        end function
    end template
end module

program template_intrinsic_func_01
    use template_intrinsic_func_01_m
    implicit none

    instantiate op_t(integer, integer, min), only: int_min => call_op_func
    instantiate op_t(integer, integer, max), only: int_max => call_op_func
    instantiate op_t(real, real, min), only: real_min => call_op_func
    instantiate op_t(real, real, max), only: real_max => call_op_func

    if (int_min(3, 7) /= 3) error stop
    if (int_max(3, 7) /= 7) error stop
    if (int_min(-2, -9) /= -9) error stop
    if (int_max(-2, -9) /= -2) error stop
    if (abs(real_min(1.5, 2.5) - 1.5) > 1.0e-6) error stop
    if (abs(real_max(1.5, 2.5) - 2.5) > 1.0e-6) error stop

    print *, "OK"
end program
