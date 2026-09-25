module template_operator_scope_03_m
    implicit none
    requirement op_r {t, u, op}
        deferred type :: t
        deferred type :: u
        deferred interface
            pure function op(x, y) result(r)
                type(t), intent(in) :: x, y
                type(u) :: r
            end function
        end interface
    end requirement
contains
    template function apply {t, u, op}(x, y) result(r)
        require op_r {t, u, op}
        type(t), intent(in) :: x, y
        type(u) :: r
        r = op(x, y)
    end function

    template subroutine apply_sub {t, u, op}(x, y, r)
        require op_r {t, u, op}
        type(t), intent(in) :: x, y
        type(u), intent(out) :: r
        r = op(x, y)
    end subroutine
end module

program template_operator_scope_03
    use template_operator_scope_03_m
    implicit none
    real :: a, b, difference, sum
    integer :: truncated

    a = 5.5
    b = 3.0
    truncated = apply{real, integer, operator(-)}(a, b)
    if (truncated /= 2) error stop
    difference = a - b
    if (difference /= 2.5) error stop
    call apply_sub{real, integer, operator(+)}(a, b, truncated)
    if (truncated /= 8) error stop
    sum = a + b
    if (sum /= 8.5) error stop
    if (apply{integer, integer, operator(-)}(9, 2) /= 7) error stop
    call check_host_scope()
contains
    subroutine check_host_scope()
        real :: result
        result = a - b
        if (result /= 2.5) error stop
    end subroutine
end program
