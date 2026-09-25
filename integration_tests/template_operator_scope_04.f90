module template_operator_scope_04_m
    implicit none
    requirement elemental_op {t, op}
        deferred type :: t
        deferred interface
            elemental function op(x, y) result(r)
                type(t), intent(in) :: x, y
                type(t) :: r
            end function
        end interface
    end requirement

    template seed {t, op}
        require elemental_op {t, op}
    end template
contains
    template function apply_array {t, op}(x, y) result(r)
        require elemental_op {t, op}
        type(t), intent(in) :: x(2), y(2)
        type(t) :: r(2)
        r = op(x, y)
    end function

    template subroutine apply_array_sub {t, op}(x, y, r)
        require elemental_op {t, op}
        type(t), intent(in) :: x(2), y(2)
        type(t), intent(out) :: r(2)
        r = op(x, y)
    end subroutine
end module

program template_operator_scope_04
    use template_operator_scope_04_m
    implicit none
    instantiate seed {real, operator(-)}
    real :: x(2), y(2), result(2)

    x = [9.0, 20.0]
    y = [2.0, 4.0]
    result = apply_array{real, operator(-)}(x, y)
    if (result(1) /= 7.0 .or. result(2) /= 16.0) error stop
    call apply_array_sub{real, operator(-)}(y, x, result)
    if (result(1) /= -7.0 .or. result(2) /= -16.0) error stop
end program
