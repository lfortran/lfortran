! A `procedure(iface) ::` dummy whose interface is a deferred interface over a
! deferred type takes the argument types of the instantiation, whether the
! interface is bound to a named function or to an operator.
module template_procedure_dummy_02_m
    implicit none
    template t {T, plus}
        deferred type :: T
        deferred interface
            function plus(x, y) result(z)
                type(T), intent(in) :: x, y
                type(T) :: z
            end function
        end interface
    contains
        function apply(f, a) result(r)
            procedure(plus) :: f
            type(T), intent(in) :: a
            type(T) :: r
            r = f(a, a)
        end function

        function same(f, a) result(r)
            procedure(plus) :: f
            type(T), intent(in) :: a
            type(T) :: r
            r = a
        end function
    end template
contains
    integer function add(x, y) result(z)
        integer, intent(in) :: x, y
        z = x + y
    end function

    real function mul(x, y) result(z)
        real, intent(in) :: x, y
        z = x * y
    end function
end module

program template_procedure_dummy_02
    use template_procedure_dummy_02_m, only: t, add, mul
    implicit none
    instantiate t {integer, add}
    instantiate t {real, mul}, only: apply_real => apply
    instantiate t {integer, operator(+)}, only: apply_op => apply
    if (apply(add, 4) /= 8) error stop
    if (same(add, 7) /= 7) error stop
    if (abs(apply_real(mul, 3.0) - 9.0) > 1e-6) error stop
    if (apply_op(add, 5) /= 10) error stop
    print *, "ok"
end program
