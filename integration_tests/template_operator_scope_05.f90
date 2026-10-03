module template_operator_scope_05_ops
    implicit none
    type :: s
        integer :: v
    end type
    interface operator(-)
        procedure sub
    end interface
    interface operator(+)
        procedure add
    end interface
contains
    function sub(x, y) result(r)
        type(s), intent(in) :: x, y
        type(s) :: r
        r%v = x%v - y%v
    end function

    function add(x, y) result(r)
        type(s), intent(in) :: x, y
        type(s) :: r
        r%v = x%v + y%v
    end function
end module

module template_operator_scope_05_m
    use template_operator_scope_05_ops, only: s
    implicit none
    template apply_t {op}
        interface
            function op(x, y) result(r)
                import :: s
                type(s), intent(in) :: x, y
                type(s) :: r
            end function
        end interface
    contains
        function apply_generic(x, y) result(r)
            type(s), intent(in) :: x, y
            type(s) :: r
            r = op(x, y)
        end function
    end template
contains
    template function apply {op}(x, y) result(r)
        deferred interface
            function op(x, y) result(r)
                import :: s
                type(s), intent(in) :: x, y
                type(s) :: r
            end function
        end interface
        type(s), intent(in) :: x, y
        type(s) :: r
        r = op(x, y)
    end function
end module

program template_operator_scope_05
    ! Only the operators are imported, not their specific procedures
    use template_operator_scope_05_ops, only: s, operator(-), operator(+)
    use template_operator_scope_05_m, only: apply, apply_t
    implicit none
    instantiate apply_t {operator(+)}, only: add_s => apply_generic
    type(s) :: r

    r = apply{operator(-)}(s(9), s(2))
    if (r%v /= 7) error stop
    r = apply{operator(+)}(s(9), s(2))
    if (r%v /= 11) error stop
    r = add_s(s(40), s(2))
    if (r%v /= 42) error stop
    print *, r%v
end program
