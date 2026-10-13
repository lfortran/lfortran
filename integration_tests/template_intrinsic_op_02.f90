! The intrinsic unary operators `-`, `+` and `.not.` as instantiation
! arguments for a deferred procedure of one argument (#14731), applied
! intrinsically or through a user-defined operator.
module template_intrinsic_op_02_m
    implicit none

    type :: v
        integer :: x
    end type

    interface operator(.not.)
        module procedure not_v
    end interface

    template unary_t {T, V, op}
        deferred type :: T
        deferred type :: V
        deferred interface
            pure elemental function op(x) result(res)
                type(T), intent(in) :: x
                type(V) :: res
            end function
        end interface
    contains
        pure elemental function call_op(x) result(res)
            type(T), intent(in) :: x
            type(V) :: res
            res = op(x)
        end function
    end template

contains
    elemental function not_v(a) result(r)
        type(v), intent(in) :: a
        type(v) :: r
        r%x = 100 - a%x
    end function

    template function apply{op}(x) result(r)
        deferred interface
            pure real function op(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: x
        real :: r
        r = op(x)
    end function

    template function apply_l{op}(x) result(r)
        deferred interface
            pure logical function op(x)
                logical, intent(in) :: x
            end function
        end interface
        logical, intent(in) :: x
        logical :: r
        r = op(x)
    end function
end module

program template_intrinsic_op_02
    use template_intrinsic_op_02_m
    implicit none
    integer :: a(3) = [1, -2, 3]
    type(v) :: b

    instantiate unary_t {integer, integer, operator(-)}, only: int_neg => call_op
    instantiate unary_t {integer, real, operator(-)}, only: int_neg_r => call_op
    instantiate unary_t {real, real, operator(+)}, only: real_pos => call_op
    instantiate unary_t {logical, logical, operator(.not.)}, only: log_not => call_op
    instantiate unary_t {v, v, operator(.not.)}, only: v_not => call_op

    if (int_neg(5) /= -5) error stop 1
    if (any(int_neg(a) /= [-1, 2, -3])) error stop 2
    if (abs(int_neg_r(5) + 5.0) > 1.0e-6) error stop 3
    if (abs(real_pos(2.5) - 2.5) > 1.0e-6) error stop 4
    if (log_not(.true.)) error stop 5
    if (.not. log_not(.false.)) error stop 6
    b = v_not(v(4))
    if (b%x /= 96) error stop 10

    if (apply{operator(-)}(2.0) /= -2.0) error stop 7
    if (apply{operator(+)}(2.0) /= 2.0) error stop 8
    if (apply_l{operator(.not.)}(.true.)) error stop 9

    print *, "OK"
end program
