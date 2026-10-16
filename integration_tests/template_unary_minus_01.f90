! A unary `-` on a value of a deferred type resolves to the user-defined
! `operator(-)` of the template (#14719)
module template_unary_minus_01_m
    implicit none
    type :: vec
        real :: x
    end type

    template neg_t {a_t, b_t, p}
        deferred type :: a_t, b_t
        deferred interface
            function p(x)
                type(a_t), intent(in) :: x
                type(b_t) :: p
            end function
        end interface
        interface operator(-)
            module procedure p
        end interface
    contains
        subroutine s(x, y)
            type(a_t), intent(in) :: x
            type(b_t), intent(out) :: y
            y = -x
        end subroutine
    end template

    template twice_t {t, neg}
        deferred type :: t
        deferred interface
            function neg(x)
                type(t), intent(in) :: x
                type(t) :: neg
            end function
        end interface
        interface operator(-)
            module procedure neg
        end interface
    contains
        function twice_neg(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            r = -(-x)
        end function
    end template
contains
    function neg_real(i) result(r)
        integer, intent(in) :: i
        real :: r
        r = -real(i)
    end function

    function neg_vec(v) result(r)
        type(vec), intent(in) :: v
        type(vec) :: r
        r%x = -v%x
    end function

    ! Differs from the intrinsic negation, so the test detects which applies.
    function neg_int(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = -2*i
    end function
end module

program template_unary_minus_01
    use template_unary_minus_01_m
    implicit none
    instantiate neg_t {integer, real, neg_real}, only: s_int_real => s
    instantiate neg_t {vec, vec, neg_vec}, only: s_vec => s
    instantiate twice_t {integer, neg_int}, only: twice_int => twice_neg
    real :: y
    type(vec) :: v, w

    call s_int_real(3, y)
    if (abs(y + 3.0) > 1e-6) error stop 1
    v%x = 2.5
    call s_vec(v, w)
    if (abs(w%x + 2.5) > 1e-6) error stop 2
    if (twice_int(7) /= 28) error stop 3
    print *, "OK"
end program
