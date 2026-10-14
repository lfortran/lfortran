! A unary `-` on an array of a deferred type applies the elemental
! user-defined `operator(-)` of the template to each element (#13657)
module template_unary_minus_02_m
    implicit none
    type :: vec
        real :: x
    end type

    template neg_t {t, p}
        deferred type :: t
        deferred interface
            elemental function p(x)
                type(t), intent(in) :: x
                type(t) :: p
            end function
        end interface
        interface operator(-)
            module procedure p
        end interface
    contains
        subroutine s(x, y)
            type(t), intent(in) :: x(:)
            type(t), intent(out) :: y(:)
            y = -x
        end subroutine
    end template
contains
    ! Differ from the intrinsic negation, so the test detects which applies.
    elemental function neg_int(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = -2*i
    end function

    elemental function neg_vec(v) result(r)
        type(vec), intent(in) :: v
        type(vec) :: r
        r%x = -3*v%x
    end function
end module

program template_unary_minus_02
    use template_unary_minus_02_m
    implicit none
    instantiate neg_t {integer, neg_int}, only: s_int => s
    instantiate neg_t {vec, neg_vec}, only: s_vec => s
    integer :: i(3), j(3)
    type(vec) :: v(2), w(2)

    i = [1, 2, 3]
    call s_int(i, j)
    if (any(j /= [-2, -4, -6])) error stop 1
    v%x = [1.5, 2.5]
    call s_vec(v, w)
    if (any(abs(w%x - [-4.5, -7.5]) > 1e-6)) error stop 2
    print *, "OK"
end program
