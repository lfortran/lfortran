! A unary `+` on a value of a deferred type resolves to the user-defined
! `operator(+)` of the template (#14730)
module template_unary_plus_01_m
    implicit none
    type :: vec
        real :: x
    end type

    template pos_t {a_t, b_t, p}
        deferred type :: a_t, b_t
        deferred interface
            function p(x)
                type(a_t), intent(in) :: x
                type(b_t) :: p
            end function
        end interface
        interface operator(+)
            module procedure p
        end interface
    contains
        subroutine s(x, y)
            type(a_t), intent(in) :: x
            type(b_t), intent(out) :: y
            y = +x
        end subroutine
    end template

    ! An elemental `operator(+)` applies to each element of an array.
    template pos_arr_t {t, q}
        deferred type :: t
        deferred interface
            elemental function q(x)
                type(t), intent(in) :: x
                type(t) :: q
            end function
        end interface
        interface operator(+)
            module procedure q
        end interface
    contains
        subroutine s(x, y)
            type(t), intent(in) :: x(:)
            type(t), intent(out) :: y(:)
            y = +x
        end subroutine
    end template
contains
    function pos_real(i) result(r)
        integer, intent(in) :: i
        real :: r
        r = real(i) + 0.5
    end function

    function pos_vec(v) result(r)
        type(vec), intent(in) :: v
        type(vec) :: r
        r%x = 2*v%x
    end function

    elemental function pos_int(i) result(r)
        integer, intent(in) :: i
        integer :: r
        r = i + 10
    end function

    elemental function pos_vec_e(v) result(r)
        type(vec), intent(in) :: v
        type(vec) :: r
        r%x = 3*v%x
    end function
end module

program template_unary_plus_01
    use template_unary_plus_01_m
    implicit none
    instantiate pos_t {integer, real, pos_real}, only: s_int_real => s
    instantiate pos_t {vec, vec, pos_vec}, only: s_vec => s
    instantiate pos_arr_t {integer, pos_int}, only: s_int_arr => s
    instantiate pos_arr_t {vec, pos_vec_e}, only: s_vec_arr => s
    real :: y
    type(vec) :: v, w, va(2), wa(2)
    integer :: i(3), j(3)

    call s_int_real(3, y)
    if (abs(y - 3.5) > 1e-6) error stop 1
    v%x = 2.5
    call s_vec(v, w)
    if (abs(w%x - 5.0) > 1e-6) error stop 2
    i = [1, 2, 3]
    call s_int_arr(i, j)
    if (any(j /= [11, 12, 13])) error stop 3
    va%x = [1.5, 2.5]
    call s_vec_arr(va, wa)
    if (any(abs(wa%x - [4.5, 7.5]) > 1e-6)) error stop 4
    print *, "OK"
end program
