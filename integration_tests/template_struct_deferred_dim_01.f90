! A deferred integer constant used as the shape of a derived-type component
! inside a template. After instantiation the component must be a fixed-size
! array stored inline in the derived type, as it would be outside a template.

module template_struct_deferred_dim_01_m
    implicit none

    template tm {t, plus_t, n}
        deferred type :: t
        deferred integer, parameter :: n
        deferred interface
            elemental function plus_t(x, y) result(r)
                type(t), intent(in) :: x, y
                type(t) :: r
            end function
        end interface

        type :: mat
            type(t) :: e(n, n)
        end type

        type :: vec
            type(t) :: v(n)
        end type

        type :: vec0
            type(t) :: b(0:n)
        end type

        type :: vec2
            type(t) :: b(2:n)
        end type
    contains
        elemental function plus_mat(x, y) result(r)
            type(mat), intent(in) :: x, y
            type(mat) :: r
            integer :: i, j
            do i = 1, n
                do j = 1, n
                    r%e(i, j) = plus_t(x%e(i, j), y%e(i, j))
                end do
            end do
        end function
    end template
end module

program template_struct_deferred_dim_01
    use template_struct_deferred_dim_01_m
    implicit none
    integer, parameter :: n = 2, m = 3
    instantiate tm {real, operator(+), n}, only: rmat => mat, rplus => plus_mat
    instantiate tm {integer, operator(+), m}, only: ivec => vec, &
        ivec0 => vec0, ivec2 => vec2
    type(rmat) :: a, b, c
    type(ivec) :: u
    type(ivec0) :: w0, w0b
    type(ivec2) :: w2, w2b

    a%e = 0
    a%e(1, 1) = 1.5
    b%e = 1
    c = rplus(a, b)
    print *, c%e
    if (size(c%e) /= 4) error stop
    if (any(abs(c%e - reshape([2.5, 1.0, 1.0, 1.0], [2, 2])) > 1e-6)) error stop

    u%v = [4, 5, 6]
    print *, u%v
    if (size(u%v) /= 3) error stop
    if (any(u%v /= [4, 5, 6])) error stop

    w0%b = [1, 2, 3, 4]
    print *, w0%b
    if (lbound(w0%b, 1) /= 0 .or. ubound(w0%b, 1) /= 3) error stop
    if (any(w0%b /= [1, 2, 3, 4])) error stop
    w0b = w0
    if (any(w0b%b /= [1, 2, 3, 4])) error stop

    w2%b = [7, 8]
    print *, w2%b
    if (lbound(w2%b, 1) /= 2 .or. ubound(w2%b, 1) /= 3) error stop
    if (any(w2%b /= [7, 8])) error stop
    w2b = w2
    if (any(w2b%b /= [7, 8])) error stop
end program

