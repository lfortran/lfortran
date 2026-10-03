module template_operator_scope_02_values
    implicit none
    type :: sample
        real :: v
    end type
    interface operator(-)
        procedure subtract_sample
    end interface
contains
    elemental function subtract_sample(x, y) result(r)
        type(sample), intent(in) :: x, y
        type(sample) :: r
        r%v = x%v - y%v
    end function
end module

module template_operator_scope_02_m
    implicit none
    template tm {t, minus_t}
        deferred type :: t
        deferred interface
            elemental function minus_t(x, y) result(r)
                type(t), intent(in) :: x, y
                type(t) :: r
            end function
        end interface
        type :: box
            type(t) :: v
        end type
        interface operator(-)
            procedure minus_box
        end interface
    contains
        elemental function minus_box(x, y) result(r)
            type(box), intent(in) :: x, y
            type(box) :: r
            r%v = minus_t(x%v, y%v)
        end function
    end template

    template apply_t {t, op}
        deferred type :: t
        deferred interface
            elemental function op(x, y) result(r)
                type(t), intent(in) :: x, y
                type(t) :: r
            end function
        end interface
        interface operator(-)
            procedure op
        end interface
    contains
        elemental function apply(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r
            r = x - y
        end function
    end template
contains
    elemental real function add_real(x, y) result(r)
        real, intent(in) :: x, y
        r = x + y
    end function
end module

program template_operator_scope_02
    use template_operator_scope_02_values
    use template_operator_scope_02_m
    implicit none
    real :: a(2)
    instantiate tm {real, operator(-)}, only: rbox => box, rminus => minus_box
    instantiate tm {integer, operator(-)}, only: ibox => box, iminus => minus_box
    instantiate apply_t {sample, operator(-)}, only: sample_sub => apply
    instantiate apply_t {real, operator(-)}, only: real_sub => apply
    instantiate apply_t {real, add_real}, only: local_add => apply
    type(rbox) :: x, y, z
    type(ibox) :: ix, iy, iz
    type(sample) :: sx, sy, sz
    character(100) :: line
    real :: values(2)
    integer :: status

    a = [3.0, 5.0]
    print *, a - 1.0
    write(line, *) a - 1.0
    read(line, *, iostat=status) values
    if (status /= 0) error stop
    if (values(1) /= 2.0 .or. values(2) /= 4.0) error stop
    if (any(a - 1.0 /= [2.0, 4.0])) error stop

    x%v = 9.0
    y%v = 2.0
    z = rminus(x, y)
    if (z%v /= 7.0) error stop
    ix%v = 12
    iy%v = 4
    iz = iminus(ix, iy)
    if (iz%v /= 8) error stop
    sx%v = 13.0
    sy%v = 3.0
    sz = sx - sy
    if (sz%v /= 10.0) error stop
    sz = sample_sub(sx, sy)
    if (sz%v /= 10.0) error stop
    if (real_sub(9.0, 2.0) /= 7.0) error stop
    ! The template's local operator binding must not affect its caller.
    if (local_add(9.0, 2.0) /= 11.0) error stop
    if (9.0 - 2.0 /= 7.0) error stop
end program
