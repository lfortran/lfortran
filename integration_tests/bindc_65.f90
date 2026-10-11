! present() of an optional non-VALUE dummy of a bind(c) procedure checks
! whether the argument pointer is null, which is how an absent argument is
! passed, for every kind of dummy
module bindc_65_mod
    use iso_c_binding, only: c_int, c_double, c_char, c_ptr, c_loc, &
        c_null_ptr, c_associated
    implicit none
    type, bind(c) :: pt
        integer(c_int) :: x, y
    end type
contains
    integer(c_int) function opt_int(p) bind(c)
        integer(c_int), intent(in), optional :: p
        opt_int = -1
        if (present(p)) opt_int = p
    end function

    integer(c_int) function opt_double(p) bind(c)
        real(c_double), intent(inout), optional :: p
        opt_double = -1
        if (present(p)) then
            opt_double = int(p)
            p = 7
        end if
    end function

    integer(c_int) function opt_array(a) bind(c)
        integer(c_int), intent(in), optional :: a(3)
        opt_array = -1
        if (present(a)) opt_array = a(2)
    end function

    integer(c_int) function opt_adjustable(a, n) bind(c)
        integer(c_int), value :: n
        integer(c_int), intent(in), optional :: a(n)
        opt_adjustable = -1
        if (present(a)) opt_adjustable = sum(a)
    end function

    integer(c_int) function opt_assumed_shape(a) bind(c)
        integer(c_int), intent(in), optional :: a(:)
        opt_assumed_shape = -1
        if (present(a)) opt_assumed_shape = size(a)
    end function

    integer(c_int) function opt_struct(t) bind(c)
        type(pt), intent(in), optional :: t
        opt_struct = -1
        if (present(t)) opt_struct = t%y
    end function

    integer(c_int) function opt_char(c) bind(c)
        character(kind=c_char), intent(in), optional :: c
        opt_char = -1
        if (present(c)) opt_char = ichar(c)
    end function

    integer(c_int) function opt_cptr(p) bind(c)
        type(c_ptr), optional :: p
        opt_cptr = -1
        if (present(p)) opt_cptr = merge(1, 0, c_associated(p))
    end function

    integer(c_int) function opt_cptr_inout(p) bind(c)
        type(c_ptr), intent(inout), optional :: p
        opt_cptr_inout = -1
        if (present(p)) then
            opt_cptr_inout = merge(1, 0, c_associated(p))
            p = c_null_ptr
        end if
    end function
end module

program bindc_65
    use bindc_65_mod
    implicit none
    integer(c_int) :: z, x(3) = [4, 5, 6]
    real(c_double) :: d
    type(pt), target :: s
    type(c_ptr) :: q

    z = 0
    if (opt_int(z) /= 0) error stop 1
    z = 42
    if (opt_int(z) /= 42) error stop 2
    if (opt_int() /= -1) error stop 3

    d = 3
    if (opt_double(d) /= 3) error stop 4
    if (d /= 7) error stop 5
    if (opt_double() /= -1) error stop 6

    if (opt_array(x) /= 5) error stop 7
    if (opt_array() /= -1) error stop 8

    if (opt_adjustable(x, 3) /= 15) error stop 9
    if (opt_adjustable(n=3) /= -1) error stop 10

    if (opt_assumed_shape(x) /= 3) error stop 11
    if (opt_assumed_shape() /= -1) error stop 12

    s = pt(1, 9)
    if (opt_struct(s) /= 9) error stop 13
    if (opt_struct() /= -1) error stop 14

    if (opt_char('A') /= 65) error stop 15
    if (opt_char() /= -1) error stop 16

    q = c_null_ptr
    if (opt_cptr(q) /= 0) error stop 17
    q = c_loc(s)
    if (opt_cptr(q) /= 1) error stop 18
    if (opt_cptr() /= -1) error stop 19

    if (opt_cptr_inout(q) /= 1) error stop 20
    if (c_associated(q)) error stop 21
    if (opt_cptr_inout(q) /= 0) error stop 22
    if (opt_cptr_inout() /= -1) error stop 23
    print *, "ok"
end program
