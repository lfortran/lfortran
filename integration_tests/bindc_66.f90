! An absent optional dummy passed on to an optional dummy of a bind(c)
! procedure is absent there too
module bindc_66_mod
    use iso_c_binding, only: c_int, c_double, c_ptr, c_loc, c_associated
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

    subroutine opt_double(p, r) bind(c)
        real(c_double), intent(inout), optional :: p
        integer(c_int), intent(out) :: r
        r = -1
        if (present(p)) then
            r = int(p)
            p = 8
        end if
    end subroutine

    integer(c_int) function opt_struct(t) bind(c)
        type(pt), intent(in), optional :: t
        opt_struct = -1
        if (present(t)) opt_struct = t%y
    end function

    integer(c_int) function opt_cptr(p) bind(c)
        type(c_ptr), optional :: p
        opt_cptr = -1
        if (present(p)) opt_cptr = merge(1, 0, c_associated(p))
    end function

    integer function fwd_int(p)
        integer(c_int), intent(in), optional :: p
        fwd_int = opt_int(p) + 100 * opt_int(p)
    end function

    integer function fwd_double(p)
        real(c_double), intent(inout), optional :: p
        integer(c_int) :: r
        call opt_double(p, r)
        fwd_double = r
    end function

    integer function fwd_struct(t)
        type(pt), intent(in), optional :: t
        fwd_struct = opt_struct(t)
    end function

    integer function fwd_cptr(p)
        type(c_ptr), optional :: p
        fwd_cptr = opt_cptr(p)
    end function

    integer function fwd_twice(p)
        integer(c_int), intent(in), optional :: p
        fwd_twice = fwd_int(p)
    end function
end module

program bindc_66
    use bindc_66_mod
    implicit none
    integer(c_int), target :: z
    real(c_double) :: d
    type(pt) :: s
    type(c_ptr) :: q

    if (fwd_int(7) /= 707) error stop 1
    if (fwd_int() /= -101) error stop 2

    d = 3
    if (fwd_double(d) /= 3) error stop 3
    if (d /= 8) error stop 4
    if (fwd_double() /= -1) error stop 5

    s = pt(1, 9)
    if (fwd_struct(s) /= 9) error stop 6
    if (fwd_struct() /= -1) error stop 7

    q = c_loc(z)
    if (fwd_cptr(q) /= 1) error stop 8
    if (fwd_cptr() /= -1) error stop 9

    if (fwd_twice(7) /= 707) error stop 10
    if (fwd_twice() /= -101) error stop 11
    print *, "ok"
end program
