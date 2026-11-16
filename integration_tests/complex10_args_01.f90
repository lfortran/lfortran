module complex10_args_01_mod
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
contains
    function make(re, im) result(r)
        real(xp), intent(in) :: re, im
        complex(xp) :: r
        r = cmplx(re, im, kind=xp)
    end function

    complex(xp) function scale(z, f)
        complex(xp), intent(in) :: z
        real(xp), intent(in) :: f
        scale = z * f
    end function

    subroutine split(z, re, im)
        complex(xp), intent(in) :: z
        real(xp), intent(out) :: re, im
        re = real(z, xp)
        im = aimag(z)
    end subroutine

    subroutine swap_parts(z, w)
        complex(xp), intent(out) :: z
        complex(xp), intent(in) :: w
        z = cmplx(aimag(w), real(w, xp), kind=xp)
    end subroutine

    subroutine bump(z)
        complex(xp), intent(inout) :: z
        z = z + (1, 1)
    end subroutine

    subroutine by_value(z, r)
        complex(xp), value :: z
        complex(xp), intent(out) :: r
        z = z * 2
        r = z
    end subroutine

    function arr_result(n) result(a)
        integer, intent(in) :: n
        complex(xp) :: a(n)
        integer :: i
        do i = 1, n
            a(i) = cmplx(i, -i, kind=xp)
        end do
    end function

    subroutine optional_arg(z, r, extra)
        complex(xp), intent(in) :: z
        complex(xp), intent(out) :: r
        complex(xp), intent(in), optional :: extra
        r = z
        if (present(extra)) r = r + extra
    end subroutine

    subroutine apply(f, z, r)
        interface
            function f(x) result(y)
                import :: xp
                complex(xp), intent(in) :: x
                complex(xp) :: y
            end function
        end interface
        complex(xp), intent(in) :: z
        complex(xp), intent(out) :: r
        r = f(z)
    end subroutine

    function twice(x) result(y)
        complex(xp), intent(in) :: x
        complex(xp) :: y
        y = 2 * x
    end function
end module

program complex10_args_01
    use complex10_args_01_mod
    implicit none
    complex(xp) :: z, w, r
    complex(xp) :: a(3)
    real(xp) :: re, im

    z = make(1.5_xp, -2.5_xp)
    if (z /= (1.5_xp, -2.5_xp)) error stop "make"
    if (scale(z, 2.0_xp) /= (3.0_xp, -5.0_xp)) error stop "scale"
    if (scale(make(1.0_xp, 1.0_xp), 0.5_xp) /= (0.5_xp, 0.5_xp)) error stop "nested"

    call split(z, re, im)
    if (re /= 1.5_xp .or. im /= -2.5_xp) error stop "split"

    w = (9, 9)
    call swap_parts(w, z)
    if (w /= (-2.5_xp, 1.5_xp)) error stop "intent(out)"

    call bump(z)
    if (z /= (2.5_xp, -1.5_xp)) error stop "inout"

    call by_value(z, r)
    if (r /= (5.0_xp, -3.0_xp)) error stop "value result"
    if (z /= (2.5_xp, -1.5_xp)) error stop "value must not modify actual"

    a = arr_result(3)
    if (a(3) /= (3, -3)) error stop "array result"
    if (sum(a) /= (6, -6)) error stop "array result sum"

    call optional_arg(z, r)
    if (r /= z) error stop "optional absent"
    call optional_arg(z, r, cmplx(1, 1, kind=xp))
    if (r /= (3.5_xp, -0.5_xp)) error stop "optional present"

    call apply(twice, z, r)
    if (r /= (5.0_xp, -3.0_xp)) error stop "procedure argument"
    print *, "ok"
end program
