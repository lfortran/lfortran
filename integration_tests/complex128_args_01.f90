module complex128_args_01_mod
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
contains
    function make(re, im) result(r)
        real(qp), intent(in) :: re, im
        complex(qp) :: r
        r = cmplx(re, im, kind=qp)
    end function

    complex(qp) function scale(z, f)
        complex(qp), intent(in) :: z
        real(qp), intent(in) :: f
        scale = z * f
    end function

    subroutine split(z, re, im)
        complex(qp), intent(in) :: z
        real(qp), intent(out) :: re, im
        re = real(z, qp)
        im = aimag(z)
    end subroutine

    subroutine swap_parts(z, w)
        complex(qp), intent(out) :: z
        complex(qp), intent(in) :: w
        z = cmplx(aimag(w), real(w, qp), kind=qp)
    end subroutine

    subroutine bump(z)
        complex(qp), intent(inout) :: z
        z = z + (1, 1)
    end subroutine

    subroutine by_value(z, r)
        complex(qp), value :: z
        complex(qp), intent(out) :: r
        z = z * 2
        r = z
    end subroutine

    function arr_result(n) result(a)
        integer, intent(in) :: n
        complex(qp) :: a(n)
        integer :: i
        do i = 1, n
            a(i) = cmplx(i, -i, kind=qp)
        end do
    end function

    subroutine optional_arg(z, r, extra)
        complex(qp), intent(in) :: z
        complex(qp), intent(out) :: r
        complex(qp), intent(in), optional :: extra
        r = z
        if (present(extra)) r = r + extra
    end subroutine

    subroutine apply(f, z, r)
        interface
            function f(x) result(y)
                import :: qp
                complex(qp), intent(in) :: x
                complex(qp) :: y
            end function
        end interface
        complex(qp), intent(in) :: z
        complex(qp), intent(out) :: r
        r = f(z)
    end subroutine

    function twice(x) result(y)
        complex(qp), intent(in) :: x
        complex(qp) :: y
        y = 2 * x
    end function
end module

program complex128_args_01
    use complex128_args_01_mod
    implicit none
    complex(qp) :: z, w, r
    complex(qp) :: a(3)
    real(qp) :: re, im

    z = make(1.5_qp, -2.5_qp)
    if (z /= (1.5_qp, -2.5_qp)) error stop "make"
    if (scale(z, 2.0_qp) /= (3.0_qp, -5.0_qp)) error stop "scale"
    if (scale(make(1.0_qp, 1.0_qp), 0.5_qp) /= (0.5_qp, 0.5_qp)) error stop "nested"

    call split(z, re, im)
    if (re /= 1.5_qp .or. im /= -2.5_qp) error stop "split"

    w = (9, 9)
    call swap_parts(w, z)
    if (w /= (-2.5_qp, 1.5_qp)) error stop "intent(out)"

    call bump(z)
    if (z /= (2.5_qp, -1.5_qp)) error stop "inout"

    call by_value(z, r)
    if (r /= (5.0_qp, -3.0_qp)) error stop "value result"
    if (z /= (2.5_qp, -1.5_qp)) error stop "value must not modify actual"

    a = arr_result(3)
    if (a(3) /= (3, -3)) error stop "array result"
    if (sum(a) /= (6, -6)) error stop "array result sum"

    call optional_arg(z, r)
    if (r /= z) error stop "optional absent"
    call optional_arg(z, r, cmplx(1, 1, kind=qp))
    if (r /= (3.5_qp, -0.5_qp)) error stop "optional present"

    call apply(twice, z, r)
    if (r /= (5.0_qp, -3.0_qp)) error stop "procedure argument"
    print *, "ok"
end program
