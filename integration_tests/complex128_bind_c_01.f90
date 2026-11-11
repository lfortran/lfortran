module complex128_bind_c_01_mod
    use iso_c_binding, only: c_int
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
contains
    function c_add(a, b) result(r) bind(c, name="c128_add")
        complex(qp), value :: a, b
        complex(qp) :: r
        r = a + b
    end function

    subroutine c_inc(z) bind(c, name="c128_inc")
        complex(qp), intent(inout) :: z
        z = z + (1, 1)
    end subroutine

    subroutine c_sum(n, a, s) bind(c, name="c128_sum")
        integer(c_int), value :: n
        complex(qp), intent(in) :: a(n)
        complex(qp), intent(out) :: s
        integer :: i
        s = 0
        do i = 1, n
            s = s + a(i)
        end do
    end subroutine
end module

program complex128_bind_c_01
    use complex128_bind_c_01_mod
    use iso_c_binding, only: c_int
    implicit none
    complex(qp) :: z, a(3), s

    z = c_add((1.5_qp, 2.5_qp), (-0.5_qp, 1.0_qp))
    if (z /= (1.0_qp, 3.5_qp)) error stop "value args and result"
    call c_inc(z)
    if (z /= (2.0_qp, 4.5_qp)) error stop "by reference"
    a = [(1, 1), (2, 2), (3, 3)]
    call c_sum(3_c_int, a, s)
    if (s /= (6, 6)) error stop "array"
    print *, "ok"
end program
