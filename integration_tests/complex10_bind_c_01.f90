module complex10_bind_c_01_mod
    use iso_c_binding, only: c_int
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
contains
    function c_add(a, b) result(r) bind(c, name="c80_add")
        complex(xp), value :: a, b
        complex(xp) :: r
        r = a + b
    end function

    subroutine c_inc(z) bind(c, name="c80_inc")
        complex(xp), intent(inout) :: z
        z = z + (1, 1)
    end subroutine

    subroutine c_sum(n, a, s) bind(c, name="c80_sum")
        integer(c_int), value :: n
        complex(xp), intent(in) :: a(n)
        complex(xp), intent(out) :: s
        integer :: i
        s = 0
        do i = 1, n
            s = s + a(i)
        end do
    end subroutine
end module

program complex10_bind_c_01
    use complex10_bind_c_01_mod
    use iso_c_binding, only: c_int
    implicit none
    complex(xp) :: z, a(3), s

    z = c_add((1.5_xp, 2.5_xp), (-0.5_xp, 1.0_xp))
    if (z /= (1.0_xp, 3.5_xp)) error stop "value args and result"
    call c_inc(z)
    if (z /= (2.0_xp, 4.5_xp)) error stop "by reference"
    a = [(1, 1), (2, 2), (3, 3)]
    call c_sum(3_c_int, a, s)
    if (s /= (6, 6)) error stop "array"
    print *, "ok"
end program
