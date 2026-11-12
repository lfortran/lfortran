module array_constructor_18_mod
    implicit none
    integer, parameter :: wp = 16
    real(wp), parameter :: mp(3) = [1.5e-2, 2.5e-2, 3.5e-2]
    real(wp), parameter :: mi(3) = [1, 2, 3]
    real(wp), parameter :: md(3) = [1.25d0, 2.5d0, 3.75d0]
contains
    subroutine local_check()
        real(wp), dimension(3), parameter :: lp = [1.5e-2, 2.5e-2, 3.5e-2]
        real(wp), parameter :: li(3) = [4, 5, 6]
        real(wp), parameter :: ld(3) = [1.25d0, 2.5d0, 3.75d0]
        real(wp) :: np(3) = [1.5e-2, 2.5e-2, 3.5e-2]
        real(wp) :: ni(3) = [4, 5, 6]
        real(wp) :: nd(3) = [1.25d0, 2.5d0, 3.75d0]
        call check_f(lp)
        call check_i(li, 4)
        call check_d(ld)
        call check_f(np)
        call check_i(ni, 4)
        call check_d(nd)
    end subroutine
    subroutine check_f(a)
        real(wp), intent(in) :: a(3)
        real(wp) :: e(3)
        e = [real(1.5e-2, wp), real(2.5e-2, wp), real(3.5e-2, wp)]
        if (any(abs(a - e) > 1e-12_wp)) error stop "f"
    end subroutine
    subroutine check_i(a, b)
        real(wp), intent(in) :: a(3)
        integer, intent(in) :: b
        if (any(abs(a - [real(b, wp), real(b+1, wp), real(b+2, wp)]) > 1e-12_wp)) error stop "i"
    end subroutine
    subroutine check_d(a)
        real(wp), intent(in) :: a(3)
        if (any(abs(a - [1.25_wp, 2.5_wp, 3.75_wp]) > 1e-12_wp)) error stop "d"
    end subroutine
end module

program array_constructor_18
    use array_constructor_18_mod
    implicit none
    real(wp) :: vp(3) = [1.5e-2, 2.5e-2, 3.5e-2]
    real(wp) :: vi(3) = [1, 2, 3]
    real(wp) :: vd(3) = [1.25d0, 2.5d0, 3.75d0]
    call check_f(mp)
    call check_i(mi, 1)
    call check_d(md)
    call check_f(vp)
    call check_i(vi, 1)
    call check_d(vd)
    call local_check()
    print *, "ok"
end program
