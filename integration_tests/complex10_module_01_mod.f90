module complex10_module_01_mod
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
    integer :: i
    complex(xp), parameter :: zero_cxp = (0._xp, 0._xp)
    complex(xp), parameter :: i_cxp = cmplx(0, 1, kind=xp)
    complex(xp), parameter :: c1 = (0.1_xp, -0.3_xp)
    complex(xp), parameter :: c2 = cmplx(0.1_xp, 2, xp)
    complex(xp), parameter :: c3 = c1
    complex(xp), parameter :: c4 = (1, 2)
    complex(xp), save :: g = (1._xp, 2._xp)
    real(xp), parameter :: pi = 3.14159265358979323846264338327950288_xp
    real(xp), parameter :: rarr(3) = [(10._xp**(3-i), i=1,3)]
    complex(xp), parameter :: carr(2) = [(1._xp, 2._xp), (3._xp, 4._xp)]
    real(xp), parameter :: rcat(5) = [rarr, 7._xp, 8._xp]
    complex(xp), parameter :: ccat(3) = [carr, (5._xp, 6._xp)]
contains
    elemental function twice(z) result(r)
        complex(xp), intent(in) :: z
        complex(xp) :: r
        r = 2 * z
    end function
    function make(re, im) result(r)
        real(xp), intent(in) :: re, im
        complex(xp) :: r
        r = cmplx(re, im, kind=xp)
    end function
end module
