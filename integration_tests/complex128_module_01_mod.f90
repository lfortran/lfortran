module complex128_module_01_mod
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    integer :: i
    complex(qp), parameter :: zero_cqp = (0._qp, 0._qp)
    complex(qp), parameter :: i_cqp = cmplx(0, 1, kind=qp)
    complex(qp), parameter :: c1 = (0.1_qp, -0.3_qp)
    complex(qp), parameter :: c2 = cmplx(0.1_qp, 2, qp)
    complex(qp), parameter :: c3 = c1
    complex(qp), parameter :: c4 = (1, 2)
    complex(qp), save :: g = (1._qp, 2._qp)
    real(qp), parameter :: pi = 3.14159265358979323846264338327950288_qp
    real(qp), parameter :: sqrt_2_pi = sqrt(2.0_qp*acos(-1.0_qp))
    real(qp), parameter :: e_qp = exp(1.0_qp)
    real(qp), parameter :: rarr(3) = [(10._qp**(3-i), i=1,3)]
    complex(qp), parameter :: carr(2) = [(1._qp, 2._qp), (3._qp, 4._qp)]
    real(qp), parameter :: rcat(5) = [rarr, 7._qp, 8._qp]
    complex(qp), parameter :: ccat(3) = [carr, (5._qp, 6._qp)]
contains
    elemental function twice(z) result(r)
        complex(qp), intent(in) :: z
        complex(qp) :: r
        r = 2 * z
    end function
    function make(re, im) result(r)
        real(qp), intent(in) :: re, im
        complex(qp) :: r
        r = cmplx(re, im, kind=qp)
    end function
end module
