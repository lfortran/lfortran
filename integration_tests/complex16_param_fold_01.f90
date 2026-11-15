program complex16_param_fold_01
! Constant folding of complex(16) division and exponentiation (#17826)
implicit none
integer, parameter :: qp = selected_real_kind(33)
real(qp), parameter :: tol = 1e-32_qp
complex(qp), parameter :: a = (1.0_qp, 2.0_qp) / (1.0_qp, 1.0_qp)
complex(qp), parameter :: b = (1.0_qp, 2.0_qp) / 2.0_qp
complex(qp), parameter :: c = 5.0_qp / (1.0_qp, 2.0_qp)
complex(qp), parameter :: d = (1.0_qp, 2.0_qp) ** 2
complex(qp), parameter :: e = (1.0_qp, 2.0_qp) ** 0
complex(qp), parameter :: f = (1.0_qp, 1.0_qp) ** (-2)
complex(qp), parameter :: g = (-3.0_qp, 4.0_qp) ** (0.5_qp, 0.0_qp)
complex(qp), parameter :: h = (1.0_qp, 2.0_qp) / (3.0_qp, 4.0_qp)
complex(qp), parameter :: p(2) = [(4.0_qp, 2.0_qp), (0.0_qp, 3.0_qp)] &
    / [(2.0_qp, 0.0_qp), (0.0_qp, 1.0_qp)]
complex(qp), parameter :: s = sum(p)
print '(2ES44.34)', a, b, c, d, e, f, g, h, p
if (abs(a - (1.5_qp, 0.5_qp)) > tol) error stop 1
if (abs(b - (0.5_qp, 1.0_qp)) > tol) error stop 2
if (abs(c - (1.0_qp, -2.0_qp)) > tol) error stop 3
if (abs(d - (-3.0_qp, 4.0_qp)) > tol) error stop 4
if (abs(e - (1.0_qp, 0.0_qp)) > tol) error stop 5
if (abs(f - (0.0_qp, -0.5_qp)) > tol) error stop 6
if (abs(g - (1.0_qp, 2.0_qp)) > 2 * tol) error stop 7
if (abs(h - (0.44_qp, 0.08_qp)) > tol) error stop 8
if (abs(p(1) - (2.0_qp, 1.0_qp)) > tol) error stop 9
if (abs(p(2) - (3.0_qp, 0.0_qp)) > tol) error stop 10
if (abs(s - (5.0_qp, 1.0_qp)) > tol) error stop 11
end program
