program complex16_param_fold_02
! Constant folding of complex(16) elemental intrinsics (#17826)
implicit none
integer, parameter :: qp = selected_real_kind(33)
real(qp), parameter :: tol = 1e-32_qp
complex(qp), parameter :: a = (1, 1), b = (0.5_qp, 0.5_qp)
real(qp), parameter :: r = abs((3.0_qp, 4.0_qp))
complex(qp), parameter :: z(15) = [sqrt((-4.0_qp, 0.0_qp)), exp(a), log(a), &
    sin(a), cos(a), tan(a), sinh(a), cosh(a), tanh(a), asin(b), acos(b), &
    atan(b), asinh(b), acosh((1.5_qp, 0.5_qp)), atanh(b)]
complex(qp), parameter :: ref(15) = [ &
    (0.0_qp, 2.0_qp), &
    (1.4686939399158851571389675973266042e0_qp, 2.2873552871788423912081719067005019e0_qp), &
    (3.4657359027997265470861606072908829e-1_qp, 7.8539816339744830961566084581987570e-1_qp), &
    (1.2984575814159772948260423658078155e0_qp, 6.3496391478473610825508220299150976e-1_qp), &
    (8.3373002513114904888388539433509448e-1_qp, -9.8889770576286509638212954089268614e-1_qp), &
    (2.7175258531951171652884372249858893e-1_qp, 1.0839233273386945434757520612119718e0_qp), &
    (6.3496391478473610825508220299150976e-1_qp, 1.2984575814159772948260423658078155e0_qp), &
    (8.3373002513114904888388539433509448e-1_qp, 9.8889770576286509638212954089268614e-1_qp), &
    (1.0839233273386945434757520612119718e0_qp, 2.7175258531951171652884372249858893e-1_qp), &
    (4.5227844715119068206365839783097935e-1_qp, 5.3063753095251782601650945810678671e-1_qp), &
    (1.1185178796437059371676632938087721e0_qp, -5.3063753095251782601650945810678671e-1_qp), &
    (5.5357435889704525150853273008926852e-1_qp, 4.0235947810852509365018983330654690e-1_qp), &
    (5.3063753095251782601650945810678671e-1_qp, 4.5227844715119068206365839783097935e-1_qp), &
    (1.0693110431581105399815081282200815e0_qp, 3.9963906733652414187687552882437464e-1_qp), &
    (4.0235947810852509365018983330654690e-1_qp, 5.5357435889704525150853273008926852e-1_qp)]
! Elemental intrinsics of a complex(16) array constant
complex(qp), parameter :: e2(2) = exp([(0.0_qp, 0.0_qp), (1.0_qp, 1.0_qp)])
real(qp), parameter :: r2(2) = abs([(3.0_qp, 4.0_qp), (0.0_qp, -2.0_qp)])
! Exact conversion and selection of complex(16) values
complex(qp), parameter :: c8 = cmplx((0.1_8, -0.3_8), kind=qp)
complex(qp), parameter :: c4 = cmplx((0.5_4, -0.25_4), kind=qp)
complex(qp), parameter :: m = merge((1.0_qp, 2.0_qp), (3.0_qp, 4.0_qp), .true.)
integer :: i
print '(ES44.34)', r
print '(2ES44.34)', z
if (abs(r - 5.0_qp) > tol) error stop 1
do i = 1, 15
    if (abs(z(i) - ref(i)) > tol * abs(ref(i))) error stop 2
end do
if (abs(e2(1) - (1.0_qp, 0.0_qp)) > tol) error stop 3
if (abs(e2(2) - ref(2)) > tol * abs(ref(2))) error stop 4
if (abs(r2(1) - 5.0_qp) > tol .or. abs(r2(2) - 2.0_qp) > tol) error stop 5
if (c8 /= cmplx(0.1_8, -0.3_8, kind=qp)) error stop 6
if (c4 /= (0.5_qp, -0.25_qp)) error stop 7
if (m /= (1.0_qp, 2.0_qp)) error stop 8
end program
