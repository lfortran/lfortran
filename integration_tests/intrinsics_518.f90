program intrinsics_518
implicit none
! real(16) parameter intrinsics must fold as binary128 constants (#15730).
integer, parameter :: qp = selected_real_kind(33)
real(qp), parameter :: tol = 1.0e-30_qp
real(qp), parameter :: g = gamma(2.5_qp)
real(qp), parameter :: lg = log_gamma(2.5_qp)
real(qp), parameter :: g1 = gamma(1.0_qp)
real(qp), parameter :: g2 = gamma(2.0_qp)
real(qp), parameter :: g3 = gamma(3.0_qp)
real(qp), parameter :: g5 = gamma(5.0_qp)
real(qp), parameter :: g6 = gamma(6.0_qp)
real(qp), parameter :: g10 = gamma(10.0_qp)
real(qp), parameter :: lg1 = log_gamma(1.0_qp)
real(qp), parameter :: lg2 = log_gamma(2.0_qp)
real(qp), parameter :: lg3 = log_gamma(3.0_qp)
real(qp), parameter :: lg5 = log_gamma(5.0_qp)
real(qp), parameter :: lg10 = log_gamma(10.0_qp)
real(qp), parameter :: h = hypot(3.0_qp, 4.0_qp)
real(qp), parameter :: j0 = bessel_j0(0.5_qp)
real(qp), parameter :: j1 = bessel_j1(0.5_qp)
real(qp), parameter :: y0 = bessel_y0(0.5_qp)
real(qp), parameter :: jn = bessel_jn(2, 0.5_qp)
real(qp), parameter :: j0_20 = bessel_j0(20.0_qp)
real(qp), parameter :: j0_100 = bessel_j0(100.0_qp)
real(qp), parameter :: j1_20 = bessel_j1(20.0_qp)
real(qp), parameter :: y0_20 = bessel_y0(20.0_qp)
real(qp), parameter :: jn_20 = bessel_jn(5, 20.0_qp)
real(qp), parameter :: jn_big = bessel_jn(3000, 3000.0_qp)
real(qp), parameter :: es = erfc_scaled(1.0_qp)

if (abs(g  - 1.32934038817913702047362561250585890_qp) > tol) error stop
if (abs(lg - 0.284682870472919159632494669682701933_qp) > tol) error stop
if (g1 /= 1.0_qp) error stop
if (g2 /= 1.0_qp) error stop
if (g3 /= 2.0_qp) error stop
if (g5 /= 24.0_qp) error stop
if (g6 /= 120.0_qp) error stop
if (g10 /= 362880.0_qp) error stop
if (lg1 /= log(1.0_qp)) error stop
if (lg2 /= log(1.0_qp)) error stop
if (lg3 /= log(2.0_qp)) error stop
if (lg5 /= log(24.0_qp)) error stop
if (lg10 /= log(362880.0_qp)) error stop
if (abs(h  - 5.0_qp) > tol) error stop
if (abs(j0 - 0.938469807240812904228404673599712612_qp) > tol) error stop
if (abs(j1 - 0.242268457674873886383954576141531647_qp) > tol) error stop
if (abs(y0 - (-0.444518733506706557148398475068331923_qp)) > tol) error stop
if (abs(jn - 3.06040234586826413074136309664139389e-2_qp) > tol) error stop
if (abs(j0_20 - 0.16702466434058315472732054470138404_qp) > tol) error stop
if (abs(j0_100 - 1.9985850304223122424228390950848991e-2_qp) > tol) error stop
if (abs(j1_20 - 0.066833124175850045578992974193646718_qp) > tol) error stop
if (abs(y0_20 - 0.062640596809383831161729014105358175_qp) > tol) error stop
if (abs(jn_20 - 0.15116976798239497460710045572485228_qp) > tol) error stop
if (jn_big == 0.0_qp) error stop
if (abs(jn_big - 0.031014547810126577726003088210359804_qp) > tol) error stop
if (abs(es - 0.427583576155807004410750344490515188_qp) > tol) error stop
end program
