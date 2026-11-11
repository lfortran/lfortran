program intrinsics_518
implicit none
! real(16) parameter intrinsics must fold as binary128 constants (#15730).
integer, parameter :: qp = selected_real_kind(33)
real(qp), parameter :: tol = 1.0e-30_qp
real(qp), parameter :: g = gamma(2.5_qp)
real(qp), parameter :: lg = log_gamma(2.5_qp)
real(qp), parameter :: h = hypot(3.0_qp, 4.0_qp)
real(qp), parameter :: j0 = bessel_j0(0.5_qp)
real(qp), parameter :: j1 = bessel_j1(0.5_qp)
real(qp), parameter :: y0 = bessel_y0(0.5_qp)
real(qp), parameter :: jn = bessel_jn(2, 0.5_qp)
real(qp), parameter :: es = erfc_scaled(1.0_qp)

if (abs(g  - 1.32934038817913702047362561250585890_qp) > tol) error stop
if (abs(lg - 0.284682870472919159632494669682701933_qp) > tol) error stop
if (abs(h  - 5.0_qp) > tol) error stop
if (abs(j0 - 0.938469807240812904228404673599712612_qp) > tol) error stop
if (abs(j1 - 0.242268457674873886383954576141531647_qp) > tol) error stop
if (abs(y0 - (-0.444518733506706557148398475068331923_qp)) > tol) error stop
if (abs(jn - 3.06040234586826413074136309664139389e-2_qp) > tol) error stop
if (abs(es - 0.427583576155807004410750344490515188_qp) > tol) error stop
end program
