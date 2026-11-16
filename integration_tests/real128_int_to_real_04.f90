program real128_int_to_real_04
    implicit none
    integer, parameter :: qp = 16, n = 4
    real(qp), parameter :: tol = 1e-30_qp
    real(qp), parameter :: a = 2 * 1.5_qp / 3
    real(qp), parameter :: b = real(n, qp) / 3
    real(qp), parameter :: c = n ** 0.5_qp
    real(qp), parameter :: d = 2 ** (-3.0_qp)
    real(qp), parameter :: e = n * a + 1 - 2 * b
    real(qp), parameter :: f = -n + 1.0_qp
    real(qp), parameter :: g(3) = [1, 2, 3] * 0.5_qp
    real(qp), parameter :: h = real(n * 2, qp) ** 2
    real(qp), parameter :: k = a ** n
    real(qp), parameter :: l = 3 - (n - 1) / 2.0_qp
    real(qp), parameter :: m = n / 8.0_qp + int(2.5_qp)
    integer, parameter :: ii = int(real(n, qp) * 2.5_qp)
    logical, parameter :: lt = real(n, qp) > 3
    real(qp) :: third

    third = 1.0_qp / 3.0_qp
    if (abs(a - 1.0_qp) > tol) error stop 1
    if (abs(b - 4.0_qp * third) > tol) error stop 2
    if (abs(c - 2.0_qp) > tol) error stop 3
    if (abs(d - 0.125_qp) > tol) error stop 4
    if (abs(e - (4.0_qp + 1.0_qp - 8.0_qp * third)) > tol) error stop 5
    if (abs(f + 3.0_qp) > tol) error stop 6
    if (abs(g(1) - 0.5_qp) > tol) error stop 7
    if (abs(g(2) - 1.0_qp) > tol) error stop 8
    if (abs(g(3) - 1.5_qp) > tol) error stop 9
    if (abs(h - 64.0_qp) > tol) error stop 10
    if (abs(k - 1.0_qp) > tol) error stop 11
    if (abs(l - 1.5_qp) > tol) error stop 12
    if (abs(m - 2.5_qp) > tol) error stop 13
    if (ii /= 10) error stop 14
    if (.not. lt) error stop 15
end program
