program real128_array_const_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    integer :: i
    real(qp), parameter :: tol = 1.0e-30_qp
    real(qp), parameter :: x(4) = [1.5_qp, -2.25_qp, 3.125_qp, 1.0e-40_qp]
    real(qp), parameter :: y(5) = [(real(i, qp) / 4, i = 1, 5)]
    real(qp), parameter :: z(6) = [x, 7.0_qp, 8.0_qp]
    real(qp), parameter :: w(3) = [1.0_8, 2.0_8, 4.0_8]
    real(qp), parameter :: v(3) = [1, 2, 3]
    real(qp), parameter :: s(2) = z(2:3)
    complex(qp), parameter :: c(3) = [(1.5_qp, -1.0_qp), (0.0_qp, 2.25_qp), (1.0e-40_qp, 3.0_qp)]
    complex(qp), parameter :: d(4) = [(cmplx(i, -i, kind=qp), i = 1, 4)]
    complex(qp), parameter :: e(5) = [c, d(1:2)]
    complex(qp), parameter :: f(2) = [(1.0_8, 2.0_8), (3.0_8, 4.0_8)]
    complex(qp), parameter :: g(2) = [1.0_qp, 2.0_qp]
    real(qp), parameter :: re(3) = real(c)
    real(qp), parameter :: im(3) = aimag(c)
    real(qp), parameter :: tail = z(4)
    complex(qp) :: ctail
    real(qp) :: a(4), b(5)
    complex(qp) :: h(3)

    if (size(x) /= 4 .or. size(z) /= 6 .or. size(e) /= 5) error stop "size"
    if (x(1) /= 1.5_qp .or. x(2) /= -2.25_qp .or. x(3) /= 3.125_qp) error stop "x"
    if (abs(x(4) - 1.0e-40_qp) > 1.0e-70_qp) error stop "x tiny"
    if (x(4) == 0.0_qp) error stop "x tiny not zero"
    if (any(y /= [0.25_qp, 0.5_qp, 0.75_qp, 1.0_qp, 1.25_qp])) error stop "y"
    if (abs(sum(y) - 3.75_qp) > tol) error stop "sum y"
    if (z(5) /= 7.0_qp .or. z(6) /= 8.0_qp .or. z(1) /= 1.5_qp) error stop "z"
    if (tail /= x(4)) error stop "tail"
    if (any(w /= [1.0_qp, 2.0_qp, 4.0_qp])) error stop "w"
    if (any(v /= [1.0_qp, 2.0_qp, 3.0_qp])) error stop "v"
    if (s(1) /= -2.25_qp .or. s(2) /= 3.125_qp) error stop "s"
    if (abs(sum(x(1:3)) - 2.375_qp) > tol) error stop "sum x"
    if (abs(product(y(1:2)) - 0.125_qp) > tol) error stop "product y"

    if (c(1) /= (1.5_qp, -1.0_qp) .or. c(2) /= (0.0_qp, 2.25_qp)) error stop "c"
    if (abs(real(c(3)) - 1.0e-40_qp) > 1.0e-70_qp) error stop "c tiny"
    if (d(4) /= (4.0_qp, -4.0_qp) .or. d(2) /= (2.0_qp, -2.0_qp)) error stop "d"
    if (e(3) /= c(3) .or. e(4) /= d(1) .or. e(5) /= d(2)) error stop "e"
    if (f(1) /= (1.0_qp, 2.0_qp) .or. f(2) /= (3.0_qp, 4.0_qp)) error stop "f"
    if (g(1) /= (1.0_qp, 0.0_qp) .or. g(2) /= (2.0_qp, 0.0_qp)) error stop "g"
    if (re(1) /= 1.5_qp .or. re(2) /= 0.0_qp) error stop "re"
    if (im(1) /= -1.0_qp .or. im(3) /= 3.0_qp) error stop "im"
    if (abs(sum(d) - (10.0_qp, -10.0_qp)) > tol) error stop "sum d"
    if (abs(product(d(1:2)) - (1.0_qp, -1.0_qp) * (2.0_qp, -2.0_qp)) > tol) error stop "product d"
    ctail = e(5)
    if (ctail /= (2.0_qp, -2.0_qp)) error stop "ctail"

    a = x
    if (any(a /= x)) error stop "copy x"
    b = y
    b(2:3) = [-1.0_qp, -2.0_qp]
    if (b(2) /= -1.0_qp .or. b(3) /= -2.0_qp .or. b(5) /= 1.25_qp) error stop "section"
    h = c
    h(2:3) = d(3:4)
    if (h(1) /= c(1) .or. h(2) /= (3.0_qp, -3.0_qp) .or. h(3) /= (4.0_qp, -4.0_qp)) error stop "h"
    h = [(cmplx(2 * i, i, kind=qp), i = 1, 3)]
    if (h(3) /= (6.0_qp, 3.0_qp)) error stop "implied do"
    print *, "ok"
end program
