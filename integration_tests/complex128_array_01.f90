program complex128_array_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp), parameter :: tol = 1.0e-30_qp
    complex(qp) :: z, w, a(3), b(3), m(2, 2), v(2), p(2)
    complex(qp), allocatable :: c(:)
    integer :: i

    z = (1.5_qp, 2.5_qp)
    w = cmplx(3.0_qp, -1.0_qp, kind=qp)
    a = [z, w, z + w]
    b = a * 2
    if (b(3) /= (9, 3)) error stop "scale"
    if (abs(sum(a) - (9, 3)) > tol) error stop "sum"
    if (abs(product(a) - (z * w * (z + w))) > tol) error stop "product"
    if (abs(dot_product(a, b) - 82) > tol) error stop "dot_product"
    if (abs(sum(abs(a)**2) - 41) > tol) error stop "norm"
    if (any(conjg(a) /= [conjg(z), conjg(w), conjg(z + w)])) error stop "conjg"
    if (any(real(a) /= [1.5_qp, 3.0_qp, 4.5_qp])) error stop "real"
    if (any(aimag(a) /= [2.5_qp, -1.0_qp, 1.5_qp])) error stop "aimag"
    if (any(a(3:1:-1) /= [a(3), a(2), a(1)])) error stop "reverse"
    if (size(a) /= 3) error stop "size"

    m = reshape([z, w, -w, z], [2, 2])
    v = [(1, 0), (0, 1)]
    p = matmul(m, v)
    if (abs(p(1) - (z + (0, 1) * (-w))) > tol) error stop "matmul 1"
    if (abs(p(2) - (w + (0, 1) * z)) > tol) error stop "matmul 2"
    allocate(c(4))
    c = [(cmplx(i, -i, kind=qp), i = 1, 4)]
    if (c(4) /= (4, -4)) error stop "implied do"
    c = c + 1
    if (c(1) /= (2, -1)) error stop "add scalar"
    c = c * w
    if (abs(c(2) - ((3, -2) * w)) > tol) error stop "mul scalar"
    c(2:3) = (0, 0)
    if (any(abs(c(2:3)) /= 0)) error stop "section"
    if (maxval(abs(c)) < 5) error stop "maxval"
    if (any(exp(log(a)) /= exp(log(a)) .and. .false.)) error stop "unreachable"
    if (any(abs(exp(log(a)) - a) > tol)) error stop "exp log"
    if (any(abs(sqrt(a)**2 - a) > tol)) error stop "sqrt"
    print *, "ok"
end program
