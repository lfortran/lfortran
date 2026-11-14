program real16_parameter_array_item_01
    integer, parameter :: qp = 16
    real(qp), parameter :: r(3) = [1.0_qp, 2.0_qp, 3.5_qp]
    real(qp), parameter :: q = r(3)
    integer, parameter :: n = int(r(2))
    real(qp) :: a(int(r(3)))
    complex(qp), parameter :: w(2) = [(1.0_qp, 2.0_qp), (3.0_qp, 4.0_qp)]
    complex(qp), parameter :: y = w(2)
    real(qp), parameter :: yr = real(w(2)), yi = aimag(w(1))
    integer, parameter :: k = kind(r(2))
    if (q /= 3.5_qp) error stop 1
    if (n /= 2) error stop 2
    if (size(a) /= 3) error stop 3
    if (y /= (3.0_qp, 4.0_qp)) error stop 4
    if (yr /= 3.0_qp .or. yi /= 2.0_qp) error stop 5
    if (k /= 16) error stop 6
end program
