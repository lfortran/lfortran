program real10_array_constant_fold_01
    ! real(10) array constant expressions are folded with full precision
    implicit none
    real(10), parameter :: tol = 4 * epsilon(1.0_10)
    real(10) :: v(2) = 0
    real(10) :: w(2) = 1.5_10
    real(10), parameter :: a(2) = 0.0_10
    real(10), parameter :: c(2) = [1.0_10, 2.0_10]
    real(10), parameter :: d_add(2) = c + 1.0_10
    real(10), parameter :: d_mul(2) = 2*[1.0_10, 2.0_10]
    real(10), parameter :: d_neg(2) = -[1.0_10, 2.0_10]
    real(10), parameter :: d_abs(2) = abs([1.0_10, -2.0_10])
    real(10), parameter :: d_sqrt(2) = sqrt([2.0_10, 4.0_10])
    real(10), parameter :: d_sin(2) = sin([1.0_10, -1.0_10])
    real(10), parameter :: d_atan2(2) = atan2([1.0_10, -1.0_10], 1.0_10)
    real(10), parameter :: d_max(2) = max([1.0_10, -2.0_10], 0.0_10)
    real(10), parameter :: d_hypot(2) = hypot([3.0_10, -3.0_10], 4.0_10)
    real(10), parameter :: d_atan2d(2) = atan2d([1.0_10, -1.0_10], 1.0_10)
    real(10), parameter :: d_sum = sum(c)
    real(10), parameter :: third(3) = [1.0_10, 2.0_10, 4.0_10] / 3
    real(10) :: x

    if (any(v /= 0.0_10)) error stop "v"
    if (any(w /= 1.5_10)) error stop "w"
    if (any(a /= 0.0_10)) error stop "a"
    if (d_add(1) /= 2.0_10 .or. d_add(2) /= 3.0_10) error stop "add"
    if (d_mul(1) /= 2.0_10 .or. d_mul(2) /= 4.0_10) error stop "mul"
    if (d_neg(1) /= -1.0_10 .or. d_neg(2) /= -2.0_10) error stop "neg"
    if (d_abs(1) /= 1.0_10 .or. d_abs(2) /= 2.0_10) error stop "abs"
    if (d_max(1) /= 1.0_10 .or. d_max(2) /= 0.0_10) error stop "max"
    if (d_sum /= 3.0_10) error stop "sum"
    if (d_sqrt(2) /= 2.0_10) error stop "sqrt 4"

    ! The folded values carry real(10), not just real(8), precision
    if (abs(d_sqrt(1) - 1.41421356237309504880_10) > tol) error stop "sqrt 2"
    if (abs(d_sin(1) - 0.841470984807896506665_10) > tol) error stop "sin 1"
    if (abs(d_sin(2) + 0.841470984807896506665_10) > tol) error stop "sin -1"
    if (abs(d_atan2(1) - 0.785398163397448309628_10) > tol) error stop "atan2 1"
    if (abs(d_atan2(2) + 0.785398163397448309628_10) > tol) error stop "atan2 -1"
    if (any(abs(d_hypot - 5.0_10) > tol)) error stop "hypot"
    if (abs(d_atan2d(1) - 45.0_10) > 45 * tol) error stop "atan2d 1"
    if (abs(d_atan2d(2) + 45.0_10) > 45 * tol) error stop "atan2d -1"
    if (abs(third(1) - 0.333333333333333333342_10) > tol) error stop "third 1"
    if (abs(third(3) - 1.33333333333333333337_10) > 2 * tol) error stop "third 3"

    ! Folded and run time values agree
    x = 1.0_10
    if (abs(d_sin(1) - sin(x)) > tol) error stop "sin runtime"
    if (abs(d_atan2(1) - atan2(x, x)) > tol) error stop "atan2 runtime"
    x = 2.0_10
    if (abs(d_sqrt(1) - sqrt(x)) > tol) error stop "sqrt runtime"
    if (abs(third(2) - x / 3) > tol) error stop "third runtime"

    v = d_sin
    w = -v
    if (w(1) /= -d_sin(1) .or. w(2) /= -d_sin(2)) error stop "copy"
    print *, d_sin
    print *, d_neg, d_sum
end program
