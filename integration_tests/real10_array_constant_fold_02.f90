program real10_array_constant_fold_02
    ! real(10) array parameters keep full precision through a module file,
    ! and their reductions are folded with full precision
    use real10_array_constant_fold_02_mod, only: xp, c, s, z
    implicit none
    real(xp), parameter :: tol = 4 * epsilon(1.0_xp)
    real(xp), parameter :: c_sum = sum(c)
    real(xp), parameter :: c_prod = product(c)
    real(xp), parameter :: c_max = maxval(c)
    real(xp), parameter :: c_min = minval(c)
    real(xp), parameter :: c_dot = dot_product(c, c)
    real(xp), parameter :: c2 = c(2)
    real(xp), parameter :: c_norm2 = norm2(c)
    real(xp), parameter :: m(2, 2) = reshape([c, 1.0_xp], [2, 2])
    real(xp), parameter :: mm(2, 2) = matmul(m, m)
    integer, parameter :: imax(1) = maxloc(s), imin(1) = minloc(s)
    integer, parameter :: ifind(1) = findloc(c, c(2))
    integer, parameter :: n = size(s)
    integer, parameter :: sh(1) = shape(c)
    real(xp) :: x(3)

    if (n /= 4 .or. sh(1) /= 3) error stop "size"
    if (any(z /= 0.0_xp)) error stop "z"
    if (abs(c(1) - 0.333333333333333333342_xp) > tol) error stop "c(1)"
    if (abs(c2 - 0.666666666666666666685_xp) > tol) error stop "c2"
    if (abs(s(1) - 0.841470984807896506665_xp) > tol) error stop "s(1)"
    if (abs(s(4) + 0.756802495307928251372_xp) > tol) error stop "s(4)"
    if (abs(m(1, 2) - 1.33333333333333333337_xp) > 2 * tol) error stop "m"
    if (m(2, 2) /= 1.0_xp) error stop "m(2,2)"
    if (imax(1) /= 2 .or. imin(1) /= 4 .or. ifind(1) /= 2) error stop "loc"

    x = [1.0_xp, 2.0_xp, 4.0_xp]
    x = x / 3
    if (any(abs(c - x) > tol)) error stop "c runtime"
    if (abs(c_sum - sum(x)) > 4 * tol) error stop "sum"
    if (abs(c_sum - 2.33333333333333333326_xp) > 4 * tol) error stop "sum value"
    if (abs(c_prod - product(x)) > tol) error stop "product"
    if (abs(c_prod - 0.296296296296296296296_xp) > tol) error stop "product value"
    if (c_max /= x(3) .or. c_min /= x(1)) error stop "maxval minval"
    if (abs(c_dot - dot_product(x, x)) > 4 * tol) error stop "dot_product"
    if (abs(c_dot - 2.33333333333333333326_xp) > 4 * tol) error stop "dot_product value"
    if (abs(c_norm2 - norm2(x)) > 4 * tol) error stop "norm2"
    if (abs(c_norm2 - 1.52752523165194666890_xp) > 4 * tol) error stop "norm2 value"
    if (abs(mm(1, 1) - 1.0_xp) > 4 * tol) error stop "matmul 1 1"
    if (abs(mm(2, 2) - 1.88888888888888888891_xp) > 4 * tol) error stop "matmul 2 2"
    if (any(abs(mm - matmul(reshape([x, 1.0_xp], [2, 2]), reshape([x, 1.0_xp], [2, 2]))) &
            > 4 * tol)) error stop "matmul runtime"
    print *, c
    print *, s
    print *, c_sum, c_prod, c_dot
end program
